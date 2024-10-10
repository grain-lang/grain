/** Pattern Matching compiler, based on "Compiling Pattern Matching to Good Decision Trees"
    by Luc Maranget (http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf) */;

/* NOTE: Constant patterns are currently unsupported. */

open Sexplib.Conv;
open Grain_parsing;
open Grain_typed;
open Grain_utils;
open Types;
open Typedtree;
open Type_utils;
open Stdlib.Either;

/** The type for compiled match pattern decision trees.
    These trees are evaluated with respect to a stack of values. */

/* The following instruction(s) are extensions from the paper's ADT: */

[@deriving sexp]
type decision_tree =
  | /** Leaves of the decision tree. The int corresponds
      to the action number (i.e. switch branch) to be taken, followed by the
      expanded patterns of the branch that matched, followed by any addtional
      bindings needed to execute that branch. */
    Leaf(
      int,
      list(pattern),
      list((Ident.t, Ident.t)),
    )
  | /** Represents a guarded branch. The left tree corresponds to a successful
      guard check, and the right tree corresponds to a failed guard check.
      Similar to Leaf nodes, Guard nodes contain the expanded patterns of the
      potential matching row and any additional bindings which may be necessary
      to run the guard check. */
    Guard(
      match_branch,
      list(pattern),
      list((Ident.t, Ident.t)),
      decision_tree,
      decision_tree,
    )
  | /** Represents a conditional check on the current value. The alias is used
      for any alias bindings. The left tree corresponds to a true result, and
      the right tree corresponds to a false result. */
    Conditional(
      (equality_type, constant),
      Ident.t,
      decision_tree,
      decision_tree,
    )
  | /** Represents a failed match. */
    Fail
  | /** Multi-way test for an occurence. The list is a
      non-empty list of pairs of
      (constructor_tag, nested_decision_tree) tuples,
      with an optional default branch. */
    Switch(
      switch_type,
      option(Ident.t),
      list((int, decision_tree)),
      option(decision_tree),
    )
  | /** This is a control structure which swaps the item in the
      n'th position with the top of the value stack (where 'n' is the
      number saved in the node) */
    Swap(
      int,
      decision_tree,
    )
  | /** This instruction indicates that the first argument should be
      interpreted as a tuple and expanded into its contents.
      Arguments (kind of expansion, alias, rest of the tree) */
    Explode(
      matrix_type,
      Ident.t,
      decision_tree,
    )

and matrix_type =
  | TupleMatrix(int)
  | ArrayMatrix(option(int))
  | RecordMatrix(array(int))
  | ConstructorMatrix(option(int))
  | ConstantMatrix

and switch_type =
  | ConstantSwitch(constant)
  | ConstructorSwitch
  | ArraySwitch

and equality_type =
  | PhysicalEquality(wasm_repr)
  | StructuralEquality;

type conversion_result = {
  tree: decision_tree,
  branches: list((int, expression, pattern)),
};

/** Forward declarations */
let transl_anf_expression: ref(expression => Anftree.anf_expression) =
  ref(_ => failwith("forward decl"));
let transl_imm_expression:
  ref(expression => (Anftree.imm_expression, list(Anftree.anf_bind))) =
  ref(_ => failwith("forward decl"));
let transl_const:
  ref(
    (~loc: Location.t=?, ~env: Env.t=?, Asttypes.constant) =>
    Either.t(Anftree.imm_expression, (Ident.t, list(Anftree.anf_bind))),
  ) =
  ref((~loc=Location.dummy_loc, ~env=Env.empty, _) =>
    failwith("forward decl")
  );

/** Utilities */

/** Swaps the first and 'idx'-th elements of the given list */

let swap_list = (idx, lst) => {
  if (List.length(lst) == 0) {
    failwith("Impossible (swap_list): Cannot swap empty list");
  };
  List.mapi(
    (i, item) =>
      switch (i) {
      | 0 => List.nth(lst, idx)
      | index when index == idx => List.hd(lst)
      | _ => item
      },
    lst,
  );
};

/** Decision tree to ANF compilation (compiled trees evaluate to the
    corresponding label to be used in the computed goto) */;

type compiled_tree = Anftree.anf_expression;

/* Forward declaration to be populated by anf.ml */
let compile_constructor_tag: ref(constructor_tag => int) = (
  ref(_ =>
    failwith(
      "matchcomp: compile_constructor_tag: should be set by linearize.ml!",
    )
  ):
    ref(constructor_tag => int)
);

let rec pattern_always_matches = patt =>
  /* Precondition: Normalized */
  switch (patt.pat_desc) {
  | TPatAny
  | TPatVar(_) => true
  | TPatAlias(p, _, _) => pattern_always_matches(p)
  | _ => false
  };

let rec flatten_matrix = (size, cur, mtx) => {
  let rec flattened_rows = (row, binds) =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in flatten_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | TPatAlias(p, alias, _) =>
        flattened_rows([p, ...ptl], [(alias, cur), ...binds])
      | TPatVar(id, _) =>
        let wildcards = Parmatch.omegas(size);
        [(wildcards @ ptl, [(id, cur), ...binds])];
      | TPatTuple(args) => [(args @ ptl, binds)]
      | TPatRecord([(_, {lbl_all}, _), ..._] as ps, _) =>
        let patterns = Array.init(Array.length(lbl_all), _ => Parmatch.omega);
        List.iter(((_, ld, pat)) => patterns[ld.lbl_pos] = pat, ps);
        [(Array.to_list(patterns) @ ptl, binds)];
      | TPatOr(p1, p2) =>
        flattened_rows([p1, ...ptl], binds)
        @ flattened_rows([p2, ...ptl], binds)
      | _ when pattern_always_matches(p) =>
        let wildcards = Parmatch.omegas(size);
        [(wildcards @ ptl, binds)];
      | _ => [] /* Flattening for non-tuples and non-records generates no rows. */
      }
    };

  List.fold_right(
    ((row, binds, mb), rem) =>
      List.map(
        ((patts, binds)) => (patts, binds, mb),
        flattened_rows(row, binds),
      )
      @ rem,
    mtx,
    [],
  );
};

let rec matrix_type = {
  let rec get_kind =
    fun
    | TPatConstruct(_) => Some(ConstructorMatrix(None))
    | TPatTuple(ps) => Some(TupleMatrix(List.length(ps)))
    | TPatArray(_) => Some(ArrayMatrix(None))
    | TPatRecord([(_, ld, _), ..._], _) =>
      Some(RecordMatrix(Array.map(({lbl_pos}) => lbl_pos, ld.lbl_all)))
    | TPatRecord(_) =>
      failwith("Impossible: record definition with no fields")
    | TPatConstant(_) => Some(ConstantMatrix)
    | TPatAlias({pat_desc}, _, _) => get_kind(pat_desc)
    | TPatAny
    | TPatVar(_) => None
    | TPatOr({pat_desc: a}, {pat_desc: b}) =>
      switch (get_kind(a)) {
      | Some(_) as a => a
      | _ => get_kind(b)
      };
  fun
  | [] => None
  | [([{pat_desc}, ..._], _, _), ...rest] =>
    switch (get_kind(pat_desc)) {
    | Some(_) as hd => hd
    | None => matrix_type(rest)
    }
  | _ => failwith("Internal error: empty matrix row");
};
let matrix_type = mtx => {
  switch (matrix_type(mtx)) {
  | Some(ty) => ty
  // This should really only happen if the first column always matches,
  // a case which is avoided entirely in `compile_matrix`
  | None => failwith("Internal error: matrix_type unknown")
  };
};

module ConstructorSet =
  Set.Make({
    type t = constructor_description;
    let compare = Stdlib.compare;
  });

let rec pattern_head_constructors_aux = (p, acc) =>
  switch (p.pat_desc) {
  | TPatConstruct(_, cd, _) => ConstructorSet.add(cd, acc)
  | TPatAlias(p, _, _) => pattern_head_constructors_aux(p, acc)
  | TPatOr(p1, p2) =>
    pattern_head_constructors_aux(p1, pattern_head_constructors_aux(p2, acc))
  | _ => acc
  };

let pattern_head_constructors = patt =>
  pattern_head_constructors_aux(patt, ConstructorSet.empty);

let matrix_head_constructors = mtx => {
  let rec help = (mtx, acc) =>
    switch (mtx) {
    | [] => acc
    | [([], _, _), ..._] => failwith("Impossible: Empty pattern matrix")
    | [([p, ..._], _, mb), ...tl] =>
      help(tl, pattern_head_constructors_aux(p, acc))
    };
  ConstructorSet.elements @@ help(mtx, ConstructorSet.empty);
};

module IntSet =
  Set.Make({
    type t = int;
    let compare = Stdlib.compare;
  });

let rec pattern_head_arities_aux = (p, acc) =>
  switch (p.pat_desc) {
  | TPatArray(ps) => IntSet.add(List.length(ps), acc)
  | TPatAlias(p, _, _) => pattern_head_arities_aux(p, acc)
  | TPatOr(p1, p2) =>
    pattern_head_arities_aux(p1, pattern_head_arities_aux(p2, acc))
  | _ => acc
  };

let pattern_head_arities = patt =>
  pattern_head_arities_aux(patt, IntSet.empty);

let matrix_head_arities = mtx => {
  let rec help = (mtx, acc) =>
    switch (mtx) {
    | [] => acc
    | [([], _, _), ..._] => failwith("Impossible: Empty pattern matrix")
    | [([p, ..._], _, mb), ...tl] =>
      help(tl, pattern_head_arities_aux(p, acc))
    };
  IntSet.elements @@ help(mtx, IntSet.empty);
};

module ConstantSet =
  Set.Make({
    type t = constant;
    let compare = Stdlib.compare;
  });

let rec pattern_head_constants_aux = (p, acc) =>
  switch (p.pat_desc) {
  | TPatConstant(c) => ConstantSet.add(c, acc)
  | TPatAlias(p, _, _) => pattern_head_constants_aux(p, acc)
  | TPatOr(p1, p2) =>
    pattern_head_constants_aux(p1, pattern_head_constants_aux(p2, acc))
  | _ => acc
  };

let pattern_head_constants = patt =>
  pattern_head_constants_aux(patt, ConstantSet.empty);

let matrix_head_constants = mtx => {
  let rec help = (mtx, acc) =>
    switch (mtx) {
    | [] => acc
    | [([], _, _), ..._] => failwith("Impossible: Empty pattern matrix")
    | [([p, ..._], _, mb), ...tl] =>
      help(tl, pattern_head_constants_aux(p, acc))
    };
  ConstantSet.elements @@ help(mtx, ConstantSet.empty);
};

let make_matrix = branches =>
  /* Form matrix and assign each branch a number */
  List.mapi((i, {mb_pat} as mb) => ([mb_pat], [], (mb, i)), branches);

let matrix_width =
  fun
  | [] => raise(Not_found)
  | [(pats, _, _), ..._] => List.length(pats);

let rec col_is_wildcard = (mtx, col) =>
  switch (mtx) {
  | [] => true
  | [(pats, _, _), ..._] when !pattern_always_matches(List.nth(pats, col)) =>
    false
  | [_, ...tl] => col_is_wildcard(tl, col)
  };

let rec rotate_matrix = (mtx, col) =>
  switch (mtx) {
  | [] => []
  | [(pats, binds, mb), ...tl] =>
    let tl = rotate_matrix(tl, col);
    let rotated_row = swap_list(col, pats);
    [(rotated_row, binds, mb), ...tl];
  };

let rec rotate_if_needed = mtx =>
  if (!col_is_wildcard(mtx, 0)) {
    (0, mtx);
  } else {
    switch (mtx) {
    | [] => (0, mtx)
    | _ =>
      /* Safe, since size 0 matrix is wildcard */
      let width = matrix_width(mtx);
      let rec find_rotation = n =>
        if (n >= width) {
          raise(Not_found);
        } else if (!col_is_wildcard(mtx, n)) {
          (n, rotate_matrix(mtx, n));
        } else {
          find_rotation(n + 1);
        };
      find_rotation(1);
    };
  };

/* [See paper for details]

      Row: ([p_1; p_2; ...], a_j)

      Pattern p_1 (for row j)             Row(s) of S(c, P -> A)
      -------------------------           ----------------------
           c(q_1,...,q_n)           [([q_1; ...; q_n; p_2; ...], a_j)]
      c'(q_1,...,q_n) (c' <> c)                  No row
                 _                    [([_; ...; _; p_2; ...], a_j)] [n wildcards]
            (q_1 | q_2)               [(S(c, ([q_1; p_2; ...], a_j)));
                                       (S(c, ([q_2; p_2; ...], a_j)))]
   */
let rec specialize_matrix = (cd, cur, mtx) => {
  let arity = cd.cstr_arity;
  let rec specialized_rows = (row, binds) =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in specialize_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | TPatAlias(p, alias, _) =>
        specialized_rows([p, ...ptl], [(alias, cur), ...binds])
      | TPatVar(id, _) =>
        let wildcards = Parmatch.omegas(arity);
        [(wildcards @ ptl, [(id, cur), ...binds])];
      | TPatConstruct(_, pcd, args) when cd == pcd => [(args @ ptl, binds)]
      | TPatOr(p1, p2) =>
        specialized_rows([p1, ...ptl], binds)
        @ specialized_rows([p2, ...ptl], binds)
      | _ when pattern_always_matches(p) =>
        let wildcards = Parmatch.omegas(arity);
        [(wildcards @ ptl, binds)];
      | _ => [] /* Specialization for non-constructors generates no rows. */
      }
    };

  List.fold_right(
    ((row, binds, mb), rem) =>
      List.map(
        ((patts, binds)) => (patts, binds, mb),
        specialized_rows(row, binds),
      )
      @ rem,
    mtx,
    [],
  );
};

let rec specialize_array_matrix = (arity, cur, mtx) => {
  let rec specialized_rows = (row, binds) =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in specialize_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | TPatAlias(p, alias, _) =>
        specialized_rows([p, ...ptl], [(alias, cur), ...binds])
      | TPatVar(id, _) =>
        let wildcards = Parmatch.omegas(arity);
        [(wildcards @ ptl, [(id, cur), ...binds])];
      | TPatArray(args) when arity == List.length(args) => [
          (args @ ptl, binds),
        ]
      | TPatOr(p1, p2) =>
        specialized_rows([p1, ...ptl], binds)
        @ specialized_rows([p2, ...ptl], binds)
      | _ when pattern_always_matches(p) =>
        let wildcards = Parmatch.omegas(arity);
        [(wildcards @ ptl, binds)];
      | _ => [] /* Specialization for non-arrays generates no rows. */
      }
    };

  List.fold_right(
    ((row, binds, mb), rem) =>
      List.map(
        ((patts, binds)) => (patts, binds, mb),
        specialized_rows(row, binds),
      )
      @ rem,
    mtx,
    [],
  );
};

let rec specialize_constant_matrix = (const, cur, mtx) => {
  let rec specialized_rows = (row, binds) =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in specialize_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | TPatAlias(p, alias, _) =>
        specialized_rows([p, ...ptl], [(alias, cur), ...binds])
      | TPatVar(id, _) =>
        let wildcard = Parmatch.omega;
        [([wildcard, ...ptl], [(id, cur), ...binds])];
      | TPatConstant(c) when c == const =>
        let wildcard = Parmatch.omega;
        [([wildcard, ...ptl], binds)];
      | TPatOr(p1, p2) =>
        specialized_rows([p1, ...ptl], binds)
        @ specialized_rows([p2, ...ptl], binds)
      | _ when pattern_always_matches(p) => [(row, binds)]
      | _ => [] /* Specialization for non-constants generates no rows. */
      }
    };

  List.fold_right(
    ((row, binds, mb), rem) =>
      List.map(
        ((patts, binds)) => (patts, binds, mb),
        specialized_rows(row, binds),
      )
      @ rem,
    mtx,
    [],
  );
};

/* [See paper for details; modified slightly]

   We differ from the paper in that we don't drop the first pattern of the row
   in the case of an "any" pattern. This is done to have a fully matching row
   represent the current value stack—each value on the stack now directly
   corresponds to a pattern in the row. This allows us to efficiently use the
   bindings created during the pattern matching process in the selected branch.

   This doesn't significantly change anything about compilation of matching
   with respect to the paper. From the paper's perspective, the pattern is no
   longer needed and can just be dropped/disregarded. It is slightly faster to
   drop the pattern (in terms of compilation time) as it wouldn't need to ever
   be validated again, but we sacrifice this for ease of implementation.

      Row: ([p_1; p_2; ...], a_j)

      Pattern p_1 (for row j)       Row(s) of D(P)
      -----------------------       --------------
           c(q_1,...,q_n)               No row
                 _                [([p_1; p_2; ...], a_j)]
            (q_1 | q_2)           [D(([q_1; p_2; ...], a_j));
                                   D(([q_2; p_2; ...], a_j))]
   */
let rec default_matrix = (cur, mtx) => {
  let rec default_rows = (row, binds) =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in default_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | TPatAlias(inner, alias, _) =>
        default_rows([inner, ...ptl], [(alias, cur), ...binds])
      | _ when pattern_always_matches(p) => [(row, binds)]
      | TPatOr(p1, p2) =>
        default_rows([p1, ...ptl], binds)
        @ default_rows([p2, ...ptl], binds)
      | _ => []
      }
    };

  List.fold_right(
    ((row, binds, mb), rem) =>
      List.map(
        ((patts, binds)) => (patts, binds, mb),
        default_rows(row, binds),
      )
      @ rem,
    mtx,
    [],
  );
};

let equality_type =
  fun
  | Const_number(_)
  | Const_int32(_)
  | Const_int64(_)
  | Const_uint32(_)
  | Const_uint64(_)
  | Const_bigint(_)
  | Const_rational(_)
  | Const_float32(_)
  | Const_float64(_)
  | Const_bytes(_)
  | Const_string(_) => StructuralEquality
  | Const_void
  | Const_bool(_)
  | Const_char(_)
  | Const_int8(_)
  | Const_int16(_)
  | Const_uint8(_)
  | Const_uint16(_)
  | Const_wasmi32(_) => PhysicalEquality(WasmI32)
  | Const_wasmi64(_) => PhysicalEquality(WasmI64)
  | Const_wasmf32(_) => PhysicalEquality(WasmF32)
  | Const_wasmf64(_) => PhysicalEquality(WasmF64);

let get_constant_tag = const =>
  switch (const) {
  | Const_char(char) =>
    let uchar = List.hd @@ Utf8.decodeUtf8String(char);
    let uchar_int: int = Utf8__Uchar.toInt(uchar);
    Some(uchar_int);
  | Const_int8(i) => Some(Int32.to_int(i))
  | Const_int16(i) => Some(Int32.to_int(i))
  | Const_uint8(i) => Some(Int32.to_int(i))
  | Const_uint16(i) => Some(Int32.to_int(i))
  | Const_wasmi32(i) => Some(Int32.to_int(i))
  | _ => None
  };

let rec compile_matrix = mtx =>
  switch (mtx) {
  | [] => Fail /* No branches to match. */
  | [(pats, aliases, (mb, i)), ...rest_mtx]
      when List.for_all(pattern_always_matches, pats) =>
    let rest_tree =
      switch (mb.mb_guard) {
      | Some(guard) =>
        let result = compile_matrix(rest_mtx);
        Guard(mb, pats, aliases, Leaf(i, pats, aliases), result);
      | None => Leaf(i, pats, aliases)
      };
    rest_tree;
  | _ when !col_is_wildcard(mtx, 0) =>
    /* If the first column contains a non-wildcard pattern */

    let alias = Ident.create("match_explode_alias");

    let matrix_type = matrix_type(mtx);
    switch (matrix_type) {
    | TupleMatrix(arity) =>
      let mtx = flatten_matrix(arity, alias, mtx);
      let result = compile_matrix(mtx);
      Explode(matrix_type, alias, result);
    | ArrayMatrix(_) =>
      let arities = matrix_head_arities(mtx);
      let handle_arity = ((_, switch_branches), arity) => {
        let specialized = specialize_array_matrix(arity, alias, mtx);
        let result = compile_matrix(specialized);
        let final_tree = Explode(ArrayMatrix(Some(arity)), alias, result);
        (final_tree, [(arity, final_tree), ...switch_branches]);
      };

      switch (arities) {
      | [] =>
        failwith(
          "Internal error: compile_matrix: non-wildcard column returned no arities",
        )
      | _ =>
        let (_, switch_branches) =
          List.fold_left(handle_arity, (Fail, []), arities);

        let default = default_matrix(alias, mtx);
        let default_tree =
          if (List.length(default) != 0) {
            let tree = compile_matrix(default);
            Some(tree);
          } else {
            None;
          };
        Switch(ArraySwitch, None, switch_branches, default_tree);
      };
    | RecordMatrix(labels) =>
      let mtx = flatten_matrix(Array.length(labels), alias, mtx);
      let result = compile_matrix(mtx);
      Explode(matrix_type, alias, result);
    | ConstructorMatrix(_) =>
      let constructors = matrix_head_constructors(mtx);
      /* Printf.eprintf "constructors:\n%s\n" (Sexplib.Sexp.to_string_hum ((Sexplib.Conv.sexp_of_list sexp_of_constructor_description) constructors)); */
      let handle_constructor = ((_, switch_branches), cstr) => {
        let specialized = specialize_matrix(cstr, alias, mtx);
        let (arity, mtx) =
          switch (cstr.cstr_inlined) {
          | None => (cstr.cstr_arity, specialized)
          | Some(t) =>
            switch (t.type_kind) {
            | TDataRecord(rfs) =>
              let arity = List.length(rfs);
              let mtx = flatten_matrix(arity, alias, specialized);
              (arity, mtx);
            | _ =>
              failwith(
                "Impossible: inlined record constructor pattern with non-record data",
              )
            }
          };
        let result = compile_matrix(mtx);
        let final_tree =
          Explode(ConstructorMatrix(Some(arity)), alias, result);
        (
          final_tree,
          [
            (compile_constructor_tag^(cstr.cstr_tag), final_tree),
            ...switch_branches,
          ],
        );
      };

      switch (constructors) {
      | [] =>
        failwith(
          "Internal error: compile_matrix: non-wildcard column returned no constructors",
        )
      | _ =>
        let (result, switch_branches) =
          List.fold_left(handle_constructor, (Fail, []), constructors);

        let default = default_matrix(alias, mtx);
        let default_tree =
          if (List.length(default) != 0) {
            let tree = compile_matrix(default);
            Some(tree);
          } else {
            None;
          };
        Switch(ConstructorSwitch, None, switch_branches, default_tree);
      };
    | ConstantMatrix =>
      let constants = matrix_head_constants(mtx);
      let constant_type = List.hd(constants);
      let equality_type = equality_type(constant_type);

      // TODO(#1185): Optimize physical equality checks into a switch.
      // We can also do partial switches on Numbers if some of the
      // patterns are stack-allocated numbers. Addtionally, since we
      // know the types of the non-Number number types, we can make
      // compilation smarter and also switch on those values after
      // loaded from the heap.

      let default = default_matrix(alias, mtx);
      let default_tree =
        if (List.length(default) != 0) {
          compile_matrix(default);
        } else {
          Fail;
        };

      switch (constant_type) {
      | Const_char(_)
      | Const_int8(_)
      | Const_int16(_)
      | Const_uint8(_)
      | Const_uint16(_)
      | Const_wasmi32(_) =>
        let cases =
          List.fold_right(
            (const, acc) => {
              let specialized = specialize_constant_matrix(const, alias, mtx);
              let result = compile_matrix(specialized);
              let tag =
                switch (get_constant_tag(const)) {
                | Some(tag) => tag
                | None =>
                  failwith(
                    "Internal error: compile_matrix: no tag found for constant",
                  )
                };
              [(tag, result), ...acc];
            },
            constants,
            [],
          );
        Switch(
          ConstantSwitch(constant_type),
          Some(alias),
          cases,
          default_tree == Fail ? None : Some(default_tree),
        );
      | _ =>
        List.fold_right(
          (const, acc) => {
            let specialized = specialize_constant_matrix(const, alias, mtx);
            let result = compile_matrix(specialized);
            Conditional((equality_type, const), alias, result, acc);
          },
          constants,
          default_tree,
        )
      };
    };
  | _ =>
    /* Adjust stack so non-wildcard column is on top */
    let (i, rotated) =
      try(rotate_if_needed(mtx)) {
      | Not_found =>
        failwith(
          "Internal error: convert_match_branches: no non-wildcard column found but not all patterns always match (should be impossible)",
        )
      };
    let result = compile_matrix(rotated);
    Swap(i, result);
  };

let convert_match_branches =
    (match_branches: list(Typedtree.match_branch)): conversion_result => {
  let mtx = make_matrix(match_branches);
  let branches =
    List.map(((_, _, (mb, i))) => (i, mb.mb_body, mb.mb_pat), mtx);
  {tree: compile_matrix(mtx), branches};
};

module MatchTreeCompiler = {
  open Anftree;
  open Anf_helper;

  let pattern_could_contain_binding = patt =>
    switch (patt.pat_desc) {
    | TPatConstant(_)
    | TPatAny => false
    | _ => true
    };

  let fold_tree = (setup, ans) => {
    List.fold_right(
      (bind, body) =>
        switch (bind) {
        | BLet(name, exp, global) =>
          AExp.let_(
            ~loc=exp.comp_loc,
            ~global,
            Nonrecursive,
            [(name, exp)],
            body,
          )
        | BLetMut(name, exp, global) =>
          AExp.let_(
            ~loc=exp.comp_loc,
            ~global,
            ~mut_flag=Mutable,
            Nonrecursive,
            [(name, exp)],
            body,
          )
        | BSeq(exp) => AExp.seq(~loc=exp.comp_loc, exp, body)
        | _ =>
          failwith("match_comp: compile_tree_help: unsupported binding type")
        },
      setup,
      AExp.comp(~loc=ans.comp_loc, ans),
    );
  };

  let set_alias_bindings = (~mut_boxing, env, binds) => {
    List.map(
      ((name, value)) =>
        if (mut_boxing) {
          BSeq(
            Comp.assign(
              ~loc=Location.dummy_loc,
              ~env,
              ~allocation_type=Managed,
              Imm.id(~loc=Location.dummy_loc, name),
              Imm.id(~loc=Location.dummy_loc, value),
            ),
          );
        } else {
          BSeq(
            Comp.local_assign(
              ~loc=Location.dummy_loc,
              ~env,
              ~allocation_type=Managed,
              name,
              Imm.id(~loc=Location.dummy_loc, value),
            ),
          );
        },
      binds,
    );
  };

  let set_row_bindings = (~mut_boxing, env, patterns, values) => {
    List.fold_left2(
      (binds, pat, value) => {
        let rec collect_binds = (pat, binds) => {
          let assign = id =>
            if (mut_boxing) {
              Comp.assign(
                ~loc=Location.dummy_loc,
                ~env,
                ~allocation_type=
                  get_allocation_type(pat.pat_env, pat.pat_type),
                Imm.id(~loc=Location.dummy_loc, id),
                value,
              );
            } else {
              Comp.local_assign(
                ~loc=Location.dummy_loc,
                ~env,
                ~allocation_type=
                  get_allocation_type(pat.pat_env, pat.pat_type),
                id,
                value,
              );
            };
          switch (pat.pat_desc) {
          | TPatAny => binds
          | TPatVar(id, _) => [BSeq(assign(id)), ...binds]
          | TPatAlias(pat, id, _) =>
            collect_binds(pat, [BSeq(assign(id)), ...binds])
          | _ =>
            failwith("compile_tree_help: non-bind pattern in collect_binds")
          };
        };
        collect_binds(pat, []) @ binds;
      },
      [],
      patterns,
      values,
    );
  };

  let get_bindings = (~mut_boxing, env, row, values, aliases) => {
    set_alias_bindings(~mut_boxing, env, aliases)
    @ set_row_bindings(~mut_boxing, env, row, values);
  };

  let rec compile_tree_help =
          (
            ~loc=Location.dummy_loc,
            ~env=Env.empty,
            ~mut_boxing,
            tree,
            values,
            expr,
            helpI,
            helpConst,
          ) => {
    switch (tree) {
    | Leaf(i, patterns, aliases) =>
      let env = expr.imm_env;
      (
        Comp.imm(
          ~loc=Location.dummy_loc,
          ~allocation_type=Unmanaged(WasmI32),
          Imm.const(
            ~loc=Location.dummy_loc,
            Const_wasmi32(Int32.of_int(i)),
          ),
        ),
        get_bindings(~mut_boxing, env, patterns, values, aliases),
      );
    | Guard(mb, patterns, aliases, true_tree, false_tree) =>
      let guard =
        switch (mb.mb_guard) {
        | Some(guard) => guard
        | None =>
          failwith(
            "Impossible: (compile_tree_help): guarded match branch contained no guard",
          )
        };
      let bindings =
        get_bindings(
          ~mut_boxing,
          mb.mb_body.exp_env,
          patterns,
          values,
          aliases,
        );
      let (cond, cond_setup) = helpI(guard);
      let (true_comp, true_setup) =
        compile_tree_help(
          ~loc,
          ~env,
          ~mut_boxing,
          true_tree,
          values,
          expr,
          helpI,
          helpConst,
        );
      let (false_comp, false_setup) =
        compile_tree_help(
          ~loc,
          ~env,
          ~mut_boxing,
          false_tree,
          values,
          expr,
          helpI,
          helpConst,
        );
      (
        Comp.if_(
          ~loc=Location.dummy_loc,
          ~allocation_type=true_comp.comp_allocation_type,
          cond,
          fold_tree(true_setup, true_comp),
          fold_tree(false_setup, false_comp),
        ),
        bindings @ cond_setup,
      );
    | Conditional((equality_type, const), alias, true_tree, false_tree) =>
      let (cur_value, rest_values) =
        switch (values) {
        | [] => failwith("Impossible (compile_tree_help): Empty value stack")
        | [hd, ...tl] => (hd, tl)
        };
      let equality_op =
        switch (equality_type) {
        | StructuralEquality => Eq
        | PhysicalEquality(WasmI32) => Is
        | PhysicalEquality(WasmI64) =>
          WasmBinaryI64({
            wasm_op: Op_eq_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          })
        | PhysicalEquality(WasmF32) =>
          WasmBinaryF32({
            wasm_op: Op_eq_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          })
        | PhysicalEquality(WasmF64) =>
          WasmBinaryF64({
            wasm_op: Op_eq_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          })
        };
      let (const, const_setup) =
        switch (helpConst(const)) {
        | Left(imm) => (imm, [])
        | Right((name, binds)) => (
            Imm.id(~loc=Location.dummy_loc, name),
            binds,
          )
        };
      let cond =
        Comp.prim2(
          ~loc=Location.dummy_loc,
          ~allocation_type=Unmanaged(WasmI32),
          equality_op,
          cur_value,
          const,
        );
      let cond_id = Ident.create("match_conditional");
      let cond_setup = const_setup @ [BLet(cond_id, cond, Nonglobal)];
      let (true_comp, true_setup) =
        compile_tree_help(
          ~loc,
          ~env,
          ~mut_boxing,
          true_tree,
          values,
          expr,
          helpI,
          helpConst,
        );
      let (false_comp, false_setup) =
        compile_tree_help(
          ~loc,
          ~env,
          ~mut_boxing,
          false_tree,
          values,
          expr,
          helpI,
          helpConst,
        );

      // Add a binding for aliases
      let alias_binding =
        BLet(
          alias,
          Comp.imm(
            ~loc=Location.dummy_loc,
            ~allocation_type=Managed,
            cur_value,
          ),
          Nonglobal,
        );

      (
        Comp.if_(
          ~loc=Location.dummy_loc,
          ~allocation_type=true_comp.comp_allocation_type,
          Imm.id(~loc=Location.dummy_loc, cond_id),
          fold_tree(true_setup, true_comp),
          fold_tree(false_setup, false_comp),
        ),
        [alias_binding, ...cond_setup],
      );
    | Fail => (
        Comp.imm(
          ~loc=Location.dummy_loc,
          ~allocation_type=Unmanaged(WasmI32),
          Imm.trap(~loc=Location.dummy_loc, ()),
        ),
        [],
      )
    | Explode(matrix_type, alias, rest) =>
      /* Tack on the new bindings. We assume that the indices of 'rest'
         already account for the new indices. */
      let (cur_value, rest_values) =
        switch (values) {
        | [] => failwith("Impossible (compile_tree_help): Empty value stack")
        | [hd, ...tl] => (hd, tl)
        };
      let bindings =
        switch (matrix_type) {
        | ConstructorMatrix(Some(arity)) =>
          List.init(
            arity,
            idx => {
              let id = Ident.create("match_explode");
              BLet(
                id,
                Comp.adt_get(
                  ~loc=Location.dummy_loc,
                  ~allocation_type=Managed,
                  Int32.of_int(idx),
                  cur_value,
                ),
                Nonglobal,
              );
            },
          )
        | ConstructorMatrix(None) =>
          failwith("Internal error: must supply constructor arity")
        | TupleMatrix(arity) =>
          List.init(
            arity,
            idx => {
              let id = Ident.create("match_explode");
              BLet(
                id,
                Comp.tuple_get(
                  ~loc=Location.dummy_loc,
                  ~allocation_type=Managed,
                  Int32.of_int(idx),
                  cur_value,
                ),
                Nonglobal,
              );
            },
          )
        | ArrayMatrix(Some(arity)) =>
          List.init(
            arity,
            idx => {
              let id = Ident.create("match_explode");
              BLet(
                id,
                Comp.array_get(
                  ~loc=Location.dummy_loc,
                  ~allocation_type=Managed,
                  Imm.const(
                    ~loc=Location.dummy_loc,
                    Const_number(Const_number_int(Int64.of_int(idx))),
                  ),
                  cur_value,
                ),
                Nonglobal,
              );
            },
          )
        | ArrayMatrix(None) =>
          failwith("Internal error: must supply array arity")
        | RecordMatrix(label_poses) =>
          List.map(
            label_pos => {
              let id = Ident.create("match_explode");
              BLet(
                id,
                Comp.record_get(
                  ~loc=Location.dummy_loc,
                  ~allocation_type=Managed,
                  Int32.of_int(label_pos),
                  cur_value,
                ),
                Nonglobal,
              );
            },
            Array.to_list(label_poses),
          )
        | ConstantMatrix =>
          failwith("Internal error: Explode of a constant matrix")
        };

      let new_values =
        List.map(
          fun
          | BLet(id, _, _) => Imm.id(~loc=Location.dummy_loc, id)
          | _ =>
            failwith(
              "Impossible: matchcomp: compile_tree_help: binding was not BLet",
            ),
          bindings,
        )
        @ rest_values;

      // Add a binding for aliases
      let bindings = [
        BLet(
          alias,
          Comp.imm(
            ~loc=Location.dummy_loc,
            ~allocation_type=Managed,
            cur_value,
          ),
          Nonglobal,
        ),
        ...bindings,
      ];

      let (rest_ans, rest_setup) =
        compile_tree_help(
          ~loc,
          ~env,
          ~mut_boxing,
          rest,
          new_values,
          expr,
          helpI,
          helpConst,
        );
      (rest_ans, bindings @ rest_setup);
    | Swap(idx, rest_tree) =>
      compile_tree_help(
        ~loc,
        ~env,
        ~mut_boxing,
        rest_tree,
        swap_list(idx, values),
        expr,
        helpI,
        helpConst,
      )
    | Switch(switch_type, alias, cases, default_tree) =>
      let (cur_value, _) =
        switch (values) {
        | [] => failwith("Impossible (compile_tree_help): Empty value stack")
        | [hd, ...tl] => (hd, tl)
        };

      /* Runs when no cases match */
      let base_tree = Option.value(~default=Fail, default_tree);
      let base =
        compile_tree_help(
          ~loc,
          ~env,
          ~mut_boxing,
          base_tree,
          values,
          expr,
          helpI,
          helpConst,
        );
      // TODO: Convert array tag and constructor tag to ConstantSwitch on an int32 and untag alternatively we could use low level get functions or primitives
      let (switch_content, switch_setup) =
        switch (switch_type) {
        | ConstructorSwitch
        | ArraySwitch =>
          let value_constr_name = Ident.create("match_constructor");
          let value_constr_id =
            Imm.id(~loc=Location.dummy_loc, value_constr_name);
          let value_constr =
            switch (switch_type) {
            | ConstantSwitch(_) => failwith("Impossible")
            | ConstructorSwitch =>
              Comp.adt_get_tag(~loc=Location.dummy_loc, cur_value)
            | ArraySwitch =>
              Comp.prim1(
                ~loc=Location.dummy_loc,
                ~allocation_type=Unmanaged(WasmI32),
                ArrayLength,
                cur_value,
              )
            };
          /* Fold left should be safe here, since there should be at most one branch
             per value */
          let (switch_body_ans, switch_body_setup) =
            List.fold_left(
              ((body_ans, body_setup), (tag, tree)) => {
                let cmp_id_name = Ident.create("match_cmp_values");
                let cmp_id = Imm.id(~loc=Location.dummy_loc, cmp_id_name);
                /* If the value has the correct tag, execute this branch.
                   Otherwise continue. */
                let branch_condition =
                  Comp.prim2(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=Unmanaged(WasmI32),
                    Is,
                    value_constr_id,
                    Imm.const(
                      ~loc=Location.dummy_loc,
                      Const_number(Const_number_int(Int64.of_int(tag))),
                    ),
                  );
                let setup = [BLet(cmp_id_name, branch_condition, Nonglobal)];
                let (tree_ans, tree_setup) =
                  compile_tree_help(
                    ~loc,
                    ~env,
                    ~mut_boxing,
                    tree,
                    values,
                    expr,
                    helpI,
                    helpConst,
                  );
                let ans =
                  Comp.if_(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=tree_ans.comp_allocation_type,
                    cmp_id,
                    fold_tree(tree_setup, tree_ans),
                    fold_tree(body_setup, body_ans),
                  );
                (ans, setup);
              },
              base,
              cases,
            );
          (
            switch_body_ans,
            [
              BLet(value_constr_name, value_constr, Nonglobal),
              ...switch_body_setup,
            ],
          );
        | ConstantSwitch(const) =>
          // Get Value Constructor
          let compile_cond_i32 = v =>
            Imm.const(
              ~loc=Location.dummy_loc,
              Const_wasmi32(Int32.of_int(v)),
            );
          let (value_op, value_typ, compile_cond) =
            switch (const) {
            | Const_char(_) => (Some(UntagChar), WasmI32, compile_cond_i32)
            | Const_int8(_) => (Some(UntagInt8), WasmI32, compile_cond_i32)
            | Const_int16(_) => (Some(UntagInt16), WasmI32, compile_cond_i32)
            | Const_uint8(_) => (Some(UntagUint8), WasmI32, compile_cond_i32)
            | Const_uint16(_) => (
                Some(UntagUint16),
                WasmI32,
                compile_cond_i32,
              )
            | Const_wasmi32(_) => (None, WasmI32, compile_cond_i32)
            | _ =>
              failwith("Impossible: should fall back to equality matching")
            };
          let cond_name = Ident.create("match_cond");
          let cond_id = Imm.id(~loc=Location.dummy_loc, cond_name);
          let condition =
            switch (value_op) {
            | Some(tag_op) =>
              Comp.prim1(
                ~loc=Location.dummy_loc,
                ~allocation_type=Unmanaged(value_typ),
                tag_op,
                cur_value,
              )
            | None =>
              Comp.imm(
                ~loc=Location.dummy_loc,
                ~allocation_type=Unmanaged(value_typ),
                cur_value,
              )
            };
          let equality_op =
            switch (value_typ) {
            | WasmI32 =>
              WasmBinaryI32({
                wasm_op: Op_eq_int32,
                arg_types: (Wasm_int32, Wasm_int32),
                ret_type: Grain_bool,
              })
            | WasmI64 =>
              WasmBinaryI64({
                wasm_op: Op_eq_int64,
                arg_types: (Wasm_int64, Wasm_int64),
                ret_type: Grain_bool,
              })
            | WasmF32 =>
              WasmBinaryF32({
                wasm_op: Op_eq_float32,
                arg_types: (Wasm_float32, Wasm_float32),
                ret_type: Grain_bool,
              })
            | WasmF64 =>
              WasmBinaryF64({
                wasm_op: Op_eq_float64,
                arg_types: (Wasm_float64, Wasm_float64),
                ret_type: Grain_bool,
              })
            };
          /* Fold left should be safe here, since there should be at most one branch
             per value */
          let (switch_body_ans, switch_body_setup) =
            List.fold_left(
              ((body_ans, body_setup), (tag, tree)) => {
                let cmp_id_name = Ident.create("match_cmp_values");
                let cmp_id = Imm.id(~loc=Location.dummy_loc, cmp_id_name);
                let branch_condition =
                  Comp.prim2(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=Unmanaged(WasmI32),
                    equality_op,
                    cond_id,
                    compile_cond(tag),
                  );
                let setup = [BLet(cmp_id_name, branch_condition, Nonglobal)];
                let (tree_ans, tree_setup) =
                  compile_tree_help(
                    ~loc,
                    ~env,
                    ~mut_boxing,
                    tree,
                    values,
                    expr,
                    helpI,
                    helpConst,
                  );
                let ans =
                  Comp.if_(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=tree_ans.comp_allocation_type,
                    cmp_id,
                    fold_tree(tree_setup, tree_ans),
                    fold_tree(body_setup, body_ans),
                  );
                (ans, setup);
              },
              base,
              cases,
            );
          (
            switch_body_ans,
            [BLet(cond_name, condition, Nonglobal), ...switch_body_setup],
          );
        };
      let switch_body_setup =
        switch (alias) {
        | Some(alias) => [
            BLet(
              alias,
              Comp.imm(
                ~loc=Location.dummy_loc,
                ~allocation_type=Managed,
                cur_value,
              ),
              Nonglobal,
            ),
            ...switch_setup,
          ]
        | None => switch_setup
        };
      (switch_content, switch_body_setup);
    };
  };

  let collect_bindings = (~mut_boxing=false, ~global=Nonglobal, branches) => {
    let rec collect_bindings = pat => {
      let bind = id => {
        let allocation_type = get_allocation_type(pat.pat_env, pat.pat_type);
        // Dummy value to be filled in during matching
        let dummy_value =
          switch (allocation_type) {
          | Managed
          | Unmanaged(WasmI32) =>
            Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l))
          | Unmanaged(WasmI64) =>
            Imm.const(~loc=Location.dummy_loc, Const_wasmi64(0L))
          | Unmanaged(WasmF32) =>
            Imm.const(~loc=Location.dummy_loc, Const_wasmf32(0.))
          | Unmanaged(WasmF64) =>
            Imm.const(~loc=Location.dummy_loc, Const_wasmf64(0.))
          };
        if (mut_boxing) {
          BLetMut(
            id,
            Comp.prim1(
              ~loc=Location.dummy_loc,
              ~allocation_type=Managed,
              BoxBind,
              dummy_value,
            ),
            Nonglobal,
          );
        } else {
          BLetMut(
            id,
            Comp.imm(~loc=Location.dummy_loc, ~allocation_type, dummy_value),
            global,
          );
        };
      };
      switch (pat.pat_desc) {
      | TPatAny
      | TPatConstant(_) => []
      | TPatVar(id, _) => [bind(id)]
      | TPatConstruct(_, _, pats)
      | TPatTuple(pats)
      | TPatArray(pats) => List.flatten @@ List.map(collect_bindings, pats)
      | TPatRecord(pats, _) =>
        List.flatten @@
        List.map(((_, _, pat)) => collect_bindings(pat), pats)
      | TPatAlias(pat, id, _) => [bind(id), ...collect_bindings(pat)]
      | TPatOr(left, _) =>
        // Bindings are the same on both sides of the OR
        collect_bindings(left)
      };
    };
    List.flatten @@
    List.map(((_, _, pat)) => collect_bindings(pat), branches);
  };

  let compile_match =
      (~allocation_type, ~partial, branches, expr)
      : (Anftree.comp_expression, list(Anftree.anf_bind)) => {
    let helpA = transl_anf_expression^;
    let helpI = transl_imm_expression^;
    let helpConst = transl_const^;

    let {tree, branches} = convert_match_branches(branches);

    // Create slots for bindings in each of the branches
    let bind_setup = collect_bindings(branches);
    let {imm_loc: loc, imm_env: env} = expr;
    let (ans, setup) =
      compile_tree_help(
        ~loc,
        ~env,
        ~mut_boxing=false,
        tree,
        [expr],
        expr,
        helpI,
        helpConst,
      );
    let jmp_name = Ident.create("match_dest");
    let setup = bind_setup @ setup @ [BLet(jmp_name, ans, Nonglobal)];
    let switch_branches =
      List.map(
        ((tag, branch, orig_pat)) => (tag, helpA(branch)),
        branches,
      );
    (
      Comp.switch_(
        ~loc=Location.dummy_loc,
        ~allocation_type,
        Imm.id(~loc=Location.dummy_loc, jmp_name),
        switch_branches,
        partial,
      ),
      setup,
    );
  };

  let destructure =
      (~mut_flag=Immutable, ~global=Nonglobal, pattern, expr)
      : list(Anftree.anf_bind) => {
    let helpI = transl_imm_expression^;
    let helpConst = transl_const^;

    // Treat destructuring as a match on a single (but total) branch
    let branch = {
      mb_pat: pattern,
      mb_body: {
        exp_desc: TExpConstant(Const_void),
        exp_loc: Location.dummy_loc,
        exp_extra: [],
        exp_attributes: [],
        exp_type: Builtin_types.type_void,
        exp_env: pattern.pat_env,
      },
      mb_guard: None,
      mb_loc: Location.dummy_loc,
    };

    let {tree, branches} = convert_match_branches([branch]);

    let mut_boxing =
      switch (mut_flag, global) {
      | (Mutable, Nonglobal) => true
      | _ => false
      };

    // Create slots for bindings in each of the branches
    let bind_setup = collect_bindings(~mut_boxing, ~global, branches);
    let {imm_loc: loc, imm_env: env} = expr;
    let (ans, setup) =
      compile_tree_help(
        ~loc,
        ~env,
        ~mut_boxing,
        tree,
        [expr],
        expr,
        helpI,
        helpConst,
      );
    bind_setup @ setup @ [BSeq(ans)];
  };
};
