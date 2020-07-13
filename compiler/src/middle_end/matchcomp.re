/** Pattern Matching compiler, based on "Compiling Pattern Matching to Good Decision Trees"
    by Luc Maranget (http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf) */;

/* NOTE: Constant patterns are currently unsupported, since
   they require logic for guard expressions. */

open Sexplib.Conv;
open Grain_parsing;
open Grain_typed;
open Grain_utils;
open Types;
open Typedtree;

/** The type for compiled match pattern decision trees.
    These trees are evaluated with respect to a stack of values. */

/* The following instruction(s) are extensions from the paper's ADT: */

[@deriving sexp]
type decision_tree =
  | /** Leaves of the decision tree. The int corresponds
      to the action number (i.e. switch branch) to be taken */
    Leaf(
      int,
    )
  | /** Represents a failed match. */
    Fail
  | /** Multi-way test for an occurence. The list is a
      non-empty list of pairs of
      (constructor_tag, nested_decision_tree) tuples,
      with an optional default branch. */
    Switch(
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
      Argument: (kind of expansion) */
    Explode(
      matrix_type,
      decision_tree,
    )

and matrix_type =
  | TupleMatrix(int)
  | RecordMatrix(array(int))
  | ConstructorMatrix(option(int));

type conversion_result = {
  tree: decision_tree,
  branches: list((int, expression, pattern)),
};

/** Utilities */;

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

module MatchTreeCompiler = {
  open Anftree;
  open Anf_helper;

  let pattern_could_contain_binding = patt =>
    switch (patt.pat_desc) {
    | TPatConstant(_)
    | TPatAny => false
    | _ => true
    };

  let no_op = x => x;
  let compose_binding = (id, cexp, func, x) =>
    AExp.let_(Nonrecursive, [(id, cexp)], func(x));
  let with_binding = (id, cexp) => compose_binding(id, cexp, no_op);

  let extract_bindings =
      (patt: pattern, expr: Anftree.comp_expression)
      : list((Ident.t, Anftree.comp_expression)) => {
    /*Printf.eprintf "Extracting bindings from:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_pattern patt));*/
    let rec extract_bindings = (patt, expr) => {
      let {pat_loc: loc, pat_env: env} = patt;
      switch (patt.pat_desc) {
      | TPatConstant(_)
      | TPatAny => []
      | [@implicit_arity] TPatVar(id, _) => [(id, expr)]
      | [@implicit_arity] TPatAlias(p, id, _) =>
        let bindings_p = extract_bindings(p, expr);
        [(id, expr), ...bindings_p];
      | TPatTuple(args) =>
        let tup_name = Ident.create("match_bind_tup");
        let tup_id = Imm.id(~loc, ~env, tup_name);
        let process_nested = (other_binds, idx, nested_pat) => {
          let this_binds =
            if (pattern_could_contain_binding(nested_pat)) {
              let tup_arg_name = Ident.create("match_bind_tup_arg");
              let tup_arg_imm = Imm.id(~loc, ~env, tup_arg_name);
              let arg_binds =
                extract_bindings(
                  nested_pat,
                  Comp.imm(~loc, ~env, tup_arg_imm),
                );
              switch (arg_binds) {
              | [] => []
              | _ => [
                  (
                    tup_arg_name,
                    Comp.tuple_get(~loc, ~env, Int32.of_int(idx), tup_id),
                  ),
                  ...arg_binds,
                ]
              };
            } else {
              [];
            };
          this_binds @ other_binds;
        };
        let binds = List_utils.fold_lefti(process_nested, [], args);
        switch (binds) {
        | [] => []
        | _ => [(tup_name, expr), ...binds]
        };
      | [@implicit_arity] TPatRecord(fields, _) =>
        let rec_name = Ident.create("match_bind_rec");
        let rec_id = Imm.id(~loc, ~env, rec_name);
        let process_nested = (other_binds, (lid, ld, nested_pat)) => {
          let this_binds =
            if (pattern_could_contain_binding(nested_pat)) {
              let rec_field_name =
                Ident.create @@
                "match_bind_rec_field_"
                ++ Identifier.last(lid.txt);
              let rec_field_imm = Imm.id(~loc, ~env, rec_field_name);
              let field_binds =
                extract_bindings(
                  nested_pat,
                  Comp.imm(~loc, ~env, rec_field_imm),
                );
              switch (field_binds) {
              | [] => []
              | _ => [
                  (
                    rec_field_name,
                    Comp.record_get(
                      ~loc,
                      ~env,
                      Int32.of_int(ld.lbl_pos),
                      rec_id,
                    ),
                  ),
                  ...field_binds,
                ]
              };
            } else {
              [];
            };
          this_binds @ other_binds;
        };
        let binds = List.fold_left(process_nested, [], fields);
        switch (binds) {
        | [] => []
        | _ => [(rec_name, expr), ...binds]
        };
      | [@implicit_arity] TPatConstruct(_, _, args) =>
        let data_name = Ident.create("match_bind_data");
        let data_id = Imm.id(~loc, ~env, data_name);
        let process_nested = (other_binds, idx, nested_pat) => {
          let this_binds =
            if (pattern_could_contain_binding(nested_pat)) {
              let cstr_arg_name = Ident.create("match_bind_cstr_arg");
              let cstr_arg_imm = Imm.id(~loc, ~env, cstr_arg_name);
              let arg_binds =
                extract_bindings(
                  nested_pat,
                  Comp.imm(~loc, ~env, cstr_arg_imm),
                );
              switch (arg_binds) {
              | [] => []
              | _ => [
                  (
                    cstr_arg_name,
                    Comp.adt_get(~loc, ~env, Int32.of_int(idx), data_id),
                  ),
                  ...arg_binds,
                ]
              };
            } else {
              [];
            };
          this_binds @ other_binds;
        };
        let binds = List_utils.fold_lefti(process_nested, [], args);
        switch (binds) {
        | [] => []
        | _ => [(data_name, expr), ...binds]
        };
      | TPatOr(_) => failwith("NYI: extract_bindings > TPatOr")
      };
    };
    let res = extract_bindings(patt, expr);
    /*Printf.eprintf "Bindings:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list (sexp_of_pair sexp_of_string (fun x -> (sexp_of_string (Pretty.string_of_cexpr x)))) res));*/
    res;
  };

  let fold_tree = (setup, ans) =>
    List.fold_right(
      ((name, exp), body) => AExp.let_(Nonrecursive, [(name, exp)], body),
      setup,
      AExp.comp(ans),
    );

  let rec compile_tree_help = (tree, values) => {
    let (cur_value, rest_values) =
      switch (values) {
      | [] => failwith("Impossible (compile_tree_help): Empty value stack")
      | [hd, ...tl] => (hd, tl)
      };
    switch (tree) {
    | Leaf(i) => (Comp.imm(Imm.const(Const_int(i))), [])
    | Fail =>
      /* FIXME: We need a "throw error" node in ANF */
      (Comp.imm(Imm.const(Const_int(0))), [])
    /* Optimizations to avoid unneeded destructuring: */
    | [@implicit_arity] Explode(_, Leaf(_) as inner)
    | [@implicit_arity] Explode(_, Fail as inner) =>
      compile_tree_help(inner, values)
    | [@implicit_arity] Explode(matrix_type, rest) =>
      /* Tack on the new bindings. We assume that the indices of 'rest'
         already account for the new indices. */
      let bindings =
        switch (matrix_type) {
        | ConstructorMatrix(Some(arity)) =>
          List.init(
            arity,
            idx => {
              let id = Ident.create("match_explode");
              (id, Comp.adt_get(Int32.of_int(idx), cur_value));
            },
          )
        | ConstructorMatrix(None) =>
          failwith("Internal error: must supply constructor arity")
        | TupleMatrix(arity) =>
          List.init(
            arity,
            idx => {
              let id = Ident.create("match_explode");
              (id, Comp.tuple_get(Int32.of_int(idx), cur_value));
            },
          )
        | RecordMatrix(label_poses) =>
          List.map(
            label_pos => {
              let id = Ident.create("match_explode");
              (id, Comp.record_get(Int32.of_int(label_pos), cur_value));
            },
            Array.to_list(label_poses),
          )
        };

      let new_values =
        List.map(((id, _)) => Imm.id(id), bindings) @ rest_values;
      let (rest_ans, rest_setup) = compile_tree_help(rest, new_values);
      (rest_ans, bindings @ rest_setup);
    | [@implicit_arity] Swap(idx, rest_tree) =>
      compile_tree_help(rest_tree, swap_list(idx, values))
    | [@implicit_arity] Switch(branches, default_tree) =>
      /* Runs when no branches match */
      let base_tree = Option.value(~default=Fail, default_tree);
      let base = compile_tree_help(base_tree, values);
      let value_constr_name = Ident.create("match_constructor");
      let value_constr_id = Imm.id(value_constr_name);
      let value_constr = Comp.adt_get_tag(cur_value);
      /* Fold left should be safe here, since there should be at most one branch
         per constructor */
      let (switch_body_ans, switch_body_setup) =
        List.fold_left(
          ((body_ans, body_setup), (tag, tree)) => {
            let cmp_id_name = Ident.create("match_cmp_constructors");
            let cmp_id = Imm.id(cmp_id_name);
            /* If the constructor has the correct tag, execute this branch.
               Otherwise continue. */
            let setup = [
              (
                cmp_id_name,
                Comp.prim2(Eq, value_constr_id, Imm.const(Const_int(tag))),
              ),
            ];
            let (tree_ans, tree_setup) = compile_tree_help(tree, values);
            let ans =
              Comp.if_(
                cmp_id,
                fold_tree(tree_setup, tree_ans),
                fold_tree(body_setup, body_ans),
              );
            (ans, setup);
          },
          base,
          branches,
        );
      (
        switch_body_ans,
        [(value_constr_name, value_constr), ...switch_body_setup],
      );
    };
  };

  let compile_result =
      ({tree, branches}, helpA, expr)
      : (Anftree.comp_expression, list((Ident.t, Anftree.comp_expression))) => {
    /*prerr_string "Compiling tree:";
      prerr_string (Sexplib.Sexp.to_string_hum (sexp_of_decision_tree tree));
      prerr_newline();*/
    let (ans, setup) = compile_tree_help(tree, [expr]);
    let jmp_name = Ident.create("match_dest");
    let setup = setup @ [(jmp_name, ans)];
    let switch_branches =
      List.map(
        ((tag, branch, orig_pat)) =>
          (
            tag,
            List.fold_right(
              ((name, exp), body) =>
                AExp.let_(Nonrecursive, [(name, exp)], body),
              extract_bindings(orig_pat, Comp.imm(expr)),
              helpA(branch),
            ),
          ),
        branches,
      );
    (Comp.switch_(Imm.id(jmp_name), switch_branches), setup);
  };
};

let rec pattern_always_matches = patt =>
  /* Precondition: Normalized */
  switch (patt.pat_desc) {
  | TPatAny
  | TPatVar(_) => true
  | [@implicit_arity] TPatAlias(p, _, _) => pattern_always_matches(p)
  | TPatTuple(args) when List.for_all(pattern_always_matches, args) => true
  | [@implicit_arity] TPatRecord(fields, _)
      when List.for_all(((_, _, p)) => pattern_always_matches(p), fields) =>
    true
  | _ => false
  };

let flatten_pattern = (size, {pat_desc}) =>
  switch (pat_desc) {
  | TPatTuple(args) => args
  | [@implicit_arity] TPatRecord([(_, {lbl_all}, _), ..._] as ps, _) =>
    let patterns = Array.init(Array.length(lbl_all), _ => Parmatch.omega);
    List.iter(((_, ld, pat)) => patterns[ld.lbl_pos] = pat, ps);
    Array.to_list(patterns);
  | TPatVar(_)
  | TPatAny => Parmatch.omegas(size)
  | _ =>
    failwith(
      "Internal error: flatten_pattern on non-tuple or non-record pattern",
    )
  };

let rec matrix_type =
  fun
  | []
  | [([{pat_desc: TPatConstruct(_)}, ..._], _), ..._] =>
    ConstructorMatrix(None)
  | [([{pat_desc: TPatTuple(ps)}, ..._], _), ..._] =>
    TupleMatrix(List.length(ps))
  | [
      (
        [
          {pat_desc: [@implicit_arity] TPatRecord([(_, ld, _), ..._], _)},
          ..._,
        ],
        _,
      ),
      ..._,
    ] =>
    RecordMatrix(Array.map(({lbl_pos}) => lbl_pos, ld.lbl_all))
  | [_, ...rest] => matrix_type(rest);

let rec pattern_head_constructors_aux = (p, acc) =>
  switch (p.pat_desc) {
  | [@implicit_arity] TPatConstruct(_, cd, _) when !List.mem(cd, acc) => [
      cd,
      ...acc,
    ]
  | [@implicit_arity] TPatOr(p1, p2) =>
    pattern_head_constructors_aux(p1, pattern_head_constructors_aux(p2, acc))
  | _ => acc
  };

let pattern_head_constructors = patt =>
  pattern_head_constructors_aux(patt, []);

let matrix_head_constructors = mtx => {
  let rec help = (mtx, acc) =>
    switch (mtx) {
    | [] => acc
    | [([], _), ..._] => failwith("Impossible: Empty pattern matrix")
    | [([p, ..._], mb), ...tl] =>
      help(tl, pattern_head_constructors_aux(p, acc))
    };
  help(mtx, []);
};

let make_matrix = branches =>
  /* Form matrix and assign each branch a number */
  List.mapi((i, {mb_pat} as mb) => ([mb_pat], (mb, i)), branches);

let matrix_width =
  fun
  | [] => raise(Not_found)
  | [(pats, _), ..._] => List.length(pats);

let rec col_is_wildcard = (mtx, col) =>
  switch (mtx) {
  | [] => true
  | [(pats, _), ..._] when !pattern_always_matches(List.nth(pats, col)) =>
    false
  | [_, ...tl] => col_is_wildcard(tl, col)
  };

let rec rotate_matrix = (mtx, col) =>
  switch (mtx) {
  | [] => []
  | [(pats, mb), ...tl] =>
    let tl = rotate_matrix(tl, col);
    let rotated_row = swap_list(col, pats);
    [(rotated_row, mb), ...tl];
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
let rec specialize_matrix = (cd, mtx) => {
  let arity = cd.cstr_arity;
  let rec specialized_rows = row =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in specialize_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | _ when pattern_always_matches(p) =>
        let wildcards = List.init(arity, _ => {...p, pat_desc: TPatAny});
        [wildcards @ ptl];
      | [@implicit_arity] TPatConstruct(_, pcd, args) when cd == pcd => [
          args @ ptl,
        ]
      | [@implicit_arity] TPatOr(p1, p2) =>
        specialized_rows([p1, ...ptl]) @ specialized_rows([p2, ...ptl])
      | [@implicit_arity] TPatAlias(p, _, _) => specialized_rows([p, ...ptl])
      | _ => [] /* Specialization for non-constructors generates no rows. */
      }
    };

  List.fold_right(
    ((row, mb), rem) =>
      List.map(patts => (patts, mb), specialized_rows(row)) @ rem,
    mtx,
    [],
  );
};

/* [See paper for details]

      Row: ([p_1; p_2; ...], a_j)

      Pattern p_1 (for row j)       Row(s) of D(P)
      -----------------------       --------------
           c(q_1,...,q_n)               No row
                 _                [([p_2; ...], a_j)]
            (q_1 | q_2)           [D(([q_1; p_2; ...], a_j));
                                   D(([q_2; p_2; ...], a_j))]
   */
let rec default_matrix = mtx => {
  let rec default_rows = row =>
    switch (row) {
    | [] => failwith("Impossible: Empty pattern row in default_matrix")
    | [{pat_desc} as p, ...ptl] =>
      switch (pat_desc) {
      | _ when pattern_always_matches(p) => [ptl]
      | [@implicit_arity] TPatOr(p1, p2) =>
        default_rows([p1, ...ptl]) @ default_rows([p2, ...ptl])
      | _ => []
      }
    };

  List.fold_right(
    ((row, mb), rem) =>
      List.map(patts => (patts, mb), default_rows(row)) @ rem,
    mtx,
    [],
  );
};

let rec compile_matrix = (branches, mtx) =>
  switch (mtx) {
  | [] => {tree: Fail, branches} /* No branches to match. */
  | [(pats, (mb, i)), ..._] when List.for_all(pattern_always_matches, pats) => {
      tree: Leaf(i),
      branches: [(i, mb.mb_body, mb.mb_pat), ...branches],
    }
  | _ when !col_is_wildcard(mtx, 0) =>
    /* If the first column contains a non-wildcard pattern */
    let matrix_type = matrix_type(mtx);
    switch (matrix_type) {
    | TupleMatrix(arity) =>
      let mtx =
        List.map(
          ((row, mb)) => (flatten_pattern(arity, List.hd(row)), mb),
          mtx,
        );
      let result = compile_matrix(branches, mtx);
      {...result, tree: [@implicit_arity] Explode(matrix_type, result.tree)};
    | RecordMatrix(labels) =>
      let mtx =
        List.map(
          ((row, mb)) =>
            (flatten_pattern(Array.length(labels), List.hd(row)), mb),
          mtx,
        );
      let result = compile_matrix(branches, mtx);
      {...result, tree: [@implicit_arity] Explode(matrix_type, result.tree)};
    | ConstructorMatrix(_) =>
      let constructors = matrix_head_constructors(mtx);
      /* Printf.eprintf "constructors:\n%s\n" (Sexplib.Sexp.to_string_hum ((Sexplib.Conv.sexp_of_list sexp_of_constructor_description) constructors)); */
      let handle_constructor = (({branches}, switch_branches), cstr) => {
        let arity = cstr.cstr_arity;
        let specialized = specialize_matrix(cstr, mtx);
        let {tree} as result = compile_matrix(branches, specialized);
        let final_tree =
          [@implicit_arity] Explode(ConstructorMatrix(Some(arity)), tree);
        (
          {...result, tree: final_tree},
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
          List.fold_left(
            handle_constructor,
            ({branches, tree: Fail}, []),
            constructors,
          );
        let {tree, branches} = result;

        let default = default_matrix(mtx);
        let (default_tree, branches) =
          if (List.length(default) != 0) {
            let {tree, branches} = compile_matrix(branches, default);
            (Some(tree), branches);
          } else {
            (None, branches);
          };
        {
          tree: [@implicit_arity] Switch(switch_branches, default_tree),
          branches,
        };
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
    let {tree, _} as result = compile_matrix(branches, rotated);
    {...result, tree: [@implicit_arity] Swap(i, tree)};
  };

let convert_match_branches =
    (match_branches: list(Typedtree.match_branch)): conversion_result => {
  let mtx = make_matrix(match_branches);
  /*prerr_string "Initial matrix:\n";
    prerr_string (Sexplib.Sexp.to_string_hum (Sexplib.Conv.sexp_of_list
                                                (Sexplib.Conv.sexp_of_pair
                                                   (Sexplib.Conv.sexp_of_list sexp_of_pattern)
                                                   sexp_of_match_branch) mtx));
    prerr_newline();*/
  compile_matrix([], mtx);
};
