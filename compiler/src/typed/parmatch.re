/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Detection of partial matches and unused match cases. */

open Grain_utils;
open Grain_parsing;
open Misc;
open Asttypes;
open Types;
open Typedtree;

/*************************************/
/* Utilities for building patterns   */
/*************************************/

let make_pat = (desc, ty, tenv) => {
  pat_desc: desc,
  pat_loc: Location.dummy_loc,
  pat_extra: [],
  pat_type: ty,
  pat_env: tenv,
};

let omega = make_pat(TPatAny, Ctype.none, Env.empty);

let extra_pat =
  make_pat(
    [@implicit_arity] TPatVar(Ident.create("+"), mknoloc("+")),
    Ctype.none,
    Env.empty,
  );

let rec omegas = i =>
  if (i <= 0) {
    [];
  } else {
    [omega, ...omegas(i - 1)];
  };

let omega_list = l => List.map(_ => omega, l);

let zero = make_pat(TPatConstant(Const_int(0)), Ctype.none, Env.empty);

/*******************/
/* Coherence check */
/*******************/

/* For some of the operations we do in this module, we would like (because it
      simplifies matters) to assume that patterns appearing on a given column in a
      pattern matrix are /coherent/ (think "of the same type").
      Unfortunately that is not always true.
      Consider the following (well-typed) example:
      {[
        type _ t = S : string t | U : unit t
        let f (type a) (t1 : a t) (t2 : a t) (a : a) =
          match t1, t2, a with
          | U, _, () -> ()
          | _, S, "" -> ()
      ]}
      Clearly the 3rd column contains incoherent patterns.
      On the example above, most of the algorithms will explore the pattern matrix
      as illustrated by the following tree:
      {v
                                                      S
                                                   -------> | "" |
                                U     | S, "" | __/         | () |
                            --------> | _, () |   \  ¬ S
           | U, _, () | __/                        -------> | () |
           | _, S, "" |   \
                           ---------> | S, "" | ----------> | "" |
                              ¬ U                    S
      v}
      where following an edge labelled by a pattern P means "assuming the value I
      am matching on is filtered by [P] on the column I am currently looking at,
      then the following submatrix is still reachable".
      Notice that at any point of that tree, if the first column of a matrix is
      incoherent, then the branch leading to it can only be taken if the scrutinee
      is ill-typed.
      In the example above the only case where we have a matrix with an incoherent
      first column is when we consider [t1, t2, a] to be [U, S, ...]. However such
      a value would be ill-typed, so we can never actually get there.
      Checking the first column at each step of the recursion and making the
      concious decision of "aborting" the algorithm whenever the first column
      becomes incoherent, allows us to retain the initial assumption in later
      stages of the algorithms.
      ---
      N.B. two patterns can be considered coherent even though they might not be of
      the same type.
      That's in part because we only care about the "head" of patterns and leave
      checking coherence of subpatterns for the next steps of the algorithm:
      ('a', 'b') and (1, ()) will be deemed coherent because they are both a tuples
      of arity 2 (we'll notice at a later stage the incoherence of 'a' and 1).
      But also because it can be hard/costly to determine exactly whether two
      patterns are of the same type or not (eg. in the example above with _ and S,
      but see also the module [Coherence_illustration] in
      testsuite/tests/basic-more/robustmatch.ml).
      For the moment our weak, loosely-syntactic, coherence check seems to be
      enough and we leave it to each user to consider (and document!) what happens
      when an "incoherence" is not detected by this check.
   */

/* Given the first column of a simplified matrix, this function first looks for
      a "discriminating" pattern on that column (i.e. a non-omega one) and then
      check that every other head pattern in the column is coherent with that one.
   */
let all_coherent = column => {
  let coherent_heads = (hp1, hp2) =>
    switch (hp1.pat_desc, hp2.pat_desc) {
    | (TPatVar(_) | TPatAlias(_) | TPatOr(_), _)
    | (_, TPatVar(_) | TPatAlias(_) | TPatOr(_)) => assert(false)
    | (
        [@implicit_arity] TPatConstruct(_, c, _),
        [@implicit_arity] TPatConstruct(_, c', _),
      ) =>
      c.cstr_consts == c'.cstr_consts && c.cstr_nonconsts == c'.cstr_nonconsts
    | (TPatConstant(c1), TPatConstant(c2)) =>
      switch (c1, c2) {
      | (Const_int(_), Const_int(_))
      | (Const_int32(_), Const_int32(_))
      | (Const_int64(_), Const_int64(_))
      | (Const_float(_), Const_float(_))
      | (Const_bool(_), Const_bool(_))
      | (Const_void, Const_void)
      | (Const_string(_), Const_string(_)) => true
      | (
          Const_int(_) | Const_int32(_) | Const_int64(_) | Const_float(_) |
          Const_bool(_) |
          Const_void |
          Const_string(_),
          _,
        ) =>
        false
      }
    | (TPatTuple(l1), TPatTuple(l2)) => List.length(l1) == List.length(l2)
    | (TPatAny, _)
    | (_, TPatAny) => true
    | (_, _) => false
    };

  switch (
    List.find(
      head_pat =>
        switch (head_pat.pat_desc) {
        | TPatVar(_)
        | TPatAlias(_)
        | TPatOr(_) => assert(false)
        | TPatAny => false
        | _ => true
        },
      column,
    )
  ) {
  | exception Not_found =>
    /* only omegas on the column: the column is coherent. */
    true
  | discr_pat => List.for_all(coherent_heads(discr_pat), column)
  };
};

let first_column = simplified_matrix => List.map(fst, simplified_matrix);

/***********************/
/* Compatibility check */
/***********************/

/* Patterns p and q compatible means:
      there exists value V that matches both, However....

     The case of extension types is dubious, as constructor rebind permits
     that different constructors are the same (and are thus compatible).

     Compilation must take this into account, consider:

     type t = ..
     type t += A|B
     type t += C=A

     let f x y = match x,y with
     | true,A  -> '1'
     | _,C     -> '2'
     | false,A -> '3'
     | _,_     -> '_'

     As C is bound to A the value of f false A is '2' (and not '3' as it would
     be in the absence of rebinding).

     Not considering rebinding, patterns "false,A" and "_,C" are incompatible
     and the compiler can swap the second and third clause, resulting in the
     (more efficiently compiled) matching

     match x,y with
     | true,A  -> '1'
     | false,A -> '3'
     | _,C     -> '2'
     | _,_     -> '_'

     This is not correct: when C is bound to A, "f false A" returns '2' (not '3')



     However, diagnostics do not take constructor rebinding into account.
     Notice, that due to module abstraction constructor rebinding is hidden.

     module X : sig type t = .. type t += A|B end = struct
       type t = ..
       type t += A
       type t += B=A
     end

     open X

     let f x = match x with
     | A -> '1'
     | B -> '2'
     | _ -> '_'

     The second clause above will NOT (and cannot) be flagged as useless.

     Finally, there are two compatibility fonction
      compat p q      ---> 'syntactic compatibility, used for diagnostics.
      may_compat p q --->   a safe approximation of possible compat,
                            for compilation
   */

let const_compare = (x, y) =>
  switch (x, y) {
  | (Const_float(f1), Const_float(f2)) =>
    Stdlib.compare(float_of_string(f1), float_of_string(f2))
  | (Const_string(s1), Const_string(s2)) => String.compare(s1, s2)
  | (Const_bool(b1), Const_bool(b2)) =>
    Stdlib.compare(if (b1) {1} else {0}, if (b2) {1} else {0})
  | (
      Const_int(_) | Const_string(_) | Const_float(_) | Const_bool(_) |
      Const_void |
      Const_int32(_) |
      Const_int64(_),
      _,
    ) =>
    Stdlib.compare(x, y)
  };

module Compat =
       (
         Constr: {
           let equal:
             (Types.constructor_description, Types.constructor_description) =>
             bool;
         },
       ) => {
  let rec compat = (p, q) =>
    switch (p.pat_desc, q.pat_desc) {
    /* Variables match any value */
    | (TPatAny | TPatVar(_), _)
    | (_, TPatAny | TPatVar(_)) => true
    /* Structural induction */
    | ([@implicit_arity] TPatAlias(p, _, _), _) => compat(p, q)
    | (_, [@implicit_arity] TPatAlias(q, _, _)) => compat(p, q)
    | ([@implicit_arity] TPatOr(p1, p2), _) =>
      compat(p1, q) || compat(p2, q)
    | (_, [@implicit_arity] TPatOr(q1, q2)) =>
      compat(p, q1) || compat(p, q2)
    /* Constructors, with special case for extension */
    | (
        [@implicit_arity] TPatConstruct(_, c1, ps1),
        [@implicit_arity] TPatConstruct(_, c2, ps2),
      ) =>
      Constr.equal(c1, c2) && compats(ps1, ps2)
    /* More standard stuff */
    | (TPatConstant(c1), TPatConstant(c2)) => const_compare(c1, c2) == 0
    | (TPatTuple(ps), TPatTuple(qs)) => compats(ps, qs)
    | (_, _) => false
    }

  and ocompat = (op, oq) =>
    switch (op, oq) {
    | (None, None) => true
    | (Some(p), Some(q)) => compat(p, q)
    | (None, Some(_))
    | (Some(_), None) => false
    }

  and compats = (ps, qs) =>
    switch (ps, qs) {
    | ([], []) => true
    | ([p, ...ps], [q, ...qs]) => compat(p, q) && compats(ps, qs)
    | (_, _) => false
    };
};

module SyntacticCompat =
  Compat({
    let equal = (c1, c2) => Types.equal_tag(c1.cstr_tag, c2.cstr_tag);
  });

let compat = SyntacticCompat.compat
and compats = SyntacticCompat.compats;

exception Empty; /* Empty pattern */

/****************************************/
/* Utilities for retrieving type paths  */
/****************************************/

/* May need a clean copy, cf. PR#4745 */
let clean_copy = ty =>
  if (ty.level == Btype.generic_level) {
    ty;
  } else {
    Subst.type_expr(Subst.identity, ty);
  };

let get_constructor_type_path = (ty, tenv) => {
  let ty = Ctype.repr(Ctype.expand_head(tenv, clean_copy(ty)));
  switch (ty.desc) {
  | [@implicit_arity] TTyConstr(path, _, _) => path
  | _ => assert(false)
  };
};

/****************************/
/* Utilities for matching   */
/****************************/

/* Check top matching */
let simple_match = (p1, p2) =>
  switch (p1.pat_desc, p2.pat_desc) {
  | (
      [@implicit_arity] TPatConstruct(_, c1, _),
      [@implicit_arity] TPatConstruct(_, c2, _),
    ) =>
    Types.equal_tag(c1.cstr_tag, c2.cstr_tag)
  | (TPatConstant(c1), TPatConstant(c2)) => const_compare(c1, c2) == 0
  | (TPatTuple(p1s), TPatTuple(p2s)) => List.length(p1s) == List.length(p2s)
  | (_, TPatAny | TPatVar(_)) => true
  | (_, _) => false
  };

/* Build argument list when p2 >= p1, where p1 is a simple pattern */
let rec simple_match_args = (p1, p2) =>
  switch (p2.pat_desc) {
  | [@implicit_arity] TPatAlias(p2, _, _) => simple_match_args(p1, p2)
  | [@implicit_arity] TPatConstruct(_, _, args) => args
  | TPatTuple(args) => args
  | TPatAny
  | TPatVar(_) =>
    switch (p1.pat_desc) {
    | [@implicit_arity] TPatConstruct(_, _, args) => omega_list(args)
    | TPatTuple(args) => omega_list(args)
    | _ => []
    }
  | _ => []
  };

/*
   Normalize a pattern ->
    all arguments are omega (simple pattern) and no more variables
 */

let rec normalize_pat = q =>
  switch (q.pat_desc) {
  | TPatAny
  | TPatConstant(_) => q
  | TPatVar(_) => make_pat(TPatAny, q.pat_type, q.pat_env)
  | [@implicit_arity] TPatAlias(p, _, _) => normalize_pat(p)
  | TPatTuple(args) =>
    make_pat(TPatTuple(omega_list(args)), q.pat_type, q.pat_env)
  | [@implicit_arity] TPatRecord(fields, c) =>
    make_pat(
      [@implicit_arity]
      TPatRecord(List.map(((id, ld, pat)) => (id, ld, omega), fields), c),
      q.pat_type,
      q.pat_env,
    )
  | [@implicit_arity] TPatConstruct(lid, c, args) =>
    make_pat(
      [@implicit_arity] TPatConstruct(lid, c, omega_list(args)),
      q.pat_type,
      q.pat_env,
    )
  | TPatOr(_) => fatal_error("Parmatch.normalize_pat")
  };

/* Consider a pattern matrix whose first column has been simplified to contain
      only _ or a head constructor
        | p1, r1...
        | p2, r2...
        | p3, r3...
        | ...

      We build a normalized /discriminating/ pattern from a pattern [q] by folding
      over the first column of the matrix, "refining" [q] as we go:

      - when we encounter a row starting with [Tpat_tuple] then we
      can stop and return that pattern, as we cannot refine any further. Indeed,
      these constructors are alone in their signature, so they will subsume
      whatever other pattern we might find, as well as the pattern we're threading
      along.

      - rows starting with a wildcard do not bring any information, so we ignore
      them and keep going

      - if we encounter anything else (i.e. any other constructor), then we just
      stop and return our accumulator.
   */
let discr_pat = (q, pss) => {
  let rec refine_pat = acc =>
    fun
    | [] => acc
    | [(head, _), ...rows] =>
      switch (head.pat_desc) {
      | TPatOr(_)
      | TPatVar(_)
      | TPatAlias(_) => assert(false)
      | TPatAny => refine_pat(acc, rows)
      | TPatTuple(_)
      | TPatRecord(_) => normalize_pat(head)
      | TPatConstant(_)
      | TPatConstruct(_) => acc
      };

  let q = normalize_pat(q);
  /* short-circuiting: clearly if we have anything other than
     [Tpat_any] to start with, we're not going to be able refine at all. So
     there's no point going over the matrix. */
  switch (q.pat_desc) {
  | TPatAny => refine_pat(q, pss)
  | _ => q
  };
};

/*
    In case a matching value is found, set actual arguments
    of the matching pattern.
 */

let rec read_args = (xs, r) =>
  switch (xs, r) {
  | ([], _) => ([], r)
  | ([_, ...xs], [arg, ...rest]) =>
    let (args, rest) = read_args(xs, rest);
    ([arg, ...args], rest);
  | (_, _) => fatal_error("Parmatch.read_args")
  };

let do_set_args = (erase_mutable, q, r) =>
  switch (q) {
  | {pat_desc: TPatTuple(omegas)} =>
    let (args, rest) = read_args(omegas, r);
    [make_pat(TPatTuple(args), q.pat_type, q.pat_env), ...rest];
  | {pat_desc: [@implicit_arity] TPatConstruct(lid, c, omegas)} =>
    let (args, rest) = read_args(omegas, r);
    [
      make_pat(
        [@implicit_arity] TPatConstruct(lid, c, args),
        q.pat_type,
        q.pat_env,
      ),
      ...rest,
    ];
  | {pat_desc: TPatConstant(_) | TPatAny} => [q, ...r] /* case any is used in matching.ml */
  | _ => fatal_error("Parmatch.set_args")
  };

let set_args = (q, r) => do_set_args(false, q, r)
and set_args_erase_mutable = (q, r) => do_set_args(true, q, r);

/* Given a matrix of non-empty rows
     p1 :: r1...
     p2 :: r2...
     p3 :: r3...

     Simplify the first column [p1 p2 p3] by splitting all or-patterns.
     The result is a list of couples
       (simple pattern, rest of row)
     where a "simple pattern" starts with either the catch-all pattern omega (_)
     or a head constructor.

     For example,
       x :: r1
       (Some _) as y :: r2
       (None as x) as y :: r3
       (Some x | (None as x)) :: r4
     becomes
       (_, r1)
       (Some _, r2)
       (None, r3)
       (Some x, r4)
       (None, r4)
   */
let simplify_head_pat = (~add_column, p, ps, k) => {
  let rec simplify_head_pat = (p, ps, k) =>
    switch (p.pat_desc) {
    | [@implicit_arity] TPatAlias(p, _, _) => simplify_head_pat(p, ps, k)
    | [@implicit_arity] TPatVar(_, _) => add_column(omega, ps, k)
    | [@implicit_arity] TPatOr(p1, p2) =>
      simplify_head_pat(p1, ps, simplify_head_pat(p2, ps, k))
    | _ => add_column(p, ps, k)
    };
  simplify_head_pat(p, ps, k);
};

let rec simplify_first_col =
  fun
  | [] => []
  | [[], ..._] => assert(false) /* the rows are non-empty! */
  | [[p, ...ps], ...rows] => {
      let add_column = (p, ps, k) => [(p, ps), ...k];
      simplify_head_pat(~add_column, p, ps, simplify_first_col(rows));
    };

/* Builds the specialized matrix of [pss] according to pattern [q].
      See section 3.1 of http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

      NOTES:
      - expects [pss] to be a "simplified matrix", cf. [simplify_first_col]
      - [q] was produced by [discr_pat]
      - we are polymorphic on the type of matrices we work on, in particular a row
      might not simply be a [pattern list]. That's why we have the [extend_row]
      parameter.
   */
let build_specialized_submatrix = (~extend_row, q, pss) => {
  let rec filter_rec =
    fun
    | [({pat_desc: TPatAlias(_) | TPatOr(_) | TPatVar(_)}, _), ..._] =>
      assert(false)
    | [(p, ps), ...pss] =>
      if (simple_match(q, p)) {
        [extend_row(simple_match_args(q, p), ps), ...filter_rec(pss)];
      } else {
        filter_rec(pss);
      }
    | _ => [];
  filter_rec(pss);
};

/* The "default" and "specialized" matrices of a given matrix.
      See section 3.1 of http://moscova.inria.fr/~maranget/papers/warn/warn.pdf .
   */
type specialized_matrices('matrix) = {
  default: 'matrix,
  constrs: list((pattern, 'matrix)),
};

/* Consider a pattern matrix whose first column has been simplified
      to contain only _ or a head constructor
        | p1, r1...
        | p2, r2...
        | p3, r3...
        | ...

      We split this matrix into a list of /specialized/ sub-matrices, one for
      each head constructor appearing in the first column. For each row whose
      first column starts with a head constructor, remove this head
      column, prepend one column for each argument of the constructor,
      and add the resulting row in the sub-matrix corresponding to this
      head constructor.

      Rows whose left column is omega (the Any pattern _) may match any
      head constructor, so they are added to all sub-matrices.

      In the case where all the rows in the matrix have an omega on their first
      column, then there is only one /specialized/ sub-matrix, formed of all these
      omega rows.
      This matrix is also called the /default/ matrix.

      See the documentation of [build_specialized_submatrix] for an explanation of
      the [extend_row] parameter.
   */
let build_specialized_submatrices = (~extend_row, q, rows) => {
  let extend_group = (discr, p, r, rs) => {
    let r = extend_row(simple_match_args(discr, p), r);
    (discr, [r, ...rs]);
  };

  /* insert a row of head [p] and rest [r] into the right group */
  let rec insert_constr = (p, r) =>
    fun
    | [] =>
      /* if no group matched this row, it has a head constructor that
         was never seen before; add a new sub-matrix for this head */
      [extend_group(normalize_pat(p), p, r, [])]
    | [(q0, rs) as bd, ...env] =>
      if (simple_match(q0, p)) {
        [extend_group(q0, p, r, rs), ...env];
      } else {
        [bd, ...insert_constr(p, r, env)];
      };

  /* insert a row of head omega into all groups */
  let insert_omega = (r, env) =>
    List.map(((q0, rs)) => extend_group(q0, omega, r, rs), env);

  let rec form_groups = (constr_groups, omega_tails) =>
    fun
    | [] => (constr_groups, omega_tails)
    | [({pat_desc: TPatVar(_) | TPatAlias(_) | TPatOr(_)}, _), ..._] =>
      assert(false)
    | [({pat_desc: TPatAny}, tail), ...rest] =>
      /* note that calling insert_omega here would be wrong
         as some groups may not have been formed yet, if the
         first row with this head pattern comes after in the list */
      form_groups(constr_groups, [tail, ...omega_tails], rest)
    | [(p, r), ...rest] =>
      form_groups(insert_constr(p, r, constr_groups), omega_tails, rest);

  let (constr_groups, omega_tails) = {
    let initial_constr_group =
      switch (q.pat_desc) {
      | TPatTuple(_) =>
        /* [q] comes from [discr_pat], and in this case subsumes any of the
           patterns we could find on the first column of [rows]. So it is better
           to use it for our initial environment than any of the normalized
           pattern we might obtain from the first column. */
        [(q, [])]
      | _ => []
      };

    form_groups(initial_constr_group, [], rows);
  };

  {
    default: omega_tails,
    constrs:
      /* insert omega rows in all groups */
      List.fold_right(insert_omega, omega_tails, constr_groups),
  };
};

/* Variant related functions */

let set_last = a => {
  let rec loop =
    fun
    | [] => assert(false)
    | [_] => [a]
    | [x, ...l] => [x, ...loop(l)];

  fun
  | (_, []) => (a, [])
  | (first, row) => (first, loop(row));
};

/* mark constructor lines for failure when they are incomplete

   Precondition: the input matrix has been simplified so that its
   first column only contains _ or head constructors. */
let mark_partial =
  List.map(
    fun
    | ({pat_desc: TPatVar(_) | TPatAlias(_) | TPatOr(_)}, _) =>
      assert(false)
    | ({pat_desc: TPatAny}, _) as ps => ps
    | ps => set_last(zero, ps),
  );

/*
   Check whether the first column of env makes up a complete signature or
   not. We work on the discriminating patterns of each sub-matrix: they
   are simplified, and are not omega/Tpat_any.
 */
let full_match = (closing, env) =>
  switch (env) {
  | [({pat_desc: TPatAny | TPatVar(_) | TPatAlias(_) | TPatOr(_)}, _), ..._] =>
    /* discriminating patterns are simplified */
    assert(false)
  | [] => false
  | [({pat_desc: [@implicit_arity] TPatConstruct(_, c, _)}, _), ..._] =>
    if (c.cstr_consts < 0) {
      false;
    } else {
      /* extensions */
      List.length(env) == c.cstr_consts + c.cstr_nonconsts;
    }
  | [({pat_desc: TPatConstant(_)}, _), ..._] => false
  | [({pat_desc: TPatTuple(_)}, _), ..._]
  | [({pat_desc: TPatRecord(_)}, _), ..._] => true
  };

/* Written as a non-fragile matching, PR#7451 originated from a fragile matching below. */
let should_extend = (ext, env) =>
  switch (ext) {
  | None => false
  | Some(ext) =>
    switch (env) {
    | [] => assert(false)
    | [(p, _), ..._] =>
      switch (p.pat_desc) {
      | [@implicit_arity]
        TPatConstruct(
          _,
          {cstr_tag: CstrConstant(_) | CstrBlock(_) | CstrUnboxed},
          _,
        ) =>
        let path = get_constructor_type_path(p.pat_type, p.pat_env);
        Path.same(path, ext);
      | TPatConstant(_)
      | TPatTuple(_)
      | TPatRecord(_) => false
      | TPatAny
      | TPatVar(_)
      | TPatAlias(_)
      | TPatOr(_) => assert(false)
      }
    }
  };

module ConstructorTagHashtbl =
  Hashtbl.Make({
    type t = Types.constructor_tag;
    let hash = Hashtbl.hash;
    let equal = Types.equal_tag;
  });

/* complement constructor tags */
let complete_tags = (nconsts, nconstrs, tags) => {
  let seen_const = Array.make(nconsts, false)
  and seen_constr = Array.make(nconstrs, false);
  List.iter(
    fun
    | CstrConstant(i) => seen_const[i] = true
    | CstrBlock(i) => seen_constr[i] = true
    | _ => assert(false),
    tags,
  );
  let r = ConstructorTagHashtbl.create(nconsts + nconstrs);
  for (i in 0 to nconsts - 1) {
    if (!seen_const[i]) {
      ConstructorTagHashtbl.add(r, CstrConstant(i), ());
    };
  };
  for (i in 0 to nconstrs - 1) {
    if (!seen_constr[i]) {
      ConstructorTagHashtbl.add(r, CstrBlock(i), ());
    };
  };
  r;
};

/* build a pattern from a constructor description */
let pat_of_constr = (ex_pat, cstr) => {
  ...ex_pat,
  pat_desc:
    [@implicit_arity]
    TPatConstruct(
      mknoloc(Identifier.IdentName("?pat_of_constr?")),
      cstr,
      omegas(cstr.cstr_arity),
    ),
};

let orify = (x, y) =>
  make_pat([@implicit_arity] TPatOr(x, y), x.pat_type, x.pat_env);

let rec orify_many =
  fun
  | [] => assert(false)
  | [x] => x
  | [x, ...xs] => orify(x, orify_many(xs));

/* build an or-pattern from a constructor list */
let pat_of_constrs = (ex_pat, cstrs) =>
  if (cstrs == []) {
    raise(Empty);
  } else {
    orify_many(List.map(pat_of_constr(ex_pat), cstrs));
  };

let pats_of_type = (~always=false, env, ty) => {
  let ty' = Ctype.expand_head(env, ty);
  switch (ty'.desc) {
  | [@implicit_arity] TTyConstr(path, _, _) =>
    try(
      switch (Env.find_type(path, env).type_kind) {
      | TDataVariant(cl)
          when
            always
            || List.length(cl) == 1
            || List.for_all(cd => cd.Types.cd_res != None, cl) =>
        let (cstrs, _) = Env.find_type_descrs(path, env);
        List.map(pat_of_constr(make_pat(TPatAny, ty, env)), cstrs);
      | _ => [omega]
      }
    ) {
    | Not_found => [omega]
    }
  | TTyTuple(tl) => [
      make_pat(TPatTuple(omegas(List.length(tl))), ty, env),
    ]
  | _ => [omega]
  };
};

let rec get_variant_constructors = (env, ty) =>
  switch (Ctype.repr(ty).desc) {
  | [@implicit_arity] TTyConstr(path, _, _) =>
    try(
      switch (Env.find_type(path, env)) {
      | {type_kind: TDataVariant(_)} => Env.find_type_descrs(path, env)
      | {type_manifest: Some(_)} =>
        get_variant_constructors(
          env,
          Ctype.expand_head_once(env, clean_copy(ty)),
        )
      | _ => fatal_error("Parmatch.get_variant_constructors")
      }
    ) {
    | Not_found => fatal_error("Parmatch.get_variant_constructors")
    }
  | _ => fatal_error("Parmatch.get_variant_constructors")
  };

/* Sends back a pattern that complements constructor tags all_tag */
let complete_constrs = (p, all_tags) => {
  let c =
    switch (p.pat_desc) {
    | [@implicit_arity] TPatConstruct(_, c, _) => c
    | _ => assert(false)
    };
  let not_tags = complete_tags(c.cstr_consts, c.cstr_nonconsts, all_tags);
  let (constrs, _) = get_variant_constructors(p.pat_env, c.cstr_res);
  let others =
    List.filter(
      cnstr => ConstructorTagHashtbl.mem(not_tags, cnstr.cstr_tag),
      constrs,
    );
  let (const, nonconst) =
    List.partition(cnstr => cnstr.cstr_arity == 0, others);
  const @ nonconst;
};

let build_other_constrs = (env, p) =>
  switch (p.pat_desc) {
  | [@implicit_arity]
    TPatConstruct(_, {cstr_tag: CstrConstant(_) | CstrBlock(_)}, _) =>
    let get_tag = (
      fun
      | {pat_desc: [@implicit_arity] TPatConstruct(_, c, _)} => c.cstr_tag
      | _ => fatal_error("Parmatch.get_tag")
    );
    let all_tags = List.map(((p, _)) => get_tag(p), env);
    pat_of_constrs(p, complete_constrs(p, all_tags));
  | _ => extra_pat
  };

/* Auxiliary for build_other */

let build_other_constant = (proj, make, first, next, p, env) => {
  let all = List.map(((p, _)) => proj(p.pat_desc), env);
  let rec try_const = i =>
    if (List.mem(i, all)) {
      try_const(next(i));
    } else {
      make_pat(make(i), p.pat_type, p.pat_env);
    };
  try_const(first);
};

/*
   Builds a pattern that is incompatible with all patterns in
   the first column of env
 */

let some_private_tag = "<some private tag>";

let build_other = (ext, env) =>
  switch (env) {
  | [({pat_desc: TPatConstruct(_)} as p, _), ..._] =>
    switch (ext) {
    | Some(ext) =>
      if (Path.same(ext, get_constructor_type_path(p.pat_type, p.pat_env))) {
        extra_pat;
      } else {
        build_other_constrs(env, p);
      }
    | _ => build_other_constrs(env, p)
    }
  | [({pat_desc: TPatConstant(Const_int(_))} as p, _), ..._] =>
    build_other_constant(
      fun
      | TPatConstant(Const_int(i)) => i
      | _ => assert(false),
      fun
      | i => TPatConstant(Const_int(i)),
      0,
      succ,
      p,
      env,
    )
  | [({pat_desc: TPatConstant(Const_int32(_))} as p, _), ..._] =>
    build_other_constant(
      fun
      | TPatConstant(Const_int32(i)) => i
      | _ => assert(false),
      fun
      | i => TPatConstant(Const_int32(i)),
      0l,
      Int32.succ,
      p,
      env,
    )
  | [({pat_desc: TPatConstant(Const_int64(_))} as p, _), ..._] =>
    build_other_constant(
      fun
      | TPatConstant(Const_int64(i)) => i
      | _ => assert(false),
      fun
      | i => TPatConstant(Const_int64(i)),
      0L,
      Int64.succ,
      p,
      env,
    )
  /*| ({pat_desc=(TPatConstant (Const_bool _))} as p,_) :: _ ->
    build_other_constant
      (function TPatConstant(Const_bool i) -> i | _ -> assert false)
      (function i -> TPatConstant(Const_bool i))
      ??? p env*/
  | [({pat_desc: TPatConstant(Const_string(_))} as p, _), ..._] =>
    build_other_constant(
      fun
      | TPatConstant(Const_string(s)) => String.length(s)
      | _ => assert(false),
      fun
      | i => TPatConstant(Const_string(String.make(i, '*'))),
      0,
      succ,
      p,
      env,
    )
  | [({pat_desc: TPatConstant(Const_float(_))} as p, _), ..._] =>
    build_other_constant(
      fun
      | TPatConstant(Const_float(f)) => float_of_string(f)
      | _ => assert(false),
      fun
      | f => TPatConstant(Const_float(string_of_float(f))),
      0.0,
      f => f +. 1.0,
      p,
      env,
    )

  | [] => omega
  | _ => omega
  };

let rec has_instance = p =>
  switch (p.pat_desc) {
  | TPatAny
  | TPatVar(_)
  | TPatConstant(_) => true
  | [@implicit_arity] TPatAlias(p, _, _) => has_instance(p)
  | [@implicit_arity] TPatOr(p1, p2) => has_instance(p1) || has_instance(p2)
  | [@implicit_arity] TPatConstruct(_, _, ps)
  | TPatTuple(ps) => has_instances(ps)
  | [@implicit_arity] TPatRecord(fields, _) =>
    let ps = List.map(((_, _, p)) => p, fields);
    has_instances(ps);
  }

and has_instances =
  fun
  | [] => true
  | [q, ...rem] => has_instance(q) && has_instances(rem);

/*
   Core function :
   Is the last row of pattern matrix pss + qs satisfiable ?
   That is :
     Does there exists at least one value vector, es such that :
      1- for all ps in pss ps # es (ps and es are not compatible)
      2- qs <= es                  (es matches qs)

    ---

    In two places in the following function, we check the coherence of the first
    column of (pss + qs).
    If it is incoherent, then we exit early saying that (pss + qs) is not
    satisfiable (which is equivalent to saying "oh, we shouldn't have considered
    that branch, no good result came come from here").

    But what happens if we have a coherent but ill-typed column?
    - we might end up returning [false], which is equivalent to noticing the
    incompatibility: clearly this is fine.
    - if we end up returning [true] then we're saying that [qs] is useful while
    it is not. This is sad but not the end of the world, we're just allowing dead
    code to survive.
 */
let rec satisfiable = (pss, qs) =>
  switch (pss) {
  | [] => has_instances(qs)
  | _ =>
    switch (qs) {
    | [] => false
    | [{pat_desc: [@implicit_arity] TPatOr(q1, q2)}, ...qs] =>
      satisfiable(pss, [q1, ...qs]) || satisfiable(pss, [q2, ...qs])
    | [{pat_desc: [@implicit_arity] TPatAlias(q, _, _)}, ...qs] =>
      satisfiable(pss, [q, ...qs])
    | [{pat_desc: TPatAny | TPatVar(_)}, ...qs] =>
      let pss = simplify_first_col(pss);
      if (!all_coherent(first_column(pss))) {
        false;
      } else {
        let {default, constrs} = {
          let q0 = discr_pat(omega, pss);
          build_specialized_submatrices(~extend_row=(@), q0, pss);
        };
        if (!full_match(false, constrs)) {
          satisfiable(default, qs);
        } else {
          List.exists(
            ((p, pss)) =>
              satisfiable(pss, simple_match_args(p, omega) @ qs),
            constrs,
          );
        };
      };
    | [q, ...qs] =>
      let pss = simplify_first_col(pss);
      if (!all_coherent([q, ...first_column(pss)])) {
        false;
      } else {
        let q0 = discr_pat(q, pss);
        satisfiable(
          build_specialized_submatrix(~extend_row=(@), q0, pss),
          simple_match_args(q0, q) @ qs,
        );
      };
    }
  };

/* While [satisfiable] only checks whether the last row of [pss + qs] is
   satisfiable, this function returns the (possibly empty) list of vectors [es]
   which verify:
     1- for all ps in pss, ps # es (ps and es are not compatible)
     2- qs <= es                   (es matches qs)

   This is done to enable GADT handling

   For considerations regarding the coherence check, see the comment on
   [satisfiable] above.  */
let rec list_satisfying_vectors = (pss, qs) =>
  switch (pss) {
  | [] =>
    if (has_instances(qs)) {
      [qs];
    } else {
      [];
    }
  | _ =>
    switch (qs) {
    | [] => []
    | [{pat_desc: [@implicit_arity] TPatOr(q1, q2)}, ...qs] =>
      list_satisfying_vectors(pss, [q1, ...qs])
      @ list_satisfying_vectors(pss, [q2, ...qs])
    | [{pat_desc: [@implicit_arity] TPatAlias(q, _, _)}, ...qs] =>
      list_satisfying_vectors(pss, [q, ...qs])
    | [{pat_desc: TPatAny | TPatVar(_)}, ...qs] =>
      let pss = simplify_first_col(pss);
      if (!all_coherent(first_column(pss))) {
        [];
      } else {
        let q0 = discr_pat(omega, pss);
        let wild = (default_matrix, p) =>
          List.map(
            qs => [p, ...qs],
            list_satisfying_vectors(default_matrix, qs),
          );

        switch (build_specialized_submatrices(~extend_row=(@), q0, pss)) {
        | {default, constrs: []} =>
          /* first column of pss is made of variables only */
          wild(default, omega)
        | {default, constrs: [(p, _), ..._] as constrs} =>
          let for_constrs = () =>
            List.flatten(
              List.map(
                ((p, pss)) => {
                  let witnesses =
                    list_satisfying_vectors(
                      pss,
                      simple_match_args(p, omega) @ qs,
                    );

                  List.map(set_args(p), witnesses);
                },
                constrs,
              ),
            );

          if (full_match(false, constrs)) {
            for_constrs();
          } else {
            switch (p.pat_desc) {
            | TPatConstruct(_) =>
              /* activate this code for checking non-gadt constructors */
              wild(default, build_other_constrs(constrs, p)) @ for_constrs()
            | _ => wild(default, omega)
            };
          };
        };
      };
    | [q, ...qs] =>
      let pss = simplify_first_col(pss);
      if (!all_coherent([q, ...first_column(pss)])) {
        [];
      } else {
        let q0 = discr_pat(q, pss);
        List.map(
          set_args(q0),
          list_satisfying_vectors(
            build_specialized_submatrix(~extend_row=(@), q0, pss),
            simple_match_args(q0, q) @ qs,
          ),
        );
      };
    }
  };

/******************************************/
/* Look for a row that matches some value */
/******************************************/

/*
   Useful for seeing if the example of
   non-matched value can indeed be matched
   (by a guarded clause)
 */

let rec do_match = (pss, qs) =>
  switch (qs) {
  | [] =>
    switch (pss) {
    | [[], ..._] => true
    | _ => false
    }
  | [q, ...qs] =>
    switch (q) {
    | {pat_desc: [@implicit_arity] TPatOr(q1, q2)} =>
      do_match(pss, [q1, ...qs]) || do_match(pss, [q2, ...qs])
    | {pat_desc: TPatAny} =>
      let rec remove_first_column = (
        fun
        | [[_, ...ps], ...rem] => [ps, ...remove_first_column(rem)]
        | _ => []
      );

      do_match(remove_first_column(pss), qs);
    | _ =>
      let q0 = normalize_pat(q);
      let pss = simplify_first_col(pss);
      /* [pss] will (or won't) match [q0 :: qs] regardless of the coherence of
         its first column. */
      do_match(
        build_specialized_submatrix(~extend_row=(@), q0, pss),
        simple_match_args(q0, q) @ qs,
      );
    }
  };

type exhaust_result('a) =
  | No_matching_value
  | Witnesses(list('a));

let rappend = (r1, r2) =>
  switch (r1, r2) {
  | (No_matching_value, _) => r2
  | (_, No_matching_value) => r1
  | (Witnesses(l1), Witnesses(l2)) => Witnesses(l1 @ l2)
  };

let rec try_many = f =>
  fun
  | [] => No_matching_value
  | [(p, pss), ...rest] => rappend(f((p, pss)), try_many(f, rest));

/*
   Now another satisfiable function that additionally
   supplies an example of a matching value.
   This function should be called for exhaustiveness check only.
 */
let rec exhaust = (ext: option(Path.t), pss, n) =>
  switch (pss) {
  | [] => Witnesses([omegas(n)])
  | [[], ..._] => No_matching_value
  | pss =>
    let pss = simplify_first_col(pss);
    if (!all_coherent(first_column(pss))) {
      No_matching_value; // We're considering an ill-typed branch, we won't actually be able to produce a well typed value taking that branch.
    } else {
      /* Assuming the first column is ill-typed but considered coherent, we
         might end up producing an ill-typed witness of non-exhaustivity
         corresponding to the current branch.

         If [exhaust] has been called by [do_check_partial], then the witnesses
         produced get typechecked and the ill-typed ones are discarded.

         If [exhaust] has been called by [do_check_fragile], then it is possible
         we might fail to warn the user that the matching is fragile. See for
         example testsuite/tests/warnings/w04_failure.ml. */
      let q0 = discr_pat(omega, pss);
      switch (build_specialized_submatrices(~extend_row=(@), q0, pss)) {
      | {default, constrs: []} =>
        /* first column of pss is made of variables only */
        switch (exhaust(ext, default, n - 1)) {
        | Witnesses(r) => Witnesses(List.map(row => [q0, ...row], r))
        | r => r
        }
      | {default, constrs} =>
        let try_non_omega = ((p, pss)) =>
          switch (
            exhaust(
              ext,
              pss,
              List.length(simple_match_args(p, omega)) + n - 1,
            )
          ) {
          | Witnesses(r) => Witnesses(List.map(row => set_args(p, row), r))
          | r => r
          };
        let before = try_many(try_non_omega, constrs);
        if (full_match(false, constrs) && !should_extend(ext, constrs)) {
          before;
        } else {
          let r = exhaust(ext, default, n - 1);
          switch (r) {
          | No_matching_value => before
          | Witnesses(r) =>
            try({
              let p = build_other(ext, constrs);
              let dug = List.map(tail => [p, ...tail], r);
              switch (before) {
              | No_matching_value => Witnesses(dug)
              | Witnesses(x) => Witnesses(x @ dug)
              };
            }) {
            /* cannot occur, since constructors don't make a full signature */
            | Empty => fatal_error("Parmatch.exhaust")
            }
          };
        };
      };
    };
  };

let exhaust = (ext, pss, n) => {
  let ret = exhaust(ext, pss, n);
  switch (ret) {
  | No_matching_value => No_matching_value
  | Witnesses(lst) =>
    let singletons =
      List.map(
        fun
        | [x] => x
        | _ => assert(false),
        lst,
      );

    Witnesses([orify_many(singletons)]);
  };
};

/*
    Another exhaustiveness check, enforcing variant typing.
    Note that it does not check exact exhaustiveness, but whether a
    matching could be made exhaustive by closing all variant types.
    When this is true of all other columns, the current column is left
    open (even if it means that the whole matching is not exhaustive as
    a result).
    When this is false for the matrix minus the current column, and the
    current column is composed of variant tags, we close the variant
    (even if it doesn't help in making the matching exhaustive).
 */

let rec pressure_variants = tdefs =>
  fun
  | [] => false
  | [[], ..._] => true
  | pss => {
      let pss = simplify_first_col(pss);
      if (!all_coherent(first_column(pss))) {
        true;
      } else {
        let q0 = discr_pat(omega, pss);
        switch (build_specialized_submatrices(~extend_row=(@), q0, pss)) {
        | {default, constrs: []} => pressure_variants(tdefs, default)
        | {default, constrs} =>
          let rec try_non_omega = (
            fun
            | [(_p, pss), ...rem] => {
                let ok = pressure_variants(tdefs, pss);
                /* The order below matters : we want [pressure_variants] to be
                   called on all the specialized submatrices because we might
                   close some variant in any of them regardless of whether [ok]
                   is true for [pss] or not */
                try_non_omega(rem) && ok;
              }
            | [] => true
          );

          if (full_match(tdefs == None, constrs)) {
            try_non_omega(constrs);
          } else if (tdefs == None) {
            pressure_variants(None, default);
          } else {
            let full = full_match(true, constrs);
            let ok =
              if (full) {
                try_non_omega(constrs);
              } else {
                let {constrs: partial_constrs, _} =
                  build_specialized_submatrices(
                    ~extend_row=(@),
                    q0,
                    mark_partial(pss),
                  );

                try_non_omega(partial_constrs);
              };

            ok;
          };
        };
      };
    };

/* Yet another satisfiable function */

/*
    This time every_satisfiable pss qs checks the
    utility of every expansion of qs.
    Expansion means expansion of or-patterns inside qs
 */

type answer =
  | Used /* Useful pattern */
  | Unused /* Useless pattern */
  | Upartial(list(Typedtree.pattern)); /* Mixed, with list of useless ones */

/* this row type enable column processing inside the matrix
       - left  ->  elements not to be processed,
       - right ->  elements to be processed
   */
type usefulness_row = {
  no_ors: list(pattern),
  ors: list(pattern),
  active: list(pattern),
};

/* Initial build */
let make_row = ps => {ors: [], no_ors: [], active: ps};

let make_rows = pss => List.map(make_row, pss);

/* Useful to detect and expand  or pats inside as pats */
let rec unalias = p =>
  switch (p.pat_desc) {
  | [@implicit_arity] TPatAlias(p, _, _) => unalias(p)
  | _ => p
  };

let is_var = p =>
  switch (unalias(p).pat_desc) {
  | TPatAny
  | TPatVar(_) => true
  | _ => false
  };

let is_var_column = rs =>
  List.for_all(
    r =>
      switch (r.active) {
      | [p, ..._] => is_var(p)
      | [] => assert(false)
      },
    rs,
  );

/* Standard or-args for left-to-right matching */
let rec or_args = p =>
  switch (p.pat_desc) {
  | [@implicit_arity] TPatOr(p1, p2) => (p1, p2)
  | [@implicit_arity] TPatAlias(p, _, _) => or_args(p)
  | _ => assert(false)
  };

/* Just remove current column */
let remove = r =>
  switch (r.active) {
  | [_, ...rem] => {...r, active: rem}
  | [] => assert(false)
  };

let remove_column = rs => List.map(remove, rs);

/* Current column has been processed */
let push_no_or = r =>
  switch (r.active) {
  | [p, ...rem] => {...r, no_ors: [p, ...r.no_ors], active: rem}
  | [] => assert(false)
  };

let push_or = r =>
  switch (r.active) {
  | [p, ...rem] => {...r, ors: [p, ...r.ors], active: rem}
  | [] => assert(false)
  };

let push_or_column = rs => List.map(push_or, rs)
and push_no_or_column = rs => List.map(push_no_or, rs);

let rec simplify_first_usefulness_col =
  fun
  | [] => []
  | [row, ...rows] =>
    switch (row.active) {
    | [] => assert(false) /* the rows are non-empty! */
    | [p, ...ps] =>
      let add_column = (p, ps, k) => [(p, {...row, active: ps}), ...k];
      simplify_head_pat(
        ~add_column,
        p,
        ps,
        simplify_first_usefulness_col(rows),
      );
    };

/* Back to normal matrices */
let make_vector = r => List.rev(r.no_ors);

let make_matrix = rs => List.map(make_vector, rs);

/* Standard union on answers */
let union_res = (r1, r2) =>
  switch (r1, r2) {
  | (Unused, _)
  | (_, Unused) => Unused
  | (Used, _) => r2
  | (_, Used) => r1
  | (Upartial(u1), Upartial(u2)) => Upartial(u1 @ u2)
  };

/* propose or pats for expansion */
let extract_elements = qs => {
  let rec do_rec = seen =>
    fun
    | [] => []
    | [q, ...rem] => [
        {
          no_ors: List.rev_append(seen, rem) @ qs.no_ors,
          ors: [],
          active: [q],
        },
        ...do_rec([q, ...seen], rem),
      ];
  do_rec([], qs.ors);
};

/* idem for matrices */
let transpose = rs =>
  switch (rs) {
  | [] => assert(false)
  | [r, ...rem] =>
    let i = List.map(x => [x], r);
    List.fold_left(List.map2((r, x) => [x, ...r]), i, rem);
  };

let extract_columns = (pss, qs) =>
  switch (pss) {
  | [] => List.map(_ => [], qs.ors)
  | _ =>
    let rows = List.map(extract_elements, pss);
    transpose(rows);
  };

/* Core function
      The idea is to first look for or patterns (recursive case), then
      check or-patterns argument usefulness (terminal case)
   */

let rec every_satisfiables = (pss, qs) =>
  switch (qs.active) {
  | [] =>
    /* qs is now partitionned,  check usefulness */
    switch (qs.ors) {
    | [] =>
      /* no or-patterns */
      if (satisfiable(make_matrix(pss), make_vector(qs))) {
        Used;
      } else {
        Unused;
      }
    | _ =>
      /* n or-patterns -> 2n expansions */
      List.fold_right2(
        (pss, qs, r) =>
          switch (r) {
          | Unused => Unused
          | _ =>
            switch (qs.active) {
            | [q] =>
              let (q1, q2) = or_args(q);
              let r_loc = every_both(pss, qs, q1, q2);
              union_res(r, r_loc);
            | _ => assert(false)
            }
          },
        extract_columns(pss, qs),
        extract_elements(qs),
        Used,
      )
    }
  | [q, ...rem] =>
    let uq = unalias(q);
    switch (uq.pat_desc) {
    | TPatAny
    | TPatVar(_) =>
      if (is_var_column(pss)) {
        /* forget about ``all-variable''  columns now */
        every_satisfiables(
          remove_column(pss),
          remove(qs),
        );
      } else {
        /* otherwise this is direct food for satisfiable */
        every_satisfiables(
          push_no_or_column(pss),
          push_no_or(qs),
        );
      }
    | [@implicit_arity] TPatOr(q1, q2) =>
      if (q1.pat_loc.Location.loc_ghost && q2.pat_loc.Location.loc_ghost) {
        /* syntactically generated or-pats should not be expanded */
        every_satisfiables(
          push_no_or_column(pss),
          push_no_or(qs),
        );
      } else {
        /* this is a real or-pattern */
        every_satisfiables(
          push_or_column(pss),
          push_or(qs),
        );
      }
    | _ =>
      /* standard case, filter matrix */
      let pss = simplify_first_usefulness_col(pss);
      /* The handling of incoherent matrices is kept in line with
         [satisfiable] */
      if (!all_coherent([uq, ...first_column(pss)])) {
        Unused;
      } else {
        let q0 = discr_pat(q, pss);
        every_satisfiables(
          build_specialized_submatrix(q0, pss, ~extend_row=(ps, r) =>
            {...r, active: ps @ r.active}
          ),
          {...qs, active: simple_match_args(q0, q) @ rem},
        );
      };
    };
  }

/*
 This function ``every_both'' performs the usefulness check
 of or-pat q1|q2.
 The trick is to call every_satisfied twice with
 current active columns restricted to q1 and q2,
 That way,
 - others orpats in qs.ors will not get expanded.
 - all matching work performed on qs.no_ors is not performed again.
 */
and every_both = (pss, qs, q1, q2) => {
  let qs1 = {...qs, active: [q1]}
  and qs2 = {...qs, active: [q2]};
  let r1 = every_satisfiables(pss, qs1)
  and r2 =
    every_satisfiables(
      if (compat(q1, q2)) {
        [qs1, ...pss];
      } else {
        pss;
      },
      qs2,
    );
  switch (r1) {
  | Unused =>
    switch (r2) {
    | Unused => Unused
    | Used => Upartial([q1])
    | Upartial(u2) => Upartial([q1, ...u2])
    }
  | Used =>
    switch (r2) {
    | Unused => Upartial([q2])
    | _ => r2
    }
  | Upartial(u1) =>
    switch (r2) {
    | Unused => Upartial(u1 @ [q2])
    | Used => r1
    | Upartial(u2) => Upartial(u1 @ u2)
    }
  };
};

/* le_pat p q  means, forall V,  V matches q implies V matches p */
let rec le_pat = (p, q) =>
  switch (p.pat_desc, q.pat_desc) {
  | (TPatVar(_) | TPatAny, _) => true
  | ([@implicit_arity] TPatAlias(p, _, _), _) => le_pat(p, q)
  | (_, [@implicit_arity] TPatAlias(q, _, _)) => le_pat(p, q)
  | (TPatConstant(c1), TPatConstant(c2)) => const_compare(c1, c2) == 0
  | (
      [@implicit_arity] TPatConstruct(_, c1, ps),
      [@implicit_arity] TPatConstruct(_, c2, qs),
    ) =>
    Types.equal_tag(c1.cstr_tag, c2.cstr_tag) && le_pats(ps, qs)

  | (TPatTuple(ps), TPatTuple(qs)) => le_pats(ps, qs)
  /* In all other cases, enumeration is performed */
  | (_, _) => !satisfiable([[p]], [q])
  }

and le_pats = (ps, qs) =>
  switch (ps, qs) {
  | ([p, ...ps], [q, ...qs]) => le_pat(p, q) && le_pats(ps, qs)
  | (_, _) => true
  };

let get_mins = (le, ps) => {
  let rec select_rec = r =>
    fun
    | [] => r
    | [p, ...ps] =>
      if (List.exists(p0 => le(p0, p), ps)) {
        select_rec(r, ps);
      } else {
        select_rec([p, ...r], ps);
      };
  select_rec([], select_rec([], ps));
};

/*
   lub p q is a pattern that matches all values matched by p and q
   may raise Empty, when p and q are not compatible
 */

let rec lub = (p, q) =>
  switch (p.pat_desc, q.pat_desc) {
  | ([@implicit_arity] TPatAlias(p, _, _), _) => lub(p, q)
  | (_, [@implicit_arity] TPatAlias(q, _, _)) => lub(p, q)
  | (TPatAny | TPatVar(_), _) => q
  | (_, TPatAny | TPatVar(_)) => p
  | ([@implicit_arity] TPatOr(p1, p2), _) => orlub(p1, p2, q)
  | (_, [@implicit_arity] TPatOr(q1, q2)) => orlub(q1, q2, p) /* Thanks god, lub is commutative */
  | (TPatConstant(c1), TPatConstant(c2)) when const_compare(c1, c2) == 0 => p
  | (TPatTuple(ps), TPatTuple(qs)) =>
    let rs = lubs(ps, qs);
    make_pat(TPatTuple(rs), p.pat_type, p.pat_env);
  | (
      [@implicit_arity] TPatConstruct(lid, c1, ps1),
      [@implicit_arity] TPatConstruct(_, c2, ps2),
    )
      when Types.equal_tag(c1.cstr_tag, c2.cstr_tag) =>
    let rs = lubs(ps1, ps2);
    make_pat(
      [@implicit_arity] TPatConstruct(lid, c1, rs),
      p.pat_type,
      p.pat_env,
    );
  | (_, _) => raise(Empty)
  }

and orlub = (p1, p2, q) =>
  try({
    let r1 = lub(p1, q);
    try({...q, pat_desc: [@implicit_arity] TPatOr(r1, lub(p2, q))}) {
    | Empty => r1
    };
  }) {
  | Empty => lub(p2, q)
  }

and lubs = (ps, qs) =>
  switch (ps, qs) {
  | ([p, ...ps], [q, ...qs]) => [lub(p, q), ...lubs(ps, qs)]
  | (_, _) => []
  };

/******************************/
/* Exported variant closing   */
/******************************/

/* Apply pressure to variants */

let pressure_variants = (tdefs, patl) => {
  let pss = List.map(p => [p, omega], patl);
  ignore(pressure_variants(Some(tdefs), pss));
};

/*****************************/
/* Utilities for diagnostics */
/*****************************/

/*
   Build up a working pattern matrix by forgetting
   about guarded patterns
 */

let rec initial_matrix =
  fun
  | [] => []
  | [{mb_pat: hd}, ...tl] => [[hd], ...initial_matrix(tl)];
/*{c_guard=Some _} :: rem -> initial_matrix rem
  | {c_guard=None; c_lhs=p} :: rem -> [p] :: initial_matrix rem*/

/*
    Build up a working pattern matrix by keeping
    only the patterns which are guarded
 */
let rec initial_only_guarded =
  fun
  | [] => []
  | [{mb_pat: hd}, ...tl] => [[hd], ...initial_only_guarded(tl)];
/*{ c_guard = None; _} :: rem ->
      initial_only_guarded rem
  | { c_lhs = pat; _ } :: rem ->
      [pat] :: initial_only_guarded rem*/

/************************/
/* Exhaustiveness check */
/************************/

/* FIXME: If we port over untypeast, then use its function instead */
let untype_constant =
  fun
  | Const_int(i) => Parsetree.PConstNumber(i)
  | Const_string(s) => Parsetree.PConstString(s)
  | Const_bool(b) => Parsetree.PConstBool(b)
  | _ => failwith("NYI untype_constant");

/* conversion from Typedtree.pattern to Parsetree.pattern list */
module Conv = {
  open Parsetree;
  let mkpat = desc => Ast_helper.Pat.mk(desc);

  let name_counter = ref(0);
  let fresh = name => {
    let current = name_counter^;
    name_counter := name_counter^ + 1;
    "#$" ++ name ++ string_of_int(current);
  };

  let conv = typed => {
    let constrs = Hashtbl.create(7);
    let rec loop = pat =>
      switch (pat.pat_desc) {
      | TPatAny
      | TPatVar(_) => mkpat(PPatAny)
      | [@implicit_arity] TPatAlias(p, _, _) => loop(p)
      | [@implicit_arity] TPatOr(pa, pb) =>
        mkpat([@implicit_arity] PPatOr(loop(pa), loop(pb)))
      | TPatConstant(c) => mkpat(PPatConstant(untype_constant(c)))
      | TPatTuple(lst) => mkpat(PPatTuple(List.map(loop, lst)))
      | [@implicit_arity] TPatRecord(fields, c) =>
        mkpat(
          [@implicit_arity]
          PPatRecord(
            List.map(((id, _, pat)) => (id, loop(pat)), fields),
            c,
          ),
        )
      | [@implicit_arity] TPatConstruct(cstr_lid, cstr, lst) =>
        let id = fresh(cstr.cstr_name);
        let lid = {...cstr_lid, txt: Identifier.IdentName(id)};
        Hashtbl.add(constrs, id, cstr);
        mkpat([@implicit_arity] PPatConstruct(lid, List.map(loop, lst)));
      };

    let ps = loop(typed);
    (ps, constrs);
  };
};

/* Whether the counter-example contains an extension pattern */
let contains_extension = pat => false;

/* Build an untyped or-pattern from its expected type */
let ppat_of_type = (env, ty) =>
  switch (pats_of_type(env, ty)) {
  | [{pat_desc: TPatAny}] => (
      Conv.mkpat(Parsetree.PPatAny),
      Hashtbl.create(0),
    )
  | pats => Conv.conv(orify_many(pats))
  };

let do_check_partial = (~pred, loc, casel: list(match_branch), pss) =>
  switch (pss) {
  | [] =>
    /*
     This can occur
     - For empty matches generated by ocamlp4 (no warning)
     - when all patterns have guards (then, casel <> [])
     (specific warning)
     Then match MUST be considered non-exhaustive,
     otherwise compilation of PM is broken.
     */
    switch (casel) {
    | [] => ()
    | _ =>
      if (Warnings.is_active(Warnings.AllClausesGuarded)) {
        Location.prerr_warning(loc, Warnings.AllClausesGuarded);
      }
    };
    Partial;
  | [ps, ..._] =>
    switch (exhaust(None, pss, List.length(ps))) {
    | No_matching_value => Total
    | Witnesses([u]) =>
      let v = {
        let (pattern, constrs) = Conv.conv(u);
        let u' = pred(constrs, pattern);
        u';
      };

      switch (v) {
      | None => Total
      | Some(v) =>
        if (Warnings.is_active(Warnings.PartialMatch(""))) {
          let errmsg =
            try({
              let buf = Buffer.create(16);
              let fmt = Format.formatter_of_buffer(buf);
              Printpat.top_pretty(fmt, v);
              if (do_match(initial_only_guarded(casel), [v])) {
                Buffer.add_string(
                  buf,
                  "\n(However, some guarded clause may match this value.)",
                );
              };
              if (contains_extension(v)) {
                Buffer.add_string(
                  buf,
                  "\nMatching over values of extensible variant types (the *extension* above)\nmust include a wild card pattern in order to be exhaustive.",
                );
              };

              Buffer.contents(buf);
            }) {
            | _ => ""
            };

          Location.prerr_warning(loc, Warnings.PartialMatch(errmsg));
        };
        Partial;
      };
    | _ => fatal_error("Parmatch.check_partial")
    }
  };

/*****************/
/* Fragile check */
/*****************/

/* Collect all data types in a pattern */

let rec add_path = path =>
  fun
  | [] => [path]
  | [x, ...rem] as paths =>
    if (Path.same(path, x)) {
      paths;
    } else {
      [x, ...add_path(path, rem)];
    };

let extendable_path = path =>
  !(
    Path.same(path, Builtin_types.path_bool)
    /*Path.same path Predef.path_list ||*/
    || Path.same(path, Builtin_types.path_void) /*Path.same path Predef.path_option*/
  );

let rec collect_paths_from_pat = (r, p) =>
  switch (p.pat_desc) {
  | [@implicit_arity]
    TPatConstruct(
      _,
      {cstr_tag: CstrConstant(_) | CstrBlock(_) | CstrUnboxed},
      ps,
    ) =>
    let path = get_constructor_type_path(p.pat_type, p.pat_env);
    List.fold_left(
      collect_paths_from_pat,
      if (extendable_path(path)) {
        add_path(path, r);
      } else {
        r;
      },
      ps,
    );
  | TPatAny
  | TPatVar(_)
  | TPatConstant(_) => r
  | TPatTuple(ps) => List.fold_left(collect_paths_from_pat, r, ps)
  | [@implicit_arity] TPatRecord(fields, _) =>
    let ps = List.map(((_, _, pat)) => pat, fields);
    List.fold_left(collect_paths_from_pat, r, ps);
  | [@implicit_arity] TPatAlias(p, _, _) => collect_paths_from_pat(r, p)
  | [@implicit_arity] TPatOr(p1, p2) =>
    collect_paths_from_pat(collect_paths_from_pat(r, p1), p2)
  };

/*
   Actual fragile check
    1. Collect data types in the patterns of the match.
    2. One exhaustivity check per datatype, considering that
       the type is extended.
 */

let do_check_fragile = (loc, casel, pss) => {
  let exts =
    List.fold_left(
      (r, c) => collect_paths_from_pat(r, c.mb_pat),
      [],
      casel,
    );
  switch (exts) {
  | [] => ()
  | _ =>
    switch (pss) {
    | [] => ()
    | [ps, ..._] =>
      List.iter(
        ext =>
          switch (exhaust(Some(ext), pss, List.length(ps))) {
          | No_matching_value =>
            Location.prerr_warning(
              loc,
              Warnings.FragileMatch(Path.name(ext)),
            )
          | Witnesses(_) => ()
          },
        exts,
      )
    }
  };
};

/********************************/
/* Exported unused clause check */
/********************************/

let check_unused = (pred, casel) =>
  if (Warnings.is_active(Warnings.UnusedMatch)) {
    /*|| List.exists (fun c -> c.mb_body.exp_desc = TExpUnreachable) casel*/
    let rec do_rec = pref =>
      fun
      | [] => ()
      | [{mb_pat: q, /*c_guard;*/ mb_body}, ...rem] => {
          let qs = [q];
          try({
            let pss = get_mins(le_pats, List.filter(compats(qs), pref));
            /* First look for redundant or partially redundant patterns */
            let r = every_satisfiables(make_rows(pss), make_row(qs));
            let refute = false /*(c_rhs.exp_desc = Texp_unreachable)*/;
            /* Do not warn for unused [pat -> .] */
            if (r == Unused && refute) {
              ();
            } else {
              let r = {
                /* Do not refine if either:
                   - we already know the clause is unused
                   - the clause under consideration is not a refutation clause
                     and either:
                     + there are no other lines
                     + we do not care whether the types prevent this clause to be
                       reached.
                     If the clause under consideration *is* a refutation clause
                     then we do need to check more carefully whether it can be
                     refuted or not.  */
                let skip =
                  r == Unused
                  || !refute
                  && pref == []
                  || !(refute || Warnings.is_active(Warnings.UnreachableCase));
                if (skip) {
                  r;
                } else {
                  /* Then look for empty patterns */
                  let sfs = list_satisfying_vectors(pss, qs);
                  if (sfs == []) {
                    Unused;
                  } else {
                    let sfs =
                      List.map(
                        fun
                        | [u] => u
                        | _ => assert(false),
                        sfs,
                      );
                    let u = orify_many(sfs);
                    /*Format.eprintf "%a@." pretty_val u;*/
                    let (pattern, constrs) = Conv.conv(u);
                    let pattern = {...pattern, Parsetree.ppat_loc: q.pat_loc};
                    switch (pred(refute, constrs, pattern)) {
                    | None when !refute =>
                      Location.prerr_warning(
                        q.pat_loc,
                        Warnings.UnreachableCase,
                      );
                      Used;
                    | _ => r
                    };
                  };
                };
              };

              switch (r) {
              | Unused =>
                Location.prerr_warning(q.pat_loc, Warnings.UnusedMatch)
              | Upartial(ps) =>
                List.iter(
                  p => Location.prerr_warning(p.pat_loc, Warnings.UnusedPat),
                  ps,
                )
              | Used => ()
              };
            };
          }) {
          | Empty
          | Not_found => assert(false)
          };

          /*if c_guard <> None then
            do_rec pref rem
            else*/
          do_rec([[q], ...pref], rem);
        };

    do_rec([], casel);
  };

/*********************************/
/* Exported irrefutability tests */
/*********************************/

let irrefutable = pat => le_pat(pat, omega);

let inactive = (~partial, pat) =>
  switch (partial) {
  | Partial => false
  | Total =>
    let rec loop = pat =>
      switch (pat.pat_desc) {
      | TPatAny
      | TPatVar(_) => true
      | TPatConstant(c) =>
        switch (c) {
        | Const_string(_)
        | Const_void
        | Const_int(_)
        | Const_bool(_)
        | Const_float(_)
        | Const_int32(_)
        | Const_int64(_) => true
        }
      | TPatTuple(ps)
      | [@implicit_arity] TPatConstruct(_, _, ps) =>
        List.for_all(p => loop(p), ps)
      | [@implicit_arity] TPatRecord(fields, _) =>
        List.for_all(((_, _, p)) => loop(p), fields)
      | [@implicit_arity] TPatAlias(p, _, _) => loop(p)
      | [@implicit_arity] TPatOr(p, q) => loop(p) && loop(q)
      };

    loop(pat);
  };

/*********************************/
/* Exported exhaustiveness check */
/*********************************/

/*
    Fragile check is performed when required and
    on exhaustive matches only.
 */

let check_partial = (pred, loc, casel) => {
  let pss = initial_matrix(casel);
  let pss = get_mins(le_pats, pss);
  let total = do_check_partial(~pred, loc, casel, pss);
  if (total == Total && Warnings.is_active(Warnings.FragileMatch(""))) {
    do_check_fragile(loc, casel, pss);
  };
  total;
};

/*************************************/
/* Ambiguous variable in or-patterns */
/*************************************/

/* Specification: ambiguous variables in or-patterns.

      The semantics of or-patterns in OCaml is specified with
      a left-to-right bias: a value [v] matches the pattern [p | q] if it
      matches [p] or [q], but if it matches both, the environment
      captured by the match is the environment captured by [p], never the
      one captured by [q].

      While this property is generally well-understood, one specific case
      where users expect a different semantics is when a pattern is
      followed by a when-guard: [| p when g -> e]. Consider for example:

        | ((Const x, _) | (_, Const x)) when is_neutral x -> branch

      The semantics is clear: match the scrutinee against the pattern, if
      it matches, test the guard, and if the guard passes, take the
      branch.

      However, consider the input [(Const a, Const b)], where [a] fails
      the test [is_neutral f], while [b] passes the test [is_neutral
      b]. With the left-to-right semantics, the clause above is *not*
      taken by its input: matching [(Const a, Const b)] against the
      or-pattern succeeds in the left branch, it returns the environment
      [x -> a], and then the guard [is_neutral a] is tested and fails,
      the branch is not taken. Most users, however, intuitively expect
      that any pair that has one side passing the test will take the
      branch. They assume it is equivalent to the following:

        | (Const x, _) when is_neutral x -> branch
        | (_, Const x) when is_neutral x -> branch

      while it is not.

      The code below is dedicated to finding these confusing cases: the
      cases where a guard uses "ambiguous" variables, that are bound to
      different parts of the scrutinees by different sides of
      a or-pattern. In other words, it finds the cases where the
      specified left-to-right semantics is not equivalent to
      a non-deterministic semantics (any branch can be taken) relatively
      to a specific guard.
   */
let pattern_vars = p => Ident.Set.of_list(Typedtree.pattern_bound_idents(p));

/* Row for ambiguous variable search,
      row is the traditional pattern row,
      varsets contain a list of head variable sets (varsets)

      A given varset contains all the variables that appeared at the head
      of a pattern in the row at some point during traversal: they would
      all be bound to the same value at matching time. On the contrary,
      two variables of different varsets appeared at different places in
      the pattern and may be bound to distinct sub-parts of the matched
      value.

      All rows of a (sub)matrix have rows of the same length,
      but also varsets of the same length.
      Varsets are populated when simplifying the first column
      -- the variables of the head pattern are collected in a new varset.
      For example,
        { row = x :: r1; varsets = s1 }
        { row = (Some _) as y :: r2; varsets  = s2 }
        { row = (None as x) as y :: r3; varsets = s3 }
        { row = (Some x | (None as x)) :: r4 with varsets = s4 }
      becomes
        (_, { row = r1; varsets = {x} :: s1 })
        (Some _, { row = r2; varsets = {y} :: s2 })
        (None, { row = r3; varsets = {x, y} :: s3 })
        (Some x, { row = r4; varsets = {} :: s4 })
        (None, { row = r4; varsets = {x} :: s4 })
   */
type amb_row = {
  row: list(pattern),
  varsets: list(Ident.Set.t),
};

let simplify_head_amb_pat =
    (head_bound_variables, varsets, ~add_column, p, ps, k) => {
  let rec simpl = (head_bound_variables, varsets, p, ps, k) =>
    switch (p.pat_desc) {
    | [@implicit_arity] TPatAlias(p, x, _) =>
      simpl(Ident.Set.add(x, head_bound_variables), varsets, p, ps, k)
    | [@implicit_arity] TPatVar(x, _) =>
      let rest_of_the_row = {
        row: ps,
        varsets: [Ident.Set.add(x, head_bound_variables), ...varsets],
      };

      add_column(omega, rest_of_the_row, k);
    | [@implicit_arity] TPatOr(p1, p2) =>
      simpl(
        head_bound_variables,
        varsets,
        p1,
        ps,
        simpl(head_bound_variables, varsets, p2, ps, k),
      )
    | _ =>
      add_column(
        p,
        {row: ps, varsets: [head_bound_variables, ...varsets]},
        k,
      )
    };
  simpl(head_bound_variables, varsets, p, ps, k);
};

/*
    To accurately report ambiguous variables, one must consider
    that previous clauses have already matched some values.
    Consider for example:

      | (Foo x, Foo y) -> ...
      | ((Foo x, _) | (_, Foo x)) when bar x -> ...

    The second line taken in isolation uses an unstable variable,
    but the discriminating values, of the shape [(Foo v1, Foo v2)],
    would all be filtered by the line above.

    To track this information, the matrices we analyze contain both
    *positive* rows, that describe the rows currently being analyzed
    (of type Varsets.row, so that their varsets are tracked) and
    *negative rows*, that describe the cases already matched against.

    The values matched by a signed matrix are the values matched by
    some of the positive rows but none of the negative rows. In
    particular, a variable is stable if, for any value not matched by
    any of the negative rows, the environment captured by any of the
    matching positive rows is identical.
 */
type signed('a, 'b) =
  | Positive('a)
  | Negative('b);

let rec simplify_first_amb_col =
  fun
  | [] => []
  | [Negative([]) | Positive({row: [], _}), ..._] => assert(false)
  | [Negative([n, ...ns]), ...rem] => {
      let add_column = (n, ns, k) => [(n, Negative(ns)), ...k];
      simplify_head_pat(~add_column, n, ns, simplify_first_amb_col(rem));
    }
  | [Positive({row: [p, ...ps], varsets}), ...rem] => {
      let add_column = (p, ps, k) => [(p, Positive(ps)), ...k];
      simplify_head_amb_pat(
        Ident.Set.empty,
        varsets,
        ~add_column,
        p,
        ps,
        simplify_first_amb_col(rem),
      );
    };

/* Compute stable bindings */

type stable_vars =
  | All
  | Vars(Ident.Set.t);

let stable_inter = (sv1, sv2) =>
  switch (sv1, sv2) {
  | (All, sv)
  | (sv, All) => sv
  | (Vars(s1), Vars(s2)) => Vars(Ident.Set.inter(s1, s2))
  };

let reduce = f =>
  fun
  | [] => invalid_arg("reduce")
  | [x, ...xs] => List.fold_left(f, x, xs);

exception Negative_empty_row;

let rec matrix_stable_vars = m =>
  switch (m) {
  | [] => All
  | [Positive({row: [], _}) | Negative([]), ..._] as empty_rows =>
    /* if at least one empty row is negative, the matrix matches no value */
    let get_varsets = (
      fun
      | Negative(n) => {
          /* All rows have the same number of columns;
             if the first row is empty, they all are. */
          assert(n == []);
          raise(Negative_empty_row);
        }
      | Positive(p) => {
          assert(p.row == []);
          p.varsets;
        }
    );
    switch (List.map(get_varsets, empty_rows)) {
    | exception Negative_empty_row => All
    | rows_varsets =>
      let stables_in_varsets =
        reduce(List.map2(Ident.Set.inter), rows_varsets);
      /* The stable variables are those stable at any position */
      Vars(
        List.fold_left(Ident.Set.union, Ident.Set.empty, stables_in_varsets),
      );
    };
  | m =>
    let is_negative = (
      fun
      | Negative(_) => true
      | Positive(_) => false
    );
    if (List.for_all(is_negative, m)) {
      All;
         /* optimization: quit early if there are no positive rows.
            This may happen often when the initial matrix has many
            negative cases and few positive cases (a small guarded
            clause after a long list of clauses) */
    } else {
      let m = simplify_first_amb_col(m);
      if (!all_coherent(first_column(m))) {
        All;
      } else {
        /* If the column is ill-typed but deemed coherent, we might
           spuriously warn about some variables being unstable.
           As sad as that might be, the warning can be silenced by
           splitting the or-pattern...  */
        let submatrices = {
          let extend_row = columns => (
            fun
            | Negative(r) => Negative(columns @ r)
            | Positive(r) => Positive({...r, row: columns @ r.row})
          );
          let q0 = discr_pat(omega, m);
          let {default, constrs} =
            build_specialized_submatrices(~extend_row, q0, m);
          let non_default = List.map(snd, constrs);
          if (full_match(false, constrs)) {
            non_default;
          } else {
            [default, ...non_default];
          };
        };
        /* A stable variable must be stable in each submatrix. */
        let submat_stable = List.map(matrix_stable_vars, submatrices);
        List.fold_left(stable_inter, All, submat_stable);
      };
    };
  };

let pattern_stable_vars = (ns, p) =>
  matrix_stable_vars(
    List.fold_left(
      (m, n) => [Negative(n), ...m],
      [Positive({varsets: [], row: [p]})],
      ns,
    ),
  );

/* All identifier paths that appear in an expression that occurs
      as a clause right hand side or guard.

     The function is rather complex due to the compilation of
     unpack patterns by introducing code in rhs expressions
     and **guards**.

     For pattern (module M:S)  -> e the code is
     let module M_mod = unpack M .. in e

     Hence M is "free" in e iff M_mod is free in e.

     Not doing so will yield excessive  warning in
     (module (M:S) } ...) when true -> ....
     as M is always present in
     let module M_mod = unpack M .. in true
   */
let all_rhs_idents = exp => {
  let ids = ref(Ident.Set.empty);
  module Iterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;
      let enter_expression = exp =>
        switch (exp.exp_desc) {
        | [@implicit_arity] TExpIdent(path, _lid, _descr) =>
          List.iter(id => ids := Ident.Set.add(id, ids^), Path.heads(path))
        | _ => ()
        };

      /* Very hackish, detect unpack pattern  compilation
         and perform "indirect check for them" */
      let is_unpack = exp => false;
      /*List.exists
        (fun (attr, _) -> attr.txt = "#modulepat") exp.exp_attributes*/

      let leave_expression = exp =>
        if (is_unpack(exp)) {
          switch (exp.exp_desc) {
          | _ => assert(false)
          };
        };
    });
  Iterator.iter_expression(exp);
  ids^;
};

let check_ambiguous_bindings =
    /*let open Warnings in
      let warn0 = AmbiguousPattern [] in*/
    cases =>
  ();
/*if is_active warn0 then
  let check_case ns case = match case with
    | { mb_pat = p; _} -> [p]::ns
  in
  ignore (List.fold_left check_case [] cases)*/
