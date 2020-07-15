open Grain_parsing;
open Misc;
open Asttypes;
open Parsetree;
open Types;
open Typedtree;
open Btype;
open Ctype;
open Checkertypes;
open Typepat;
open Disambiguation;

type error =
  | Arity_mismatch(type_expr, int)
  | Polymorphic_label(Identifier.t)
  | Constructor_arity_mismatch(Identifier.t, int, int)
  | Label_mismatch(Identifier.t, list((type_expr, type_expr)))
  | Pattern_type_clash(list((type_expr, type_expr)))
  | Or_pattern_type_clash(Ident.t, list((type_expr, type_expr)))
  | Multiply_bound_variable(string)
  | Orpat_vars(Ident.t, list(Ident.t))
  | Expr_type_clash(
      list((type_expr, type_expr)),
      option(type_forcing_context),
    )
  | Apply_non_function(type_expr)
  | Label_multiply_defined(string)
  | Label_missing(list(Ident.t))
  | Label_not_mutable(Identifier.t)
  | Assign_not_mutable(Identifier.t)
  | Wrong_name(string, type_expected, string, Path.t, string, list(string))
  | Name_type_mismatch(
      string,
      Identifier.t,
      (Path.t, Path.t),
      list((Path.t, Path.t)),
    )
  | Invalid_format(string)
  | Undefined_method(type_expr, string, option(list(string)))
  | Undefined_inherited_method(string, list(string))
  | Virtual_class(Identifier.t)
  | Private_type(type_expr)
  | Private_label(Identifier.t, type_expr)
  | Unbound_instance_variable(string, list(string))
  | Instance_variable_not_mutable(bool, string)
  | Not_subtype(
      list((type_expr, type_expr)),
      list((type_expr, type_expr)),
    )
  | Outside_class
  | Value_multiply_overridden(string)
  | Coercion_failure(
      type_expr,
      type_expr,
      list((type_expr, type_expr)),
      bool,
    )
  | Too_many_arguments(bool, type_expr, option(type_forcing_context))
  | Scoping_let_module(string, type_expr)
  | Masked_instance_variable(Identifier.t)
  | Not_a_variant_type(Identifier.t)
  | Incoherent_label_order
  | Less_general(string, list((type_expr, type_expr)))
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module(type_expr)
  | Recursive_local_constraint(list((type_expr, type_expr)))
  | Unexpected_existential
  | Unqualified_gadt_pattern(Path.t, string)
  | Invalid_interval
  | Invalid_for_loop_index
  | No_value_clauses
  | Exception_pattern_below_toplevel
  | Inlined_record_escape
  | Inlined_record_expected
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Literal_overflow(string)
  | Unknown_literal(string, char)
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr
  | Unbound_value_missing_rec(Identifier.t, Location.t);

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

type recarg =
  | Allowed
  | Required
  | Rejected;

let prim1_type =
  fun
  | Incr
  | Decr => (Builtin_types.type_number, Builtin_types.type_number)
  | Not => (Builtin_types.type_bool, Builtin_types.type_bool)
  | Box => {
      let var = newvar(~name="a", ());
      (var, Builtin_types.type_box(var));
    }
  | Unbox => {
      let var = newvar(~name="a", ());
      (Builtin_types.type_box(var), var);
    }
  | Ignore => {
      let var = newvar(~name="a", ());
      (var, Builtin_types.type_void);
    }
  | ArrayLength => {
      let var = newvar(~name="a", ());
      (Builtin_types.type_array(var), Builtin_types.type_number);
    }
  | Assert => (Builtin_types.type_bool, Builtin_types.type_void)
  | FailWith => (Builtin_types.type_string, newvar(~name="a", ()))
  | Int64FromNumber => (Builtin_types.type_number, Builtin_types.type_int64)
  | Int64ToNumber => (Builtin_types.type_int64, Builtin_types.type_number)
  | Int64Lnot => (Builtin_types.type_int64, Builtin_types.type_int64);

let prim2_type =
  fun
  | Plus
  | Minus
  | Times
  | Divide
  | Mod => (
      Builtin_types.type_number,
      Builtin_types.type_number,
      Builtin_types.type_number,
    )
  | Less
  | Greater
  | LessEq
  | GreaterEq => (
      Builtin_types.type_number,
      Builtin_types.type_number,
      Builtin_types.type_bool,
    )
  | And
  | Or => (
      Builtin_types.type_bool,
      Builtin_types.type_bool,
      Builtin_types.type_bool,
    )
  | Eq => {
      let v1 = newvar(~name="equal", ())
      and v2 = newvar(~name="equal", ());
      (v1, v2, Builtin_types.type_bool);
    }
  | ArrayMake => {
      let var = newvar(~name="a", ());
      (Builtin_types.type_number, var, Builtin_types.type_array(var));
    }
  | ArrayInit => {
      let var = newvar(~name="a", ());
      (
        Builtin_types.type_number,
        Builtin_types.type_lambda([Builtin_types.type_number], var),
        Builtin_types.type_array(var),
      );
    }
  | Int64Land
  | Int64Lor
  | Int64Lxor => (
      Builtin_types.type_int64,
      Builtin_types.type_int64,
      Builtin_types.type_int64,
    )
  | Int64Lsl
  | Int64Lsr
  | Int64Asr => (
      Builtin_types.type_int64,
      Builtin_types.type_number,
      Builtin_types.type_int64,
    )
  | Int64Gt
  | Int64Gte
  | Int64Lt
  | Int64Lte => (
      Builtin_types.type_int64,
      Builtin_types.type_int64,
      Builtin_types.type_bool,
    );

let maybe_add_pattern_variables_ghost = (loc_let, env, pv) =>
  List.fold_right(
    ((id, ty, _name, _loc, _as_var), env) => {
      let lid = Identifier.IdentName(Ident.name(id));
      switch (Env.lookup_value(~mark=false, lid, env)) {
      | _ => env
      | exception Not_found =>
        Env.add_value(
          id,
          {
            val_type: ty,
            val_fullpath: Path.PIdent(id),
            val_kind: TValUnbound(ValUnboundGhostRecursive),
            val_loc: loc_let,
            val_mutable: false,
          },
          env,
        )
      };
    },
    pv,
    env,
  );

let all_idents_cases = el => {
  open Ast_iterator;
  let idents = Hashtbl.create(8);
  let rec f_expr = iter =>
    fun
    | {pexp_desc: PExpId({txt: Identifier.IdentName(id), _}), _} =>
      Hashtbl.replace(idents, id, ())
    | e => default_iterator.expr(iter, e);

  let iterator = {...default_iterator, expr: f_expr};
  List.iter(
    cp =>
      /*may (iterator.expr iterator) cp.pc_guard;*/
      iterator.expr(iterator, cp.pmb_body),
    el,
  );
  Hashtbl.fold((x, (), rest) => [x, ...rest], idents, []);
};

let constant:
  Parsetree.constant => result(Asttypes.constant, Checkertypes.error) = (
  Checkertypes.constant:
    Parsetree.constant => result(Asttypes.constant, Checkertypes.error)
);

let constant_or_raise = Checkertypes.constant_or_raise;

/* Specific version of type_option, using newty rather than newgenty */

/*let type_option ty =
  newty (TTyConstr(Predef.path_option,[ty], ref TMemNil))*/

let mkexp = (exp_desc, exp_type, exp_loc, exp_env) => {
  exp_desc,
  exp_type,
  exp_loc,
  exp_env,
  exp_extra: [],
};

/* Typing of patterns */

/* unification inside type_pat*/
let unify_pat_types = (loc, env, ty, ty') =>
  try(unify(env, ty, ty')) {
  | Unify(trace) =>
    raise([@implicit_arity] Error(loc, env, Pattern_type_clash(trace)))
  };

/* unification inside type_exp and type_expect */
let unify_exp_types = (loc, env, ty, expected_ty) =>
  /*Format.eprintf "Unifying: @[%a@ %a@]@." Printtyp.raw_type_expr ty
    Printtyp.raw_type_expr expected_ty;*/
  try(unify(env, ty, expected_ty)) {
  | Unify(trace) =>
    raise(
      [@implicit_arity]
      Error(loc, env, [@implicit_arity] Expr_type_clash(trace, None)),
    )
  };

/* level at which to create the local type declarations */
let newtype_level = ref(None);
let get_newtype_level = () =>
  switch (newtype_level^) {
  | Some(y) => y
  | None => assert(false)
  };

let rec last = lst =>
  switch (lst) {
  | [] => raise(Not_found)
  | [e] => e
  | [_, ...es] => last(es)
  };

let rec final_subexpression = sexp =>
  switch (sexp.pexp_desc) {
  | [@implicit_arity] PExpLet(_, _, _, e)
  | [@implicit_arity] PExpIf(_, e, _)
  | [@implicit_arity] PExpWhile(_, e)
  | [@implicit_arity] PExpMatch(_, [{pmb_body: e}, ..._]) =>
    final_subexpression(e)
  | PExpBlock(es) =>
    try(final_subexpression(last(es))) {
    | Not_found => sexp
    }
  | _ => sexp
  };

let rec is_nonexpansive = exp =>
  switch (exp.exp_desc) {
  | TExpIdent(_)
  | TExpConstant(_)
  | TExpLambda(_)
  | TExpNull => true
  | TExpTuple(es) => List.for_all(is_nonexpansive, es)
  | [@implicit_arity] TExpLet(rec_flag, mut_flag, binds, body) =>
    List.for_all(vb => is_nonexpansive(vb.vb_expr), binds)
    && is_nonexpansive(body)
  | [@implicit_arity] TExpMatch(e, cases, _) =>
    is_nonexpansive(e)
    && List.for_all(({mb_pat, mb_body}) => is_nonexpansive(mb_body), cases)
  | [@implicit_arity] TExpPrim1(_, e) => is_nonexpansive(e)
  | [@implicit_arity] TExpPrim2(_, e1, e2) =>
    is_nonexpansive(e1) && is_nonexpansive(e2)
  | [@implicit_arity] TExpIf(c, t, f) =>
    is_nonexpansive(t) && is_nonexpansive(f)
  | [@implicit_arity] TExpWhile(c, b) => is_nonexpansive(b)
  | TExpBlock([_, ..._] as es) => is_nonexpansive(last(es))
  | [@implicit_arity] TExpApp(e, args) =>
    is_nonexpansive(e) && List.for_all(is_nonexpansive, args)
  | [@implicit_arity] TExpConstruct(_, _, el) =>
    List.for_all(is_nonexpansive, el)
  | _ => false
  };

let maybe_expansive = e => !is_nonexpansive(e);

/* Approximate the type of an expression, for better recursion */

let rec approx_type = (env, sty) =>
  switch (sty.ptyp_desc) {
  | [@implicit_arity] PTyArrow(args, ret) =>
    newty(
      [@implicit_arity]
      TTyArrow(
        List.map(x => newvar(), args),
        approx_type(env, ret),
        TComOk,
      ),
    )
  | PTyTuple(args) => newty(TTyTuple(List.map(approx_type(env), args)))
  | [@implicit_arity] PTyConstr(id, args) =>
    try({
      let path = Env.lookup_type(id.txt, env);
      let decl = Env.find_type(path, env);
      if (List.length(args) != decl.type_arity) {
        raise(Not_found);
      };
      let tyl = List.map(approx_type(env), args);
      newconstr(path, tyl);
    }) {
    | Not_found => newvar()
    }
  | _ => newvar()
  };

let rec type_approx = (env, sexp: Parsetree.expression) =>
  switch (sexp.pexp_desc) {
  | [@implicit_arity] PExpLet(_, _, _, e) => type_approx(env, e)
  | [@implicit_arity] PExpMatch(_, [{pmb_body: e}, ..._]) =>
    type_approx(env, e)
  | [@implicit_arity] PExpIf(_, e, _) => type_approx(env, e)
  | [@implicit_arity] PExpWhile(_, e) => type_approx(env, e)
  | [@implicit_arity] PExpLambda(args, e) =>
    newty(
      [@implicit_arity]
      TTyArrow(List.map(x => newvar(), args), type_approx(env, e), TComOk),
    )
  | PExpBlock([_, ..._] as es) => type_approx(env, last(es))
  | _ => newvar()
  };

/* Check that all univars are safe in a type */
let check_univars = (env, expans, kind, exp, ty_expected, vars) => {
  if (expans && !is_nonexpansive(exp)) {
    generalize_expansive(env, exp.exp_type);
  };
  /* need to expand twice? cf. Ctype.unify2 */
  let vars = List.map(expand_head(env), vars);
  let vars = List.map(expand_head(env), vars);
  let vars' =
    List.filter(
      t => {
        let t = repr(t);
        generalize(t);
        switch (t.desc) {
        | TTyVar(name) when t.level == generic_level =>
          log_type(t);
          t.desc = TTyUniVar(name);
          true;
        | _ => false
        };
      },
      vars,
    );
  if (List.length(vars) == List.length(vars')) {
    ();
  } else {
    let ty = newgenty([@implicit_arity] TTyPoly(repr(exp.exp_type), vars'))
    and ty_expected = repr(ty_expected);
    raise(
      [@implicit_arity]
      Error(
        exp.exp_loc,
        env,
        [@implicit_arity]
        Less_general(kind, [(ty, ty), (ty_expected, ty_expected)]),
      ),
    );
  };
};

/* Check that a type is generalizable at some level */
let generalizable = (level, ty) => {
  let rec check = ty => {
    let ty = repr(ty);
    if (ty.level < lowest_level) {
      ();
    } else if (ty.level <= level) {
      raise(Exit);
    } else {
      mark_type_node(ty);
      iter_type_expr(check, ty);
    };
  };

  try(
    {
      check(ty);
      unmark_type(ty);
      true;
    }
  ) {
  | Exit =>
    unmark_type(ty);
    false;
  };
};

/* Duplicate types of values in the environment */
/* XXX Should we do something about global type variables too? */

let duplicate_ident_types = (caselist, env) => {
  let caselist =
    List.filter(
      ({pmb_pat}) => /*contains_gadt env pc_lhs*/ false,
      caselist,
    );
  Env.copy_types(all_idents_cases(caselist), env);
};

/* Getting proper location of already typed expressions.
      Used to avoid confusing locations on type error messages in presence of
      type constraints.
      For example:
          (* Before patch *)
          # let x : string = (5 : int);;
                              ^
          (* After patch *)
          # let x : string = (5 : int);;
                             ^^^^^^^^^
   */
let proper_exp_loc = exp => {
  let rec aux =
    fun
    | [] => exp.exp_loc
    /*| ((Texp_constraint _ | Texp_coerce _), loc, _) :: _ -> loc*/
    | [_, ...rest] => aux(rest);

  aux(exp.exp_extra);
};

/* To find reasonable names for let-bound and lambda-bound idents */

let rec name_pattern = default =>
  fun
  | [] => Ident.create(default)
  | [{mb_pat: p, _}, ...rem] =>
    switch (p.pat_desc) {
    | [@implicit_arity] TPatVar(id, _) => id
    /*| Tpat_alias(_, id, _) -> id*/
    | _ => name_pattern(default, rem)
    };

/* Typing of expressions */

let unify_exp = (env, exp, expected_ty) => {
  let loc = proper_exp_loc(exp);
  /*Printf.eprintf "Typed (pre-unification): %s\n"
    (Sexplib.Sexp.to_string_hum (Typedtree.sexp_of_expression exp));*/
  unify_exp_types(loc, env, exp.exp_type, expected_ty);
};

let rec type_exp = (~recarg=?, env, sexp) =>
  /* We now delegate everything to type_expect */
  type_expect(~recarg?, env, sexp, mk_expected(newvar()))

/* Typing of an expression with an expected type.
     This provide better error messages, and allows controlled
     propagation of return type information.
     In the principal case, [type_expected'] may be at generic_level.
   */

and type_expect =
    (~in_function=?, ~recarg=?, env, sexp, ty_expected_explained) => {
  /*let previous_saved_types = Cmt_format.get_saved_types () in*/
  let exp =
    type_expect_(~in_function?, ~recarg?, env, sexp, ty_expected_explained);

  /*Cmt_format.set_saved_types
    (Cmt_format.Partial_expression exp :: previous_saved_types);*/
  exp;
}

and with_explanation = (explanation, f) =>
  switch (explanation) {
  | None => f()
  | Some(explanation) =>
    try(f()) {
    | [@implicit_arity]
      Error(loc', env', [@implicit_arity] Expr_type_clash(trace', None))
        when !loc'.Location.loc_ghost =>
      raise(
        [@implicit_arity]
        Error(
          loc',
          env',
          [@implicit_arity] Expr_type_clash(trace', Some(explanation)),
        ),
      )
    }
  }

and type_expect_ =
    (~in_function=?, ~recarg=Rejected, env, sexp, ty_expected_explained) => {
  let {ty: ty_expected, explanation} = ty_expected_explained;
  let loc = sexp.pexp_loc;
  /* Record the expression type before unifying it with the expected type */
  let with_explanation = with_explanation(explanation);
  let rue = exp => {
    with_explanation(() =>
      unify_exp(env, re(exp), instance(env, ty_expected))
    );
    exp;
  };

  switch (sexp.pexp_desc) {
  | PExpId(id) =>
    let (path, desc) = Typetexp.find_value(env, id.loc, id.txt);
    rue({
      exp_desc:
        switch (desc.val_kind) {
        | TValUnbound(ValUnboundGhostRecursive) =>
          raise(
            [@implicit_arity]
            Error(
              loc,
              env,
              [@implicit_arity]
              Unbound_value_missing_rec(id.txt, desc.val_loc),
            ),
          )
        | _ => [@implicit_arity] TExpIdent(path, id, desc)
        },
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, desc.val_type),
      exp_env: env,
    });
  | PExpConstant(cst) =>
    let cst = constant_or_raise(env, loc, cst);
    rue({
      exp_desc: TExpConstant(cst),
      exp_loc: loc,
      exp_extra: [],
      exp_type: type_constant(cst),
      exp_env: env,
    });
  | PExpTuple(es) =>
    let subtypes = List.map(_ => newgenvar(), es);
    let to_unify = newgenty(TTyTuple(subtypes));
    with_explanation(() => unify_exp_types(loc, env, to_unify, ty_expected));
    let expl =
      List.map2(
        (body, ty) => type_expect(env, body, mk_expected(ty)),
        es,
        subtypes,
      );

    re({
      exp_desc: TExpTuple(expl),
      exp_loc: loc,
      exp_extra: [],
      exp_type: newty(TTyTuple(List.map(e => e.exp_type, expl))),
      exp_env: env,
    });
  | PExpArray(es) =>
    let ty = newgenvar();
    let to_unify = Builtin_types.type_array(ty);
    with_explanation(() => unify_exp_types(loc, env, to_unify, ty_expected));
    let expl =
      List.map(sarg => type_expect(env, sarg, mk_expected(ty)), es);
    re({
      exp_desc: TExpArray(expl),
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, ty_expected),
      exp_env: env,
    });
  | [@implicit_arity] PExpArrayGet(sarrexp, sidx) =>
    let array_type = newvar(~name="a", ());
    let arrexp =
      type_expect(
        env,
        sarrexp,
        mk_expected(
          ~explanation=Assign_not_array,
          Builtin_types.type_array(array_type),
        ),
      );
    let idx =
      type_expect(
        env,
        sidx,
        mk_expected(
          ~explanation=Assign_not_array_index,
          Builtin_types.type_number,
        ),
      );
    rue({
      exp_desc: [@implicit_arity] TExpArrayGet(arrexp, idx),
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, array_type),
      exp_env: env,
    });
  | [@implicit_arity] PExpArraySet(sarrexp, sidx, se) =>
    let array_type = newvar(~name="a", ());
    let arrexp =
      type_expect(
        env,
        sarrexp,
        mk_expected(
          ~explanation=Assign_not_array,
          Builtin_types.type_array(array_type),
        ),
      );
    let idx =
      type_expect(
        env,
        sidx,
        mk_expected(
          ~explanation=Assign_not_array_index,
          Builtin_types.type_number,
        ),
      );
    let e = type_expect(env, se, mk_expected(array_type));
    rue({
      exp_desc: [@implicit_arity] TExpArraySet(arrexp, idx, e),
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, e.exp_type),
      exp_env: env,
    });
  | PExpRecord(es) =>
    let (ty_record, opath) = {
      let get_path = ty =>
        try({
          let (p0, p, _) = extract_concrete_record(env, ty);
          let principal = repr(ty).level == generic_level;

          Some((p0, p, principal));
        }) {
        | Not_found => None
        };

      switch (get_path(ty_expected)) {
      | None => (newvar(), None)
      | op => (ty_expected, op)
      };
    };

    let closed = true;
    let lbl_exp_list =
      wrap_disambiguate(
        "This record expression is expected to have",
        mk_expected(ty_record),
        type_label_a_list(
          loc,
          closed,
          env,
          (e, k) => k(type_label_exp(true, env, loc, ty_record, e)),
          opath,
          es,
        ),
        x =>
        x
      );

    /* let subtypes = List.map (fun ({txt=name}, _) -> Identifier.string_of_ident name, newgenvar()) es in
       let to_unify = newgenty (TTyRecord subtypes) in */
    with_explanation(() =>
      unify_exp_types(loc, env, ty_record, instance(env, ty_expected))
    );
    /* let expl =
         List.map2 (fun (name, body) (_, ty_body) -> name, type_expect env body (mk_expected ty_body))
           es subtypes
       in */
    /* type_label_a_list returns a list of labels sorted by lbl_pos */
    /* note: check_duplicates would better be implemented in
       type_label_a_list directly */
    let rec check_duplicates = (
      fun
      | [(_, lbl1, _), (_, lbl2, _), ..._] when lbl1.lbl_pos == lbl2.lbl_pos =>
        raise(
          [@implicit_arity]
          Error(loc, env, Label_multiply_defined(lbl1.lbl_name)),
        )
      | [_, ...rem] => check_duplicates(rem)
      | [] => ()
    );

    check_duplicates(lbl_exp_list);
    let label_definitions = {
      let (_lid, lbl, _lbl_exp) = List.hd(lbl_exp_list);
      let matching_label = lbl =>
        List.find(
          ((_, lbl', _)) => lbl'.lbl_pos == lbl.lbl_pos,
          lbl_exp_list,
        );

      Array.map(
        lbl =>
          switch (matching_label(lbl)) {
          | (lid, _lbl, lbl_exp) =>
            [@implicit_arity] Overridden(lid, lbl_exp)
          | exception Not_found =>
            let present_indices =
              List.map(((_, lbl, _)) => lbl.lbl_pos, lbl_exp_list);

            let label_names = extract_label_names(env, ty_expected);
            let rec missing_labels = n => (
              fun
              | [] => []
              | [lbl, ...rem] =>
                if (List.mem(n, present_indices)) {
                  missing_labels(n + 1, rem);
                } else {
                  [lbl, ...missing_labels(n + 1, rem)];
                }
            );

            let missing = missing_labels(0, label_names);
            raise(
              [@implicit_arity] Error(loc, env, Label_missing(missing)),
            );
          },
        lbl.lbl_all,
      );
    };

    let label_descriptions = {
      let (_, {lbl_all}, _) = List.hd(lbl_exp_list);
      lbl_all;
    };

    let fields =
      Array.map2(
        (descr, def) => (descr, def),
        label_descriptions,
        label_definitions,
      );

    re({
      exp_desc: TExpRecord(fields),
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, ty_expected),
      exp_env: env,
    });
  | [@implicit_arity] PExpRecordGet(srecord, lid) =>
    let (record, label, _) = type_label_access(env, srecord, lid);
    let (_, ty_arg, ty_res) = instance_label(false, label);
    unify_exp(env, record, ty_res);
    rue({
      exp_desc: [@implicit_arity] TExpRecordGet(record, lid, label),
      exp_loc: loc,
      exp_extra: [],
      exp_type: ty_arg,
      exp_env: env,
    });
  | [@implicit_arity] PExpRecordSet(srecord, lid, sval) =>
    let (record, label, _) = type_label_access(env, srecord, lid);
    if (!label.lbl_mut) {
      raise(Error(loc, env, Label_not_mutable(lid.txt)));
    };
    let (_, ty_arg, ty_res) = instance_label(false, label);
    unify_exp(env, record, ty_res);
    let val_ = type_expect(env, sval, ty_expected_explained);
    unify_exp(env, val_, ty_arg);
    rue({
      exp_desc: [@implicit_arity] TExpRecordSet(record, lid, label, val_),
      exp_loc: loc,
      exp_extra: [],
      exp_type: ty_arg,
      exp_env: env,
    });
  | [@implicit_arity] PExpLet(rec_flag, mut_flag, pats, body) =>
    let scp = None;
    let (pat_exp_list, new_env, unpacks) =
      type_let(env, rec_flag, mut_flag, pats, scp, true);
    let body = type_expect(new_env, body, ty_expected_explained);
    /*let () =
      if rec_flag = Recursive then
        check_recursive_bindings env pat_exp_list
      in*/
    re({
      exp_desc:
        [@implicit_arity] TExpLet(rec_flag, mut_flag, pat_exp_list, body),
      exp_loc: loc,
      exp_extra: [],
      exp_type: body.exp_type,
      exp_env: env,
    });
  | [@implicit_arity] PExpLambda(args, body) =>
    open Ast_helper;
    let pat =
      switch (args) {
      | [] =>
        Pat.construct(Location.mknoloc(Identifier.IdentName("()")), [])
      | [x] => x
      | args => Pat.tuple(args)
      };
    type_function(
      ~in_function?,
      loc,
      [],
      env,
      ty_expected_explained,
      (),
      [Mb.mk(pat, body)],
    );
  | [@implicit_arity] PExpApp(func, args) =>
    begin_def(); /* one more level for non-returning functions */
    if (Grain_utils.Config.principal^) {
      begin_def();
    };
    let funct = type_exp(env, func);
    if (Grain_utils.Config.principal^) {
      end_def();
      generalize_structure(funct.exp_type);
    };
    /* TODO: What does this do? */
    /*let rec lower_args seen ty_fun =
        let ty = expand_head env ty_fun in
        if List.memq ty seen then () else
          match ty.desc with
          | TTyArrow (ty_args, ty_fun, _com) ->
              (try List.map (fun a -> (unify_var env (newvar()) a)) ty_args with Unify _ -> assert false);
              lower_args (ty::seen) ty_fun
          | _ -> ()
      in*/
    end_def();
    /*lower_args [] ty;*/
    begin_def();
    let (args, ty_res) = type_application(env, funct, args);
    end_def();
    unify_var(env, newvar(), funct.exp_type);
    rue({
      exp_desc: [@implicit_arity] TExpApp(funct, args),
      exp_loc: loc,
      exp_extra: [],
      exp_type: ty_res,
      exp_env: env,
    });
  | [@implicit_arity] PExpMatch(arg, branches) =>
    begin_def();
    let arg = type_exp(env, arg);
    end_def();
    if (!is_nonexpansive(arg)) {
      generalize_expansive(env, arg.exp_type);
    };
    generalize(arg.exp_type);
    let (val_cases, partial) =
      type_cases(env, arg.exp_type, ty_expected, true, loc, branches);
    re({
      exp_desc: [@implicit_arity] TExpMatch(arg, val_cases, partial),
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, ty_expected),
      exp_env: env,
    });
  | [@implicit_arity] PExpPrim1(p1, sarg) =>
    let (argtype, rettype) = prim1_type(p1);
    let arg = type_expect(env, sarg, mk_expected(argtype));
    rue({
      exp_desc: [@implicit_arity] TExpPrim1(p1, arg),
      exp_loc: loc,
      exp_extra: [],
      exp_type: rettype,
      exp_env: env,
    });
  | [@implicit_arity] PExpPrim2(p2, sarg1, sarg2) =>
    let (arg1type, arg2type, rettype) = prim2_type(p2);
    let arg1 = type_expect(env, sarg1, mk_expected(arg1type));
    let arg2 = type_expect(env, sarg2, mk_expected(arg2type));
    rue({
      exp_desc: [@implicit_arity] TExpPrim2(p2, arg1, arg2),
      exp_loc: loc,
      exp_extra: [],
      exp_type: rettype,
      exp_env: env,
    });
  | [@implicit_arity] PExpBoxAssign(sboxexpr, sval) =>
    let boxexpr =
      type_expect(
        env,
        sboxexpr,
        mk_expected(~explanation=Assign_not_box) @@
        Builtin_types.type_box @@
        newvar(~name="a", ()),
      );
    let val_ = type_expect(env, sval, ty_expected_explained);
    unify_exp(env, boxexpr) @@ Builtin_types.type_box(val_.exp_type);
    re({
      exp_desc: [@implicit_arity] TExpBoxAssign(boxexpr, val_),
      exp_loc: loc,
      exp_extra: [],
      exp_type: val_.exp_type,
      exp_env: env,
    });
  | [@implicit_arity] PExpAssign(sidexpr, sval) =>
    let idexpr = type_expect(env, sidexpr, ty_expected_explained);
    let (id, {val_mutable}) =
      switch (idexpr.exp_desc) {
      | TExpIdent(_, id, vd) => (id, vd)
      | _ =>
        failwith("lhs of assign was not identifier; impossible by parsing")
      };
    if (!val_mutable) {
      raise(Error(loc, env, Assign_not_mutable(id.txt)));
    };
    let val_ = type_expect(env, sval, ty_expected_explained);
    unify_exp(env, val_, idexpr.exp_type);
    re({
      exp_desc: TExpAssign(idexpr, val_),
      exp_loc: loc,
      exp_extra: [],
      exp_type: val_.exp_type,
      exp_env: env,
    });
  | [@implicit_arity] PExpIf(scond, sifso, sifnot) =>
    let cond =
      type_expect(
        env,
        scond,
        mk_expected(~explanation=If_conditional, Builtin_types.type_bool),
      );

    let (ifso, ifnot) =
      switch (sifnot.pexp_desc) {
      | PExpBlock([]) =>
        let void_exp = {
          exp_desc: TExpConstant(Const_void),
          exp_loc: loc,
          exp_extra: [],
          exp_type: Builtin_types.type_void,
          exp_env: env,
        };
        let ifso =
          type_expect(
            env,
            sifso,
            mk_expected(
              ~explanation=If_no_else_branch,
              Builtin_types.type_void,
            ),
          );
        let ifnot = {...void_exp, exp_desc: TExpBlock([void_exp])};
        (ifso, ifnot);
      | _ =>
        let ifso = type_expect(env, sifso, ty_expected_explained);
        let ifnot = type_expect(env, sifnot, ty_expected_explained);
        /* Both types should match */
        unify_exp(env, ifnot, ifso.exp_type);
        (ifso, ifnot);
      };

    re({
      exp_desc: [@implicit_arity] TExpIf(cond, ifso, ifnot),
      exp_loc: loc,
      exp_extra: [],
      exp_type: ifso.exp_type,
      exp_env: env,
    });
  | [@implicit_arity] PExpWhile(scond, sbody) =>
    let cond =
      type_expect(
        env,
        scond,
        mk_expected(
          ~explanation=While_loop_conditional,
          Builtin_types.type_bool,
        ),
      );
    let body = type_expect(env, sbody, ty_expected_explained);
    re({
      exp_desc: [@implicit_arity] TExpWhile(cond, body),
      exp_loc: loc,
      exp_extra: [],
      /* While loops don't evaluate to anything */
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | [@implicit_arity] PExpConstraint(sarg, styp) =>
    begin_def();
    let cty = Typetexp.transl_simple_type(env, false, styp);
    let ty = cty.ctyp_type;
    end_def();
    generalize_structure(ty);
    let (arg, ty') = (
      List.hd @@ type_arguments(env, [sarg], [ty], [instance(env, ty)]),
      instance(env, ty),
    );
    rue({
      exp_desc: arg.exp_desc,
      exp_loc: arg.exp_loc,
      exp_type: ty',
      exp_env: env,
      exp_extra: [(TExpConstraint(cty), loc), ...arg.exp_extra],
    });
  | PExpBlock([]) => failwith("Internal error: type_expect_ block was empty")
  | PExpBlock(es) =>
    let rec process_es = rem =>
      switch (rem) {
      | [] => failwith("Impossible: empty case in process_es")
      | [e] =>
        let expr = type_expect(env, e, ty_expected_explained);
        ([expr], expr.exp_type);
      | [e, ...es] =>
        let (exprs, typ) = process_es(es);
        (
          [
            type_statement_expr(~explanation=Sequence_left_hand_side, env, e),
            ...exprs,
          ],
          typ,
        );
      };

    let (exprs, typ) = process_es(es);
    re({
      exp_desc: TExpBlock(exprs),
      exp_loc: loc,
      exp_extra: [],
      exp_type: typ,
      exp_env: env,
    });
  | PExpNull =>
    rue({
      exp_desc: TExpNull,
      exp_loc: loc,
      exp_extra: [],
      exp_type: instance(env, Builtin_types.type_void),
      exp_env: env,
    })
  };
}

and type_function =
    (~in_function=?, loc, attrs, env, ty_expected_explained, l, caselist) => {
  let {ty: ty_expected, explanation} = ty_expected_explained;
  /*Format.eprintf "@[type_function: expected: %a@]@." Printtyp.raw_type_expr ty_expected;*/
  let (loc_fun, ty_fun) =
    switch (in_function) {
    | Some(p) => p
    | None => (loc, instance(env, ty_expected))
    };

  let separate =
    Grain_utils.Config.principal^ || Env.has_local_constraints(env);
  if (separate) {
    begin_def();
  };
  let rec arity = caselist =>
    switch (caselist) {
    | [] => failwith("Impossible: type_function: empty lambda")
    | [
        {pmb_pat: {ppat_desc: [@implicit_arity] PPatConstraint(p, _)}, _} as mb,
      ] =>
      arity([{...mb, pmb_pat: p}])
    | [{pmb_pat: {ppat_desc: PPatTuple(args)}, _}] => List.length(args)
    | [{pmb_pat: {ppat_desc: PPatVar(_)}, _}] => 1
    /* FIXME: Less hard-coding, please */
    | [
        {
          pmb_pat: {
            ppat_desc: [@implicit_arity] PPatConstruct({txt: ident, _}, []),
            _,
          },
          _,
        },
      ]
        when Identifier.equal(ident, Identifier.IdentName("()")) => 0
    | _ => failwith("Impossible: type_function: impossible caselist")
    };
  let arity = arity(caselist);
  let exp_inst = instance(env, ty_expected);
  /*Format.eprintf "@[type_function: pre: %a@]@." Printtyp.raw_type_expr exp_inst;*/
  let (ty_arg, ty_res) =
    try(filter_arrow(arity, env, exp_inst)) {
    | Unify(_) =>
      raise(
        [@implicit_arity]
        Error(
          loc_fun,
          env,
          [@implicit_arity]
          Too_many_arguments(in_function != None, ty_fun, explanation),
        ),
      )
    };

  /*let rec fmt_args ppf = function
      | [] -> Format.fprintf ppf ")"
      | a::tl ->
        Format.fprintf ppf "%a, %a" Printtyp.raw_type_expr a fmt_args tl in
    Format.eprintf "@[type_function: %i@ (%a -> %a@]@." (get_current_level())
      fmt_args (ty_arg) Printtyp.raw_type_expr ty_res;*/
  if (separate) {
    end_def();
    List.iter(generalize_structure, ty_arg);
    generalize_structure(ty_res);
  };
  let normalized_arg_type =
    switch (ty_arg) {
    | [] => Builtin_types.type_void
    | [x] => x
    | _ => newty(TTyTuple(ty_arg))
    };
  let (cases, partial) =
    type_cases(
      ~in_function=(loc_fun, ty_fun),
      env,
      normalized_arg_type,
      ty_res,
      true,
      loc,
      caselist,
    );
  /* TODO: Decide if this should be added to TExpLambda */
  /*let param = name_pattern "param" cases in*/
  re({
    exp_desc: [@implicit_arity] TExpLambda(cases, partial),
    exp_loc: loc,
    exp_extra: [],
    exp_type:
      instance(
        env,
        newgenty([@implicit_arity] TTyArrow(ty_arg, ty_res, TComOk)),
      ),
    exp_env: env,
  });
}

and type_arguments = (~recarg=?, env, sargs, tys_expected', tys_expected) =>
  /* ty_expected' may be generic */
  /* Note (Philip): I think the heavy lifting of this function
     was there to support optional arguments (which we currently don't). */
  List.map2(
    (sarg, (targ', targ)) => {
      let texp = type_expect(~recarg?, env, sarg, mk_expected(targ'));
      unify_exp(env, texp, targ);
      texp;
    },
    sargs,
    List.combine(tys_expected', tys_expected),
  )

and type_application = (env, funct, args) => {
  /* funct.exp_type may be generic */
  /*** Arguments, return value */
  let ty_fun = expand_head(env, funct.exp_type);
  let (ty_args, ty_ret, ty_level) =
    switch (ty_fun.desc) {
    | TTyVar(_) =>
      let t_args = List.map(x => newvar(), args)
      and t_ret = newvar();
      /*let not_identity = function
          | TExpIdent(_,_,{val_kind=TValPrim
                               {Primitive.prim_name="%identity"}}) ->
            false
          | _ -> true
        in
        List.iter2 (fun arg t_arg ->
            if ty_fun.level >= t_arg.level && not_identity funct.exp_desc then
              Location.prerr_warning arg.pexp_loc Warnings.Unused_argument
          ) args t_args;*/
      unify(
        env,
        ty_fun,
        newty(
          [@implicit_arity]
          TTyArrow(t_args, t_ret, TComLink(ref(TComUnknown))),
        ),
      );
      (t_args, t_ret, ty_fun.level);
    | [@implicit_arity] TTyArrow(t_args, t_ret, _)
        when List.length(t_args) == List.length(args) => (
        t_args,
        t_ret,
        ty_fun.level,
      )
    | [@implicit_arity] TTyArrow(t_args, t_ret, _) =>
      raise(
        [@implicit_arity]
        Error(
          funct.exp_loc,
          env,
          [@implicit_arity]
          Arity_mismatch(
            expand_head(env, funct.exp_type),
            List.length(args),
          ),
        ),
      )
    | td =>
      raise(
        [@implicit_arity]
        Error(
          funct.exp_loc,
          env,
          Apply_non_function(expand_head(env, funct.exp_type)),
        ),
      )
    };

  let typed_args =
    type_arguments(env, args, ty_args, List.map(instance(env), ty_args));
  (typed_args, instance(env, ty_ret));
}

and type_construct = (env, loc, lid, sarg, ty_expected_explained, attrs) => {
  let {ty: ty_expected, explanation} = ty_expected_explained;
  let opath =
    try({
      let (p0, p, _) = extract_concrete_variant(env, ty_expected);
      Some((
        p0,
        p,
        ty_expected.level == generic_level || ! Grain_utils.Config.principal^,
      ));
    }) {
    | Not_found => None
    };

  let constrs = Typetexp.find_all_constructors(env, lid.loc, lid.txt);
  let constr =
    wrap_disambiguate(
      "This variant expression is expected to have",
      ty_expected_explained,
      Constructor.disambiguate(lid, env, opath),
      constrs,
    );
  /*Env.mark_constructor Env.Positive env (Identifier.last lid.txt) constr;*/
  let sargs =
    switch (sarg) {
    | None => []
    | Some({pexp_desc: PExpTuple(sel)}) when constr.cstr_arity > 1 =>
      /*|| Builtin_attributes.explicit_arity attrs*/
      sel
    | Some(se) => [se]
    };
  if (List.length(sargs) != constr.cstr_arity) {
    raise(
      [@implicit_arity]
      Error(
        loc,
        env,
        [@implicit_arity]
        Constructor_arity_mismatch(
          lid.txt,
          constr.cstr_arity,
          List.length(sargs),
        ),
      ),
    );
  };
  let separate =
    Grain_utils.Config.principal^ || Env.has_local_constraints(env);
  if (separate) {
    begin_def();
    begin_def();
  };
  let (ty_args, ty_res) = instance_constructor(constr);
  let texp =
    re({
      exp_desc: [@implicit_arity] TExpConstruct(lid, constr, []),
      exp_loc: loc,
      exp_extra: [],
      exp_type: ty_res,
      exp_env: env,
    });
  if (separate) {
    end_def();
    generalize_structure(ty_res);
    with_explanation(explanation, () =>
      unify_exp(
        env,
        {...texp, exp_type: instance_def(ty_res)},
        instance(env, ty_expected),
      )
    );
    end_def();
    List.iter(generalize_structure, ty_args);
    generalize_structure(ty_res);
  };
  /* ty_args0 : type_expr list; ty_res : type_expr */
  let (ty_args0, ty_res) =
    switch (instance_list(env, [ty_res, ...ty_args])) {
    | [t, ...tl] => (tl, t)
    | _ => assert(false)
    };

  let texp = {...texp, exp_type: ty_res};
  if (!separate) {
    unify_exp(env, texp, instance(env, ty_expected));
  };
  let recarg = Rejected;
  /*match constr.cstr_inlined with
    | None -> Rejected
    | Some _ ->
        begin match sargs with
        | [{pexp_desc =
              Pexp_ident _ |
              Pexp_record (_, (Some {pexp_desc = Pexp_ident _}| None))}] ->
            Required
        | _ ->
            raise (Error(loc, env, Inlined_record_expected))
        end*/

  let args = type_arguments(~recarg, env, sargs, ty_args, ty_args0);
  /* NOTE: shouldn't we call "re" on this final expression? -- AF */
  {...texp, exp_desc: [@implicit_arity] TExpConstruct(lid, constr, args)};
}

/* Typing of statements (expressions whose values are discarded) */

and type_statement_expr = (~explanation=?, env, sexp) => {
  let loc = final_subexpression(sexp).pexp_loc;
  begin_def();
  let exp = type_exp(env, sexp);
  end_def();
  let ty = expand_head(env, exp.exp_type)
  and tv = newvar();
  if (is_Tvar(ty) && ty.level > tv.level) {
    Location.prerr_warning(loc, Grain_utils.Warnings.NonreturningStatement);
  };
  if (Grain_utils.Config.strict_sequence^) {
    let expected_ty = instance_def(Builtin_types.type_void);
    with_explanation(explanation, () => unify_exp(env, exp, expected_ty));
    exp;
  } else {
    switch (ty.desc) {
    /*| Tarrow _ -> [not really applicable with our syntax]
      Location.prerr_warning loc Warnings.Partial_application*/
    | [@implicit_arity] TTyConstr(p, _, _)
        when Path.same(p, Builtin_types.path_void) =>
      ()
    /*| Tvar _ ->
      add_delayed_check (fun () -> check_application_result env true exp)*/
    | _ => ()
    /* This isn't quite relevant to Grain mechanics
       Location.prerr_warning loc Grain_utils.Warnings.StatementType */
    };
    unify_var(env, tv, ty);
    exp;
  };
}

/* Typing of match cases */

and type_cases =
    (
      ~in_function=?,
      env,
      ty_arg: type_expr,
      ty_res,
      partial_flag,
      loc,
      caselist,
    ) => {
  /* ty_arg is _fully_ generalized */
  /*let patterns = List.map (fun {pmb_pat=p} -> p) caselist in*/
  /*let contains_polyvars = List.exists contains_polymorphic_variant patterns in
    let erase_either = contains_polyvars && contains_variant_either ty_arg
      and has_gadts = List.exists (contains_gadt env) patterns in*/
  /*  prerr_endline ( if has_gadts then "contains gadt" else "no gadt"); */
  let rec is_var = spat =>
    switch (spat.ppat_desc) {
    | PPatAny
    | PPatVar(_) => true
    | _ => false
    };
  let needs_exhaust_check =
    switch (caselist) {
    /*| [{pmb_body = {pexp_desc = Pexp_unreachable}}] -> true*/
    | [{pmb_pat}] when is_var(pmb_pat) => false
    | _ => true
    };

  let init_env = () => {
    /* raise level for existentials */
    begin_def();
    Ident.set_current_time(get_current_level());
    let lev = Ident.current_time();
    Ctype.init_def(lev + 1000); /* up to 1000 existentials */
    (lev, /*Env.add_gadt_instance_level lev*/ env);
  };

  let (lev, env) = /*if has_gadts then init_env () else*/ (
    get_current_level(),
    env,
  );

  /*  if has_gadts then
      Format.printf "lev = %d@.%a@." lev Printtyp.raw_type_expr ty_res; */
  /* Do we need to propagate polymorphism */
  let propagate =
    Grain_utils.Config.principal^  /*has_gadts ||*/
    || repr(ty_arg).level == generic_level
    || (
      switch (caselist) {
      | [{pmb_pat}] when is_var(pmb_pat) => false
      | _ => true
      }
    );
  if (propagate) {
    begin_def();
  }; /* propagation of the argument */
  let pattern_force = ref([]);
  /*Format.eprintf "@[%i %i@ %a@]@." lev (get_current_level())
    Printtyp.raw_type_expr ty_arg;*/
  let pat_env_list =
    List.map(
      ({pmb_pat, pmb_body}) => {
        /*let loc = pmb_body.pexp_loc
            (*let open Location in
            match pc_guard with
            | None -> pc_rhs.pexp_loc
            | Some g -> {pc_rhs.pexp_loc with loc_start=g.pexp_loc.loc_start}*)
          in*/
        if (Grain_utils.Config.principal^) {
          begin_def();
        }; /* propagation of pattern */
        let scope = None /*Some (Annot.Idef loc)*/;
        let (pat, ext_env, force, unpacks) = {
          let partial =
            if (Grain_utils.Config.principal^) {
              /* || erase_either*/
              Some(false);
            } else {
              None;
            };
          let ty_arg = instance(~partial?, env, ty_arg);
          Typepat.type_pattern(~lev, env, pmb_pat, scope, ty_arg);
        };

        pattern_force := force @ pattern_force^;
        let pat =
          if (Grain_utils.Config.principal^) {
            end_def();
            iter_pattern(({pat_type: t}) => generalize_structure(t), pat);
            {...pat, pat_type: instance(ext_env, pat.pat_type)};
          } else {
            pat;
          };

        (pat, (ext_env, unpacks));
      },
      caselist,
    );
  /* Unify all cases (delayed to keep it order-free) */
  let ty_arg' = newvar();
  let unify_pats = ty =>
    List.iter(
      ((pat, (ext_env, _))) => unify_pat(ext_env, pat, ty),
      pat_env_list,
    );
  unify_pats(ty_arg');
  /* Check for polymorphic variants to close */
  let patl = List.map(fst, pat_env_list);
  /*if List.exists has_variants patl then begin
      Parmatch.pressure_variants env patl;
      List.iter (iter_pattern finalize_variant) patl
    end;*/
  /* `Contaminating' unifications start here */
  List.iter(f => f(), pattern_force^);
  /* Post-processing and generalization */
  if (propagate) {
    /*|| erase_either*/ unify_pats(instance(env, ty_arg));
  };
  if (propagate) {
    List.iter(
      iter_pattern(({pat_type: t}) => unify_var(env, t, newvar())),
      patl,
    );
    end_def();
    List.iter(iter_pattern(({pat_type: t}) => generalize(t)), patl);
  };
  /* type bodies */
  let in_function =
    if (List.length(caselist) == 1) {
      in_function;
    } else {
      None;
    };
  let cases =
    List.map2(
      ((pat, (ext_env, unpacks)), {pmb_pat, pmb_body, pmb_loc}) => {
        let sexp = pmb_body /*wrap_unpacks pmb_body unpacks*/;
        let ty_res' =
          if (Grain_utils.Config.principal^) {
            begin_def();
            let ty = instance(~partial=true, env, ty_res);
            end_def();
            generalize_structure(ty);
            ty;
          } else {
            /*else if contains_gadt env pmb_body then correct_levels ty_res*/
            ty_res;
          };
        /*Format.eprintf "@[%i %i, ty_res' =@ %a@]@." lev (get_current_level())
          Printtyp.raw_type_expr ty_res';*/
        /*let guard =
            match pc_guard with
            | None -> None
            | Some scond ->
                Some
                  (type_expect ext_env (wrap_unpacks scond unpacks)
                     (mk_expected Predef.type_bool))
          in*/
        let exp =
          type_expect(~in_function?, ext_env, sexp, mk_expected(ty_res'));
        {
          mb_pat: pat,
          /*c_guard = guard;*/
          mb_body: {
            ...exp,
            exp_type: instance(env, ty_res'),
          },
          mb_loc: pmb_loc,
        };
      },
      pat_env_list,
      caselist,
    );

  if (Grain_utils.Config.principal^) /*|| has_gadts*/ {
    let ty_res' = instance(env, ty_res);
    List.iter(c => unify_exp(env, c.mb_body, ty_res'), cases);
  };
  let do_init = /*has_gadts ||*/ needs_exhaust_check;
  let (lev, env) =
    if (do_init) {
      /*&& not has_gadts*/ init_env();
    } else {
      (lev, env);
    };
  let ty_arg_check =
    if (do_init) {
      /* Hack: use for_saving to copy variables too */
      Subst.type_expr(
        Subst.for_saving(Subst.identity),
        ty_arg,
      );
    } else {
      ty_arg;
    };

  let partial =
    if (partial_flag) {
      check_partial(~lev, env, ty_arg_check, loc, cases);
    } else {
      Partial;
    };

  let unused_check = () => {
    /*List.iter (fun (pat, (env, _)) -> check_absent_variant env pat)
      pat_env_list;*/
    check_unused(~lev, env, instance(env, ty_arg_check), cases);
    Parmatch.check_ambiguous_bindings(cases);
  };

  if (do_init) {
    ();
  } else {
    /*add_delayed_check unused_check*/
    unused_check();
  };
  /* Check for unused cases, do not delay because of gadts */
  if (do_init) {
    end_def();
    /* Ensure that existential types do not escape */
    unify_exp_types(loc, env, instance(env, ty_res), newvar());
  };
  (cases, partial);
}

/* Typing of let bindings */

and type_let =
    (
      ~check=s => Warnings.Unused_var(s),
      ~check_strict=s => Warnings.Unused_var_strict(s),
      env,
      rec_flag,
      mut_flag,
      spat_sexp_list,
      scope,
      allow,
    ) => {
  open Ast_helper;
  begin_def();
  if (Grain_utils.Config.principal^) {
    begin_def();
  };
  /*let is_fake_let =
      match spat_sexp_list with
      | [{pvb_expr={pexp_desc=PExpMatch(
             {pexp_desc=PExpId({ txt = Identifier.IdentName "*opt*"})},_)}}] ->
          true (* the fake let-declaration introduced by fun ?(x = e) -> ... *)
      | _ ->
          false
    in*/
  /*let check = if is_fake_let then check_strict else check in*/
  let spatl =
    List.map(
      ({pvb_pat: spat, pvb_expr: sexp}) =>
        (
          [],
          switch (spat.ppat_desc, sexp.pexp_desc) {
          | (PPatAny | PPatConstraint(_), _) => spat
          /*| _, PExpConstraint (_, sty) when !Grain_utils.Config.principal ->
            (* propagate type annotation to pattern,
               to allow it to be generalized in -principal mode *)
            Pat.constraint_
              ~loc:{spat.ppat_loc with Location.loc_ghost=true}
              spat
              sty*/
          | _ => spat
          },
        ),
      spat_sexp_list,
    );
  let nvs = List.map(_ => newvar(), spatl);
  let mut = mut_flag == Mutable;
  let (pat_list, new_env, force, unpacks, pv) =
    type_pattern_list(~mut, env, spatl, scope, nvs, allow);
  let attrs_list = List.map(fst, spatl);
  let is_recursive = rec_flag == Recursive;
  /* If recursive, first unify with an approximation of the expression */
  if (is_recursive) {
    List.iter2(
      (pat, binding) => {
        let pat =
          switch (pat.pat_type.desc) {
          | [@implicit_arity] TTyPoly(ty, tl) => {
              ...pat,
              pat_type: snd(instance_poly(~keep_names=true, false, tl, ty)),
            }
          | _ => pat
          };
        unify_pat(env, pat, type_approx(env, binding.pvb_expr));
      },
      pat_list,
      spat_sexp_list,
    );
  };
  /* Polymorphic variant processing */
  /*List.iter
    (fun pat ->
      if has_variants pat then begin
        Parmatch.pressure_variants env [pat];
        iter_pattern finalize_variant pat
      end)
    pat_list;*/
  /* Generalize the structure */
  let pat_list =
    if (Grain_utils.Config.principal^) {
      end_def();
      List.map(
        pat => {
          iter_pattern(pat => generalize_structure(pat.pat_type), pat);
          {...pat, pat_type: instance(env, pat.pat_type)};
        },
        pat_list,
      );
    } else {
      pat_list;
    };
  /* Only bind pattern variables after generalizing */
  List.iter(f => f(), force);
  let exp_env =
    if (is_recursive) {
      new_env;
    } else if (!is_recursive) {
      /* add ghost bindings to help detecting missing "rec" keywords */
      switch (spat_sexp_list) {
      | [{pvb_loc, _}, ..._] =>
        maybe_add_pattern_variables_ghost(pvb_loc, env, pv)
      | _ => assert(false)
      };
    } else {
      env;
    };
  let current_slot = ref(None);
  /*let rec_needed = ref false in*/
  /*let warn_about_unused_bindings =
      List.exists
        (fun attrs ->
           Builtin_attributes.warning_scope ~ppwarning:false attrs (fun () ->
             Warnings.is_active (check "") || Warnings.is_active (check_strict "") ||
             (is_recursive && (Warnings.is_active Warnings.Unused_rec_flag))))
        attrs_list
    in*/
  let pat_slot_list =
    /* Algorithm to detect unused declarations in recursive bindings:
         - During type checking of the definitions, we capture the 'value_used'
           events on the bound identifiers and record them in a slot corresponding
           to the current definition (!current_slot).
           In effect, this creates a dependency graph between definitions.
         - After type checking the definition (!current_slot = None),
           when one of the bound identifier is effectively used, we trigger
           again all the events recorded in the corresponding slot.
           The effect is to traverse the transitive closure of the graph created
           in the first step.
         We also keep track of whether *all* variables in a given pattern
         are unused. If this is the case, for local declarations, the issued
         warning is 26, not 27.
       */
    List.map2(
      (attrs, pat) => (pat, None), // Builtin_attributes.warning_scope ~ppwarning:false attrs (fun () ->
      //   if not warn_about_unused_bindings then pat, None
      //   else
      //     let some_used = ref false in
      //     (* has one of the identifier of this pattern been used? *)
      //     let slot = ref [] in
      //     List.iter
      //       (fun id ->
      //         let vd = Env.find_value (Path.Pident id) new_env in
      //         (* note: Env.find_value does not trigger the value_used event *)
      //         let name = Ident.name id in
      //         let used = ref false in
      //         if not (name = "" || name.[0] = '_' || name.[0] = '#') then
      //           add_delayed_check
      //             (fun () ->
      //                 if not !used then
      //                   Location.prerr_warning vd.Types.val_loc
      //                     ((if !some_used then check_strict else check) name)
      //             );
      //         Env.set_value_used_callback
      //           name vd
      //           (fun () ->
      //               match !current_slot with
      //               | Some slot ->
      //                 slot := (name, vd) :: !slot; rec_needed := true
      //               | None ->
      //                 List.iter
      //                   (fun (name, vd) -> Env.mark_value_used env name vd)
      //                   (get_ref slot);
      //                 used := true;
      //                 some_used := true
      //           )
      //       )
      //       (Typedtree.pattern_bound_idents pat);
      //     pat, Some slot
      // )
      attrs_list,
      pat_list,
    );

  let exp_list =
    List.map2(
      ({pvb_expr: sexp, _}, (pat, slot)) => {
        /*let sexp =
          if rec_flag = Recursive then wrap_unpacks sexp unpacks else sexp in*/
        if (is_recursive) {
          current_slot := slot;
        };
        switch (pat.pat_type.desc) {
        | [@implicit_arity] TTyPoly(ty, tl) =>
          /*Printf.eprintf "type_let: TTyPoly\n";*/
          begin_def();
          if (Grain_utils.Config.principal^) {
            begin_def();
          };
          let (vars, ty') = instance_poly(~keep_names=true, true, tl, ty);
          if (Grain_utils.Config.principal^) {
            end_def();
            generalize_structure(ty');
          };
          let exp =
            /*Builtin_attributes.warning_scope pvb_attributes
              (fun () -> type_expect exp_env sexp (mk_expected ty'))*/
            type_expect(exp_env, sexp, mk_expected(ty'));

          end_def();
          check_univars(env, true, "definition", exp, pat.pat_type, vars);
          {...exp, exp_type: instance(env, exp.exp_type)};
        | _ =>
          /*Printf.eprintf "type_let: non-TTyPoly\n";
            Format.eprintf "@[type_let: expected: %a@]@."
              Printtyp.raw_type_expr pat.pat_type;*/
          /*Builtin_attributes.warning_scope pvb_attributes (fun () ->
            type_expect exp_env sexp (mk_expected pat.pat_type))*/
          type_expect(exp_env, sexp, mk_expected(pat.pat_type))
        };
      },
      spat_sexp_list,
      pat_slot_list,
    );
  current_slot := None;
  /*if is_recursive && not !rec_needed
    && Warnings.is_active Warnings.Unused_rec_flag then begin
      let {pvb_pat; pvb_attributes} = List.hd spat_sexp_list in
      (* See PR#6677 *)
      Builtin_attributes.warning_scope ~ppwarning:false pvb_attributes
        (fun () ->
           Location.prerr_warning pvb_pat.ppat_loc Warnings.Unused_rec_flag
        )
    end;
    List.iter2
      (fun pat (attrs, exp) ->
         Builtin_attributes.warning_scope ~ppwarning:false attrs
           (fun () ->
              ignore(check_partial env pat.pat_type pat.pat_loc
                       [case pat exp])
           )
      )
      pat_list
      (List.map2 (fun (attrs, _) e -> attrs, e) spatl exp_list);*/
  end_def();
  List.iter2(
    (pat, exp) =>
      if (!is_nonexpansive(exp)) {
        iter_pattern(pat => generalize_expansive(env, pat.pat_type), pat);
      },
    pat_list,
    exp_list,
  );
  List.iter(
    pat => iter_pattern(pat => generalize(pat.pat_type), pat),
    pat_list,
  );
  let l = List.combine(pat_list, exp_list);
  let l =
    List.map2(
      ((p, e), pvb) => {vb_pat: p, vb_expr: e, vb_loc: pvb.pvb_loc},
      l,
      spat_sexp_list,
    );

  if (is_recursive) {
    List.iter(
      ({vb_pat: pat}) =>
        switch (pat.pat_desc) {
        | TPatVar(_) => ()
        | [@implicit_arity] TPatAlias({pat_desc: TPatAny}, _, _) => ()
        | _ =>
          raise(
            [@implicit_arity] Error(pat.pat_loc, env, Illegal_letrec_pat),
          )
        },
      l,
    );
  };
  (l, new_env, unpacks);
}

and type_label_access = (env, srecord, lid) => {
  /* if !Clflags.principal then begin_def (); */
  let record = type_exp(~recarg=Allowed, env, srecord);
  /* if !Clflags.principal then begin */
  /* end_def (); */
  /* generalize_structure record.exp_type */
  /* end; */
  let ty_exp = record.exp_type;
  let opath =
    try({
      let (p0, p, _) = extract_concrete_record(env, ty_exp);
      Some((
        p0,
        p,
        repr(ty_exp).level == generic_level /*|| not !Clflags.principal*/,
      ));
    }) {
    | Not_found => None
    };

  let labels = Env.lookup_all_labels(lid.txt, env);
  let label =
    wrap_disambiguate(
      "This expression has",
      mk_expected(ty_exp),
      Label.disambiguate(lid, env, opath),
      labels,
    );
  (record, label, opath);
}

and type_label_exp = (create, env, loc, ty_expected, (lid, label, sarg)) => {
  /* Here also ty_expected may be at generic_level */
  begin_def();
  let separate = Clflags.principal^ || Env.has_local_constraints(env);
  if (separate) {
    begin_def();
    begin_def();
  };
  let (vars, ty_arg, ty_res) = instance_label(true, label);
  if (separate) {
    end_def();
    /* Generalize label information */
    generalize_structure(ty_arg);
    generalize_structure(ty_res);
  };
  try(unify(env, instance(env, ty_res), instance(env, ty_expected))) {
  | Unify(trace) =>
    raise(
      [@implicit_arity]
      Error(lid.loc, env, [@implicit_arity] Label_mismatch(lid.txt, trace)),
    )
  };
  /* Instantiate so that we can generalize internal nodes */
  let ty_arg = instance(env, ty_arg);
  if (separate) {
    end_def();
    /* Generalize information merged from ty_expected */
    generalize_structure(ty_arg);
  };
  let arg = {
    let snap =
      if (vars == []) {
        None;
      } else {
        Some(Btype.snapshot());
      };
    let arg =
      List.hd @@
      type_arguments(env, [sarg], [ty_arg], [instance(env, ty_arg)]);
    end_def();
    try(
      {
        check_univars(
          env,
          vars != [],
          "field value",
          arg,
          label.lbl_arg,
          vars,
        );
        arg;
      }
    ) {
    | exn when maybe_expansive(arg) =>
      /* Try to retype without propagating ty_arg, cf PR#4862 */
      try(
        {
          Option.iter(Btype.backtrack, snap);
          begin_def();
          let arg = type_exp(env, sarg);
          end_def();
          generalize_expansive(env, arg.exp_type);
          unify_exp(env, arg, ty_arg);
          check_univars(env, false, "field value", arg, label.lbl_arg, vars);
          arg;
        }
      ) {
      | [@implicit_arity] Error(_, _, Less_general(_)) as e => raise(e)
      | _ => raise(exn)
      }
    };
  }; /* In case of failure return the first error */

  (lid, label, {...arg, exp_type: instance(env, arg.exp_type)});
};

let check_recursive_bindings = (env, vbs) =>
  /* TODO: Implement */
  ();

/* Typing of toplevel bindings */
let type_binding = (env, rec_flag, mut_flag, spat_sexp_list, scope) => {
  Typetexp.reset_type_variables();
  let (pat_exp_list, new_env, _unpacks) =
    type_let(
      /*~check:(fun s -> Warnings.Unused_value_declaration s)
        ~check_strict:(fun s -> Warnings.Unused_value_declaration s)*/
      env,
      rec_flag,
      mut_flag,
      spat_sexp_list,
      scope,
      false,
    );

  (pat_exp_list, new_env);
};
let type_let = (env, rec_flag, mut_flag, spat_sexp_list, scope) => {
  let (pat_exp_list, new_env, _unpacks) =
    type_let(env, rec_flag, mut_flag, spat_sexp_list, scope, false);
  (pat_exp_list, new_env);
};

let type_expression = (env, sexp) => {
  Typetexp.reset_type_variables();
  begin_def();
  let exp = type_exp(env, sexp);
  end_def();
  if (!is_nonexpansive(exp)) {
    generalize_expansive(env, exp.exp_type);
  };
  generalize(exp.exp_type);
  exp;
};

/* Error report */
let spellcheck = (ppf, unbound_name, valid_names) =>
  Misc.did_you_mean(ppf, () => Misc.spellcheck(valid_names, unbound_name));
let spellcheck_idents = (ppf, unbound, valid_idents) =>
  spellcheck(ppf, Ident.name(unbound), List.map(Ident.name, valid_idents));
open Format;
open Printtyp;
let report_type_expected_explanation = (expl, ppf) =>
  switch (expl) {
  | If_conditional => fprintf(ppf, "the condition of an if-statement")
  | If_no_else_branch =>
    fprintf(ppf, "the result of a conditional with no else branch")
  | While_loop_conditional => fprintf(ppf, "the condition of a while-loop")
  | While_loop_body => fprintf(ppf, "the body of a while-loop")
  | For_loop_start_index => fprintf(ppf, "a for-loop start index")
  | For_loop_stop_index => fprintf(ppf, "a for-loop stop index")
  | For_loop_body => fprintf(ppf, "the body of a for-loop")
  | Assert_condition => fprintf(ppf, "the condition of an assertion")
  | Sequence_left_hand_side =>
    fprintf(ppf, "the left-hand side of a sequence")
  | Assign_not_box => fprintf(ppf, "the left-hand side of an assignment")
  | Assign_not_array =>
    fprintf(ppf, "the left-hand side of an array assignment")
  | Assign_not_array_index =>
    fprintf(ppf, "the index argument of an array assignment")
  };
let report_type_expected_explanation_opt = (expl, ppf) =>
  switch (expl) {
  | None => ()
  | Some(expl) =>
    fprintf(
      ppf,
      "@ because it is in %t",
      report_type_expected_explanation(expl),
    )
  };
let report_error = (env, ppf) =>
  fun
  | Polymorphic_label(lid) =>
    fprintf(
      ppf,
      "@[The record field %a is polymorphic.@ %s@]",
      identifier,
      lid,
      "You cannot instantiate it in a pattern.",
    )
  | [@implicit_arity] Constructor_arity_mismatch(lid, expected, provided) =>
    fprintf(
      ppf,
      "@[The constructor %a@ expects %i argument(s),@ but is applied here to %i argument(s)@]",
      identifier,
      lid,
      expected,
      provided,
    )
  | [@implicit_arity] Label_mismatch(lid, trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf =>
        fprintf(
          ppf,
          "The record field %a@ belongs to the type",
          identifier,
          lid,
        ),
      fun
      | ppf => fprintf(ppf, "but is mixed here with fields of type"),
    )
  | Pattern_type_clash(trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf => fprintf(ppf, "This pattern matches values of type"),
      fun
      | ppf =>
        fprintf(
          ppf,
          "but a pattern was expected which matches values of type",
        ),
    )
  | [@implicit_arity] Or_pattern_type_clash(id, trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf =>
        fprintf(
          ppf,
          "The variable %s on the left-hand side of this or-pattern has type",
          Ident.name(id),
        ),
      fun
      | ppf => fprintf(ppf, "but on the right-hand side it has type"),
    )
  | Multiply_bound_variable(name) =>
    fprintf(ppf, "Variable %s is bound several times in this matching", name)
  | [@implicit_arity] Orpat_vars(id, valid_idents) => {
      fprintf(
        ppf,
        "Variable %s must occur on both sides of this | pattern",
        Ident.name(id),
      );
      spellcheck_idents(ppf, id, valid_idents);
    }
  | [@implicit_arity] Expr_type_clash(trace, explanation) =>
    report_unification_error(
      ppf,
      env,
      trace,
      ~type_expected_explanation=
        report_type_expected_explanation_opt(explanation),
      fun
      | ppf => fprintf(ppf, "This expression has type"),
      fun
      | ppf => fprintf(ppf, "but an expression was expected of type"),
    )
  | Apply_non_function(typ) => {
      reset_and_mark_loops(typ);
      switch (repr(typ).desc) {
      | TTyArrow(_) =>
        fprintf(
          ppf,
          "@[<v>@[<2>This function has type@ %a@]",
          type_expr,
          typ,
        );
        fprintf(
          ppf,
          "@ @[It is applied to too many arguments;@ %s@]@]",
          "maybe you forgot a `;'.",
        );
      | _ =>
        fprintf(
          ppf,
          "@[<v>@[<2>This expression has type@ %a@]@ %s@]",
          type_expr,
          typ,
          "This is not a function; it cannot be applied.",
        )
      };
    }
  | Label_multiply_defined(s) =>
    fprintf(ppf, "The record field label %s is defined several times", s)
  | Label_missing(labels) => {
      let print_labels = ppf =>
        List.iter(lbl => fprintf(ppf, "@ %s", Ident.name(lbl)));
      fprintf(
        ppf,
        "@[<hov>Some record fields are undefined:%a@]",
        print_labels,
        labels,
      );
    }
  | Label_not_mutable(lid) =>
    fprintf(ppf, "The record field %a is not mutable", identifier, lid)
  | Assign_not_mutable(id) =>
    fprintf(ppf, "The identifier %a was not declared mutable", identifier, id)
  | [@implicit_arity] Arity_mismatch(typ, arity) =>
    fprintf(
      ppf,
      "@[The type %a cannot be called with %d argument%s@]",
      Printtyp.type_expr,
      typ,
      arity,
      if (arity == 1) {
        "";
      } else {
        "s";
      },
    )
  | [@implicit_arity]
    Wrong_name(eorp, ty_expected, kind, p, name, valid_names) => {
      let {ty, explanation} = ty_expected;
      reset_and_mark_loops(ty);
      {
        fprintf(
          ppf,
          "@[@[<2>%s type@ %a%t@]@ ",
          eorp,
          type_expr,
          ty,
          report_type_expected_explanation_opt(explanation),
        );
        fprintf(
          ppf,
          "The %s %s does not belong to type %a@]",
          label_of_kind(kind),
          name,
          /*kind*/ path,
          p,
        );
      };
      spellcheck(ppf, name, valid_names);
    }
  | [@implicit_arity] Name_type_mismatch(kind, lid, tp, tpl) => {
      let name = label_of_kind(kind);
      report_ambiguous_type_error(
        ppf,
        env,
        tp,
        tpl,
        fun
        | ppf =>
          fprintf(
            ppf,
            "The %s %a@ belongs to the %s type",
            name,
            identifier,
            lid,
            kind,
          ),
        fun
        | ppf =>
          fprintf(
            ppf,
            "The %s %a@ belongs to one of the following %s types:",
            name,
            identifier,
            lid,
            kind,
          ),
        fun
        | ppf =>
          fprintf(
            ppf,
            "but a %s was expected belonging to the %s type",
            name,
            kind,
          ),
      );
    }
  | Invalid_format(msg) => fprintf(ppf, "%s", msg)
  | [@implicit_arity] Undefined_method(ty, me, valid_methods) => {
      reset_and_mark_loops(ty);
      fprintf(
        ppf,
        "@[<v>@[This expression has type@;<1 2>%a@]@,It has no method %s@]",
        type_expr,
        ty,
        me,
      );
      switch (valid_methods) {
      | None => ()
      | Some(valid_methods) => spellcheck(ppf, me, valid_methods)
      };
    }
  | [@implicit_arity] Undefined_inherited_method(me, valid_methods) => {
      fprintf(ppf, "This expression has no method %s", me);
      spellcheck(ppf, me, valid_methods);
    }
  | Virtual_class(cl) =>
    fprintf(ppf, "Cannot instantiate the virtual class %a", identifier, cl)
  | [@implicit_arity] Unbound_instance_variable(var, valid_vars) => {
      fprintf(ppf, "Unbound instance variable %s", var);
      spellcheck(ppf, var, valid_vars);
    }
  | [@implicit_arity] Instance_variable_not_mutable(b, v) =>
    if (b) {
      fprintf(ppf, "The instance variable %s is not mutable", v);
    } else {
      fprintf(ppf, "The value %s is not an instance variable", v);
    }
  | [@implicit_arity] Not_subtype(tr1, tr2) =>
    fprintf(ppf, "<subtyping error>")
  | Outside_class =>
    fprintf(ppf, "This object duplication occurs outside a method definition")
  | Value_multiply_overridden(v) =>
    fprintf(ppf, "The instance variable %s is overridden several times", v)
  | [@implicit_arity] Coercion_failure(ty, ty', trace, b) => {
      report_unification_error(
        ppf,
        env,
        trace,
        fun
        | ppf => {
            let (ty, ty') = prepare_expansion((ty, ty'));
            fprintf(
              ppf,
              "This expression cannot be coerced to type@;<1 2>%a;@ it has type",
              type_expansion(ty),
              ty',
            );
          },
        fun
        | ppf => fprintf(ppf, "but is here used with type"),
      );
      if (b) {
        fprintf(
          ppf,
          ".@.@[<hov>%s@ %s@ %s@]",
          "This simple coercion was not fully general.",
          "Hint: Consider using a fully explicit coercion",
          "of the form: `(foo : ty1 :> ty2)'.",
        );
      };
    }
  | [@implicit_arity] Too_many_arguments(in_function, ty, explanation) => {
      reset_and_mark_loops(ty);
      if (in_function) {
        fprintf(ppf, "This function expects too many arguments,@ ");
        fprintf(
          ppf,
          "it should have type@ %a%t",
          type_expr,
          ty,
          report_type_expected_explanation_opt(explanation),
        );
      } else {
        fprintf(ppf, "This expression should not be a function,@ ");
        fprintf(
          ppf,
          "the expected type is@ %a%t",
          type_expr,
          ty,
          report_type_expected_explanation_opt(explanation),
        );
      };
    }
  | [@implicit_arity] Scoping_let_module(id, ty) => {
      reset_and_mark_loops(ty);
      fprintf(
        ppf,
        "This `let module' expression has type@ %a@ ",
        type_expr,
        ty,
      );
      fprintf(
        ppf,
        "In this type, the locally bound module name %s escapes its scope",
        id,
      );
    }
  | Masked_instance_variable(lid) =>
    fprintf(
      ppf,
      "The instance variable %a@ cannot be accessed from the definition of another instance variable",
      identifier,
      lid,
    )
  | Private_type(ty) =>
    fprintf(ppf, "Cannot create values of the private type %a", type_expr, ty)
  | [@implicit_arity] Private_label(lid, ty) =>
    fprintf(
      ppf,
      "Cannot assign field %a of the private type %a",
      identifier,
      lid,
      type_expr,
      ty,
    )
  | Not_a_variant_type(lid) =>
    fprintf(ppf, "The type %a@ is not a variant type", identifier, lid)
  | Incoherent_label_order => {
      fprintf(ppf, "This function is applied to arguments@ ");
      fprintf(ppf, "in an order different from other calls.@ ");
      fprintf(ppf, "This is only allowed when the real type is known.");
    }
  | [@implicit_arity] Less_general(kind, trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      ppf => fprintf(ppf, "This %s has type", kind),
      ppf => fprintf(ppf, "which is less general than"),
    )
  | Modules_not_allowed =>
    fprintf(ppf, "Modules are not allowed in this pattern.")
  | Cannot_infer_signature =>
    fprintf(
      ppf,
      "The signature for this packaged module couldn't be inferred.",
    )
  | Not_a_packed_module(ty) =>
    fprintf(
      ppf,
      "This expression is packed module, but the expected type is@ %a",
      type_expr,
      ty,
    )
  | Recursive_local_constraint(trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf => fprintf(ppf, "Recursive local constraint when unifying"),
      fun
      | ppf => fprintf(ppf, "with"),
    )
  | Unexpected_existential => fprintf(ppf, "Unexpected existential")
  | [@implicit_arity] Unqualified_gadt_pattern(tpath, name) =>
    fprintf(
      ppf,
      "@[The GADT constructor %s of type %a@ %s.@]",
      name,
      path,
      tpath,
      "must be qualified in this pattern",
    )
  | Invalid_interval =>
    fprintf(ppf, "@[Only character intervals are supported in patterns.@]")
  | Invalid_for_loop_index =>
    fprintf(
      ppf,
      "@[Invalid for-loop index: only variables and _ are allowed.@]",
    )
  | No_value_clauses =>
    fprintf(
      ppf,
      "None of the patterns in this 'match' expression match values.",
    )
  | Exception_pattern_below_toplevel =>
    fprintf(
      ppf,
      "@[Exception patterns must be at the top level of a match case.@]",
    )
  | Inlined_record_escape =>
    fprintf(
      ppf,
      "@[This form is not allowed as the type of the inlined record could escape.@]",
    )
  | Inlined_record_expected =>
    fprintf(ppf, "@[This constructor expects an inlined record argument.@]")
  | Invalid_extension_constructor_payload =>
    fprintf(
      ppf,
      "Invalid [%%extension_constructor] payload, a constructor is expected.",
    )
  | Not_an_extension_constructor =>
    fprintf(ppf, "This constructor is not an extension constructor.")
  | Literal_overflow(ty) =>
    fprintf(
      ppf,
      "Integer literal exceeds the range of representable integers of type %s",
      ty,
    )
  | [@implicit_arity] Unknown_literal(n, m) =>
    fprintf(ppf, "Unknown modifier '%c' for literal %s%c", m, n, m)
  | Illegal_letrec_pat =>
    fprintf(ppf, "Only variables are allowed as left-hand side of `let rec'")
  | Illegal_letrec_expr =>
    fprintf(
      ppf,
      "This kind of expression is not allowed as right-hand side of `let rec'",
    )
  | Illegal_class_expr =>
    fprintf(ppf, "This kind of recursive class expression is not allowed")
  | [@implicit_arity] Unbound_value_missing_rec(lid, loc) => {
      let (_, line, _) = Location.get_pos_info(loc.Location.loc_start);
      fprintf(
        ppf,
        "@[%s %a.@ %s %i.@]",
        "Unbound value",
        identifier,
        lid,
        "Hint: You are probably missing the `rec' keyword on line",
        line,
      );
    };
let report_error = (env, ppf, err) =>
  wrap_printing_env(~error=true, env, () => report_error(env, ppf, err));
let () =
  Location.register_error_of_exn(
    fun
    | [@implicit_arity] Error(loc, env, err) =>
      Some(Location.error_of_printer(loc, report_error(env), err))
    | Error_forward(err) => Some(err)
    | _ => None,
  );

/* Drop ?recarg */
let type_expect = (~in_function=?, env, e, ty) =>
  type_expect(~in_function?, env, e, ty);
let type_exp = (env, e) => type_exp(env, e);
let type_arguments = (env, es, t1s, t2s) =>
  type_arguments(env, es, t1s, t2s);
