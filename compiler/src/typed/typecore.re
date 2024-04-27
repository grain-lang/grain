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
  | Arity_mismatch(type_expr, option(type_forcing_context))
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
  | Apply_too_many_arguments(type_expr, list((argument_label, type_expr)))
  | Apply_too_few_arguments(list((argument_label, type_expr)))
  | Apply_unknown_label(string, list(string))
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
  | Not_a_function(type_expr, option(type_forcing_context))
  | Function_label_mismatch({
      got: argument_label,
      expected: argument_label,
      expected_type: type_expr,
      explanation: option(type_forcing_context),
    })
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
  | Inlined_record_misuse(Identifier.t, string, string)
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

let grain_type_of_wasm_prim_type =
  fun
  | Wasm_int32 => Builtin_types.type_wasmi32
  | Wasm_int64 => Builtin_types.type_wasmi64
  | Wasm_float32 => Builtin_types.type_wasmf32
  | Wasm_float64 => Builtin_types.type_wasmf64
  | Grain_bool => Builtin_types.type_bool;

let prim_type = (args, ret) =>
  newgenty(
    TTyArrow(
      List.map(
        ((name, ty)) => (Labeled(Location.mknoloc(name)), ty),
        args,
      ),
      ret,
      TComOk,
    ),
  );

let prim0_type =
  fun
  | AllocateInt32
  | AllocateInt64
  | AllocateUint32
  | AllocateUint64
  | AllocateFloat32
  | AllocateFloat64
  | AllocateRational
  | WasmMemorySize
  | HeapStart
  | HeapTypeMetadata => prim_type([], Builtin_types.type_wasmi32)
  | Unreachable => prim_type([], newgenvar(~name="a", ()));

let prim1_type =
  fun
  | AllocateArray
  | AllocateTuple
  | AllocateBytes
  | AllocateString
  | AllocateBigInt =>
    prim_type(
      [("size", Builtin_types.type_wasmi32)],
      Builtin_types.type_wasmi32,
    )
  | StringSize
  | BytesSize
  | LoadAdtVariant =>
    prim_type(
      [("ptr", Builtin_types.type_wasmi32)],
      Builtin_types.type_wasmi32,
    )
  | NewInt32 =>
    prim_type(
      [("int", Builtin_types.type_wasmi32)],
      Builtin_types.type_wasmi32,
    )
  | NewInt64 =>
    prim_type(
      [("int", Builtin_types.type_wasmi64)],
      Builtin_types.type_wasmi32,
    )
  | NewUint32 =>
    prim_type(
      [("int", Builtin_types.type_wasmi32)],
      Builtin_types.type_wasmi32,
    )
  | NewUint64 =>
    prim_type(
      [("int", Builtin_types.type_wasmi64)],
      Builtin_types.type_wasmi32,
    )
  | NewFloat32 =>
    prim_type(
      [("float", Builtin_types.type_wasmf32)],
      Builtin_types.type_wasmi32,
    )
  | NewFloat64 =>
    prim_type(
      [("float", Builtin_types.type_wasmf64)],
      Builtin_types.type_wasmi32,
    )
  | BuiltinId =>
    prim_type(
      [("str", Builtin_types.type_string)],
      Builtin_types.type_number,
    )
  | TagSimpleNumber =>
    prim_type(
      [("num", Builtin_types.type_wasmi32)],
      Builtin_types.type_number,
    )
  | UntagSimpleNumber =>
    prim_type(
      [("num", Builtin_types.type_number)],
      Builtin_types.type_wasmi32,
    )
  | TagChar =>
    prim_type(
      [("char", Builtin_types.type_wasmi32)],
      Builtin_types.type_char,
    )
  | UntagChar =>
    prim_type(
      [("char", Builtin_types.type_char)],
      Builtin_types.type_wasmi32,
    )
  | TagInt8 =>
    prim_type(
      [("int", Builtin_types.type_wasmi32)],
      Builtin_types.type_int8,
    )
  | UntagInt8 =>
    prim_type(
      [("int", Builtin_types.type_int8)],
      Builtin_types.type_wasmi32,
    )
  | TagInt16 =>
    prim_type(
      [("int", Builtin_types.type_wasmi32)],
      Builtin_types.type_int16,
    )
  | UntagInt16 =>
    prim_type(
      [("int", Builtin_types.type_int16)],
      Builtin_types.type_wasmi32,
    )
  | TagUint8 =>
    prim_type(
      [("int", Builtin_types.type_wasmi32)],
      Builtin_types.type_uint8,
    )
  | UntagUint8 =>
    prim_type(
      [("int", Builtin_types.type_uint8)],
      Builtin_types.type_wasmi32,
    )
  | TagUint16 =>
    prim_type(
      [("int", Builtin_types.type_wasmi32)],
      Builtin_types.type_uint16,
    )
  | UntagUint16 =>
    prim_type(
      [("int", Builtin_types.type_uint16)],
      Builtin_types.type_wasmi32,
    )
  | Not =>
    prim_type([("bool", Builtin_types.type_bool)], Builtin_types.type_bool)
  | Box
  | BoxBind => {
      let var = newgenvar(~name="a", ());
      prim_type([("value", var)], Builtin_types.type_box(var));
    }
  | Unbox
  | UnboxBind => {
      let var = newgenvar(~name="a", ());
      prim_type([("value", Builtin_types.type_box(var))], var);
    }
  | Ignore => {
      let var = newgenvar(~name="a", ());
      prim_type([("value", var)], Builtin_types.type_void);
    }
  | ArrayLength => {
      let var = newgenvar(~name="a", ());
      prim_type(
        [("array", Builtin_types.type_array(var))],
        Builtin_types.type_number,
      );
    }
  | Assert =>
    prim_type(
      [("condition", Builtin_types.type_bool)],
      Builtin_types.type_void,
    )
  | Throw =>
    prim_type(
      [("exn", Builtin_types.type_exception)],
      newgenvar(~name="a", ()),
    )
  | Magic =>
    prim_type(
      [("value", newgenvar(~name="a", ()))],
      newgenvar(~name="b", ()),
    )
  | WasmFromGrain =>
    prim_type(
      [("value", newgenvar(~name="a", ()))],
      Builtin_types.type_wasmi32,
    )
  | WasmToGrain =>
    prim_type(
      [("value", Builtin_types.type_wasmi32)],
      newgenvar(~name="a", ()),
    )
  | WasmUnaryI32({arg_type, ret_type})
  | WasmUnaryI64({arg_type, ret_type})
  | WasmUnaryF32({arg_type, ret_type})
  | WasmUnaryF64({arg_type, ret_type}) =>
    prim_type(
      [("num", grain_type_of_wasm_prim_type(arg_type))],
      grain_type_of_wasm_prim_type(ret_type),
    )
  | WasmMemoryGrow =>
    prim_type(
      [("size", Builtin_types.type_wasmi32)],
      Builtin_types.type_wasmi32,
    );

let prim2_type =
  fun
  | NewRational =>
    prim_type(
      [
        ("numerator", Builtin_types.type_wasmi32),
        ("denominator", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_wasmi32,
    )
  | And
  | Or =>
    prim_type(
      [
        ("left", Builtin_types.type_bool),
        ("right", Builtin_types.type_bool),
      ],
      Builtin_types.type_bool,
    )
  | Is
  | Eq => {
      let v = newgenvar(~name="a", ());
      prim_type([("left", v), ("right", v)], Builtin_types.type_bool);
    }
  | WasmBinaryI32({arg_types: (arg1_type, arg2_type), ret_type})
  | WasmBinaryI64({arg_types: (arg1_type, arg2_type), ret_type})
  | WasmBinaryF32({arg_types: (arg1_type, arg2_type), ret_type})
  | WasmBinaryF64({arg_types: (arg1_type, arg2_type), ret_type}) =>
    prim_type(
      [
        ("left", grain_type_of_wasm_prim_type(arg1_type)),
        ("right", grain_type_of_wasm_prim_type(arg2_type)),
      ],
      grain_type_of_wasm_prim_type(ret_type),
    )
  | WasmLoadI32(_) =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_wasmi32,
    )
  | WasmLoadI64(_) =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_wasmi64,
    )
  | WasmLoadF32 =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_wasmf32,
    )
  | WasmLoadF64 =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_wasmf64,
    );

let primn_type =
  fun
  | WasmStoreI32(_) =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("value", Builtin_types.type_wasmi32),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_void,
    )
  | WasmStoreI64(_) =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("value", Builtin_types.type_wasmi64),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_void,
    )
  | WasmStoreF32 =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("value", Builtin_types.type_wasmf32),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_void,
    )
  | WasmStoreF64 =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("value", Builtin_types.type_wasmf64),
        ("offset", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_void,
    )
  | WasmMemoryCopy =>
    prim_type(
      [
        ("source", Builtin_types.type_wasmi32),
        ("destination", Builtin_types.type_wasmi32),
        ("length", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_void,
    )
  | WasmMemoryFill =>
    prim_type(
      [
        ("ptr", Builtin_types.type_wasmi32),
        ("value", Builtin_types.type_wasmi32),
        ("length", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_void,
    )
  | WasmMemoryCompare =>
    prim_type(
      [
        ("ptr1", Builtin_types.type_wasmi32),
        ("ptr2", Builtin_types.type_wasmi32),
        ("length", Builtin_types.type_wasmi32),
      ],
      Builtin_types.type_wasmi32,
    );

let maybe_add_pattern_variables_ghost = (loc_let, env, pv) =>
  List.fold_right(
    ((id, ty, _name, loc, _as_var), env) => {
      let lid = Identifier.IdentName(mkloc(Ident.name(id), loc));
      switch (Env.lookup_value(~mark=false, lid, env)) {
      | _ => env
      | exception Not_found =>
        Env.add_value(
          id,
          {
            val_type: ty,
            val_repr: Type_utils.repr_of_type(env, ty),
            val_internalpath: Path.PIdent(id),
            val_fullpath: Path.PIdent(id),
            val_kind: TValUnbound(ValUnboundGhostRecursive),
            val_loc: loc_let,
            val_mutable: false,
            val_global: false,
          },
          env,
        )
      };
    },
    pv,
    env,
  );

let constant:
  (Location.t, Parsetree.constant) =>
  result(Asttypes.constant, Location.error) = (
  Checkertypes.constant:
    (Location.t, Parsetree.constant) =>
    result(Asttypes.constant, Location.error)
);

let constant_or_raise = Checkertypes.constant_or_raise;

let mkexp = (exp_desc, exp_type, exp_loc, exp_env, exp_attributes) => {
  exp_desc,
  exp_type,
  exp_loc,
  exp_env,
  exp_extra: [],
  exp_attributes,
};

/* Specific version of type_option, using newty rather than newgenty */

let type_option = ty =>
  newty(TTyConstr(Builtin_types.path_option, [ty], ref(TMemNil)));

let option_some = (env, texp) => {
  let csome =
    Env.find_constructor(Path.PIdent(Builtin_types.ident_some_cstr), env);
  mkexp(
    TExpConstruct(
      mknoloc(Identifier.IdentName(mknoloc("Some"))),
      csome,
      TExpConstrTuple([texp]),
    ),
    type_option(texp.exp_type),
    texp.exp_loc,
    texp.exp_env,
    [],
  );
};

let option_none = (env, ty, loc) => {
  let cnone =
    Env.find_constructor(Path.PIdent(Builtin_types.ident_none_cstr), env);
  mkexp(
    TExpConstruct(
      mknoloc(Identifier.IdentName(mknoloc("None"))),
      cnone,
      TExpConstrTuple([]),
    ),
    type_option(ty),
    loc,
    env,
    [],
  );
};

let extract_option_type = (env, ty) => {
  switch (expand_head(env, ty).desc) {
  | TTyConstr(path, [ty], _) when Path.same(path, Builtin_types.path_option) => ty
  | _ => failwith("Impossible: option type was not an option")
  };
};

/* Typing of patterns */

/* unification inside type_pat*/
let unify_pat_types = (loc, env, ty, ty') =>
  try(unify(env, ty, ty')) {
  | Unify(trace) => raise(Error(loc, env, Pattern_type_clash(trace)))
  };

/* unification inside type_exp and type_expect */
let unify_exp_types = (loc, env, ty, expected_ty) =>
  /*Format.eprintf ("Unifying: @[%a@ %a@]@.", Printtyp.raw_type_expr, ty,
    Printtyp.raw_type_expr, expected_ty);*/
  try(unify(env, ty, expected_ty)) {
  | Unify(trace) => raise(Error(loc, env, Expr_type_clash(trace, None)))
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
  | PExpIf(_, e, _)
  | PExpWhile(_, e)
  | PExpMatch(_, {txt: [{pmb_body: e}, ..._]}) => final_subexpression(e)
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
  | TExpLambda(_) => true
  | TExpTuple(es) => List.for_all(is_nonexpansive, es)
  | TExpLet(rec_flag, Immutable, binds) =>
    List.for_all(vb => is_nonexpansive(vb.vb_expr), binds)
  | TExpMatch(e, cases, _) =>
    is_nonexpansive(e)
    && List.for_all(({mb_pat, mb_body}) => is_nonexpansive(mb_body), cases)
  | TExpPrim1(_, e) => is_nonexpansive(e)
  | TExpPrim2(_, e1, e2) => is_nonexpansive(e1) && is_nonexpansive(e2)
  | TExpIf(c, t, f) => is_nonexpansive(t) && is_nonexpansive(f)
  | TExpWhile(c, b) => is_nonexpansive(b)
  | TExpBlock([_, ..._] as es) => is_nonexpansive(last(es))
  | TExpConstruct(_, _, TExpConstrTuple(el)) =>
    List.for_all(is_nonexpansive, el)
  | _ => false
  };

let maybe_expansive = e => !is_nonexpansive(e);

/* Approximate the type of an expression, for better recursion */

let rec approx_type = (env, sty) =>
  switch (sty.ptyp_desc) {
  | PTyArrow(args, ret) =>
    newty(
      TTyArrow(
        List.map(x => (x.ptyp_arg_label, newvar()), args),
        approx_type(env, ret),
        TComOk,
      ),
    )
  | PTyTuple(args) => newty(TTyTuple(List.map(approx_type(env), args)))
  | PTyConstr(id, args) =>
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
  | PExpLet(_, _, _) => Builtin_types.type_void
  | PExpMatch(_, {txt: [{pmb_body: e}, ..._]}) => type_approx(env, e)
  | PExpIf(_, e, _) => type_approx(env, e)
  | PExpWhile(_, e) => type_approx(env, e)
  | PExpLambda(args, e) =>
    newty(
      TTyArrow(
        List.map(x => (x.pla_label, newvar()), args),
        type_approx(env, e),
        TComOk,
      ),
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
    let ty = newgenty(TTyPoly(repr(exp.exp_type), vars'))
    and ty_expected = repr(ty_expected);
    raise(
      Error(
        exp.exp_loc,
        env,
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
    | TPatVar(id, _) => id
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

let rec type_exp = (~in_function=?, ~recarg=?, env, sexp) =>
  /* We now delegate everything to type_expect */
  type_expect(~in_function?, ~recarg?, env, sexp, mk_expected(newvar()))

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
    | Error(loc', env', Expr_type_clash(trace', None))
        when !loc'.Location.loc_ghost =>
      raise(Error(loc', env', Expr_type_clash(trace', Some(explanation))))
    }
  }

and type_expect_ =
    (~in_function=?, ~recarg=Rejected, env, sexp, ty_expected_explained) => {
  let {ty: ty_expected, explanation} = ty_expected_explained;
  let loc = sexp.pexp_loc;
  let core_loc = sexp.pexp_core_loc;
  let attributes = Typetexp.type_attributes(sexp.pexp_attributes);
  /* Record the expression type before unifying it with the expected type */
  let type_expect = type_expect(~in_function?);
  let type_exp = type_exp(~in_function?);
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
            Error(loc, env, Unbound_value_missing_rec(id.txt, desc.val_loc)),
          )
        | _ => TExpIdent(path, id, desc)
        },
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: instance(env, desc.val_type),
      exp_env: env,
    });
  | PExpConstant(cst) =>
    let cst = constant_or_raise(env, loc, cst);
    rue({
      exp_desc: TExpConstant(cst),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
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
      exp_attributes: attributes,
      exp_type: newty(TTyTuple(List.map(e => e.exp_type, expl))),
      exp_env: env,
    });
  | PExpList(es) =>
    let convert_list = (~loc, ~core_loc, ~attributes=?, a) => {
      open Ast_helper;
      let empty =
        Expression.tuple_construct(~loc, ~core_loc, ident_empty, []);
      let list =
        switch (List.rev(a)) {
        | [] => empty
        | [base, ...rest] =>
          let base =
            switch (base) {
            | ListItem(expr) =>
              Expression.tuple_construct(
                ~loc,
                ~core_loc,
                ~attributes?,
                ident_cons,
                [expr, empty],
              )
            | ListSpread(expr, _) => expr
            };
          List.fold_left(
            (acc, expr) => {
              switch (expr) {
              | ListItem(expr) =>
                Expression.tuple_construct(
                  ~loc,
                  ~core_loc,
                  ~attributes?,
                  ident_cons,
                  [expr, acc],
                )
              | ListSpread(_, loc) =>
                raise(
                  SyntaxError(
                    loc,
                    "A list spread can only appear at the end of a list.",
                  ),
                )
              }
            },
            base,
            rest,
          );
        };
      {...list, pexp_loc: loc};
    };
    type_expect(
      env,
      convert_list(~loc, ~core_loc, ~attributes=sexp.pexp_attributes, es),
      ty_expected_explained,
    );
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
      exp_attributes: attributes,
      exp_type: instance(env, ty_expected),
      exp_env: env,
    });
  | PExpArrayGet(sarrexp, sidx) =>
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
      exp_desc: TExpArrayGet(arrexp, idx),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: instance(env, array_type),
      exp_env: env,
    });
  | PExpArraySet({array: sarrexp, index: sidx, value: se, infix_op: None}) =>
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
      exp_desc:
        TExpArraySet({array: arrexp, index: idx, value: e, infix_op: None}),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpArraySet({
      lhs_loc,
      array: sarrexp,
      index: sidx,
      value: se,
      infix_op: Some(infix),
    }) =>
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
    let infix = type_exp(env, infix);
    let ty_fun = expand_head(env, infix.exp_type);
    let (ty_args, ty_ret) =
      switch (ty_fun.desc) {
      | TTyVar(_) =>
        let t_args = [(Unlabeled, newvar()), (Unlabeled, newvar())]
        and t_ret = newvar();
        unify(
          env,
          ty_fun,
          newty(TTyArrow(t_args, t_ret, TComLink(ref(TComUnknown)))),
        );
        (t_args, t_ret);
      | TTyArrow(t_args, t_ret, _) => (t_args, t_ret)
      | _ =>
        raise(
          Error(
            infix.exp_loc,
            env,
            Apply_non_function(expand_head(env, infix.exp_type)),
          ),
        )
      };
    let (ty_arg1, ty_arg2) =
      switch (ty_args) {
      | [(_, arg1), (_, arg2)] => (arg1, arg2)
      | _ =>
        raise(
          Error(
            infix.exp_loc,
            env,
            Arity_mismatch(expand_head(env, infix.exp_type), None),
          ),
        )
      };

    unify_exp_types(lhs_loc, env, array_type, ty_arg1);
    let e = type_expect(env, se, mk_expected(ty_arg2));
    let assignment_loc = {...infix.exp_loc, loc_end: se.pexp_loc.loc_end};
    unify_exp_types(assignment_loc, env, ty_ret, array_type);
    rue({
      exp_desc:
        TExpArraySet({
          array: arrexp,
          index: idx,
          value: e,
          infix_op: Some(infix),
        }),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpRecord(b, es) =>
    let opt_exp = Option.map(type_exp(env), b);

    let (ty_record, opath) = {
      let get_path = ty =>
        try({
          let (p0, p, _) = extract_concrete_record(env, ty);
          let principal = repr(ty).level == generic_level;

          Some((p0, p, principal));
        }) {
        | Not_found => None
        };

      let expected_opath = get_path(ty_expected);
      let opt_exp_opath = Option.bind(opt_exp, exp => get_path(exp.exp_type));
      switch (expected_opath, opt_exp_opath) {
      | (None, None) => (newvar(), None)
      | (Some(_), None)
      | (Some((_, _, true)), Some(_)) => (ty_expected, expected_opath)
      | (None | Some((_, _, false)), Some((_, p', _))) =>
        let decl = Env.find_type(p', env);
        begin_def();
        let ty = newconstr(p', instance_list(env, decl.type_params));
        end_def();
        generalize_structure(ty);
        (ty, opt_exp_opath);
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
        raise(Error(loc, env, Label_multiply_defined(lbl1.lbl_name)))
      | [_, ...rem] => check_duplicates(rem)
      | [] => ()
    );

    check_duplicates(lbl_exp_list);
    let (opt_exp, label_definitions) = {
      let (_lid, lbl, _lbl_exp) = List.hd(lbl_exp_list);
      let matching_label = lbl =>
        List.find(
          ((_, lbl', _)) => lbl'.lbl_pos == lbl.lbl_pos,
          lbl_exp_list,
        );

      switch (opt_exp) {
      | None =>
        let label_definitions =
          Array.map(
            lbl =>
              switch (matching_label(lbl)) {
              | (lid, _lbl, lbl_exp) => Overridden(lid, lbl_exp)
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
                raise(Error(loc, env, Label_missing(missing)));
              },
            lbl.lbl_all,
          );
        (None, label_definitions);
      | Some(exp) =>
        let ty_exp = instance(env, exp.exp_type);
        let unify_kept = lbl => {
          let (_, ty_arg1, ty_res1) = instance_label(false, lbl);
          unify_exp_types(exp.exp_loc, env, ty_exp, ty_res1);
          switch (matching_label(lbl)) {
          | (lid, _lbl, lbl_exp) =>
            // do not connect result types for overridden labels
            Overridden(lid, lbl_exp)
          | exception Not_found =>
            let (_, ty_arg2, ty_res2) = instance_label(false, lbl);
            unify_exp_types(loc, env, ty_arg1, ty_arg2);
            with_explanation(() =>
              unify_exp_types(loc, env, instance(env, ty_expected), ty_res2)
            );
            Kept;
          };
        };
        let label_definitions = Array.map(unify_kept, lbl.lbl_all);
        (Some({...exp, exp_type: ty_exp}), label_definitions);
      };
    };
    let num_fields =
      switch (lbl_exp_list) {
      | [] => assert(false)
      | [(_, lbl, _), ..._] => Array.length(lbl.lbl_all)
      };
    if (b != None && List.length(es) == num_fields) {
      Location.prerr_warning(loc, Grain_utils.Warnings.UselessRecordSpread);
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
      exp_desc: TExpRecord(opt_exp, fields),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: instance(env, ty_expected),
      exp_env: env,
    });
  | PExpRecordGet(srecord, lid) =>
    let (record, label, _) = type_label_access(env, srecord, lid);
    let (_, ty_arg, ty_res) = instance_label(false, label);
    unify_exp(env, record, ty_res);
    rue({
      exp_desc: TExpRecordGet(record, lid, label),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: ty_arg,
      exp_env: env,
    });
  | PExpRecordSet(srecord, lid, sval) =>
    let (record, label, _) = type_label_access(env, srecord, lid);
    if (!label.lbl_mut) {
      raise(Error(loc, env, Label_not_mutable(lid.txt)));
    };
    let (_, ty_arg, ty_res) = instance_label(false, label);
    unify_exp(env, record, ty_res);
    let val_ = type_expect(env, sval, mk_expected(newvar()));
    unify_exp(env, val_, ty_arg);
    rue({
      exp_desc: TExpRecordSet(record, lid, label, val_),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpLet(rec_flag, mut_flag, pats) =>
    let scp = None;
    let (pat_exp_list, new_env, unpacks) =
      type_let(
        ~in_function?,
        env,
        rec_flag,
        mut_flag,
        false,
        pats,
        scp,
        true,
      );
    /*let () =
      if rec_flag = Recursive then
        check_recursive_bindings env pat_exp_list
      in*/
    rue({
      exp_desc: TExpLet(rec_flag, mut_flag, pat_exp_list),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: Builtin_types.type_void,
      exp_env: new_env,
    });
  | PExpLambda(args, body) =>
    open Ast_helper;
    let gen_opt = label => {
      let label =
        switch (label) {
        | Unlabeled => failwith("Impossible: default argument with no label")
        | Labeled({txt: name})
        | Default({txt: name}) => name
        };
      "$default_option_" ++ label;
    };
    let (args, labels, prelude) =
      List.fold_right(
        (arg, (args, labels, prelude)) => {
          switch (arg.pla_default) {
          | Some(default) =>
            let default_value_name = mknoloc("$default_value");
            let default_loc = default.pexp_loc;
            let default_core_loc = default.pexp_core_loc;
            let scases = [
              MatchBranch.mk(
                ~loc=default_loc,
                Pattern.construct(
                  ~loc=default_loc,
                  mknoloc(Identifier.IdentName(mknoloc("Some"))),
                  PPatConstrTuple([
                    Pattern.var(~loc=default_loc, default_value_name),
                  ]),
                ),
                Expression.ident(
                  ~loc=default_loc,
                  ~core_loc=default_core_loc,
                  mknoloc(Identifier.IdentName(default_value_name)),
                ),
                None,
              ),
              MatchBranch.mk(
                ~loc=default_loc,
                Pattern.construct(
                  ~loc=default_loc,
                  mknoloc(Identifier.IdentName(mknoloc("None"))),
                  PPatConstrTuple([]),
                ),
                default,
                None,
              ),
            ];
            let sloc = {
              Location.loc_start: arg.pla_pattern.ppat_loc.Location.loc_start,
              loc_end: default_loc.Location.loc_end,
              loc_ghost: true,
            };
            let opt_name = mknoloc(gen_opt(arg.pla_label));
            let smatch =
              Expression.match(
                ~loc=sloc,
                ~core_loc=sloc,
                Expression.ident(
                  ~loc=sloc,
                  ~core_loc=sloc,
                  mknoloc(Identifier.IdentName(opt_name)),
                ),
                mknoloc(scases),
              );
            let pat = Pattern.var(~loc=sloc, opt_name);
            let prelude_expr =
              Expression.let_(
                ~loc=sloc,
                ~core_loc=sloc,
                Nonrecursive,
                Immutable,
                [ValueBinding.mk(~loc=arg.pla_loc, arg.pla_pattern, smatch)],
              );
            (
              [pat, ...args],
              [arg.pla_label, ...labels],
              [prelude_expr, ...prelude],
            );
          | None => (
              [arg.pla_pattern, ...args],
              [arg.pla_label, ...labels],
              prelude,
            )
          }
        },
        args,
        ([], [], []),
      );
    let pat =
      switch (args) {
      | [] =>
        Pattern.tuple_construct(
          ~loc=Location.dummy_loc,
          Location.mknoloc(Identifier.IdentName(Location.mknoloc("()"))),
          [],
        )
      | args => Pattern.tuple(~loc=Location.dummy_loc, args)
      };
    let body =
      switch (prelude) {
      | [] => body
      | _ =>
        Expression.block(
          ~loc=body.pexp_loc,
          ~core_loc=body.pexp_core_loc,
          prelude @ [body],
        )
      };
    type_function(
      ~in_function?,
      loc,
      attributes,
      env,
      ty_expected_explained,
      labels,
      [MatchBranch.mk(~loc, pat, body, None)],
    );
  | PExpApp(func, args) =>
    begin_def(); /* one more level for non-returning functions */
    let funct = type_exp(env, func);
    // TODO: Determine what this does
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
    let (label_order, args, ty_res) =
      type_application(~in_function?, ~loc, env, funct, args);
    end_def();
    unify_var(env, newvar(), funct.exp_type);
    rue({
      exp_desc: TExpApp(funct, label_order, args),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: ty_res,
      exp_env: env,
    });
  | PExpConstruct(cstr, arg) =>
    type_construct(
      env,
      loc,
      cstr,
      arg,
      ty_expected_explained,
      sexp.pexp_attributes,
    )
  | PExpMatch(arg, branches) =>
    begin_def();
    let arg = type_exp(env, arg);
    end_def();
    if (!is_nonexpansive(arg)) {
      generalize_expansive(env, arg.exp_type);
    };
    generalize(arg.exp_type);
    let (val_cases, partial) =
      type_cases(
        ~in_function?,
        env,
        arg.exp_type,
        ty_expected,
        true,
        loc,
        branches.txt,
      );
    re({
      exp_desc: TExpMatch(arg, val_cases, partial),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: instance(env, ty_expected),
      exp_env: env,
    });
  | PExpPrim0(p0) =>
    let rettype = prim0_type(p0);
    rue({
      exp_desc: TExpPrim0(p0),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: rettype,
      exp_env: env,
    });
  | PExpPrim1(p1, sarg) =>
    let (argtypes, rettype) =
      filter_arrow(env, instance(env, prim1_type(p1)), [Unlabeled]);
    let argtype =
      switch (argtypes) {
      | [arg] => arg
      | _ => failwith("Impossible: invalid prim1 type arity")
      };
    let arg = type_expect(env, sarg, mk_expected(argtype));
    rue({
      exp_desc: TExpPrim1(p1, arg),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: rettype,
      exp_env: env,
    });
  | PExpPrim2(p2, sarg1, sarg2) =>
    let (argtypes, rettype) =
      filter_arrow(
        env,
        instance(env, prim2_type(p2)),
        [Unlabeled, Unlabeled],
      );
    let (arg1type, arg2type) =
      switch (argtypes) {
      | [arg1, arg2] => (arg1, arg2)
      | _ => failwith("Impossible: invalid prim2 type arity")
      };
    let arg1 = type_expect(env, sarg1, mk_expected(arg1type));
    let arg2 = type_expect(env, sarg2, mk_expected(arg2type));
    rue({
      exp_desc: TExpPrim2(p2, arg1, arg2),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: rettype,
      exp_env: env,
    });
  | PExpPrimN(p, sargs) =>
    let (argtypes, rettype) =
      filter_arrow(
        env,
        instance(env, primn_type(p)),
        List.map(_ => Unlabeled, sargs),
      );
    let args =
      List.map2(
        (sarg, argtype) => type_expect(env, sarg, mk_expected(argtype)),
        sargs,
        argtypes,
      );
    rue({
      exp_desc: TExpPrimN(p, args),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: rettype,
      exp_env: env,
    });
  | PExpBoxAssign(sboxexpr, sval) =>
    let boxexpr =
      type_expect(
        env,
        sboxexpr,
        mk_expected(~explanation=Assign_not_box) @@
        Builtin_types.type_box @@
        newvar(~name="a", ()),
      );
    let val_ = type_expect(env, sval, mk_expected(newvar()));
    unify_exp(env, boxexpr) @@ Builtin_types.type_box(val_.exp_type);
    rue({
      exp_desc: TExpBoxAssign(boxexpr, val_),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpAssign(sidexpr, sval) =>
    let idexpr = type_expect(env, sidexpr, mk_expected(newvar()));
    let (id, {val_mutable}) =
      switch (idexpr.exp_desc) {
      | TExpIdent(_, id, vd) => (id, vd)
      | _ =>
        failwith("lhs of assign was not identifier; impossible by parsing")
      };
    if (!val_mutable) {
      raise(Error(loc, env, Assign_not_mutable(id.txt)));
    };
    let val_ = type_expect(env, sval, mk_expected(newvar()));
    unify_exp(env, val_, idexpr.exp_type);
    rue({
      exp_desc: TExpAssign(idexpr, val_),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpIf(scond, sifso, sifnot) =>
    let cond =
      type_expect(
        env,
        scond,
        mk_expected(~explanation=If_conditional, Builtin_types.type_bool),
      );

    let (ifso, ifnot) =
      switch (sifnot) {
      | None =>
        let void_exp = {
          exp_desc: TExpConstant(Const_void),
          exp_loc: loc,
          exp_extra: [],
          exp_attributes: [],
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
      | Some(sifnot) =>
        let ifso = type_expect(env, sifso, ty_expected_explained);
        let ifnot = type_expect(env, sifnot, ty_expected_explained);
        /* Both types should match */
        unify_exp(env, ifnot, ifso.exp_type);
        (ifso, ifnot);
      };

    rue({
      exp_desc: TExpIf(cond, ifso, ifnot),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: ifso.exp_type,
      exp_env: env,
    });
  | PExpWhile(scond, sbody) =>
    let cond =
      type_expect(
        env,
        scond,
        mk_expected(~explanation=Loop_conditional, Builtin_types.type_bool),
      );
    let body =
      type_expect(
        env,
        sbody,
        mk_expected(~explanation=Loop_body, Builtin_types.type_void),
      );
    rue({
      exp_desc: TExpWhile(cond, body),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      /* While loops don't evaluate to anything */
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpFor(sinit, scond, sinc, sbody) =>
    let init =
      Option.map(
        sinit => {
          type_expect(env, sinit, mk_expected(Builtin_types.type_void))
        },
        sinit,
      );
    let body_env =
      Option.value(~default=env, Option.map(init => init.exp_env, init));
    let cond =
      Option.map(
        scond => {
          type_expect(
            body_env,
            scond,
            mk_expected(
              ~explanation=Loop_conditional,
              Builtin_types.type_bool,
            ),
          )
        },
        scond,
      );
    let inc =
      Option.map(
        sinc => {type_expect(body_env, sinc, mk_expected(newvar()))},
        sinc,
      );
    let body =
      type_expect(
        body_env,
        sbody,
        mk_expected(~explanation=Loop_body, Builtin_types.type_void),
      );
    rue({
      exp_desc: TExpFor(init, cond, inc, body),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      /* For loops don't evaluate to anything */
      exp_type: Builtin_types.type_void,
      exp_env: env,
    });
  | PExpContinue =>
    rue({
      exp_desc: TExpContinue,
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      /* Doesn't evaluate to anything */
      exp_type: Builtin_types.type_void,
      exp_env: env,
    })
  | PExpBreak =>
    rue({
      exp_desc: TExpBreak,
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      /* Doesn't evaluate to anything */
      exp_type: Builtin_types.type_void,
      exp_env: env,
    })
  | PExpReturn(sarg) =>
    switch (in_function) {
    | Some((_, _, ret_type)) =>
      let arg =
        switch (sarg) {
        | Some(sarg) => Some(type_expect(env, sarg, mk_expected(ret_type)))
        | None =>
          with_explanation(() =>
            unify_exp_types(
              loc,
              env,
              instance(env, Builtin_types.type_void),
              ret_type,
            )
          );
          None;
        };
      rue({
        exp_desc: TExpReturn(arg),
        exp_loc: loc,
        exp_extra: [],
        exp_attributes: attributes,
        exp_type: newvar(),
        exp_env: env,
      });
    | None => failwith("Impossible: return outside of function")
    }
  | PExpConstraint(sarg, styp) =>
    begin_def();
    let cty = Typetexp.transl_simple_type(env, false, styp);
    let ty = cty.ctyp_type;
    end_def();
    generalize_structure(ty);
    let (arg, ty') = (
      type_argument(env, sarg, ty, instance(env, ty)),
      instance(env, ty),
    );
    rue({
      exp_desc: arg.exp_desc,
      exp_loc: arg.exp_loc,
      exp_type: ty',
      exp_env: env,
      exp_extra: [(TExpConstraint(cty), loc), ...arg.exp_extra],
      exp_attributes: attributes,
    });
  | PExpBlock([]) =>
    rue({
      exp_desc: TExpBlock([]),
      exp_loc: loc,
      exp_type: Builtin_types.type_void,
      exp_attributes: attributes,
      exp_env: env,
      exp_extra: [],
    })
  | PExpBlock(es) =>
    let rec process_es = (env, rem) =>
      switch (rem) {
      | [] => failwith("Impossible: empty case in process_es")
      | [e] =>
        let expr = type_expect(env, e, ty_expected_explained);
        ([expr], expr.exp_type);
      | [e, ...es] =>
        let expr =
          type_statement_expr(
            ~explanation=Sequence_left_hand_side,
            ~in_function?,
            env,
            e,
          );
        let (exprs, typ) = process_es(expr.exp_env, es);
        ([expr, ...exprs], typ);
      };

    let (exprs, typ) = process_es(env, es);
    re({
      exp_desc: TExpBlock(exprs),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: typ,
      exp_env: env,
    });
  | PExpUse(module_, items) =>
    let path = Typetexp.lookup_module(env, module_.loc, module_.txt, None);
    let (newenv, items) =
      switch (items) {
      | PUseAll => (Env.use_full_signature(path, env), TUseAll)
      | PUseItems(items) =>
        let (env, items) = Env.use_partial_signature(path, items, env);
        (env, TUseItems(items));
      };
    rue({
      exp_desc: TExpUse(Location.mkloc(path, module_.loc), items),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attributes,
      exp_type: instance(env, Builtin_types.type_void),
      exp_env: newenv,
    });
  };
}

and type_function =
    (~in_function=?, loc, attrs, env, ty_expected_explained, l, caselist) => {
  let {ty: ty_expected, explanation} = ty_expected_explained;
  let (loc_fun, ty_fun) = (loc, instance(env, ty_expected));

  let separate = Env.has_local_constraints(env);
  if (separate) {
    begin_def();
  };
  let exp_inst = instance(env, ty_expected);
  let (ty_args, ty_res) =
    try(filter_arrow(env, exp_inst, l)) {
    | Filter_arrow_failed(err) =>
      let err =
        switch (err) {
        | Unification_error(unif_err) => Expr_type_clash(unif_err, None)
        | Label_mismatch({got, expected, expected_type}) =>
          Function_label_mismatch({got, expected, expected_type, explanation})
        | Arity_mismatch => Arity_mismatch(ty_fun, explanation)
        | Not_a_function => Not_a_function(ty_fun, explanation)
        };
      raise(Error(loc_fun, env, err));
    };
  if (separate) {
    end_def();
    List.iter(generalize_structure, ty_args);
    generalize_structure(ty_res);
  };
  let normalized_arg_type =
    switch (ty_args) {
    | [] => Builtin_types.type_void
    | _ => newty(TTyTuple(ty_args))
    };
  let (cases, partial) =
    type_cases(
      ~in_function=(loc_fun, ty_args, ty_res),
      env,
      normalized_arg_type,
      ty_res,
      true,
      loc,
      caselist,
    );
  re({
    exp_desc: TExpLambda(cases, partial),
    exp_loc: loc,
    exp_extra: [],
    exp_attributes: attrs,
    exp_type:
      instance(
        env,
        newgenty(TTyArrow(List.combine(l, ty_args), ty_res, TComOk)),
      ),
    exp_env: env,
  });
}

and type_argument =
    (~in_function=?, ~recarg=?, env, sarg, ty_expected', ty_expected) => {
  /* ty_expected' may be generic */
  let texp =
    type_expect(
      ~in_function?,
      ~recarg?,
      env,
      sarg,
      mk_expected(ty_expected'),
    );
  unify_exp(env, texp, ty_expected);
  texp;
}

and type_application = (~in_function=?, ~loc, env, funct, sargs) => {
  /* funct.exp_type may be generic */
  let ty_fun = expand_head(env, funct.exp_type);
  let (ty_args, ty_ret) =
    switch (ty_fun.desc) {
    | TTyVar(_) =>
      let t_args = List.map(arg => (arg.paa_label, newvar()), sargs)
      and t_ret = newvar();
      unify(
        env,
        ty_fun,
        newty(TTyArrow(t_args, t_ret, TComLink(ref(TComUnknown)))),
      );
      (t_args, t_ret);
    | TTyArrow(t_args, t_ret, _) => (t_args, t_ret)
    | _ =>
      raise(
        Error(
          funct.exp_loc,
          env,
          Apply_non_function(expand_head(env, funct.exp_type)),
        ),
      )
    };

  let ordered_labels = List.map(fst, ty_args);

  let (labeled_sargs, unlabeled_sargs) =
    List.partition(
      sarg => {
        switch (sarg.paa_label) {
        | Labeled(_) => true
        | _ => false
        }
      },
      sargs,
    );

  let (used_labeled_tyargs, unused_tyargs) =
    List.partition(
      ((l, _)) => {
        List.exists(
          sarg => same_label_name(l, sarg.paa_label),
          labeled_sargs,
        )
      },
      ty_args,
    );

  let rec type_args =
          (
            args,
            remaining_sargs,
            remaining_used_labeled_tyargs,
            remaining_unused_tyargs,
          ) => {
    let rec extract_label = (l, tyargs) => {
      switch (tyargs) {
      | [] => (None, [])
      | [(tyl, _) as tyarg, ...rest_tyargs] when same_label_name(tyl, l) => (
          Some(tyarg),
          rest_tyargs,
        )
      | [tyarg, ...rest_tyargs] =>
        let (res, rest_tyargs) = extract_label(l, rest_tyargs);
        (res, [tyarg, ...rest_tyargs]);
      };
    };
    let rec next_tyarg = tyargs => {
      switch (tyargs) {
      | [] => (None, [])
      | [(tyl, _) as tyarg, ...rest_tyargs] when !is_optional(tyl) => (
          Some(tyarg),
          rest_tyargs,
        )
      | [tyarg, ...rest_tyargs] =>
        let (res, rest_tyargs) = next_tyarg(rest_tyargs);
        (res, [tyarg, ...rest_tyargs]);
      };
    };
    switch (remaining_sargs) {
    | [] => (args, remaining_unused_tyargs)
    | [sarg, ...remaining_sargs] =>
      let (
        corresponding_tyarg,
        remaining_used_labeled_tyargs,
        remaining_unused_tyargs,
      ) =
        switch (sarg.paa_label) {
        | Default(_) =>
          failwith("Impossible: optional argument in application")
        | Labeled(_) =>
          let (corresponding_tyarg, remaining_used_labeled_tyargs) =
            extract_label(sarg.paa_label, remaining_used_labeled_tyargs);
          (
            corresponding_tyarg,
            remaining_used_labeled_tyargs,
            remaining_unused_tyargs,
          );
        | Unlabeled =>
          let (corresponding_tyarg, remaining_unused_tyargs) =
            next_tyarg(remaining_unused_tyargs);
          (
            corresponding_tyarg,
            remaining_used_labeled_tyargs,
            remaining_unused_tyargs,
          );
        };
      switch (corresponding_tyarg) {
      | Some((l, ty)) =>
        let arg =
          if (!is_optional(l)) {
            (
              () =>
                type_argument(
                  ~in_function?,
                  env,
                  sarg.paa_expr,
                  ty,
                  instance(env, ty),
                )
            );
          } else {
            (
              () =>
                option_some(
                  env,
                  type_argument(
                    ~in_function?,
                    env,
                    sarg.paa_expr,
                    extract_option_type(env, ty),
                    extract_option_type(env, instance(env, ty)),
                  ),
                )
            );
          };
        type_args(
          [(l, arg), ...args],
          remaining_sargs,
          remaining_used_labeled_tyargs,
          remaining_unused_tyargs,
        );
      | None =>
        switch (sarg.paa_label) {
        | Unlabeled =>
          raise(
            Error(
              loc,
              env,
              Apply_too_many_arguments(
                expand_head(env, funct.exp_type),
                unused_tyargs,
              ),
            ),
          )
        | _ =>
          raise(
            Error(
              sarg.paa_loc,
              env,
              Apply_unknown_label(
                label_name(sarg.paa_label),
                List.filter_map(
                  l => {
                    switch (l) {
                    | Unlabeled => None
                    | _ => Some(label_name(l))
                    }
                  },
                  ordered_labels,
                ),
              ),
            ),
          )
        }
      };
    };
  };

  let (args, remaining_tyargs) =
    type_args([], sargs, used_labeled_tyargs, unused_tyargs);

  let omitted_args =
    List.map(
      ((l, ty)) => {
        switch (l) {
        | Default(_) =>
          // omitted optional argument
          (l, option_none(env, instance(env, ty), Location.dummy_loc))
        | _ =>
          let missing_args =
            List.filter(((l, _)) => !is_optional(l), remaining_tyargs);
          raise(Error(loc, env, Apply_too_few_arguments(missing_args)));
        }
      },
      remaining_tyargs,
    );

  // Typecheck all arguments.
  // Order here is important; rev_map would be incorrect.
  let typed_args = List.map(((l, argf)) => (l, argf()), List.rev(args));

  (ordered_labels, omitted_args @ typed_args, instance(env, ty_ret));
}

and type_construct = (env, loc, lid, sarg, ty_expected_explained, attrs) => {
  let {ty: ty_expected, explanation} = ty_expected_explained;
  let (sargs, is_record_cstr) =
    switch (sarg) {
    | PExpConstrSingleton => ([], false)
    | PExpConstrTuple(sargs) => (sargs, false)
    | PExpConstrRecord(rfs) => (
        [
          {
            pexp_desc: PExpRecord(None, rfs),
            pexp_attributes: attrs,
            pexp_loc: loc,
            pexp_core_loc: loc,
          },
        ],
        true,
      )
    };
  let attrs = Typetexp.type_attributes(attrs);
  let opath =
    try({
      let (p0, p, _) = extract_concrete_variant(env, ty_expected);
      Some((p0, p, true));
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
  let is_record_cstr_def = constr.cstr_inlined != None;
  if (is_record_cstr_def != is_record_cstr) {
    raise(
      Error(
        loc,
        env,
        Inlined_record_misuse(
          lid.txt,
          if (is_record_cstr_def) {"record"} else {"tuple"},
          if (is_record_cstr) {"record"} else {"tuple"},
        ),
      ),
    );
  };
  if (List.length(sargs) != constr.cstr_arity) {
    raise(
      Error(
        loc,
        env,
        Constructor_arity_mismatch(
          lid.txt,
          constr.cstr_arity,
          List.length(sargs),
        ),
      ),
    );
  };
  let separate = Env.has_local_constraints(env);
  if (separate) {
    begin_def();
    begin_def();
  };
  let (ty_args, ty_res) = instance_constructor(constr);
  let texp =
    re({
      exp_desc: TExpConstruct(lid, constr, TExpConstrTuple([])),
      exp_loc: loc,
      exp_extra: [],
      exp_attributes: attrs,
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

  let args =
    List.map2(
      (sarg, (ty_arg, ty_arg0)) =>
        type_argument(~recarg, env, sarg, ty_arg, ty_arg0),
      sargs,
      List.combine(ty_args, ty_args0),
    );
  let arg =
    if (is_record_cstr) {
      switch (args) {
      | [{exp_desc: TExpRecord(_, rfs)}] => TExpConstrRecord(rfs)
      | _ =>
        failwith("Impossible: record constructor inner expression not record")
      };
    } else {
      TExpConstrTuple(args);
    };
  /* NOTE: shouldn't we call "re" on this final expression? -- AF */
  {...texp, exp_desc: TExpConstruct(lid, constr, arg)};
}

/* Typing of statements (expressions whose values are discarded) */

and type_statement_expr = (~explanation=?, ~in_function=?, env, sexp) => {
  let loc = final_subexpression(sexp).pexp_loc;
  begin_def();
  let exp = type_exp(~in_function?, env, sexp);
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
    | TTyConstr(p, _, _) when Path.same(p, Builtin_types.path_void) => ()
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
    repr(ty_arg).level == generic_level
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
        let scope = None /*Some (Annot.Idef loc)*/;
        let (pat, ext_env, force, unpacks) = {
          let partial = None;
          let ty_arg = instance(~partial?, env, ty_arg);
          Typepat.type_pattern(~lev, env, pmb_pat, scope, ty_arg);
        };

        pattern_force := force @ pattern_force^;

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
  let cases =
    List.map2(
      ((pat, (ext_env, unpacks)), {pmb_pat, pmb_body, pmb_guard, pmb_loc}) => {
        let sexp = pmb_body /*wrap_unpacks pmb_body unpacks*/;

        /*Format.eprintf "@[%i %i, ty_res' =@ %a@]@." lev (get_current_level())
          Printtyp.raw_type_expr ty_res';*/
        let guard =
          switch (pmb_guard) {
          | None => None
          | Some(scond) =>
            Some(
              type_expect(
                ~in_function?,
                ext_env,
                scond,
                mk_expected(Builtin_types.type_bool),
              ),
            )
          };
        let exp =
          type_expect(~in_function?, ext_env, sexp, mk_expected(ty_res));
        {
          mb_pat: pat,
          mb_body: {
            ...exp,
            exp_type: instance(env, ty_res),
          },
          mb_guard: guard,
          mb_loc: pmb_loc,
        };
      },
      pat_env_list,
      caselist,
    );

  let do_init = /*has_gadts ||*/ needs_exhaust_check;
  let (lev, env) =
    if (do_init) {
      /*&& not has_gadts*/ init_env();
    } else {
      (lev, env);
    };
  let ty_arg_check =
    if (do_init) {
      /* Hack: use for_cmi to copy variables too */
      Subst.type_expr(
        Subst.for_cmi(Subst.identity),
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
      ~in_function=?,
      env,
      rec_flag,
      mut_flag,
      global_flag,
      spat_sexp_list,
      scope,
      allow,
    ) => {
  open Ast_helper;
  begin_def();
  /*let is_fake_let =
      match spat_sexp_list with
      | [{pvb_expr={pexp_desc=PExpMatch(
             {pexp_desc=PExpId({ txt = Identifier.IdentName "*opt*"})},_)}}] ->
          true (* the fake let-declaration introduced by fun ?(x = e) -> ... *)
      | _ ->
          false
    in*/
  /*let check = if is_fake_let then check_strict else check in*/
  let spatl = List.map(({pvb_pat: spat}) => ([], spat), spat_sexp_list);
  let nvs = List.map(_ => newvar(), spatl);
  let mut = mut_flag == Mutable;
  let (pat_list, new_env, force, unpacks, pv) =
    type_pattern_list(
      ~mut,
      ~global=global_flag,
      env,
      spatl,
      scope,
      nvs,
      allow,
    );
  let attrs_list = List.map(fst, spatl);
  let is_recursive = rec_flag == Recursive;
  /* If recursive, first unify with an approximation of the expression */
  if (is_recursive) {
    List.iter2(
      (pat, binding) => {
        let pat =
          switch (pat.pat_type.desc) {
          | TTyPoly(ty, tl) => {
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
        | TTyPoly(ty, tl) =>
          /*Printf.eprintf "type_let: TTyPoly\n";*/
          begin_def();
          let (vars, ty') = instance_poly(~keep_names=true, true, tl, ty);
          let exp =
            /*Builtin_attributes.warning_scope pvb_attributes
              (fun () -> type_expect exp_env sexp (mk_expected ty'))*/
            type_expect(~in_function?, exp_env, sexp, mk_expected(ty'));

          end_def();
          check_univars(env, true, "definition", exp, pat.pat_type, vars);
          {...exp, exp_type: instance(env, exp.exp_type)};
        | _ =>
          /*Printf.eprintf "type_let: non-TTyPoly\n";
            Format.eprintf "@[type_let: expected: %a@]@."
              Printtyp.raw_type_expr pat.pat_type;*/
          /*Builtin_attributes.warning_scope pvb_attributes (fun () ->
            type_expect exp_env sexp (mk_expected pat.pat_type))*/
          type_expect(
            ~in_function?,
            exp_env,
            sexp,
            mk_expected(pat.pat_type),
          )
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
    end;*/
  List.iter2(
    (pat, (attrs, exp)) => {
      ignore(
        check_partial(
          env,
          pat.pat_type,
          pat.pat_loc,
          [{mb_pat: pat, mb_body: exp, mb_guard: None, mb_loc: pat.pat_loc}],
        ),
      )
    },
    pat_list,
    List.map2(((attrs, _), e) => (attrs, e), spatl, exp_list),
  );
  end_def();
  let mutable_let = mut_flag == Mutable;
  List.iter2(
    (pat, exp) =>
      // All mutable bindings should be treated as expansive
      if (mutable_let || !is_nonexpansive(exp)) {
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
        | TPatAlias({pat_desc: TPatAny}, _, _) => ()
        | _ => raise(Error(pat.pat_loc, env, Illegal_letrec_pat))
        },
      l,
    );
  };
  (l, new_env, unpacks);
}

and type_label_access = (env, srecord, lid) => {
  let record = type_exp(~recarg=Allowed, env, srecord);
  let ty_exp = record.exp_type;
  let opath =
    try({
      let (p0, p, _) = extract_concrete_record(env, ty_exp);
      Some((p0, p, true));
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
  let separate = Env.has_local_constraints(env);
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
    raise(Error(lid.loc, env, Label_mismatch(lid.txt, trace)))
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
    let arg = type_argument(env, sarg, ty_arg, instance(env, ty_arg));
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
      | Error(_, _, Less_general(_)) as e => raise(e)
      | _ => raise(exn)
      }
    };
  }; /* In case of failure return the first error */

  (lid, label, {...arg, exp_type: instance(env, arg.exp_type)});
};

let check_recursive_bindings = (env, vbs) =>
  // TODO: Implement
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
      true,
      spat_sexp_list,
      scope,
      false,
    );

  (pat_exp_list, new_env);
};
let type_let = (env, rec_flag, mut_flag, global_flag, spat_sexp_list, scope) => {
  let (pat_exp_list, new_env, _unpacks) =
    type_let(
      env,
      rec_flag,
      mut_flag,
      global_flag,
      spat_sexp_list,
      scope,
      false,
    );
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
  | Loop_conditional => fprintf(ppf, "the condition of a loop")
  | Loop_body => fprintf(ppf, "the body of a loop")
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
  | Constructor_arity_mismatch(lid, expected, provided) =>
    fprintf(
      ppf,
      "@[The constructor %a@ expects %i argument(s),@ but is called with %i argument(s)@]",
      identifier,
      lid,
      expected,
      provided,
    )
  | Label_mismatch(lid, trace) =>
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
  | Or_pattern_type_clash(id, trace) =>
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
  | Orpat_vars(id, valid_idents) => {
      fprintf(
        ppf,
        "Variable %s must occur on both sides of this | pattern",
        Ident.name(id),
      );
      spellcheck_idents(ppf, id, valid_idents);
    }
  | Expr_type_clash(trace, explanation) =>
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
      fprintf(
        ppf,
        "@[<v>@[<2>This expression has type@ %a@]@ %s@]",
        type_expr,
        typ,
        "This is not a function; it cannot be called.",
      );
    }
  | Apply_too_many_arguments(typ, unused_tyargs) => {
      reset_and_mark_loops(typ);
      fprintf(ppf, "@[<v>@[<2>This function has type@ %a@]", type_expr, typ);
      fprintf(ppf, "@ @[It is called with too many arguments.@]@]");
      let unused_optional_arguments =
        List.filter_map(
          ((l, _)) =>
            switch (l) {
            | Default({txt: name}) => Some(name)
            | _ => None
            },
          unused_tyargs,
        );
      let oxford = (ppf, args) => {
        let rec oxford = (ppf, args) =>
          switch (args) {
          | [] => ()
          | [arg1, arg2] => fprintf(ppf, "%s, or %s", arg1, arg2)
          | [arg1, ...args] =>
            fprintf(ppf, "%s, ", arg1);
            oxford(ppf, args);
          };

        switch (args) {
        | [] => ()
        | [arg] => fprintf(ppf, "%s", arg)
        | [arg1, arg2] => fprintf(ppf, "%s or %s", arg1, arg2)
        | _ => oxford(ppf, args)
        };
      };
      switch (unused_optional_arguments) {
      | [] => ()
      | labels =>
        fprintf(
          ppf,
          "@ @[Did you mean to supply an argument with label %a?@]@]",
          oxford,
          unused_optional_arguments,
        )
      };
    }
  | Apply_too_few_arguments(args) => {
      List.iter(((_, typ)) => reset_and_mark_loops(typ), args);
      let print_arg = ((l, arg)) => {
        reset_and_mark_loops(arg);
        switch (l) {
        | Unlabeled => fprintf(ppf, "%a", type_expr, arg)
        | _ =>
          fprintf(ppf, "%s: %a", qualified_label_name(l), type_expr, arg)
        };
      };
      let rec print_args = (ppf, args) => {
        switch (args) {
        | [] => ()
        | [arg] => print_arg(arg)
        | [arg, ...rest] =>
          print_arg(arg);
          fprintf(ppf, ",@ ");
          print_args(ppf, rest);
        };
      };
      switch (args) {
      | [arg] =>
        fprintf(
          ppf,
          "@[<hov>This function call is missing an argument of type %a@]",
          print_args,
          args,
        )
      | _ =>
        fprintf(
          ppf,
          "@[<hov>This function call is missing arguments of type %a@]",
          print_args,
          args,
        )
      };
    }
  | Apply_unknown_label(label, valid_labels) => {
      fprintf(ppf, "This argument cannot be supplied with label %s.", label);
      spellcheck(ppf, label, valid_labels);
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
  | Arity_mismatch(ty, explanation) => {
      reset_and_mark_loops(ty);
      fprintf(ppf, "This function expects the wrong number of arguments,@ ");
      fprintf(
        ppf,
        "it should have type@ %a%t",
        type_expr,
        ty,
        report_type_expected_explanation_opt(explanation),
      );
    }
  | Wrong_name(eorp, ty_expected, kind, p, name, valid_names) => {
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
  | Name_type_mismatch(kind, lid, tp, tpl) => {
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
  | Undefined_method(ty, me, valid_methods) => {
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
  | Undefined_inherited_method(me, valid_methods) => {
      fprintf(ppf, "This expression has no method %s", me);
      spellcheck(ppf, me, valid_methods);
    }
  | Virtual_class(cl) =>
    fprintf(ppf, "Cannot instantiate the virtual class %a", identifier, cl)
  | Unbound_instance_variable(var, valid_vars) => {
      fprintf(ppf, "Unbound instance variable %s", var);
      spellcheck(ppf, var, valid_vars);
    }
  | Instance_variable_not_mutable(b, v) =>
    if (b) {
      fprintf(ppf, "The instance variable %s is not mutable", v);
    } else {
      fprintf(ppf, "The value %s is not an instance variable", v);
    }
  | Not_subtype(tr1, tr2) => fprintf(ppf, "<subtyping error>")
  | Outside_class =>
    fprintf(ppf, "This object duplication occurs outside a method definition")
  | Value_multiply_overridden(v) =>
    fprintf(ppf, "The instance variable %s is overridden several times", v)
  | Coercion_failure(ty, ty', trace, b) => {
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
  | Not_a_function(ty, explanation) => {
      reset_and_mark_loops(ty);
      fprintf(ppf, "This expression is not a function,@ ");
      fprintf(
        ppf,
        "the expected type is@ %a%t",
        type_expr,
        ty,
        report_type_expected_explanation_opt(explanation),
      );
    }
  | Function_label_mismatch({got, expected, expected_type, explanation}) => {
      reset_and_mark_loops(expected_type);
      if (is_optional(got)) {
        fprintf(
          ppf,
          "This function contains the argument %s@ ",
          qualified_label_name(got),
        );
      } else {
        fprintf(
          ppf,
          "The expected function type contains the argument %s@ ",
          qualified_label_name(expected),
        );
      };
      fprintf(
        ppf,
        "which has a default value, but the matching argument does not.@ ",
      );
      fprintf(
        ppf,
        "The expected type is@ %a%t",
        type_expr,
        expected_type,
        report_type_expected_explanation_opt(explanation),
      );
    }
  | Scoping_let_module(id, ty) => {
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
  | Private_label(lid, ty) =>
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
      fprintf(ppf, "This function is called with arguments@ ");
      fprintf(ppf, "in an order different from other calls.@ ");
      fprintf(ppf, "This is only allowed when the real type is known.");
    }
  | Less_general(kind, trace) =>
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
  | Unqualified_gadt_pattern(tpath, name) =>
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
  | Inlined_record_misuse(cstr_name, cstr_type, exp_type) =>
    fprintf(
      ppf,
      "@[%a is a %s constructor but is treated like a %s constructor.@]",
      identifier,
      cstr_name,
      cstr_type,
      exp_type,
    )
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
  | Unknown_literal(n, m) =>
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
  | Unbound_value_missing_rec(lid, loc) => {
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
    | Error(loc, env, err) =>
      Some(Location.error_of_printer(loc, report_error(env), err))
    | Error_forward(err) => Some(err)
    | _ => None,
  );

/* Drop ?recarg */
let type_expect = (~in_function=?, env, e, ty) =>
  type_expect(~in_function?, env, e, ty);
let type_exp = (env, e) => type_exp(env, e);
