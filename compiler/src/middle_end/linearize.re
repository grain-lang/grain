open Grain_parsing;
open Grain_typed;
open Types;
open Typedtree;
open Type_utils;
open Anftree;
open Anf_helper;

module MatchCompiler = Matchcomp.MatchTreeCompiler;

let compile_constructor_tag =
  Grain_typed.Types.(
    fun
    | CstrConstant(i) => i
    | CstrBlock(i) => i
    | CstrExtension(i, _, _) => i
    | CstrUnboxed =>
      failwith("compile_constructor_tag: cannot compile CstrUnboxed")
  );

let gensym = Ident.create;

let value_imports = ref([]);
/* At the linearization phase, we lift all imports */
let symbol_table = ref(Ident.empty: Ident.tbl(Ident.tbl(Ident.t)));
let type_map = Path_tbl.create(10);
let import_map = Path_tbl.create(10);

let get_type_id = (typath, env) =>
  switch (Path_tbl.find_opt(type_map, typath)) {
  | Some(id) => id
  | None =>
    let type_declaration = Env.find_type(typath, env);
    let id = Path.stamp(type_declaration.type_path);
    Path_tbl.add(type_map, typath, id);
    id;
  };

let lookup_symbol =
    (~allocation_type, ~repr, ~path, mod_, mod_decl, name, original_name) => {
  switch (Ident.find_same_opt(mod_, symbol_table^)) {
  | Some(_) => ()
  | None => symbol_table := Ident.add(mod_, Ident.empty, symbol_table^)
  };
  let modtbl = Ident.find_same(mod_, symbol_table^);
  switch (Ident.find_name_opt(name, modtbl)) {
  | Some((_, ident)) =>
    Path_tbl.add(import_map, path, ident);
    ident;
  | None =>
    let fresh = gensym(name);
    Path_tbl.add(import_map, path, fresh);
    switch (mod_decl.md_filepath) {
    | Some(filepath) =>
      let shape =
        switch (repr) {
        | ReprFunction(args, rets, Direct(_)) =>
          // Add closure argument
          let args = [
            Managed,
            ...List.map(allocation_type_of_wasm_repr, args),
          ];
          // Add return type for functions that return void
          let rets =
            switch (rets) {
            | [] => [Unmanaged(WasmI32)]
            | _ => List.map(allocation_type_of_wasm_repr, rets)
            };
          FunctionShape(args, rets);
        | _ => GlobalShape(allocation_type)
        };
      value_imports :=
        [
          Imp.grain_value(fresh, filepath, original_name, shape),
          ...value_imports^,
        ];

    | None => ()
    };
    symbol_table :=
      Ident.add(mod_, Ident.add(fresh, fresh, modtbl), symbol_table^);
    fresh;
  };
};

let convert_bind = (body, bind) =>
  switch (bind) {
  | BSeq(exp) => AExp.seq(exp, body)
  | BLet(name, exp, global) =>
    AExp.let_(~global, Nonrecursive, [(name, exp)], body)
  | BLetMut(name, exp, global) =>
    AExp.let_(~mut_flag=Mutable, ~global, Nonrecursive, [(name, exp)], body)
  | BLetRec(names, global) => AExp.let_(~global, Recursive, names, body)
  | BLetRecMut(names, global) =>
    AExp.let_(~mut_flag=Mutable, ~global, Recursive, names, body)
  };

let convert_binds = anf_binds => {
  let void_comp =
    Comp.imm(~allocation_type=Unmanaged(WasmI32), Imm.const(Const_void));
  let void = AExp.comp(void_comp);
  let (last_bind, top_binds) =
    switch (anf_binds) {
    | [bind, ...rest] => (bind, rest)
    | _ => (BSeq(void_comp), [])
    };
  let ans =
    switch (last_bind) {
    | BSeq(exp) => AExp.comp(exp)
    | BLet(name, exp, global) =>
      AExp.let_(~global, Nonrecursive, [(name, exp)], void)
    | BLetMut(name, exp, global) =>
      AExp.let_(
        ~mut_flag=Mutable,
        ~global,
        Nonrecursive,
        [(name, exp)],
        void,
      )
    | BLetRec(names, global) => AExp.let_(~global, Recursive, names, void)
    | BLetRecMut(names, global) =>
      AExp.let_(~mut_flag=Mutable, ~global, Recursive, names, void)
    };
  List.fold_left(convert_bind, ans, top_binds);
};

let transl_const =
    (~loc=Location.dummy_loc, ~env=Env.empty, c: Types.constant)
    : Either.t(imm_expression, (ident, list(anf_bind))) => {
  let with_bind = (name, f) => {
    let tmp = gensym(name);
    (tmp, f(tmp));
  };
  switch (c) {
  | Const_number(n) =>
    Right(
      with_bind("number", tmp => [BLet(tmp, Comp.number(n), Nonglobal)]),
    )
  | Const_bigint(data) =>
    Right(
      with_bind("number", tmp =>
        [BLet(tmp, Comp.number(Const_number_bigint(data)), Nonglobal)]
      ),
    )
  | Const_int32(i) =>
    Right(with_bind("int32", tmp => [BLet(tmp, Comp.int32(i), Nonglobal)]))
  | Const_int64(i) =>
    Right(with_bind("int64", tmp => [BLet(tmp, Comp.int64(i), Nonglobal)]))
  | Const_float64(i) =>
    Right(
      with_bind("float64", tmp => [BLet(tmp, Comp.float64(i), Nonglobal)]),
    )
  | Const_float32(i) =>
    Right(
      with_bind("float32", tmp => [BLet(tmp, Comp.float32(i), Nonglobal)]),
    )
  | Const_bytes(b) =>
    Right(with_bind("bytes", tmp => [BLet(tmp, Comp.bytes(b), Nonglobal)]))
  | Const_string(s) =>
    Right(with_bind("str", tmp => [BLet(tmp, Comp.string(s), Nonglobal)]))
  | Const_char(c) =>
    Right(with_bind("char", tmp => [BLet(tmp, Comp.char(c), Nonglobal)]))
  | _ => Left(Imm.const(c))
  };
};

type item_get =
  | RecordPatGet(int, type_expr)
  | ArrayPatGet(int, type_expr)
  | TuplePatGet(int, type_expr);

let rec transl_imm =
        (
          ~boxed=false,
          {
            exp_desc,
            exp_loc: loc,
            exp_env: env,
            exp_type: typ,
            exp_attributes: attributes,
            _,
          } as e: expression,
        )
        : (imm_expression, list(anf_bind)) => {
  let allocation_type = get_allocation_type(env, typ);
  switch (exp_desc) {
  | TExpIdent(_, _, {val_kind: TValUnbound(_)}) =>
    failwith("Impossible: val_kind was unbound")
  | TExpIdent(
      Path.PExternal(Path.PIdent(mod_) as p, ident, _),
      _,
      {
        val_fullpath: Path.PExternal(_, original_name, _),
        val_mutable,
        val_global,
        val_repr,
      },
    ) =>
    let mod_decl = Env.find_module(p, None, env);
    let id =
      Imm.id(
        ~loc,
        ~env,
        lookup_symbol(
          ~allocation_type,
          ~repr=val_repr,
          ~path=p,
          mod_,
          mod_decl,
          ident,
          original_name,
        ),
      );
    if (val_mutable && !val_global && !boxed) {
      let tmp = gensym("unbox_mut");
      let setup = [
        BLet(
          tmp,
          Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id),
          Nonglobal,
        ),
      ];
      (Imm.id(~loc, ~env, tmp), setup);
    } else {
      (id, []);
    };
  | TExpIdent(
      Path.PExternal(Path.PIdent(mod_) as p, ident, _),
      _,
      {val_mutable, val_global, val_repr},
    ) =>
    let mod_decl = Env.find_module(p, None, env);
    let id =
      Imm.id(
        ~loc,
        ~env,
        lookup_symbol(
          ~allocation_type,
          ~repr=val_repr,
          ~path=p,
          mod_,
          mod_decl,
          ident,
          ident,
        ),
      );
    if (val_mutable && !val_global && !boxed) {
      let tmp = gensym("unbox_mut");
      let setup = [
        BLet(
          tmp,
          Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id),
          Nonglobal,
        ),
      ];
      (Imm.id(~loc, ~env, tmp), setup);
    } else {
      (id, []);
    };
  | TExpIdent(Path.PExternal(_), _, _) =>
    failwith("NYI: transl_imm: TExpIdent with multiple PExternal")
  | TExpIdent(Path.PIdent(ident) as path, _, _) =>
    switch (Env.find_value(path, env)) {
    | {val_fullpath: Path.PIdent(_), val_mutable, val_global} =>
      let id = Imm.id(~loc, ~env, ident);
      if (val_mutable && !val_global && !boxed) {
        let tmp = gensym("unbox_mut");
        let setup = [
          BLet(
            tmp,
            Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id),
            Nonglobal,
          ),
        ];
        (Imm.id(~loc, ~env, tmp), setup);
      } else {
        (id, []);
      };
    | {
        val_fullpath: Path.PExternal(Path.PIdent(mod_) as p, ident, _),
        val_mutable,
        val_global,
        val_repr,
      } =>
      let mod_decl = Env.find_module(p, None, env);
      let id =
        Imm.id(
          ~loc,
          ~env,
          lookup_symbol(
            ~allocation_type,
            ~repr=val_repr,
            ~path=p,
            mod_,
            mod_decl,
            ident,
            ident,
          ),
        );
      if (val_mutable && !val_global && !boxed) {
        let tmp = gensym("unbox_mut");
        let setup = [
          BLet(
            tmp,
            Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id),
            Nonglobal,
          ),
        ];
        (Imm.id(~loc, ~env, tmp), setup);
      } else {
        (id, []);
      };
    | {val_fullpath: Path.PExternal(_)} =>
      failwith("NYI: transl_imm: TExpIdent with multiple PExternal")
    }
  | TExpConstant(c) =>
    switch (transl_const(~loc, ~env, c)) {
    | Left(imm) => (imm, [])
    | Right((name, cexprs)) => (Imm.id(~loc, ~env, name), cexprs)
    }
  | TExpNull => (Imm.const(~loc, ~env, Const_bool(false)), [])
  | TExpPrim0(op) =>
    let tmp = gensym("prim0");
    (
      Imm.id(~loc, ~env, tmp),
      [BLet(tmp, Comp.prim0(~loc, ~env, ~allocation_type, op), Nonglobal)],
    );
  | TExpPrim1(BuiltinId, arg) =>
    switch (arg.exp_desc) {
    | TExpConstant(Const_string(builtin)) =>
      switch (Hashtbl.find_opt(Builtin_types.builtin_idents, builtin)) {
      | Some(id) => (
          Imm.const(
            ~loc,
            ~env,
            Const_number(Const_number_int(Int64.of_int(id.stamp))),
          ),
          [],
        )
      | None => failwith(Printf.sprintf("Unknown builtin `%s`", builtin))
      }
    | _ => failwith("Builtin must be a string literal")
    }
  | TExpPrim1(op, arg) =>
    let tmp = gensym("unary");
    let (comp, comp_setup) = transl_comp_expression(e);
    (Imm.id(~loc, ~env, tmp), comp_setup @ [BLet(tmp, comp, Nonglobal)]);
  | TExpPrim2(And, left, right) =>
    let tmp = gensym("boolBinop");
    let (left_imm, left_setup) = transl_imm(left);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ [
        BLet(
          tmp,
          Comp.if_(
            ~loc,
            ~env,
            ~allocation_type,
            left_imm,
            transl_anf_expression(right),
            AExp.comp(
              ~loc,
              ~env,
              Comp.imm(~loc, ~env, ~allocation_type, left_imm),
            ),
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpPrim2(Or, left, right) =>
    let tmp = gensym("boolBinop");
    let (left_imm, left_setup) = transl_imm(left);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ [
        BLet(
          tmp,
          Comp.if_(
            ~loc,
            ~env,
            ~allocation_type,
            left_imm,
            AExp.comp(
              ~loc,
              ~env,
              Comp.imm(~loc, ~env, ~allocation_type, left_imm),
            ),
            transl_anf_expression(right),
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpPrim2(op, left, right) =>
    let tmp = gensym("binop");
    let (left_imm, left_setup) = transl_imm(left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ right_setup
      @ [
        BLet(
          tmp,
          Comp.prim2(~loc, ~env, ~allocation_type, op, left_imm, right_imm),
          Nonglobal,
        ),
      ],
    );
  | TExpPrimN(op, args) =>
    let tmp = gensym("primn");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [
        BLet(
          tmp,
          Comp.primn(~loc, ~env, ~allocation_type, op, new_args),
          Nonglobal,
        ),
      ],
    );
  | TExpBoxAssign(left, right) =>
    let tmp = gensym("assign");
    let (left_imm, left_setup) = transl_imm(left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ right_setup
      @ [
        BLet(
          tmp,
          Comp.box_assign(~loc, ~env, ~allocation_type, left_imm, right_imm),
          Nonglobal,
        ),
      ],
    );
  | TExpAssign(
      {exp_desc: TExpIdent(_, _, {val_global: true})} as left,
      right,
    ) =>
    let tmp = gensym("assign");
    let (left_imm, left_setup) = transl_imm(left);
    let id =
      switch (left_imm) {
      | {imm_desc: ImmId(id)} => id
      | _ => failwith("Impossible by syntax")
      };

    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      right_setup
      @ [
        BLet(
          tmp,
          Comp.local_assign(~loc, ~env, ~allocation_type, id, right_imm),
          Nonglobal,
        ),
      ],
    );
  | TExpAssign(left, right) =>
    let tmp = gensym("assign");
    let (left_imm, left_setup) = transl_imm(~boxed=true, left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ right_setup
      @ [
        BLet(
          tmp,
          Comp.assign(~loc, ~env, ~allocation_type, left_imm, right_imm),
          Nonglobal,
        ),
      ],
    );
  | TExpIf(cond, _then, _else) =>
    let tmp = gensym("if");
    let (cond_imm, cond_setup) = transl_imm(cond);
    (
      Imm.id(~loc, ~env, tmp),
      cond_setup
      @ [
        BLet(
          tmp,
          Comp.if_(
            ~loc,
            ~env,
            ~allocation_type,
            cond_imm,
            transl_anf_expression(_then),
            transl_anf_expression(_else),
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpWhile(cond, body) =>
    let tmp = gensym("while");
    (
      Imm.id(~loc, ~env, tmp),
      [
        BLet(
          tmp,
          Comp.for_(
            ~loc,
            ~env,
            Some(transl_anf_expression(cond)),
            None,
            transl_anf_expression(body),
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpFor(init, cond, inc, body) =>
    let tmp = gensym("for");
    let init_setup =
      Option.fold(
        ~none=[],
        ~some=init => snd(transl_comp_expression(init)),
        init,
      );
    (
      Imm.id(~loc, ~env, tmp),
      init_setup
      @ [
        BLet(
          tmp,
          Comp.for_(
            ~loc,
            ~env,
            Option.map(transl_anf_expression, cond),
            Option.map(transl_anf_expression, inc),
            transl_anf_expression(body),
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpContinue => (
      Imm.const(Const_void),
      [BSeq(Comp.continue(~loc, ~env, ()))],
    )
  | TExpBreak => (
      Imm.const(Const_void),
      [BSeq(Comp.break(~loc, ~env, ()))],
    )
  | TExpReturn(value) =>
    let (value_comp, value_setup) =
      switch (value) {
      | Some(value) =>
        let (value_comp, value_setup) = transl_comp_expression(value);
        (Some(value_comp), value_setup);
      | None => (None, [])
      };
    (
      Imm.const(Const_void),
      value_setup @ [BSeq(Comp.return(~loc, ~env, value_comp))],
    );
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim("@throw")})},
      _,
    ) =>
    let (ans, ans_setup) = transl_comp_expression(e);
    (Imm.trap(~loc, ~env, ()), ans_setup @ [BSeq(ans)]);
  | TExpApp({exp_desc: TExpIdent(_, _, {val_kind: TValPrim(prim)})}, args) =>
    Translprim.(
      switch (PrimMap.find_opt(prim_map, prim), args) {
      | (Some(Primitive0(prim)), []) =>
        transl_imm({...e, exp_desc: TExpPrim0(prim)})
      | (Some(Primitive1(prim)), [arg]) =>
        transl_imm({...e, exp_desc: TExpPrim1(prim, arg)})
      | (Some(Primitive2(prim)), [arg1, arg2]) =>
        transl_imm({...e, exp_desc: TExpPrim2(prim, arg1, arg2)})
      | (Some(PrimitiveN(prim)), args) =>
        transl_imm({...e, exp_desc: TExpPrimN(prim, args)})
      | (Some(_), _) => failwith("transl_imm: invalid primitive arity")
      | (None, _) => failwith("transl_imm: unknown primitive")
      }
    )
  | TExpApp(func, args) =>
    let tmp = gensym("app");
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      (func_setup @ List.concat(new_setup))
      @ [
        BLet(
          tmp,
          Comp.app(
            ~loc,
            ~env,
            ~allocation_type,
            (new_func, get_fn_allocation_type(func.exp_env, func.exp_type)),
            new_args,
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpBlock([]) => (Imm.const(Const_void), [])
  | TExpBlock([stmt]) => transl_imm(stmt)
  | TExpBlock([fst, ...rest]) =>
    let (fst_ans, fst_setup) = transl_comp_expression(fst);
    let (rest_ans, rest_setup) =
      transl_imm({...e, exp_desc: TExpBlock(rest)});
    (rest_ans, fst_setup @ [BSeq(fst_ans)] @ rest_setup);
  | TExpLet(Nonrecursive, _, []) => (Imm.const(Const_void), [])
  | TExpLet(
      Nonrecursive,
      mut_flag,
      [
        {
          vb_expr,
          vb_pat: {
            pat_desc:
              TPatVar(id, _) | TPatAlias({pat_desc: TPatAny}, id, _),
            pat_env,
            pat_type,
          },
        },
        ...rest,
      ],
    ) =>
    // Fast path for simple bindings
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let binds =
      switch (mut_flag) {
      | Immutable =>
        let (exp_ans, exp_setup) = transl_comp_expression(vb_expr);
        exp_setup @ [BLet(id, exp_ans, Nonglobal)];
      | Mutable =>
        let (exp_ans, exp_setup) = transl_imm(vb_expr);
        let boxed =
          Comp.prim1(
            ~allocation_type=get_allocation_type(pat_env, pat_type),
            BoxBind,
            exp_ans,
          );
        exp_setup @ [BLet(id, boxed, Nonglobal)];
      };
    let (body_ans, body_setup) =
      transl_imm({...e, exp_desc: TExpLet(Nonrecursive, mut_flag, rest)});
    (body_ans, binds @ body_setup);
  | TExpLet(Nonrecursive, mut_flag, [{vb_expr, vb_pat}, ...rest]) =>
    // More complex bindings use the match compiler
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let (exp_ans, exp_setup) = transl_imm(vb_expr);
    let binds =
      MatchCompiler.destructure(
        ~mut_flag,
        ~global=Nonglobal,
        vb_pat,
        exp_ans,
      );
    let (body_ans, body_setup) =
      transl_imm({...e, exp_desc: TExpLet(Nonrecursive, mut_flag, rest)});
    (body_ans, exp_setup @ binds @ body_setup);
  | TExpLet(Recursive, mut_flag, binds) =>
    if (mut_flag == Mutable) {
      failwith("mutable let rec");
    };
    let tmp = gensym("lam_letrec");
    let (binds, new_binds_setup) =
      List.split(
        List.map(
          ({vb_pat, vb_expr}) =>
            (
              vb_pat,
              transl_comp_expression({
                ...vb_expr,
                exp_attributes: attributes @ vb_expr.exp_attributes,
              }),
            ),
          binds,
        ),
      );
    let (new_binds, new_setup) = List.split(new_binds_setup);

    let names =
      List.map(
        fun
        | {
            pat_desc: TPatVar(id, _) | TPatAlias({pat_desc: TPatAny}, id, _),
          } => id
        | _ => failwith("Non-name not allowed on LHS of let rec."),
        binds,
      );
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [
        BLetRec(List.combine(names, new_binds), Nonglobal),
        BLet(
          tmp,
          Comp.imm(~allocation_type, Imm.const(Const_void)),
          Nonglobal,
        ),
      ],
    );
  | TExpLambda([{mb_pat, mb_body: body}], _) =>
    let tmp = gensym("lam_lambda");
    let (lam, _) = transl_comp_expression(e);
    (Imm.id(~loc, ~env, tmp), [BLet(tmp, lam, Nonglobal)]);
  | TExpLambda([], _) => failwith("Impossible: transl_imm: Empty lambda")
  | TExpLambda(_, _) => failwith("NYI: transl_imm: Multi-branch lambda")
  | TExpTuple(args) =>
    let tmp = gensym("tup");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [BLet(tmp, Comp.tuple(~loc, ~env, new_args), Nonglobal)],
    );
  | TExpArray(args) =>
    let tmp = gensym("tup");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [BLet(tmp, Comp.array(~loc, ~env, new_args), Nonglobal)],
    );
  | TExpArrayGet(arr, idx) =>
    let tmp = gensym("array_access");
    let (arr_var, arr_setup) = transl_imm(arr);
    let (idx_var, idx_setup) = transl_imm(idx);
    (
      Imm.id(~loc, ~env, tmp),
      arr_setup
      @ idx_setup
      @ [
        BLet(
          tmp,
          Comp.array_get(~loc, ~env, ~allocation_type, idx_var, arr_var),
          Nonglobal,
        ),
      ],
    );
  | TExpArraySet(arr, idx, arg) =>
    let tmp = gensym("array_access");
    let (arr_var, arr_setup) = transl_imm(arr);
    let (idx_var, idx_setup) = transl_imm(idx);
    let (arg_var, arg_setup) = transl_imm(arg);
    (
      Imm.id(~loc, ~env, tmp),
      arr_setup
      @ idx_setup
      @ arg_setup
      @ [
        BLet(
          tmp,
          Comp.array_set(
            ~loc,
            ~env,
            ~allocation_type,
            idx_var,
            arr_var,
            arg_var,
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpRecord(base, args) =>
    let base_imm = Option.map(transl_imm, base);
    let tmp = gensym("record");
    let (new_args, new_setup) =
      List.split(
        List.map(
          arg =>
            switch (arg) {
            | (ld, Kept) =>
              let (base_var, _) = Option.get(base_imm);
              let fieldtmp = gensym("field");
              (
                (None, Imm.id(~loc, ~env, fieldtmp)),
                [
                  BLet(
                    fieldtmp,
                    Comp.record_get(
                      ~loc,
                      ~env,
                      ~allocation_type,
                      Int32.of_int(ld.lbl_pos),
                      base_var,
                    ),
                    Nonglobal,
                  ),
                ],
              );
            | (_, Overridden({txt: name, loc}, expr)) =>
              let (var, setup) = transl_imm(expr);
              (
                (
                  Some(
                    Location.mkloc(Identifier.string_of_ident(name), loc),
                  ),
                  var,
                ),
                setup,
              );
            },
          Array.to_list(args),
        ),
      );
    let (typath, _, _) = Typepat.extract_concrete_record(env, typ);
    let ty_id = get_type_id(typath, env);
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(
        Option.to_list(Option.map(((_, setup)) => setup, base_imm))
        @ new_setup,
      )
      @ [
        BLet(
          tmp,
          Comp.record(
            ~loc,
            ~env,
            Imm.const(
              ~loc,
              ~env,
              Const_number(Const_number_int(Int64.of_int(ty_id))),
            ),
            new_args,
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpRecordGet(expr, field, ld) =>
    let tmp = gensym("field");
    let (var, setup) = transl_imm(expr);
    (
      Imm.id(~loc, ~env, tmp),
      setup
      @ [
        BLet(
          tmp,
          Comp.record_get(
            ~loc,
            ~env,
            ~allocation_type,
            Int32.of_int(ld.lbl_pos),
            var,
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpRecordSet(expr, field, ld, arg) =>
    let tmp = gensym("field_set");
    let (record, rec_setup) = transl_imm(expr);
    let (arg, arg_setup) = transl_imm(arg);
    (
      Imm.id(~loc, ~env, tmp),
      rec_setup
      @ arg_setup
      @ [
        BLet(
          tmp,
          Comp.record_set(
            ~loc,
            ~env,
            ~allocation_type,
            Int32.of_int(ld.lbl_pos),
            record,
            arg,
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpMatch(exp, branches, partial) =>
    let tmp = gensym("match");
    let (exp_ans, exp_setup) = transl_imm(exp);
    let (ans, setup) =
      MatchCompiler.compile_match(
        ~allocation_type,
        ~partial,
        branches,
        exp_ans,
      );
    (
      Imm.id(~loc, ~env, tmp),
      (exp_setup @ setup) @ [BLet(tmp, ans, Nonglobal)],
    );
  | TExpConstruct(_, {cstr_tag}, args) =>
    let tmp = gensym("adt");
    let (_, typath, _) = Ctype.extract_concrete_typedecl(env, typ);
    let ty_id = get_type_id(typath, env);
    let compiled_tag = compile_constructor_tag(cstr_tag);
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    let imm_tytag =
      Imm.const(
        ~loc,
        ~env,
        Const_number(Const_number_int(Int64.of_int(ty_id))),
      );
    let imm_tag =
      Imm.const(
        ~loc,
        ~env,
        Const_number(Const_number_int(Int64.of_int(compiled_tag))),
      );
    let adt = Comp.adt(~loc, ~env, imm_tytag, imm_tag, new_args);
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup) @ [BLet(tmp, adt, Nonglobal)],
    );
  };
}

and transl_comp_expression =
    (
      ~name=?,
      {
        exp_desc,
        exp_type,
        exp_attributes: attributes,
        exp_loc: loc,
        exp_env: env,
        _,
      } as e: expression,
    )
    : (comp_expression, list(anf_bind)) => {
  let allocation_type = get_allocation_type(env, exp_type);
  switch (exp_desc) {
  | TExpPrim0(op) => (
      Comp.prim0(~loc, ~attributes, ~allocation_type, ~env, op),
      [],
    )
  | TExpPrim1(BuiltinId, arg) =>
    switch (arg.exp_desc) {
    | TExpConstant(Const_string(builtin)) =>
      switch (Hashtbl.find_opt(Builtin_types.builtin_idents, builtin)) {
      | Some(id) => (
          Comp.imm(
            ~allocation_type,
            Imm.const(
              ~loc,
              ~env,
              Const_number(Const_number_int(Int64.of_int(id.stamp))),
            ),
          ),
          [],
        )
      | None => failwith(Printf.sprintf("Unknown builtin `%s`", builtin))
      }
    | _ => failwith("Builtin must be a string literal")
    }
  | TExpPrim1(Assert, arg) =>
    let (arg_imm, arg_setup) = transl_imm(arg);
    let assertion_error =
      Env.find_constructor(PIdent(Builtin_types.ident_assertion_error), env);
    let assertion_error_identifier =
      Location.mkloc(
        Identifier.IdentName(
          Location.mkloc(assertion_error.cstr_name, assertion_error.cstr_loc),
        ),
        assertion_error.cstr_loc,
      );
    let mkexp = exp_desc => {...e, exp_desc};
    let error_message =
      mkexp(
        TExpConstant(
          Const_string(
            Printf.sprintf(
              "AssertionError: Assertion failed in %s, line %d",
              loc.loc_start.pos_fname,
              loc.loc_start.pos_lnum,
            ),
          ),
        ),
      );
    let throw_error =
      transl_anf_expression(
        mkexp(
          TExpPrim1(
            Throw,
            {
              ...e,
              exp_desc:
                TExpConstruct(
                  assertion_error_identifier,
                  assertion_error,
                  [error_message],
                ),
            },
          ),
        ),
      );
    (
      Comp.if_(
        ~loc,
        ~env,
        ~allocation_type,
        arg_imm,
        AExp.comp(
          Comp.imm(
            ~allocation_type=Unmanaged(WasmI32),
            Imm.const(Const_void),
          ),
        ),
        throw_error,
      ),
      arg_setup,
    );
  | TExpPrim1(op, arg) =>
    let (arg_imm, arg_setup) = transl_imm(arg);
    (
      Comp.prim1(~loc, ~attributes, ~allocation_type, ~env, op, arg_imm),
      arg_setup,
    );
  | TExpPrim2(And, left, right) =>
    let (left_imm, left_setup) = transl_imm(left);
    (
      Comp.if_(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        left_imm,
        transl_anf_expression(right),
        AExp.comp(
          ~loc,
          ~env,
          Comp.imm(~loc, ~attributes, ~allocation_type, ~env, left_imm),
        ),
      ),
      left_setup,
    );
  | TExpPrim2(Or, left, right) =>
    let (left_imm, left_setup) = transl_imm(left);
    (
      Comp.if_(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        left_imm,
        AExp.comp(
          ~loc,
          ~env,
          Comp.imm(~loc, ~attributes, ~allocation_type, ~env, left_imm),
        ),
        transl_anf_expression(right),
      ),
      left_setup,
    );
  | TExpPrim2(op, left, right) =>
    let (left_imm, left_setup) = transl_imm(left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Comp.prim2(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        op,
        left_imm,
        right_imm,
      ),
      left_setup @ right_setup,
    );
  | TExpIf(cond, _then, _else) =>
    let (cond_imm, cond_setup) = transl_imm(cond);
    (
      Comp.if_(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        cond_imm,
        transl_anf_expression(_then),
        transl_anf_expression(_else),
      ),
      cond_setup,
    );
  | TExpBlock([]) => (Comp.imm(~allocation_type, Imm.const(Const_void)), [])
  | TExpBlock(stmts) =>
    let stmts = List.rev_map(transl_comp_expression, stmts);
    // stmts is non-empty, so this cannot fail
    let (last_ans, last_setup) = List.hd(stmts);
    let stmts = List.tl(stmts);
    let setup =
      List.concat @@
      List.fold_left(
        (acc, (ans, setup)) => {[setup, [BSeq(ans)], ...acc]},
        [last_setup],
        stmts,
      );
    (last_ans, setup);
  | TExpLet(Nonrecursive, _, []) => (
      Comp.imm(~allocation_type, Imm.const(Const_void)),
      [],
    )

  | TExpLet(
      Nonrecursive,
      mut_flag,
      [
        {
          vb_expr,
          vb_pat: {
            pat_desc:
              TPatVar(bind, _) | TPatAlias({pat_desc: TPatAny}, bind, _),
          },
        },
        ...rest,
      ],
    ) =>
    // Fast path for simple binding
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let (exp_ans, exp_setup) =
      if (mut_flag == Mutable) {
        let (imm, imm_setup) = transl_imm(vb_expr);
        (
          Comp.prim1(
            ~allocation_type=
              get_allocation_type(vb_expr.exp_env, vb_expr.exp_type),
            BoxBind,
            imm,
          ),
          imm_setup,
        );
      } else {
        transl_comp_expression(vb_expr);
      };
    let (body_ans, body_setup) =
      transl_comp_expression({
        ...e,
        exp_desc: TExpLet(Nonrecursive, mut_flag, rest),
      });
    (body_ans, exp_setup @ [BLet(bind, exp_ans, Nonglobal)] @ body_setup);
  | TExpLet(Nonrecursive, mut_flag, [{vb_expr, vb_pat}, ...rest]) =>
    // More complex bindings use the match compiler
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let (exp_ans, exp_setup) = transl_imm(vb_expr);
    let binds =
      MatchCompiler.destructure(
        ~mut_flag,
        ~global=Nonglobal,
        vb_pat,
        exp_ans,
      );
    let (body_ans, body_setup) =
      transl_comp_expression({
        ...e,
        exp_desc: TExpLet(Nonrecursive, mut_flag, rest),
      });
    (body_ans, exp_setup @ binds @ body_setup);
  | TExpLet(Recursive, mut_flag, binds) =>
    if (mut_flag == Mutable) {
      failwith("mutable let rec");
    };
    let (binds, new_binds_setup) =
      List.split(
        List.map(
          ({vb_pat, vb_expr}) => {
            let vb_expr = {
              ...vb_expr,
              exp_attributes: attributes @ vb_expr.exp_attributes,
            };
            (vb_pat, transl_comp_expression(vb_expr));
          },
          binds,
        ),
      );
    let (new_binds, new_setup) = List.split(new_binds_setup);

    let names =
      List.map(
        fun
        | {
            pat_desc: TPatVar(id, _) | TPatAlias({pat_desc: TPatAny}, id, _),
          } => id
        | _ => failwith("Non-name not allowed on LHS of let rec."),
        binds,
      );
    (
      Comp.imm(~allocation_type, Imm.const(Const_void)),
      List.concat(new_setup)
      @ [BLetRec(List.combine(names, new_binds), Nonglobal)],
    );
  | TExpLambda(
      [
        {
          mb_pat: {pat_desc: TPatConstruct({txt: ident}, _, [])},
          mb_body: body,
        },
      ],
      _,
    )
      when Identifier.equal(ident, Identifier.IdentName(mknoloc("()"))) =>
    let anf_body = transl_anf_expression(body);
    (
      Comp.lambda(
        ~loc,
        ~attributes,
        ~env,
        ~name?,
        [],
        (anf_body, get_allocation_type(body.exp_env, body.exp_type)),
      ),
      [],
    );
  | TExpLambda([{mb_pat, mb_body: body}], _) =>
    switch (mb_pat.pat_desc) {
    | TPatTuple(args) =>
      let anf_body = transl_anf_expression(body);
      let (anf_args, anf_body) =
        List.fold_right(
          (arg, (anf_args, body)) => {
            let tmp = gensym("lambda_arg");
            let binds =
              switch (arg.pat_desc) {
              | TPatVar(id, _)
              | TPatAlias({pat_desc: TPatAny}, id, _) => [
                  BLet(
                    id,
                    Comp.imm(
                      ~allocation_type=
                        get_allocation_type(arg.pat_env, arg.pat_type),
                      Imm.id(~loc, ~env, tmp),
                    ),
                    Nonglobal,
                  ),
                ]
              | _ => MatchCompiler.destructure(arg, Imm.id(~loc, ~env, tmp))
              };
            (
              [
                (tmp, get_allocation_type(arg.pat_env, arg.pat_type)),
                ...anf_args,
              ],
              List.fold_right(
                (bind, body) => convert_bind(body, bind),
                binds,
                body,
              ),
            );
          },
          args,
          ([], anf_body),
        );
      (
        Comp.lambda(
          ~loc,
          ~attributes,
          ~env,
          ~name?,
          anf_args,
          (anf_body, get_allocation_type(body.exp_env, body.exp_type)),
        ),
        [],
      );
    | _ =>
      failwith("Impossible: transl_imm: Lambda contained non-tuple pattern")
    }
  | TExpLambda([], _) =>
    failwith("transl_comp_expression: impossible: empty lambda")
  | TExpLambda(_, _) =>
    failwith("transl_comp_expression: NYI: multi-branch lambda")
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim("@throw")})} as func,
      args,
    ) =>
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    let (ans, ans_setup) = (
      Comp.app(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        (new_func, ([Managed], Unmanaged(WasmI32))),
        new_args,
      ),
      func_setup @ List.concat(new_setup),
    );
    (
      Comp.imm(~attributes, ~allocation_type, ~env, Imm.trap(~loc, ~env, ())),
      ans_setup @ [BSeq(ans)],
    );
  | TExpApp({exp_desc: TExpIdent(_, _, {val_kind: TValPrim(prim)})}, args) =>
    Translprim.(
      switch (PrimMap.find_opt(prim_map, prim), args) {
      | (Some(Primitive0(prim)), []) =>
        transl_comp_expression({...e, exp_desc: TExpPrim0(prim)})
      | (Some(Primitive1(prim)), [arg]) =>
        transl_comp_expression({...e, exp_desc: TExpPrim1(prim, arg)})
      | (Some(Primitive2(prim)), [arg1, arg2]) =>
        transl_comp_expression({...e, exp_desc: TExpPrim2(prim, arg1, arg2)})
      | (Some(PrimitiveN(prim)), args) =>
        transl_comp_expression({...e, exp_desc: TExpPrimN(prim, args)})
      | (Some(_), _) =>
        failwith("transl_comp_expression: invalid primitive arity")
      | (None, _) => failwith("transl_comp_expression: unknown primitive")
      }
    )
  | TExpApp(func, args) =>
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Comp.app(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        (new_func, get_fn_allocation_type(func.exp_env, func.exp_type)),
        new_args,
      ),
      func_setup @ List.concat(new_setup),
    );
  | TExpTuple(args) =>
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (Comp.tuple(~loc, ~attributes, ~env, new_args), List.concat(new_setup));
  | TExpArray(args) =>
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (Comp.array(~loc, ~attributes, ~env, new_args), List.concat(new_setup));
  | TExpMatch(expr, branches, partial) =>
    let (exp_ans, exp_setup) = transl_imm(expr);
    let (ans, setup) =
      MatchCompiler.compile_match(
        ~allocation_type,
        ~partial,
        branches,
        exp_ans,
      );
    (ans, exp_setup @ setup);
  | _ =>
    let (imm, setup) = transl_imm(e);
    (Comp.imm(~loc, ~attributes, ~allocation_type, ~env, imm), setup);
  };
}

and transl_anf_expression =
    ({exp_desc, exp_loc: loc, exp_env: env, _} as e: expression)
    : anf_expression => {
  let (ans, ans_setup) = transl_comp_expression(e);
  List.fold_right(
    (bind, body) =>
      switch (bind) {
      | BLet(_, _, Global)
      | BLetMut(_, _, Global)
      | BLetRec(_, Global) => failwith("Global bind at non-toplevel")
      | BSeq(exp) => AExp.seq(~loc, ~env, exp, body)
      | BLet(name, exp, global) =>
        AExp.let_(~loc, ~env, ~global, Nonrecursive, [(name, exp)], body)
      | BLetMut(name, exp, global) =>
        AExp.let_(
          ~loc,
          ~env,
          ~mut_flag=Mutable,
          ~global,
          Nonrecursive,
          [(name, exp)],
          body,
        )
      | BLetRec(names, global) =>
        AExp.let_(~loc, ~env, ~global, Recursive, names, body)
      | BLetRecMut(names, global) =>
        AExp.let_(
          ~loc,
          ~env,
          ~mut_flag=Mutable,
          ~global,
          Recursive,
          names,
          body,
        )
      },
    ans_setup,
    AExp.comp(~loc, ~env, ans),
  );
};

let rec transl_anf_statement =
        (
          {
            ttop_desc,
            ttop_attributes: attributes,
            ttop_env: env,
            ttop_loc: loc,
          } as s: toplevel_stmt,
        )
        : (option(list(anf_bind)), list(import_spec)) =>
  switch (ttop_desc) {
  | TTopLet(_, _, []) => (None, [])
  | TTopLet(
      Nonrecursive,
      mut_flag,
      [
        {
          vb_expr,
          vb_pat: {
            pat_desc:
              TPatVar(bind, _) | TPatAlias({pat_desc: TPatAny}, bind, _),
          },
        },
        ...rest,
      ],
    ) =>
    // Fast path for simple bindings
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let name = Some(Ident.name(bind));
    let (exp_ans, exp_setup) = transl_comp_expression(~name?, vb_expr);
    let binds =
      switch (mut_flag) {
      | Mutable => [BLetMut(bind, exp_ans, Global)]
      | Immutable => [BLet(bind, exp_ans, Global)]
      };
    let (rest_setup, rest_imp) =
      transl_anf_statement({
        ...s,
        ttop_desc: TTopLet(Nonrecursive, mut_flag, rest),
      });
    let rest_setup = Option.value(~default=[], rest_setup);
    (Some(exp_setup @ binds @ rest_setup), rest_imp);
  | TTopLet(Nonrecursive, mut_flag, [{vb_expr, vb_pat}, ...rest]) =>
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let name =
      switch (vb_pat.pat_desc) {
      | TPatVar(bind, _)
      | TPatAlias(_, bind, _) => Some(Ident.name(bind))
      | _ => None
      };
    let (exp_ans, exp_setup) = transl_comp_expression(~name?, vb_expr);
    let tmp = gensym("destructure_target");
    let destructure_setup = [BLet(tmp, exp_ans, Nonglobal)];
    let (rest_setup, rest_imp) =
      transl_anf_statement({
        ...s,
        ttop_desc: TTopLet(Nonrecursive, mut_flag, rest),
      });
    let rest_setup = Option.value(~default=[], rest_setup);
    let setup =
      destructure_setup
      @ MatchCompiler.destructure(
          ~mut_flag,
          ~global=Global,
          vb_pat,
          Imm.id(tmp),
        );
    (Some(exp_setup @ setup @ rest_setup), rest_imp);
  | TTopLet(Recursive, mut_flag, binds) =>
    let (binds, new_binds_setup) =
      List.split(
        List.map(
          ({vb_pat, vb_expr}) => {
            let name =
              switch (vb_pat.pat_desc) {
              | TPatVar(bind, _)
              | TPatAlias(_, bind, _) => Some(Ident.name(bind))
              | _ => None
              };
            let vb_expr = {
              ...vb_expr,
              exp_attributes: attributes @ vb_expr.exp_attributes,
            };
            (vb_pat, transl_comp_expression(~name?, vb_expr));
          },
          binds,
        ),
      );
    let (new_binds, new_setup) = List.split(new_binds_setup);

    let names =
      List.map(
        fun
        | {
            pat_desc: TPatVar(id, _) | TPatAlias({pat_desc: TPatAny}, id, _),
          } => id
        | _ => failwith("Non-name not allowed on LHS of let rec."),
        binds,
      );

    let bind =
      switch (mut_flag) {
      | Mutable => [BLetRecMut(List.combine(names, new_binds), Global)]
      | Immutable => [BLetRec(List.combine(names, new_binds), Global)]
      };
    (Some(List.concat(new_setup) @ bind), []);
  | TTopExpr(expr) =>
    let expr = {...expr, exp_attributes: attributes};
    let (comp, setup) = transl_comp_expression(expr);
    (Some(setup @ [BSeq(comp)]), []);
  | TTopData(decls) => (None, [])
  | TTopException(ext) => (None, [])
  | TTopForeign(desc) =>
    let external_name =
      List.fold_left(
        name =>
          fun
          | External_name(name) => name
          | _ => name,
        desc.tvd_name.txt,
        attributes,
      );
    Path_tbl.add(import_map, desc.tvd_val.val_fullpath, desc.tvd_id);
    switch (desc.tvd_desc.ctyp_type.desc) {
    | TTyArrow(_) =>
      let (argsty, retty) =
        get_fn_allocation_type(env, desc.tvd_desc.ctyp_type);
      let retty =
        if (returns_void(env, desc.tvd_desc.ctyp_type)) {
          [];
        } else {
          [retty];
        };
      (
        None,
        [
          Imp.wasm_func(
            ~global=Global,
            desc.tvd_id,
            desc.tvd_mod.txt,
            external_name,
            FunctionShape(argsty, retty),
          ),
        ],
      );
    | _ =>
      let ty = get_allocation_type(env, desc.tvd_desc.ctyp_type);
      (
        None,
        [
          Imp.wasm_value(
            ~global=Global,
            desc.tvd_id,
            desc.tvd_mod.txt,
            external_name,
            GlobalShape(ty),
          ),
        ],
      );
    };
  | TTopExport(exports) =>
    List.iter(
      ({tex_path}) => {
        let {val_fullpath, val_type, val_repr} =
          Env.find_value(tex_path, env);
        switch (val_fullpath) {
        | Path.PExternal(Path.PIdent(mod_) as p, ident, _) =>
          let allocation_type = get_allocation_type(env, val_type);
          let mod_decl = Env.find_module(p, None, env);
          // Lookup external exports to import them into the module
          ignore @@
          lookup_symbol(
            ~allocation_type,
            ~repr=val_repr,
            ~path=val_fullpath,
            mod_,
            mod_decl,
            ident,
            ident,
          );
        | Path.PIdent(_) => ()
        | _ => failwith("NYI: Path with multiple PExternal")
        };
      },
      exports,
    );
    (None, []);
  | _ => (None, [])
  };

let gather_type_metadata = statements => {
  List.fold_left(
    (metadata, {ttop_desc, ttop_env}) => {
      switch (ttop_desc) {
      | TTopData(decls) =>
        let info =
          List.filter_map(
            decl => {
              let typath = decl.data_type.type_path;
              let id = get_type_id(typath, ttop_env);
              switch (decl.data_kind) {
              | TDataVariant(cnstrs) =>
                let descrs =
                  Datarepr.constructors_of_type(typath, decl.data_type);
                let meta =
                  List.map(
                    ((_, cstr)) =>
                      (
                        compile_constructor_tag(cstr.cstr_tag),
                        cstr.cstr_name,
                      ),
                    descrs,
                  );
                Some(ADTMetadata(id, meta));
              | TDataRecord(fields) =>
                Some(
                  RecordMetadata(
                    id,
                    List.map(field => Ident.name(field.rf_name), fields),
                  ),
                )
              | TDataAbstract => None
              };
            },
            decls,
          );
        List.append(info, metadata);
      | TTopException(ext) =>
        let ty_id = get_type_id(ext.ext_type.ext_type_path, ttop_env);
        let id = ext.ext_id;
        let cstr = Datarepr.extension_descr(Path.PIdent(id), ext.ext_type);
        [
          ExceptionMetadata(
            ty_id,
            compile_constructor_tag(cstr.cstr_tag),
            cstr.cstr_name,
          ),
          ...metadata,
        ];
      | TTopExpr(_)
      | TTopImport(_)
      | TTopExport(_)
      | TTopForeign(_)
      | TTopLet(_) => metadata
      }
    },
    [],
    statements,
  );
};

let transl_anf_module =
    ({statements, env, signature}: typed_program): anf_program => {
  Path_tbl.clear(type_map);
  Path_tbl.clear(import_map);
  value_imports := [];
  symbol_table := Ident.empty;
  let (top_binds, imports) =
    List.fold_left(
      ((acc_bind, acc_imp), cur) =>
        switch (cur) {
        | (None, lst) => (acc_bind, List.rev_append(lst, acc_imp))
        | (Some(b), lst) => (
            List.rev_append(b, acc_bind),
            List.rev_append(lst, acc_imp),
          )
        },
      ([], []),
      List.map(transl_anf_statement, statements),
    );
  let imports = List.rev(imports);
  let body = convert_binds(top_binds);
  let imports = {
    specs: imports @ value_imports^,
    path_map: Path_tbl.copy(import_map),
  };
  let type_metadata = gather_type_metadata(statements);
  {body, env, imports, signature, type_metadata, analyses: ref([])};
};

let () = Matchcomp.compile_constructor_tag := compile_constructor_tag;
let () = Matchcomp.transl_anf_expression := transl_anf_expression;
let () = Matchcomp.transl_imm_expression := transl_imm;
let () = Matchcomp.transl_const := transl_const;
