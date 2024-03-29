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
    | CstrExtension(i, _, _, _) => i
    | CstrUnboxed =>
      failwith("compile_constructor_tag: cannot compile CstrUnboxed")
  );

let gensym = Ident.create;

let value_imports = ref([]);
let module_symbol_map = Path_tbl.create(256);
let type_map = Path_tbl.create(256);
let include_map = Path_tbl.create(256);
// Use special value of 0 as type hash for exceptions
let exception_type_hash = 0;
let get_type_hash = tydecl => {
  switch (tydecl.type_kind) {
  | TDataVariant(cstrs) =>
    Hashtbl.hash(
      List.flatten(
        List.map(
          cstr => {
            let inline_rec_fields =
              switch (cstr.Types.cd_args) {
              | TConstrRecord(rfs) =>
                List.map(rf => rf.Types.rf_name.name, rfs)
              | _ => []
              };
            [cstr.Types.cd_id.name, ...inline_rec_fields];
          },
          cstrs,
        ),
      ),
    )
  | TDataRecord(rfs) =>
    Hashtbl.hash(List.map(rf => rf.Types.rf_name.name, rfs))
  | _ =>
    failwith(
      "Impossible: attempt to get type hash for non-record or enum type",
    )
  };
};

let get_type_id = (typath, env) =>
  switch (Path_tbl.find_opt(type_map, typath)) {
  | Some(id) => id
  | None =>
    let type_declaration = Env.find_type(typath, env);
    let id = Path.stamp(type_declaration.type_path);
    Path_tbl.add(type_map, typath, id);
    id;
  };

let lookup_symbol = (~env, ~allocation_type, ~repr, path) => {
  switch (path) {
  | Path.PIdent(id) => id
  | Path.PExternal(mod_, name) =>
    let mod_map =
      switch (Path_tbl.find_opt(module_symbol_map, mod_)) {
      | Some(map) => map
      | None =>
        let mod_map = Hashtbl.create(256);
        Path_tbl.add(module_symbol_map, mod_, mod_map);
        mod_map;
      };
    switch (Hashtbl.find_opt(mod_map, name)) {
    | Some(id) => id
    | None =>
      let (path_hd, path_tl) = Path.flatten(mod_);
      let mod_names = [Ident.name(path_hd), ...path_tl];
      let module_chain = Env.find_module_chain(mod_, env);
      let (file, prefix) =
        List.fold_left(
          ((file, prefix), (decl, name)) => {
            switch (decl.md_filepath) {
            | Some(filepath) => (Some(filepath), "")
            | None => (file, prefix ++ name ++ ".")
            }
          },
          (None, ""),
          List.combine(List.rev(module_chain), mod_names),
        );
      switch (file) {
      | Some(filepath) =>
        let fresh = gensym(name);
        Path_tbl.add(include_map, path, fresh);
        let shape =
          switch (repr) {
          | ReprFunction(args, rets, Direct({closure: has_closure})) =>
            // Add closure argument
            let args = [
              Managed,
              ...List.map(allocation_type_of_wasm_repr, args),
            ];
            // Add return type for functions that return void
            let returns =
              switch (rets) {
              | [] => [Unmanaged(WasmI32)]
              | _ => List.map(allocation_type_of_wasm_repr, rets)
              };
            FunctionShape({args, returns, has_closure});
          | _ => GlobalShape(allocation_type)
          };
        value_imports :=
          [
            IncludeDeclaration.grain_value(
              fresh,
              filepath,
              prefix ++ name,
              shape,
            ),
            ...value_imports^,
          ];
        Hashtbl.add(mod_map, name, fresh);
        fresh;

      | None =>
        switch (module_chain) {
        | [{md_type: TModSignature(signature)}, ..._] =>
          List.iter(
            item =>
              switch (item) {
              | TSigValue(_, {val_fullpath: PIdent(id)}) =>
                let name = Ident.name(id);
                Hashtbl.add(mod_map, name, id);
                Path_tbl.add(include_map, PExternal(mod_, name), id);
              | TSigValue(_) =>
                failwith("Impossible: internal value with external path")
              | TSigType(_, _, _)
              | TSigTypeExt(_)
              | TSigModule(_)
              | TSigModType(_) => ()
              },
            signature,
          );
          Hashtbl.find(mod_map, name);
        | _ => failwith("Impossible: internal module has no signature")
        }
      };
    };
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

// reorder arguments according to labels
let reorder_arguments = (args, order) => {
  let rec reorder = (reordered_args, args, order) => {
    let rec extract_label = (l, arg) => {
      switch (arg) {
      | [] => failwith("Impossible: no argument matching label")
      | [(argl, arg), ...rest_args] when Btype.same_label_name(argl, l) => (
          arg,
          rest_args,
        )
      | [arg, ...rest_args] =>
        let (res, rest_args) = extract_label(l, rest_args);
        (res, [arg, ...rest_args]);
      };
    };
    switch (order) {
    | [] => reordered_args
    | [tyl, ...order] =>
      let (value, args) = extract_label(tyl, args);
      reorder([value, ...reordered_args], args, order);
    };
  };
  List.rev(reorder([], args, order));
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
  | Const_rational(data) =>
    Right(
      with_bind("rational", tmp =>
        [BLet(tmp, Comp.number(Const_number_rational(data)), Nonglobal)]
      ),
    )
  | Const_int32(i) =>
    Right(with_bind("int32", tmp => [BLet(tmp, Comp.int32(i), Nonglobal)]))
  | Const_int64(i) =>
    Right(with_bind("int64", tmp => [BLet(tmp, Comp.int64(i), Nonglobal)]))
  | Const_uint32(i) =>
    Right(
      with_bind("uint32", tmp => [BLet(tmp, Comp.uint32(i), Nonglobal)]),
    )
  | Const_uint64(i) =>
    Right(
      with_bind("uint64", tmp => [BLet(tmp, Comp.uint64(i), Nonglobal)]),
    )
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
          ~tail=false,
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
  | TExpIdent(_, _, {val_fullpath, val_mutable, val_global, val_repr}) =>
    let id =
      Imm.id(
        ~loc,
        ~env,
        lookup_symbol(~env, ~allocation_type, ~repr=val_repr, val_fullpath),
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
  | TExpConstant(c) =>
    switch (transl_const(~loc, ~env, c)) {
    | Left(imm) => (imm, [])
    | Right((name, cexprs)) => (Imm.id(~loc, ~env, name), cexprs)
    }
  | TExpUse(_) => (Imm.const(~loc, ~env, Const_void), [])
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
  | TExpPrim1(Magic, arg) => transl_imm(~boxed, ~tail, arg)
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
  | TExpReturn(Some({exp_desc: TExpApp(_)} as return)) =>
    transl_imm(~boxed, ~tail=true, return)
  | TExpReturn(value) =>
    let (value_imm, value_setup) =
      switch (value) {
      | Some(value) =>
        let (value_imm, value_setup) = transl_imm(value);
        (Some(value_imm), value_setup);
      | None => (None, [])
      };
    (
      Imm.const(Const_void),
      value_setup @ [BSeq(Comp.return(~loc, ~env, value_imm))],
    );
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim("@throw")})},
      _,
      _,
    ) =>
    let (ans, ans_setup) = transl_comp_expression(e);
    (Imm.trap(~loc, ~env, ()), ans_setup @ [BSeq(ans)]);
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim(prim)})},
      _,
      args,
    ) =>
    let (imm, setup) =
      Translprim.(
        switch (PrimMap.find_opt(prim_map, prim), args) {
        | (Some(Primitive0(prim)), []) =>
          transl_imm({...e, exp_desc: TExpPrim0(prim)})
        | (Some(Primitive1(prim)), [(_, arg)]) =>
          transl_imm({...e, exp_desc: TExpPrim1(prim, arg)})
        | (Some(Primitive2(prim)), [(_, arg1), (_, arg2)]) =>
          transl_imm({...e, exp_desc: TExpPrim2(prim, arg1, arg2)})
        | (Some(PrimitiveN(prim)), args) =>
          transl_imm({...e, exp_desc: TExpPrimN(prim, List.map(snd, args))})
        | (Some(_), _) => failwith("transl_imm: invalid primitive arity")
        | (None, _) => failwith("transl_imm: unknown primitive")
        }
      );
    if (tail) {
      (
        Imm.const(Const_void),
        setup @ [BSeq(Comp.return(~loc, ~env, Some(imm)))],
      );
    } else {
      (imm, setup);
    };
  | TExpApp(func, order, args) =>
    let tmp = gensym("app");
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) =
      List.split(
        List.map(
          ((l, arg)) => {
            let (arg, setup) = transl_imm(arg);
            ((l, arg), setup);
          },
          args,
        ),
      );
    let new_args = reorder_arguments(new_args, order);
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
            ~tail,
            (new_func, get_fn_allocation_type(func.exp_env, func.exp_type)),
            new_args,
          ),
          Nonglobal,
        ),
      ],
    );
  | TExpBlock([]) => (Imm.const(Const_void), [])
  | TExpBlock([stmt]) => transl_imm(stmt)
  | TExpBlock(stmts) =>
    let stmts = List.rev_map(transl_comp_expression, stmts);
    // stmts is non-empty, so this cannot fail
    let (last_ans, last_setup) = List.hd(stmts);
    let stmts = List.tl(stmts);
    let tmp = gensym("block_result");
    let setup =
      List.concat @@
      List.fold_left(
        (acc, (ans, setup)) => {[setup, [BSeq(ans)], ...acc]},
        [last_setup, [BLet(tmp, last_ans, Nonglobal)]],
        stmts,
      );
    (Imm.id(~loc, ~env, tmp), setup);
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
  | TExpArraySet({array, index, value, infix_op}) =>
    let tmp = gensym("array_access");
    let (arr_var, arr_setup) = transl_imm(array);
    let (idx_var, idx_setup) = transl_imm(index);
    let (arg_var, arg_setup) = transl_imm(value);
    let (infix_app_var, infix_app_setup) =
      switch (infix_op) {
      | Some(infix_op) =>
        let infix_arg1 = gensym("infix_arg1");
        let infix_app = gensym("infix_app");
        let (infix_func, infix_func_setup) = transl_imm(infix_op);
        let (infix_alloc_args, infix_alloc_ret) =
          get_fn_allocation_type(env, infix_op.exp_type);
        (
          Imm.id(~loc, ~env, infix_app),
          infix_func_setup
          @ [
            BLet(
              infix_arg1,
              Comp.array_get(~loc, ~env, ~allocation_type, idx_var, arr_var),
              Nonglobal,
            ),
            BLet(
              infix_app,
              Comp.app(
                ~loc=infix_op.exp_loc,
                ~env,
                ~allocation_type=infix_alloc_ret,
                (infix_func, (infix_alloc_args, infix_alloc_ret)),
                [Imm.id(~loc, ~env, infix_arg1), arg_var],
              ),
              Nonglobal,
            ),
          ],
        );
      | None => (arg_var, [])
      };
    (
      Imm.id(~loc, ~env, tmp),
      arr_setup
      @ idx_setup
      @ arg_setup
      @ infix_app_setup
      @ [
        BLet(
          tmp,
          Comp.array_set(
            ~loc,
            ~env,
            ~allocation_type,
            idx_var,
            arr_var,
            infix_app_var,
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
    let (typath, _, tydecl) = Ctype.extract_concrete_typedecl(env, typ);
    let ty_id = get_type_id(typath, env);
    let type_hash = get_type_hash(tydecl);
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
              Const_number(Const_number_int(Int64.of_int(type_hash))),
            ),
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
  | TExpConstruct(_, {cstr_name, cstr_tag}, arg) =>
    let tmp = gensym("adt");
    let (_, typath, tydecl) = Ctype.extract_concrete_typedecl(env, typ);
    let ty_id = get_type_id(typath, env);
    let compiled_tag = compile_constructor_tag(cstr_tag);
    let (new_args, new_setup) =
      switch (arg) {
      | TExpConstrRecord(fields) =>
        List.split(
          List.map(
            field =>
              switch (field) {
              | (_, Kept) =>
                failwith("Impossible: inline record variant with Kept field")
              | (_, Overridden({txt: name, loc}, expr)) => transl_imm(expr)
              },
            Array.to_list(fields),
          ),
        )
      | TExpConstrTuple(args) => List.split(List.map(transl_imm, args))
      };
    let type_hash =
      switch (cstr_tag) {
      | CstrExtension(_) => exception_type_hash
      | _ => get_type_hash(tydecl)
      };
    let imm_type_hash =
      Imm.const(
        ~loc,
        ~env,
        Const_number(Const_number_int(Int64.of_int(type_hash))),
      );
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
    let adt =
      Comp.adt(~loc, ~env, imm_type_hash, imm_tytag, imm_tag, new_args);
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup) @ [BLet(tmp, adt, Nonglobal)],
    );
  };
}

and transl_comp_expression =
    (
      ~name=?,
      ~tail=false,
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
  | TExpPrim1(Magic, arg) => transl_comp_expression(~name?, ~tail, arg)
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
                  TExpConstrTuple([error_message]),
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
  | TExpReturn(Some({exp_desc: TExpApp(_)} as return)) =>
    transl_comp_expression(~name?, ~tail=true, return)
  | TExpReturn(value) =>
    let (value_imm, value_setup) =
      switch (value) {
      | Some(value) =>
        let (value_imm, value_setup) = transl_imm(value);
        (Some(value_imm), value_setup);
      | None => (None, [])
      };
    (Comp.return(~loc, ~env, value_imm), value_setup);
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim("@throw")})} as func,
      order,
      args,
    ) =>
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) =
      List.split(
        List.map(
          ((l, arg)) => {
            let (arg, setup) = transl_imm(arg);
            ((l, arg), setup);
          },
          args,
        ),
      );
    let new_args = reorder_arguments(new_args, order);
    let (ans, ans_setup) = (
      Comp.app(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        ~tail,
        (new_func, ([Managed], Unmanaged(WasmI32))),
        new_args,
      ),
      func_setup @ List.concat(new_setup),
    );
    (
      Comp.imm(~attributes, ~allocation_type, ~env, Imm.trap(~loc, ~env, ())),
      ans_setup @ [BSeq(ans)],
    );
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim(prim)})},
      _,
      args,
    ) =>
    if (tail) {
      let (imm, setup) =
        Translprim.(
          switch (PrimMap.find_opt(prim_map, prim), args) {
          | (Some(Primitive0(prim)), []) =>
            transl_imm({...e, exp_desc: TExpPrim0(prim)})
          | (Some(Primitive1(prim)), [(_, arg)]) =>
            transl_imm({...e, exp_desc: TExpPrim1(prim, arg)})
          | (Some(Primitive2(prim)), [(_, arg1), (_, arg2)]) =>
            transl_imm({...e, exp_desc: TExpPrim2(prim, arg1, arg2)})
          | (Some(PrimitiveN(prim)), args) =>
            transl_imm({
              ...e,
              exp_desc: TExpPrimN(prim, List.map(snd, args)),
            })
          | (Some(_), _) =>
            failwith("transl_comp_expression: invalid primitive arity")
          | (None, _) =>
            failwith("transl_comp_expression: unknown primitive")
          }
        );
      (Comp.return(~loc, ~attributes, ~env, Some(imm)), setup);
    } else {
      Translprim.(
        switch (PrimMap.find_opt(prim_map, prim), args) {
        | (Some(Primitive0(prim)), []) =>
          transl_comp_expression({...e, exp_desc: TExpPrim0(prim)})
        | (Some(Primitive1(prim)), [(_, arg)]) =>
          transl_comp_expression({...e, exp_desc: TExpPrim1(prim, arg)})
        | (Some(Primitive2(prim)), [(_, arg1), (_, arg2)]) =>
          transl_comp_expression({
            ...e,
            exp_desc: TExpPrim2(prim, arg1, arg2),
          })
        | (Some(PrimitiveN(prim)), args) =>
          transl_comp_expression({
            ...e,
            exp_desc: TExpPrimN(prim, List.map(snd, args)),
          })
        | (Some(_), _) =>
          failwith("transl_comp_expression: invalid primitive arity")
        | (None, _) => failwith("transl_comp_expression: unknown primitive")
        }
      );
    }
  | TExpApp(func, order, args) =>
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) =
      List.split(
        List.map(
          ((l, arg)) => {
            let (arg, setup) = transl_imm(arg);
            ((l, arg), setup);
          },
          args,
        ),
      );
    let new_args = reorder_arguments(new_args, order);
    (
      Comp.app(
        ~loc,
        ~attributes,
        ~allocation_type,
        ~env,
        ~tail,
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
          | {txt: External_name(name)} => name
          | _ => name,
        desc.tvd_name,
        attributes,
      );
    Path_tbl.add(include_map, desc.tvd_val.val_fullpath, desc.tvd_id);
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
          IncludeDeclaration.wasm_func(
            ~global=Global,
            desc.tvd_id,
            desc.tvd_mod.txt,
            external_name.txt,
            FunctionShape({args: argsty, returns: retty, has_closure: true}),
          ),
        ],
      );
    | _ =>
      let ty = get_allocation_type(env, desc.tvd_desc.ctyp_type);
      (
        None,
        [
          IncludeDeclaration.wasm_value(
            ~global=Global,
            desc.tvd_id,
            desc.tvd_mod.txt,
            external_name.txt,
            GlobalShape(ty),
          ),
        ],
      );
    };
  | TTopProvide(exports) =>
    List.iter(
      ({tex_path}) => {
        let {val_fullpath, val_type, val_repr} =
          Env.find_value(tex_path, env);
        let allocation_type = get_allocation_type(env, val_type);
        // Lookup external exports to import them into the module
        ignore @@
        lookup_symbol(~env, ~allocation_type, ~repr=val_repr, val_fullpath);
      },
      exports,
    );
    (None, []);
  | TTopModule(decl) =>
    let (binds, imports) =
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
        List.map(transl_anf_statement, decl.tmod_statements),
      );
    (Some(List.rev(binds)), imports);
  | _ => (None, [])
  };

let rec gather_type_metadata = statements => {
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
                let type_hash = get_type_hash(decl.data_type);
                let descrs =
                  Datarepr.constructors_of_type(typath, decl.data_type);
                let meta =
                  List.map(
                    ((_, cstr)) =>
                      (
                        compile_constructor_tag(cstr.cstr_tag),
                        cstr.cstr_name,
                        switch (cstr.cstr_inlined) {
                        | None => TupleConstructor
                        | Some(t) =>
                          let label_names =
                            switch (t.type_kind) {
                            | TDataRecord(rfs) =>
                              List.map(
                                rf => Ident.name(rf.Types.rf_name),
                                rfs,
                              )
                            | _ =>
                              failwith(
                                "Impossible: inlined record constructor with non-record underlying type",
                              )
                            };
                          RecordConstructor(label_names);
                        },
                      ),
                    descrs,
                  );
                Some((ADTMetadata(id, meta), type_hash));
              | TDataRecord(fields) =>
                let type_hash = get_type_hash(decl.data_type);
                Some((
                  RecordMetadata(
                    id,
                    List.map(field => Ident.name(field.rf_name), fields),
                  ),
                  type_hash,
                ));
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
          (
            ExceptionMetadata(
              ty_id,
              compile_constructor_tag(cstr.cstr_tag),
              cstr.cstr_name,
              switch (cstr.cstr_inlined) {
              | None => TupleConstructor
              | Some(t) =>
                let label_names =
                  switch (t.type_kind) {
                  | TDataRecord(rfs) =>
                    List.map(rf => Ident.name(rf.Types.rf_name), rfs)
                  | _ =>
                    failwith(
                      "Impossible: inlined exception record constructor with non-record underlying type",
                    )
                  };
                RecordConstructor(label_names);
              },
            ),
            exception_type_hash,
          ),
          ...metadata,
        ];
      | TTopModule(decl) =>
        List.append(gather_type_metadata(decl.tmod_statements), metadata)
      | TTopExpr(_)
      | TTopInclude(_)
      | TTopProvide(_)
      | TTopForeign(_)
      | TTopLet(_) => metadata
      }
    },
    [],
    statements,
  );
};

type type_metadata =
  | ADTMeta(int, list((int, string, Types.adt_constructor_type)))
  | RecordMeta(int, list(string));

let construct_type_metadata_buffer = type_metadata => {
  open Types;

  // More information about this function can be found in the printing.md
  // contributor document.

  // Extension constructors defined by the module must be grouped together.
  // For now, this only includes exceptions.
  let (exception_meta, non_exception_meta) =
    List.fold_left(
      ((exception_meta, non_exception_meta), (meta, type_hash) as md_info) => {
        switch (meta) {
        | ExceptionMetadata(_) => (
            [md_info, ...exception_meta],
            non_exception_meta,
          )
        | ADTMetadata(id, variants) => (
            exception_meta,
            [(ADTMeta(id, variants), type_hash), ...non_exception_meta],
          )
        | RecordMetadata(id, fields) => (
            exception_meta,
            [(RecordMeta(id, fields), type_hash), ...non_exception_meta],
          )
        }
      },
      ([], []),
      type_metadata,
    );

  let round_to_8 = n => Int.logand(n + 7, Int.lognot(7));

  let offset = ref(0);
  let non_exception_buf = Buffer.create(256);

  let alignBuffer = (buf, amount) => {
    for (_ in 1 to amount) {
      Buffer.add_int8(buf, 0);
    };
  };

  let iter_record_fields = (buf, fields) => {
    List.iter(
      field => {
        let length = String.length(field);
        let aligned_length = round_to_8(length);
        let rec_length = aligned_length + 8;
        Buffer.add_int32_le(buf, Int32.of_int(rec_length));
        Buffer.add_int32_le(buf, Int32.of_int(length));
        Buffer.add_string(buf, field);
        alignBuffer(buf, aligned_length - length);
      },
      fields,
    );
  };

  let process_adt = (buf, is_exception, id, cstrs) => {
    // For inline record constructors, store field names after other ADT info
    let extra_required =
      List.map(
        ((_, _, cstr_type)) =>
          switch (cstr_type) {
          | TupleConstructor => 0
          | RecordConstructor(fields) =>
            List.fold_left(
              (total, field) =>
                total + 8 + round_to_8(String.length(field)),
              0,
              fields,
            )
          },
        cstrs,
      );

    let section_length =
      List.fold_left2(
        (total, (_, cstr, cstr_type), extra) => {
          total + 16 + round_to_8(String.length(cstr)) + extra
        },
        4,
        cstrs,
        extra_required,
      );
    if (!is_exception) {
      // Add section length during linking for exceptions
      Buffer.add_int32_le(
        buf,
        Int32.of_int(section_length),
      );
    };
    List.iter2(
      ((id, cstr, cstr_type), fields_section_length) => {
        let length = String.length(cstr);
        let aligned_length = round_to_8(length);
        let constr_length = aligned_length + 16;
        Buffer.add_int32_le(
          buf,
          Int32.of_int(constr_length + fields_section_length),
        );
        // Indicates offset to field data; special value of 0 can be interpreted
        // to indicate that this is not a record variant
        Buffer.add_int32_le(
          buf,
          Int32.of_int(
            if (cstr_type == TupleConstructor) {
              0;
            } else {
              constr_length;
            },
          ),
        );
        Buffer.add_int32_le(buf, Int32.of_int(id));
        Buffer.add_int32_le(buf, Int32.of_int(length));
        Buffer.add_string(buf, cstr);
        alignBuffer(buf, aligned_length - length);
        switch (cstr_type) {
        | TupleConstructor => ()
        | RecordConstructor(fields) => iter_record_fields(buf, fields)
        };
      },
      cstrs,
      extra_required,
    );
    section_length;
  };

  let hash_to_offset =
    List.map(
      ((meta, type_hash: int)) => {
        let begin_offset = offset^;
        switch (meta) {
        | ADTMeta(id, cstrs) =>
          let section_length =
            process_adt(non_exception_buf, false, id, cstrs);
          offset := offset^ + section_length;
        | RecordMeta(id, fields) =>
          let section_length =
            List.fold_left(
              (total, field) =>
                total + 8 + round_to_8(String.length(field)),
              4,
              fields,
            );
          offset := offset^ + section_length;
          Buffer.add_int32_le(
            non_exception_buf,
            Int32.of_int(section_length),
          );
          iter_record_fields(non_exception_buf, fields);
        };
        (type_hash, begin_offset);
      },
      non_exception_meta,
    );

  let exceptions_buf = Buffer.create(256);
  switch (exception_meta) {
  | [(ExceptionMetadata(id, _, _, _), _), ..._] =>
    let cstrs =
      List.map(
        meta => {
          switch (meta) {
          | (ExceptionMetadata(_, variant, name, cstr_type), _) => (
              variant,
              name,
              cstr_type,
            )
          | _ => failwith("impossible by partition")
          }
        },
        exception_meta,
      );
    ignore(process_adt(exceptions_buf, true, id, cstrs));
  | _ => ()
  };

  Cmi_format.{
    ctm_metadata: Buffer.contents(non_exception_buf),
    ctm_exceptions: Buffer.contents(exceptions_buf),
    ctm_offsets_tbl: hash_to_offset,
  };
};

let transl_anf_module =
    ({statements, env, signature}: typed_program): anf_program => {
  Path_tbl.clear(type_map);
  Path_tbl.clear(include_map);
  Path_tbl.clear(module_symbol_map);
  value_imports := [];
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
    path_map: Path_tbl.copy(include_map),
  };
  let type_metadata_and_hashes = gather_type_metadata(statements);
  let type_metadata =
    List.map(((meta, _)) => meta, type_metadata_and_hashes);
  let metadata = construct_type_metadata_buffer(type_metadata_and_hashes);
  {
    body,
    env,
    imports,
    signature: {
      ...signature,
      cmi_type_metadata: metadata,
    },
    type_metadata,
    analyses: ref([]),
  };
};

let () = Matchcomp.compile_constructor_tag := compile_constructor_tag;
let () = Matchcomp.transl_anf_expression := transl_anf_expression;
let () = Matchcomp.transl_imm_expression := transl_imm;
let () = Matchcomp.transl_const := transl_const;
