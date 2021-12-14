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
module PathMap =
  Hashtbl.Make({
    type t = Path.t;
    let hash = x => Hashtbl.hash(Path.name(x));
    let equal = (a, b) => Path.compare(a, b) === 0;
  });
let type_map = PathMap.create(10);

let get_type_id = typath =>
  switch (PathMap.find_opt(type_map, typath)) {
  | Some(id) => id
  | None =>
    let id = Path.stamp(typath);
    PathMap.add(type_map, typath, id);
    id;
  };

let lookup_symbol = (~allocation_type, mod_, mod_decl, name, original_name) => {
  switch (Ident.find_same_opt(mod_, symbol_table^)) {
  | Some(_) => ()
  | None => symbol_table := Ident.add(mod_, Ident.empty, symbol_table^)
  };
  let modtbl = Ident.find_same(mod_, symbol_table^);
  switch (Ident.find_name_opt(name, modtbl)) {
  | Some((_, ident)) => ident
  | None =>
    let fresh = gensym(name);
    switch (mod_decl.md_filepath) {
    | Some(filepath) =>
      value_imports :=
        [
          Imp.grain_value(
            fresh,
            filepath,
            original_name,
            GlobalShape(allocation_type),
          ),
          ...value_imports^,
        ]
    | None => ()
    };
    symbol_table :=
      Ident.add(mod_, Ident.add(fresh, fresh, modtbl), symbol_table^);
    fresh;
  };
};

type either('a, 'b) =
  | Left('a)
  | Right('b);

let convert_binds = anf_binds => {
  let void_comp =
    Comp.imm(
      ~allocation_type=StackAllocated(WasmI32),
      Imm.const(Const_void),
    );
  let void = AExp.comp(void_comp);
  let (last_bind, top_binds) =
    switch (anf_binds) {
    | [bind, ...rest] => (bind, rest)
    | _ => (BSeq(void_comp), [])
    };
  let ans =
    switch (last_bind) {
    | BSeq(exp) => AExp.comp(exp)
    | BLet(name, exp) => AExp.let_(Nonrecursive, [(name, exp)], void)
    | BLetRec(names) => AExp.let_(Recursive, names, void)
    | BLetExport(rf, binds) => AExp.let_(~global=Global, rf, binds, void)
    };
  List.fold_left(
    (body, bind) =>
      switch (bind) {
      | BSeq(exp) => AExp.seq(exp, body)
      | BLet(name, exp) => AExp.let_(Nonrecursive, [(name, exp)], body)
      | BLetRec(names) => AExp.let_(Recursive, names, body)
      | BLetExport(rf, binds) => AExp.let_(~global=Global, rf, binds, body)
      },
    ans,
    top_binds,
  );
};

let extract_bindings = (mut_flag, pat, cexpr) => {
  let get_imm =
    fun
    | CImmExpr(imm) => imm
    | _ => failwith("MatchCompiler returned non-immediate for binding");
  let map_fn =
    switch (mut_flag) {
    | Mutable => (
        ((name, e)) =>
          BLet(
            name,
            {...e, comp_desc: CPrim1(BoxBind, get_imm(e.comp_desc))},
          )
      )
    | Immutable => (((name, e)) => BLet(name, e))
    };
  List.map(map_fn, MatchCompiler.extract_bindings(pat, cexpr));
};

let transl_const =
    (c: Types.constant): either(imm_expression, (string, comp_expression)) =>
  switch (c) {
  | Const_number(n) => Right(("number", Comp.number(n)))
  | Const_int32(i) => Right(("int32", Comp.int32(i)))
  | Const_int64(i) => Right(("int64", Comp.int64(i)))
  | Const_float64(i) => Right(("float64", Comp.float64(i)))
  | Const_float32(i) => Right(("float32", Comp.float32(i)))
  | Const_bytes(b) => Right(("bytes", Comp.bytes(b)))
  | Const_string(s) => Right(("str", Comp.string(s)))
  | Const_char(c) => Right(("char", Comp.char(c)))
  | _ => Left(Imm.const(c))
  };

type item_get =
  | RecordPatGet(int, type_expr)
  | ArrayPatGet(int, type_expr)
  | TuplePatGet(int, type_expr);

let rec transl_imm =
        (
          ~boxed=false,
          {exp_desc, exp_loc: loc, exp_env: env, exp_type: typ, _} as e: expression,
        )
        : (imm_expression, list(anf_bind)) => {
  let allocation_type = get_allocation_type(env, typ);
  switch (exp_desc) {
  | TExpIdent(_, _, {val_kind: TValUnbound(_)}) =>
    failwith("Impossible: val_kind was unbound")
  | TExpIdent(
      Path.PExternal(Path.PIdent(mod_) as p, ident, _),
      _,
      {val_fullpath: Path.PExternal(_, original_name, _), val_mutable},
    ) =>
    let mod_decl = Env.find_module(p, None, env);
    let id =
      Imm.id(
        ~loc,
        ~env,
        lookup_symbol(~allocation_type, mod_, mod_decl, ident, original_name),
      );
    if (val_mutable && !boxed) {
      let tmp = gensym("unbox_mut");
      let setup = [
        BLet(tmp, Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id)),
      ];
      (Imm.id(~loc, ~env, tmp), setup);
    } else {
      (id, []);
    };
  | TExpIdent(
      Path.PExternal(Path.PIdent(mod_) as p, ident, _),
      _,
      {val_mutable},
    ) =>
    let mod_decl = Env.find_module(p, None, env);
    let id =
      Imm.id(
        ~loc,
        ~env,
        lookup_symbol(~allocation_type, mod_, mod_decl, ident, ident),
      );
    if (val_mutable && !boxed) {
      let tmp = gensym("unbox_mut");
      let setup = [
        BLet(tmp, Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id)),
      ];
      (Imm.id(~loc, ~env, tmp), setup);
    } else {
      (id, []);
    };
  | TExpIdent(Path.PExternal(_), _, _) =>
    failwith("NYI: transl_imm: TExpIdent with multiple PExternal")
  | TExpIdent(Path.PIdent(ident) as path, _, _) =>
    switch (Env.find_value(path, env)) {
    | {val_fullpath: Path.PIdent(_), val_mutable} =>
      let id = Imm.id(~loc, ~env, ident);
      if (val_mutable && !boxed) {
        let tmp = gensym("unbox_mut");
        let setup = [
          BLet(tmp, Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id)),
        ];
        (Imm.id(~loc, ~env, tmp), setup);
      } else {
        (id, []);
      };
    | {
        val_fullpath: Path.PExternal(Path.PIdent(mod_) as p, ident, _),
        val_mutable,
      } =>
      let mod_decl = Env.find_module(p, None, env);
      let id =
        Imm.id(
          ~loc,
          ~env,
          lookup_symbol(~allocation_type, mod_, mod_decl, ident, ident),
        );
      if (val_mutable && !boxed) {
        let tmp = gensym("unbox_mut");
        let setup = [
          BLet(tmp, Comp.prim1(~loc, ~env, ~allocation_type, UnboxBind, id)),
        ];
        (Imm.id(~loc, ~env, tmp), setup);
      } else {
        (id, []);
      };
    | {val_fullpath: Path.PExternal(_)} =>
      failwith("NYI: transl_imm: TExpIdent with multiple PExternal")
    }
  | TExpConstant(c) =>
    switch (transl_const(c)) {
    | Left(imm) => (imm, [])
    | Right((name, cexpr)) =>
      let tmp = gensym(name);
      (Imm.id(~loc, ~env, tmp), [BLet(tmp, cexpr)]);
    }
  | TExpNull => (Imm.const(~loc, ~env, Const_bool(false)), [])
  | TExpPrim1(op, arg) =>
    let tmp = gensym("unary");
    let (arg_imm, arg_setup) = transl_imm(arg);
    (
      Imm.id(~loc, ~env, tmp),
      arg_setup
      @ [BLet(tmp, Comp.prim1(~loc, ~env, ~allocation_type, op, arg_imm))],
    );
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
        ),
      ],
    );
  | TExpPrimN(op, args) =>
    let tmp = gensym("primn");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [BLet(tmp, Comp.primn(~loc, ~env, ~allocation_type, op, new_args))],
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
  | TExpApp(
      {exp_desc: TExpIdent(_, _, {val_kind: TValPrim("@throw")})},
      _,
    ) =>
    let (ans, ans_setup) = transl_comp_expression(e);
    (Imm.trap(~loc, ~env, ()), ans_setup @ [BSeq(ans)]);
  | TExpApp({exp_desc: TExpIdent(_, _, {val_kind: TValPrim(prim)})}, args) =>
    Translprim.(
      switch (PrimMap.find_opt(prim_map, prim), args) {
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
  | TExpLet(Nonrecursive, mut_flag, [{vb_expr, vb_pat}, ...rest]) =>
    /* TODO: Destructuring on letrec */
    let (exp_ans, exp_setup) = transl_comp_expression(vb_expr);
    let binds_setup = extract_bindings(mut_flag, vb_pat, exp_ans);
    let (body_ans, body_setup) =
      transl_imm({...e, exp_desc: TExpLet(Nonrecursive, mut_flag, rest)});
    (body_ans, exp_setup @ binds_setup @ body_setup);
  | TExpLet(Recursive, mut_flag, binds) =>
    if (mut_flag == Mutable) {
      failwith("mutable let rec");
    };
    let tmp = gensym("lam_letrec");
    let (binds, new_binds_setup) =
      List.split(
        List.map(
          ({vb_pat, vb_expr}) => (vb_pat, transl_comp_expression(vb_expr)),
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
        BLetRec(List.combine(names, new_binds)),
        BLet(tmp, Comp.imm(~allocation_type, Imm.const(Const_void))),
      ],
    );
  | TExpLambda([{mb_pat, mb_body: body}], _) =>
    let tmp = gensym("lam_lambda");
    let (lam, _) = transl_comp_expression(e);
    (Imm.id(~loc, ~env, tmp), [BLet(tmp, lam)]);
  | TExpLambda([], _) => failwith("Impossible: transl_imm: Empty lambda")
  | TExpLambda(_, _) => failwith("NYI: transl_imm: Multi-branch lambda")
  | TExpTuple(args) =>
    let tmp = gensym("tup");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [BLet(tmp, Comp.tuple(~loc, ~env, new_args))],
    );
  | TExpArray(args) =>
    let tmp = gensym("tup");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [BLet(tmp, Comp.array(~loc, ~env, new_args))],
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
        ),
      ],
    );
  | TExpRecord(args) =>
    let tmp = gensym("record");
    let definitions =
      Array.to_list @@ Array.map(((desc, def)) => def, args);
    let definitions =
      List.map(
        fun
        | Kept(_) => assert(false)
        | Overridden(name, def) => (name, def),
        definitions,
      );
    let (new_args, new_setup) =
      List.split(
        List.map(
          (({txt: name, loc}, expr)) => {
            let (var, setup) = transl_imm(expr);
            (
              (Location.mkloc(Identifier.string_of_ident(name), loc), var),
              setup,
            );
          },
          definitions,
        ),
      );
    let (typath, _, _) = Typepat.extract_concrete_record(env, typ);
    let ty_id = get_type_id(typath);
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
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
        ),
      ],
    );
  | TExpMatch(exp, branches, partial) =>
    let tmp = gensym("match");
    let (exp_ans, exp_setup) = transl_imm(exp);
    let (ans, setup) =
      MatchCompiler.compile_result(
        ~allocation_type,
        ~partial,
        Matchcomp.convert_match_branches(env, branches),
        transl_anf_expression,
        transl_imm,
        exp_ans,
      );
    (Imm.id(~loc, ~env, tmp), (exp_setup @ setup) @ [BLet(tmp, ans)]);
  | TExpConstruct(_) => failwith("NYI: transl_imm: Construct")
  };
}

and bind_patts =
    (~exported=false, ~mut_flag, pat: pattern): (Ident.t, list(anf_bind)) => {
  let postprocess_item = (cur, acc) =>
    switch (cur) {
    | None => acc
    | Some((ident, (src, idx), extras)) =>
      let access =
        switch (idx) {
        | RecordPatGet(idx, ty) =>
          Comp.record_get(
            ~allocation_type=get_allocation_type(pat.pat_env, ty),
            Int32.of_int(idx),
            Imm.id(src),
          )
        | TuplePatGet(idx, ty) =>
          Comp.tuple_get(
            ~allocation_type=get_allocation_type(pat.pat_env, ty),
            Int32.of_int(idx),
            Imm.id(src),
          )
        | ArrayPatGet(idx, ty) =>
          Comp.array_get(
            ~allocation_type=get_allocation_type(pat.pat_env, ty),
            Imm.const(Const_number(Const_number_int(Int64.of_int(idx)))),
            Imm.id(src),
          )
        };
      let binds =
        switch (mut_flag) {
        | Mutable =>
          let tmp = gensym("mut_bind_destructure");
          let boxed =
            Comp.prim1(
              ~allocation_type=get_allocation_type(pat.pat_env, pat.pat_type),
              BoxBind,
              Imm.id(tmp),
            );
          if (exported) {
            [
              BLet(tmp, access),
              BLetExport(Nonrecursive, [(ident, boxed)]),
            ];
          } else {
            [BLet(tmp, access), BLet(ident, boxed)];
          };
        | Immutable =>
          if (exported) {
            [BLetExport(Nonrecursive, [(ident, access)])];
          } else {
            [BLet(ident, access)];
          }
        };
      binds @ extras @ acc;
    };
  let postprocess = items => List.fold_right(postprocess_item, items, []);
  /* Pass one: Some(<identifier>, <access path>, <extra binds>)*/
  let rec anf_patts_pass_one = (src, i, {pat_desc, pat_extra}) => {
    switch (pat_extra) {
    | [] => ()
    | _ => failwith("NYI: anf_patts_pass_one: TPatConstraint")
    };
    switch (pat_desc) {
    | TPatVar(bind, _) => Some((bind, (src, i), []))
    | TPatAny => None
    | TPatTuple(patts) =>
      let tmp = gensym("tup_patt");
      Some((
        tmp,
        (src, i),
        postprocess @@
        List.mapi(
          (i, pat) =>
            anf_patts_pass_one(tmp, TuplePatGet(i, pat.pat_type), pat),
          patts,
        ),
      ));
    | TPatArray(patts) =>
      let tmp = gensym("arr_patt");
      Some((
        tmp,
        (src, i),
        postprocess @@
        List.mapi(
          (i, pat) =>
            anf_patts_pass_one(tmp, ArrayPatGet(i, pat.pat_type), pat),
          patts,
        ),
      ));
    | TPatRecord(fields, _) =>
      let tmp = gensym("rec_patt");
      Some((
        tmp,
        (src, i),
        postprocess @@
        List.map(
          ((_, ld, pat)) =>
            anf_patts_pass_one(
              tmp,
              RecordPatGet(ld.lbl_pos, pat.pat_type),
              pat,
            ),
          fields,
        ),
      ));
    | TPatConstant(_) => failwith("NYI: anf_patts_pass_one: TPatConstant")
    | TPatConstruct(_) => failwith("NYI: anf_patts_pass_one: TPatConstruct")
    | TPatOr(_) => failwith("NYI: anf_patts_pass_one: TPatOr")
    | TPatAlias(_) => failwith("NYI: anf_patts_pass_one: TPatAlias")
    };
  };
  let dummy_id = gensym("dummy");
  let dummy_idx = TuplePatGet(0, Builtin_types.type_void);
  switch (anf_patts_pass_one(dummy_id, dummy_idx, pat)) {
  | Some((tmp, _, binds)) => (tmp, binds)
  | None => failwith("Bind pattern was not destructable")
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
  | TExpBlock([stmt]) => transl_comp_expression(stmt)
  | TExpBlock([fst, ...rest]) =>
    let (fst_ans, fst_setup) = transl_comp_expression(fst);
    let (rest_ans, rest_setup) =
      transl_comp_expression({...e, exp_desc: TExpBlock(rest)});
    (rest_ans, fst_setup @ [BSeq(fst_ans)] @ rest_setup);
  | TExpLet(Nonrecursive, _, []) => (
      Comp.imm(~allocation_type, Imm.const(Const_void)),
      [],
    )

  | TExpLet(
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat: {pat_desc: TPatVar(bind, _)}}, ...rest],
    ) =>
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
    (body_ans, exp_setup @ [BLet(bind, exp_ans)] @ body_setup);
  | TExpLet(
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat: {pat_desc: TPatTuple(_)} as pat}, ...rest],
    )
  | TExpLet(
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat: {pat_desc: TPatRecord(_)} as pat}, ...rest],
    ) =>
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let (exp_ans, exp_setup) = transl_comp_expression(vb_expr);

    /* Extract items from destructure */
    let (tmp, anf_patts) = bind_patts(~mut_flag, pat);

    let (body_ans, body_setup) =
      transl_comp_expression({
        ...e,
        exp_desc: TExpLet(Nonrecursive, mut_flag, rest),
      });
    (body_ans, exp_setup @ [BLet(tmp, exp_ans), ...anf_patts] @ body_setup);

  | TExpLet(Nonrecursive, _, [_, ..._]) => failwith("Impossible by pre_anf")
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
      List.concat(new_setup) @ [BLetRec(List.combine(names, new_binds))],
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
      when Identifier.equal(ident, Identifier.IdentName("()")) =>
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
              MatchCompiler.extract_bindings(
                arg,
                Comp.imm(
                  ~loc,
                  ~env,
                  ~allocation_type=
                    get_allocation_type(arg.pat_env, arg.pat_type),
                  Imm.id(~loc, ~env, tmp),
                ),
              );
            (
              [
                (tmp, get_allocation_type(arg.pat_env, arg.pat_type)),
                ...anf_args,
              ],
              List.fold_right(
                (bind, body) =>
                  AExp.let_(~loc, ~env, Nonrecursive, [bind], body),
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
        (new_func, ([HeapAllocated], StackAllocated(WasmI32))),
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
      MatchCompiler.compile_result(
        ~allocation_type,
        ~partial,
        Matchcomp.convert_match_branches(env, branches),
        transl_anf_expression,
        transl_imm,
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
      | BSeq(exp) => AExp.seq(~loc, ~env, exp, body)
      | BLet(name, exp) =>
        AExp.let_(~loc, ~env, Nonrecursive, [(name, exp)], body)
      | BLetRec(names) => AExp.let_(~loc, ~env, Recursive, names, body)
      | BLetExport(name, exp) => failwith("Global bind at non-toplevel")
      },
    ans_setup,
    AExp.comp(~loc, ~env, ans),
  );
};

let bind_constructor =
    (env, loc, ty_id, (cd_id, {cstr_name, cstr_tag, cstr_args})) => {
  let compile_constant_constructor = () => {
    let compiled_tag = compile_constructor_tag(cstr_tag);
    Comp.adt(
      ~loc,
      ~env,
      Imm.const(
        ~loc,
        ~env,
        Const_number(Const_number_int(Int64.of_int(ty_id))),
      ),
      Imm.const(
        ~loc,
        ~env,
        Const_number(Const_number_int(Int64.of_int(compiled_tag))),
      ),
      [],
    );
  };
  let rhs =
    switch (cstr_tag) {
    | CstrConstant(_) => compile_constant_constructor()
    | CstrExtension(_, _, CstrExtensionConstant) =>
      compile_constant_constructor()
    | CstrBlock(_)
    | CstrExtension(_) =>
      let compiled_tag = compile_constructor_tag(cstr_tag);
      let args =
        List.map(
          ty => (gensym("constr_arg"), get_allocation_type(env, ty)),
          cstr_args,
        );
      let arg_ids = List.map(((a, _)) => Imm.id(~loc, ~env, a), args);
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
      Comp.lambda(
        ~loc,
        ~env,
        args,
        (
          AExp.comp(
            ~loc,
            ~env,
            Comp.adt(~loc, ~env, imm_tytag, imm_tag, arg_ids),
          ),
          HeapAllocated,
        ),
      );
    | CstrUnboxed => failwith("NYI: ANF CstrUnboxed")
    };
  BLetExport(Nonrecursive, [(cd_id, rhs)]);
};

let linearize_decl = (env, loc, typath, decl) => {
  /* FIXME: [philip] This is kind of hacky...would be better to store this in the Env directly...not to mention,
     I think this'll be much more fragile than if it were in the static info */
  let ty_id = get_type_id(typath);
  let descrs = Datarepr.constructors_of_type(typath, decl);
  List.map(bind_constructor(env, loc, ty_id), descrs);
};

let linearize_exception = (env, ext) => {
  let ty_id = get_type_id(ext.ext_type.ext_type_path);
  let id = ext.ext_id;
  let loc = ext.ext_loc;
  let ext = Datarepr.extension_descr(Path.PIdent(id), ext.ext_type);
  [bind_constructor(env, loc, ty_id, (id, ext))];
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
  | TTopLet(_, _, _, []) => (None, [])
  | TTopLet(
      export_flag,
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat}, ...rest],
    ) =>
    let vb_expr = {
      ...vb_expr,
      exp_attributes: attributes @ vb_expr.exp_attributes,
    };
    let exported = export_flag == Exported;
    let name =
      if (exported) {
        switch (vb_pat.pat_desc) {
        | TPatVar(bind, _)
        | TPatAlias(_, bind, _) => Some(Ident.name(bind))
        | _ => None
        };
      } else {
        None;
      };
    let (exp_ans, exp_setup) = transl_comp_expression(~name?, vb_expr);
    let (rest_setup, rest_imp) =
      transl_anf_statement({
        ...s,
        ttop_desc: TTopLet(export_flag, Nonrecursive, mut_flag, rest),
      });
    let rest_setup = Option.value(~default=[], rest_setup);
    let setup =
      switch (vb_pat.pat_desc) {
      | TPatVar(bind, _)
      | TPatAlias({pat_desc: TPatAny}, bind, _) =>
        switch (mut_flag) {
        | Mutable =>
          let tmp = gensym("mut_bind_destructure");
          let boxed =
            Comp.prim1(
              ~allocation_type=
                get_allocation_type(vb_expr.exp_env, vb_expr.exp_type),
              BoxBind,
              Imm.id(tmp),
            );
          if (exported) {
            [
              BLet(tmp, exp_ans),
              BLetExport(Nonrecursive, [(bind, boxed)]),
            ];
          } else {
            [BLet(tmp, exp_ans), BLet(bind, boxed)];
          };
        | Immutable =>
          if (exported) {
            [BLetExport(Nonrecursive, [(bind, exp_ans)])];
          } else {
            [BLet(bind, exp_ans)];
          }
        }
      | TPatTuple(_)
      | TPatRecord(_) =>
        let (tmp, anf_patts) = bind_patts(~exported, ~mut_flag, vb_pat);
        [BLet(tmp, exp_ans), ...anf_patts];
      | TPatAlias(patt, bind, _) =>
        let binds =
          switch (mut_flag) {
          | Mutable =>
            let tmp = gensym("mut_bind_destructure");
            let boxed =
              Comp.prim1(
                ~allocation_type=
                  get_allocation_type(patt.pat_env, patt.pat_type),
                BoxBind,
                Imm.id(tmp),
              );
            if (exported) {
              [
                BLet(tmp, exp_ans),
                BLetExport(Nonrecursive, [(bind, boxed)]),
              ];
            } else {
              [BLet(tmp, exp_ans), BLet(bind, boxed)];
            };
          | Immutable =>
            if (exported) {
              [BLetExport(Nonrecursive, [(bind, exp_ans)])];
            } else {
              [BLet(bind, exp_ans)];
            }
          };
        let (tmp, anf_patts) = bind_patts(~exported, ~mut_flag, patt);
        binds @ [BLet(tmp, exp_ans), ...anf_patts];
      | TPatAny => [BSeq(exp_ans)]
      | _ =>
        failwith(
          "NYI: transl_anf_statement: Non-record or non-tuple destructuring in let",
        )
      };
    (Some(exp_setup @ setup @ rest_setup), rest_imp);
  | TTopLet(export_flag, Recursive, mut_flag, binds) =>
    let exported = export_flag == Exported;
    let (binds, new_binds_setup) =
      List.split(
        List.map(
          ({vb_pat, vb_expr}) => {
            let name =
              if (exported) {
                switch (vb_pat.pat_desc) {
                | TPatVar(bind, _)
                | TPatAlias(_, bind, _) => Some(Ident.name(bind))
                | _ => None
                };
              } else {
                None;
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

    switch (export_flag) {
    | Exported => (
        Some(
          List.concat(new_setup)
          @ [BLetExport(Recursive, List.combine(names, new_binds))],
        ),
        [],
      )
    | Nonexported => (
        Some(
          List.concat(new_setup)
          @ [BLetRec(List.combine(names, new_binds))],
        ),
        [],
      )
    };
  | TTopExpr(expr) =>
    let expr = {...expr, exp_attributes: attributes};
    let (comp, setup) = transl_comp_expression(expr);
    (Some(setup @ [BSeq(comp)]), []);
  | TTopData(decls) =>
    open Types;
    let bindings =
      List.concat @@
      List.map(
        decl =>
          switch (decl.data_kind) {
          | TDataVariant(_) =>
            let typath = Path.PIdent(decl.data_id);
            linearize_decl(env, loc, typath, decl.data_type);
          | TDataAbstract
          | TDataRecord(_) => []
          },
        decls,
      );
    if (List.length(bindings) > 0) {
      (Some(bindings), []);
    } else {
      (None, []);
    };
  | TTopException(_, ext) => (Some(linearize_exception(env, ext)), [])
  | TTopForeign(exported, desc) =>
    let global =
      switch (exported) {
      | Exported => Global
      | Nonexported => Nonglobal
      };
    let external_name =
      List.fold_left(
        name =>
          fun
          | External_name(name) => name
          | _ => name,
        desc.tvd_name.txt,
        attributes,
      );
    switch (desc.tvd_desc.ctyp_type.desc) {
    | TTyArrow(_) =>
      let (argsty, retty) =
        get_fn_allocation_type(env, desc.tvd_desc.ctyp_type);
      let retty =
        if (returns_void(desc.tvd_desc.ctyp_type)) {
          [];
        } else {
          [retty];
        };
      (
        None,
        [
          Imp.wasm_func(
            ~global,
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
            ~global,
            desc.tvd_id,
            desc.tvd_mod.txt,
            external_name,
            GlobalShape(ty),
          ),
        ],
      );
    };
  | _ => (None, [])
  };

let linearize_builtins = (env, builtins) => {
  let bindings =
    List.concat @@
    List.map(
      decl =>
        switch (decl.type_kind) {
        | TDataVariant(_) when decl.type_allocation == HeapAllocated =>
          linearize_decl(env, Location.dummy_loc, decl.type_path, decl)
        | _ => []
        },
      builtins,
    );
  (bindings, []);
};

let gather_type_metadata = statements => {
  List.fold_left(
    (metadata, {ttop_desc}) => {
      switch (ttop_desc) {
      | TTopData(decls) =>
        let info =
          List.filter_map(
            decl => {
              let typath = Path.PIdent(decl.data_id);
              let id = get_type_id(typath);
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
      | TTopException(_, ext) =>
        let ty_id = get_type_id(ext.ext_type.ext_type_path);
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
  PathMap.clear(type_map);
  value_imports := [];
  symbol_table := Ident.empty;
  let prog_setup = linearize_builtins(env, Builtin_types.builtin_decls);
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
      prog_setup,
      List.map(transl_anf_statement, statements),
    );
  let imports = List.rev(imports);
  let body = convert_binds(top_binds);
  let imports = imports @ value_imports^;
  let type_metadata = gather_type_metadata(statements);
  {body, env, imports, signature, type_metadata, analyses: ref([])};
};

let () = Matchcomp.compile_constructor_tag := compile_constructor_tag;
