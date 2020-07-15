open Grain_parsing;
open Grain_typed;
open Types;
open Typedtree;
open Anftree;
open Anf_helper;

module MatchCompiler = Matchcomp.MatchTreeCompiler;

let compile_constructor_tag =
  Grain_typed.Types.(
    fun
    | CstrConstant(i) => i
    | CstrBlock(i) => i
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

let lookup_symbol = (mod_, mod_decl, name, original_name) => {
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
          Imp.grain_value(fresh, filepath, original_name, GlobalShape),
          ...value_imports^,
        ]
    | None => ()
    };
    symbol_table :=
      Ident.add(mod_, Ident.add(fresh, fresh, modtbl), symbol_table^);
    fresh;
  };
};

type anf_bind =
  | BSeq(comp_expression)
  | BLet(Ident.t, comp_expression)
  | BLetRec(list((Ident.t, comp_expression)))
  | BLetExport(rec_flag, list((Ident.t, comp_expression)));

type either('a, 'b) =
  | Left('a)
  | Right('b);

let extract_bindings = (mut_flag, pat, cexpr) => {
  let get_imm =
    fun
    | CImmExpr(imm) => imm
    | _ => failwith("MatchCompiler returned non-immediate for binding");
  let map_fn =
    switch (mut_flag) {
    | Mutable => (
        ((name, e)) =>
          BLet(name, {...e, comp_desc: CPrim1(Box, get_imm(e.comp_desc))})
      )
    | Immutable => (((name, e)) => BLet(name, e))
    };
  List.map(map_fn, MatchCompiler.extract_bindings(pat, cexpr));
};

let transl_const =
    (c: Types.constant): either(imm_expression, (string, comp_expression)) =>
  switch (c) {
  | Const_int32(i) => [@implicit_arity] Right("int32", Comp.int32(i))
  | Const_int64(i) => [@implicit_arity] Right("int64", Comp.int64(i))
  | Const_string(s) => [@implicit_arity] Right("str", Comp.string(s))
  | Const_float(_) => failwith("NYI: helpIConst: float")
  | _ => Left(Imm.const(c))
  };

type item_get =
  | RecordPatGet(int)
  | TuplePatGet(int);

let rec transl_imm =
        (
          ~boxed=false,
          {exp_desc, exp_loc: loc, exp_env: env, exp_type: typ, _} as e: expression,
        )
        : (imm_expression, list(anf_bind)) =>
  switch (exp_desc) {
  | [@implicit_arity] TExpIdent(_, _, {val_kind: TValUnbound(_)}) =>
    failwith("Impossible: val_kind was unbound")
  | [@implicit_arity]
    TExpIdent(
      [@implicit_arity] Path.PExternal(Path.PIdent(mod_) as p, ident, _),
      _,
      {
        val_fullpath: [@implicit_arity] Path.PExternal(_, original_name, _),
        val_mutable,
      },
    ) =>
    let mod_decl = Env.find_module(p, None, env);
    let id =
      Imm.id(
        ~loc,
        ~env,
        lookup_symbol(mod_, mod_decl, ident, original_name),
      );
    if (val_mutable && !boxed) {
      let tmp = gensym("unbox_mut");
      let setup = [BLet(tmp, Comp.prim1(~loc, ~env, Unbox, id))];
      (Imm.id(~loc, ~env, tmp), setup);
    } else {
      (id, []);
    };
  | [@implicit_arity]
    TExpIdent(
      [@implicit_arity] Path.PExternal(Path.PIdent(mod_) as p, ident, _),
      _,
      {val_mutable},
    ) =>
    let mod_decl = Env.find_module(p, None, env);
    let id = Imm.id(~loc, ~env, lookup_symbol(mod_, mod_decl, ident, ident));
    if (val_mutable && !boxed) {
      let tmp = gensym("unbox_mut");
      let setup = [BLet(tmp, Comp.prim1(~loc, ~env, Unbox, id))];
      (Imm.id(~loc, ~env, tmp), setup);
    } else {
      (id, []);
    };
  | [@implicit_arity] TExpIdent(Path.PExternal(_), _, _) =>
    failwith("NYI: transl_imm: TExpIdent with multiple PExternal")
  | [@implicit_arity] TExpIdent(Path.PIdent(ident) as path, _, _) =>
    switch (Env.find_value(path, env)) {
    | {val_fullpath: Path.PIdent(_), val_mutable} =>
      let id = Imm.id(~loc, ~env, ident);
      if (val_mutable && !boxed) {
        let tmp = gensym("unbox_mut");
        let setup = [BLet(tmp, Comp.prim1(~loc, ~env, Unbox, id))];
        (Imm.id(~loc, ~env, tmp), setup);
      } else {
        (id, []);
      };
    | {
        val_fullpath:
          [@implicit_arity] Path.PExternal(Path.PIdent(mod_) as p, ident, _),
        val_mutable,
      } =>
      let mod_decl = Env.find_module(p, None, env);
      let id =
        Imm.id(~loc, ~env, lookup_symbol(mod_, mod_decl, ident, ident));
      if (val_mutable && !boxed) {
        let tmp = gensym("unbox_mut");
        let setup = [BLet(tmp, Comp.prim1(~loc, ~env, Unbox, id))];
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
    | [@implicit_arity] Right(name, cexpr) =>
      let tmp = gensym(name);
      (Imm.id(~loc, ~env, tmp), [[@implicit_arity] BLet(tmp, cexpr)]);
    }
  | TExpNull => (Imm.const(~loc, ~env, Const_bool(false)), [])
  | [@implicit_arity] TExpPrim1(op, arg) =>
    let tmp = gensym("unary");
    let (arg_imm, arg_setup) = transl_imm(arg);
    (
      Imm.id(~loc, ~env, tmp),
      arg_setup
      @ [[@implicit_arity] BLet(tmp, Comp.prim1(~loc, ~env, op, arg_imm))],
    );
  | [@implicit_arity] TExpPrim2(And, left, right) =>
    let tmp = gensym("boolBinop");
    let (left_imm, left_setup) = transl_imm(left);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ [
        [@implicit_arity]
        BLet(
          tmp,
          Comp.if_(
            ~loc,
            ~env,
            left_imm,
            transl_anf_expression(right),
            AExp.comp(~loc, ~env, Comp.imm(~loc, ~env, left_imm)),
          ),
        ),
      ],
    );
  | [@implicit_arity] TExpPrim2(Or, left, right) =>
    let tmp = gensym("boolBinop");
    let (left_imm, left_setup) = transl_imm(left);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ [
        [@implicit_arity]
        BLet(
          tmp,
          Comp.if_(
            ~loc,
            ~env,
            left_imm,
            AExp.comp(~loc, ~env, Comp.imm(~loc, ~env, left_imm)),
            transl_anf_expression(right),
          ),
        ),
      ],
    );
  | [@implicit_arity] TExpPrim2(op, left, right) =>
    let tmp = gensym("binop");
    let (left_imm, left_setup) = transl_imm(left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ right_setup
      @ [
        [@implicit_arity]
        BLet(tmp, Comp.prim2(~loc, ~env, op, left_imm, right_imm)),
      ],
    );
  | [@implicit_arity] TExpBoxAssign(left, right) =>
    let tmp = gensym("assign");
    let (left_imm, left_setup) = transl_imm(left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ right_setup
      @ [
        [@implicit_arity]
        BLet(tmp, Comp.box_assign(~loc, ~env, left_imm, right_imm)),
      ],
    );
  | [@implicit_arity] TExpAssign(left, right) =>
    let tmp = gensym("assign");
    let (left_imm, left_setup) = transl_imm(~boxed=true, left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Imm.id(~loc, ~env, tmp),
      left_setup
      @ right_setup
      @ [
        [@implicit_arity]
        BLet(tmp, Comp.assign(~loc, ~env, left_imm, right_imm)),
      ],
    );
  | [@implicit_arity] TExpIf(cond, _then, _else) =>
    let tmp = gensym("if");
    let (cond_imm, cond_setup) = transl_imm(cond);
    (
      Imm.id(~loc, ~env, tmp),
      cond_setup
      @ [
        [@implicit_arity]
        BLet(
          tmp,
          Comp.if_(
            ~loc,
            ~env,
            cond_imm,
            transl_anf_expression(_then),
            transl_anf_expression(_else),
          ),
        ),
      ],
    );
  | [@implicit_arity] TExpWhile(cond, body) =>
    let tmp = gensym("while");
    (
      Imm.id(~loc, ~env, tmp),
      [
        [@implicit_arity]
        BLet(
          tmp,
          Comp.while_(
            ~loc,
            ~env,
            transl_anf_expression(cond),
            transl_anf_expression(body),
          ),
        ),
      ],
    );
  | [@implicit_arity] TExpApp(func, args) =>
    let tmp = gensym("app");
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      (func_setup @ List.concat(new_setup))
      @ [
        [@implicit_arity]
        BLet(tmp, Comp.app(~loc, ~env, new_func, new_args)),
      ],
    );
  | TExpBlock([]) => failwith("Impossible by syntax")
  | TExpBlock([stmt]) => transl_imm(stmt)
  | TExpBlock([fst, ...rest]) =>
    let (fst_ans, fst_setup) = transl_comp_expression(fst);
    let (rest_ans, rest_setup) =
      transl_imm({...e, exp_desc: TExpBlock(rest)});
    (rest_ans, fst_setup @ [BSeq(fst_ans)] @ rest_setup);
  | [@implicit_arity] TExpLet(Nonrecursive, _, [], body) => transl_imm(body)
  | [@implicit_arity]
    TExpLet(Nonrecursive, mut_flag, [{vb_expr, vb_pat}, ...rest], body) =>
    /* TODO: Destructuring on letrec */
    let (exp_ans, exp_setup) = transl_comp_expression(vb_expr);
    let binds_setup = extract_bindings(mut_flag, vb_pat, exp_ans);
    let (body_ans, body_setup) =
      transl_imm({
        ...e,
        exp_desc:
          [@implicit_arity] TExpLet(Nonrecursive, mut_flag, rest, body),
      });
    (body_ans, exp_setup @ binds_setup @ body_setup);
  | [@implicit_arity] TExpLet(Recursive, mut_flag, binds, body) =>
    if (mut_flag == Mutable) {
      failwith("mutable let rec");
    };
    let tmp = gensym("lam");
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
        | {pat_desc: [@implicit_arity] TPatVar(id, _)} => id
        | _ => failwith("Non-name not allowed on LHS of let rec."),
        binds,
      );

    let (body_ans, body_setup) = transl_comp_expression(body);
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [BLetRec(List.combine(names, new_binds))]
      @ body_setup
      @ [[@implicit_arity] BLet(tmp, body_ans)],
    );
  | [@implicit_arity] TExpLambda([{mb_pat, mb_body: body}], _) =>
    let tmp = gensym("lam");
    let rec args = mb_pat =>
      switch (mb_pat.pat_desc) {
      | TPatTuple(args) => (
          List.map(
            fun
            | {pat_desc: [@implicit_arity] TPatVar(id, _)} => id
            | _ =>
              failwith("NYI: transl_imm: Destructuring in lambda argument"),
            args,
          ),
          [],
        )
      | [@implicit_arity] TPatVar(v, _) => ([v], [])
      | [@implicit_arity] TPatConstruct({txt: ident}, _, [])
          when Identifier.equal(ident, Identifier.IdentName("()")) => (
          [],
          [],
        )
      | TPatAny => ([gensym("func_arg")], [])
      | [@implicit_arity] TPatAlias(pat, id, _) =>
        let (arglist, extras) = args(pat);
        /* NOTE: This is assuming that arglist has length one.
           When full pattern matching is supported, that will
           handle this instead */
        (arglist, [(List.hd(arglist), id), ...extras]);
      | _ =>
        failwith(
          "Impossible: transl_imm: Lambda contained non-tuple/var pattern",
        )
      };
    let (args, extras) = args(mb_pat);
    let anf_body = transl_anf_expression(body);
    let anf_body =
      List.fold_left(
        (body, (a, b)) =>
          AExp.let_(
            ~loc,
            ~env,
            Nonrecursive,
            [(b, Comp.imm(~loc, ~env, Imm.id(~loc, ~env, a)))],
            body,
          ),
        anf_body,
        extras,
      );
    (
      Imm.id(~loc, ~env, tmp),
      [
        [@implicit_arity] BLet(tmp, Comp.lambda(~loc, ~env, args, anf_body)),
      ],
    );
  | [@implicit_arity] TExpLambda([], _) =>
    failwith("Impossible: transl_imm: Empty lambda")
  | [@implicit_arity] TExpLambda(_, _) =>
    failwith("NYI: transl_imm: Multi-branch lambda")
  | TExpTuple(args) =>
    let tmp = gensym("tup");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [[@implicit_arity] BLet(tmp, Comp.tuple(~loc, ~env, new_args))],
    );
  | TExpArray(args) =>
    let tmp = gensym("tup");
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Imm.id(~loc, ~env, tmp),
      List.concat(new_setup)
      @ [[@implicit_arity] BLet(tmp, Comp.array(~loc, ~env, new_args))],
    );
  | [@implicit_arity] TExpArrayGet(arr, idx) =>
    let tmp = gensym("array_access");
    let (arr_var, arr_setup) = transl_imm(arr);
    let (idx_var, idx_setup) = transl_imm(idx);
    (
      Imm.id(~loc, ~env, tmp),
      arr_setup
      @ idx_setup
      @ [
        [@implicit_arity]
        BLet(tmp, Comp.array_get(~loc, ~env, idx_var, arr_var)),
      ],
    );
  | [@implicit_arity] TExpArraySet(arr, idx, arg) =>
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
        [@implicit_arity]
        BLet(tmp, Comp.array_set(~loc, ~env, idx_var, arr_var, arg_var)),
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
        | [@implicit_arity] Overridden(name, def) => (name, def),
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
        [@implicit_arity]
        BLet(
          tmp,
          Comp.record(
            ~loc,
            ~env,
            Imm.const(~loc, ~env, Const_int(ty_id)),
            new_args,
          ),
        ),
      ],
    );
  | [@implicit_arity] TExpRecordGet(expr, field, ld) =>
    let tmp = gensym("field");
    let (var, setup) = transl_imm(expr);
    (
      Imm.id(~loc, ~env, tmp),
      setup
      @ [
        [@implicit_arity]
        BLet(
          tmp,
          Comp.record_get(~loc, ~env, Int32.of_int(ld.lbl_pos), var),
        ),
      ],
    );
  | [@implicit_arity] TExpRecordSet(expr, field, ld, arg) =>
    let tmp = gensym("field_set");
    let (record, rec_setup) = transl_imm(expr);
    let (arg, arg_setup) = transl_imm(arg);
    (
      Imm.id(~loc, ~env, tmp),
      rec_setup
      @ arg_setup
      @ [
        [@implicit_arity]
        BLet(
          tmp,
          Comp.record_set(~loc, ~env, Int32.of_int(ld.lbl_pos), record, arg),
        ),
      ],
    );
  | [@implicit_arity] TExpMatch(exp, branches, _) =>
    let tmp = gensym("match");
    let (exp_ans, exp_setup) = transl_imm(exp);
    let (ans, setup) =
      MatchCompiler.compile_result(
        Matchcomp.convert_match_branches(branches),
        transl_anf_expression,
        exp_ans,
      );
    (
      Imm.id(~loc, ~env, tmp),
      (
        exp_setup
        @ List.map(((n, e)) => [@implicit_arity] BLet(n, e), setup)
      )
      @ [[@implicit_arity] BLet(tmp, ans)],
    );
  | TExpConstruct(_) => failwith("NYI: transl_imm: Construct")
  }

and bind_patts =
    (~exported=false, ~mut_flag, pat: pattern): (Ident.t, list(anf_bind)) => {
  let postprocess_item = (cur, acc) =>
    switch (cur) {
    | None => acc
    | Some((ident, (src, idx), extras)) =>
      let access =
        switch (idx) {
        | RecordPatGet(idx) =>
          Comp.record_get(Int32.of_int(idx), Imm.id(src))
        | TuplePatGet(idx) => Comp.tuple_get(Int32.of_int(idx), Imm.id(src))
        };
      let binds =
        switch (mut_flag) {
        | Mutable =>
          let tmp = gensym("mut_bind_destructure");
          let boxed = Comp.prim1(Box, Imm.id(tmp));
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
    | [@implicit_arity] TPatVar(bind, _) => Some((bind, (src, i), []))
    | TPatAny => None
    | TPatTuple(patts) =>
      let tmp = gensym("tup_patt");
      Some((
        tmp,
        (src, i),
        postprocess @@
        List.mapi(
          (i, pat) => anf_patts_pass_one(tmp, TuplePatGet(i), pat),
          patts,
        ),
      ));
    | [@implicit_arity] TPatRecord(fields, _) =>
      let tmp = gensym("rec_patt");
      Some((
        tmp,
        (src, i),
        postprocess @@
        List.map(
          ((_, ld, pat)) =>
            anf_patts_pass_one(tmp, RecordPatGet(ld.lbl_pos), pat),
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
  let dummy_idx = TuplePatGet(0);
  switch (anf_patts_pass_one(dummy_id, dummy_idx, pat)) {
  | Some((tmp, _, binds)) => (tmp, binds)
  | None => failwith("Bind pattern was not destructable")
  };
}

and transl_comp_expression =
    ({exp_desc, exp_loc: loc, exp_env: env, _} as e: expression)
    : (comp_expression, list(anf_bind)) =>
  switch (exp_desc) {
  | [@implicit_arity] TExpPrim1(op, arg) =>
    let (arg_imm, arg_setup) = transl_imm(arg);
    (Comp.prim1(~loc, ~env, op, arg_imm), arg_setup);
  | [@implicit_arity] TExpPrim2(And, left, right) =>
    let (left_imm, left_setup) = transl_imm(left);
    (
      Comp.if_(
        ~loc,
        ~env,
        left_imm,
        transl_anf_expression(right),
        AExp.comp(~loc, ~env, Comp.imm(~loc, ~env, left_imm)),
      ),
      left_setup,
    );
  | [@implicit_arity] TExpPrim2(Or, left, right) =>
    let (left_imm, left_setup) = transl_imm(left);
    (
      Comp.if_(
        ~loc,
        ~env,
        left_imm,
        AExp.comp(~loc, ~env, Comp.imm(~loc, ~env, left_imm)),
        transl_anf_expression(right),
      ),
      left_setup,
    );
  | [@implicit_arity] TExpPrim2(op, left, right) =>
    let (left_imm, left_setup) = transl_imm(left);
    let (right_imm, right_setup) = transl_imm(right);
    (
      Comp.prim2(~loc, ~env, op, left_imm, right_imm),
      left_setup @ right_setup,
    );
  | [@implicit_arity] TExpIf(cond, _then, _else) =>
    let (cond_imm, cond_setup) = transl_imm(cond);
    (
      Comp.if_(
        ~loc,
        ~env,
        cond_imm,
        transl_anf_expression(_then),
        transl_anf_expression(_else),
      ),
      cond_setup,
    );
  | TExpBlock([]) => failwith("Impossible by syntax")
  | TExpBlock([stmt]) => transl_comp_expression(stmt)
  | TExpBlock([fst, ...rest]) =>
    let (fst_ans, fst_setup) = transl_comp_expression(fst);
    let (rest_ans, rest_setup) =
      transl_comp_expression({...e, exp_desc: TExpBlock(rest)});
    (rest_ans, fst_setup @ [BSeq(fst_ans)] @ rest_setup);
  | [@implicit_arity] TExpLet(Nonrecursive, _, [], body) =>
    transl_comp_expression(body)

  | [@implicit_arity]
    TExpLet(
      Nonrecursive,
      mut_flag,
      [
        {vb_expr, vb_pat: {pat_desc: [@implicit_arity] TPatVar(bind, _)}},
        ...rest,
      ],
      body,
    ) =>
    let (exp_ans, exp_setup) =
      if (mut_flag == Mutable) {
        let (imm, imm_setup) = transl_imm(vb_expr);
        (Comp.prim1(Box, imm), imm_setup);
      } else {
        transl_comp_expression(vb_expr);
      };
    let (body_ans, body_setup) =
      transl_comp_expression({
        ...e,
        exp_desc:
          [@implicit_arity] TExpLet(Nonrecursive, mut_flag, rest, body),
      });
    (
      body_ans,
      exp_setup @ [[@implicit_arity] BLet(bind, exp_ans)] @ body_setup,
    );
  | [@implicit_arity]
    TExpLet(
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat: {pat_desc: TPatTuple(_)} as pat}, ...rest],
      body,
    )
  | [@implicit_arity]
    TExpLet(
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat: {pat_desc: TPatRecord(_)} as pat}, ...rest],
      body,
    ) =>
    let (exp_ans, exp_setup) = transl_comp_expression(vb_expr);

    /* Extract items from destructure */
    let (tmp, anf_patts) = bind_patts(~mut_flag, pat);

    let (body_ans, body_setup) =
      transl_comp_expression({
        ...e,
        exp_desc:
          [@implicit_arity] TExpLet(Nonrecursive, mut_flag, rest, body),
      });
    (
      body_ans,
      exp_setup
      @ [[@implicit_arity] BLet(tmp, exp_ans), ...anf_patts]
      @ body_setup,
    );

  | [@implicit_arity] TExpLet(Nonrecursive, _, [_, ..._], _) =>
    failwith("Impossible by pre_anf")
  | [@implicit_arity] TExpLet(Recursive, mut_flag, binds, body) =>
    if (mut_flag == Mutable) {
      failwith("mutable let rec");
    };
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
        | {pat_desc: [@implicit_arity] TPatVar(id, _)} => id
        | _ => failwith("Non-name not allowed on LHS of let rec."),
        binds,
      );

    let (body_ans, body_setup) = transl_comp_expression(body);
    (
      body_ans,
      List.concat(new_setup)
      @ [BLetRec(List.combine(names, new_binds)), ...body_setup],
    );
  | [@implicit_arity] TExpLambda([{mb_pat, mb_body: body}], _) =>
    let rec args = mb_pat =>
      switch (mb_pat.pat_desc) {
      | TPatTuple(args) => (
          List.map(
            fun
            | {pat_desc: [@implicit_arity] TPatVar(id, _)} => id
            | _ =>
              failwith("NYI: transl_imm: Destructuring in lambda argument"),
            args,
          ),
          [],
        )
      | [@implicit_arity] TPatVar(v, _) => ([v], [])
      | [@implicit_arity] TPatConstruct({txt: ident}, _, [])
          when Identifier.equal(ident, Identifier.IdentName("()")) => (
          [],
          [],
        )
      | TPatAny => ([gensym("func_arg")], [])
      | [@implicit_arity] TPatAlias(pat, id, _) =>
        let (arglist, extras) = args(pat);
        /* NOTE: This is assuming that arglist has length one.
           When full pattern matching is supported, that will
           handle this instead */
        (arglist, [(List.hd(arglist), id), ...extras]);
      | _ =>
        failwith(
          "Impossible: transl_imm: Lambda contained non-tuple/var pattern",
        )
      };
    let (args, extras) = args(mb_pat);
    let anf_body = transl_anf_expression(body);
    let anf_body =
      List.fold_left(
        (body, (a, b)) =>
          AExp.let_(
            ~loc,
            ~env,
            Nonrecursive,
            [(b, Comp.imm(~loc, ~env, Imm.id(~loc, ~env, a)))],
            body,
          ),
        anf_body,
        extras,
      );
    (Comp.lambda(~loc, ~env, args, anf_body), []);
  | [@implicit_arity] TExpLambda([], _) =>
    failwith("transl_comp_expression: impossible: empty lambda")
  | [@implicit_arity] TExpLambda(_, _) =>
    failwith("transl_comp_expression: NYI: multi-branch lambda")
  | [@implicit_arity] TExpApp(func, args) =>
    let (new_func, func_setup) = transl_imm(func);
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (
      Comp.app(~loc, ~env, new_func, new_args),
      func_setup @ List.concat(new_setup),
    );
  | TExpTuple(args) =>
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (Comp.tuple(~loc, ~env, new_args), List.concat(new_setup));
  | TExpArray(args) =>
    let (new_args, new_setup) = List.split(List.map(transl_imm, args));
    (Comp.array(~loc, ~env, new_args), List.concat(new_setup));
  | [@implicit_arity] TExpMatch(expr, branches, _) =>
    let (exp_ans, exp_setup) = transl_imm(expr);
    let (ans, setup) =
      MatchCompiler.compile_result(
        Matchcomp.convert_match_branches(branches),
        transl_anf_expression,
        exp_ans,
      );
    (
      ans,
      exp_setup @ List.map(((n, e)) => [@implicit_arity] BLet(n, e), setup),
    );
  | _ =>
    let (imm, setup) = transl_imm(e);
    (Comp.imm(~loc, ~env, imm), setup);
  }

and transl_anf_expression =
    ({exp_desc, exp_loc: loc, exp_env: env, _} as e: expression)
    : anf_expression => {
  let (ans, ans_setup) = transl_comp_expression(e);
  List.fold_right(
    (bind, body) =>
      switch (bind) {
      | BSeq(exp) => AExp.seq(~loc, ~env, exp, body)
      | [@implicit_arity] BLet(name, exp) =>
        AExp.let_(~loc, ~env, Nonrecursive, [(name, exp)], body)
      | BLetRec(names) => AExp.let_(~loc, ~env, Recursive, names, body)
      | [@implicit_arity] BLetExport(name, exp) =>
        failwith("Global bind at non-toplevel")
      },
    ans_setup,
    AExp.comp(~loc, ~env, ans),
  );
};

let linearize_decl = (env, loc, typath, decl) => {
  /* FIXME: [philip] This is kind of hacky...would be better to store this in the Env directly...not to mention,
     I think this'll be much more fragile than if it were in the static info */
  let ty_id = get_type_id(typath);
  let descrs = Datarepr.constructors_of_type(typath, decl);
  let bind_constructor = ((cd_id, {cstr_name, cstr_tag, cstr_args})) => {
    let rhs =
      switch (cstr_tag) {
      | CstrConstant(_) =>
        let compiled_tag = compile_constructor_tag(cstr_tag);
        Comp.adt(
          ~loc,
          ~env,
          Imm.const(~loc, ~env, Const_int(ty_id)),
          Imm.const(~loc, ~env, Const_int(compiled_tag)),
          [],
        );
      | CstrBlock(_) =>
        let compiled_tag = compile_constructor_tag(cstr_tag);
        let args = List.map(_ => gensym("constr_arg"), cstr_args);
        let arg_ids = List.map(a => Imm.id(~loc, ~env, a), args);
        let imm_tytag = Imm.const(~loc, ~env, Const_int(ty_id));
        let imm_tag = Imm.const(~loc, ~env, Const_int(compiled_tag));
        Comp.lambda(
          ~loc,
          ~env,
          args,
          AExp.comp(
            ~loc,
            ~env,
            Comp.adt(~loc, ~env, imm_tytag, imm_tag, arg_ids),
          ),
        );
      | CstrUnboxed => failwith("NYI: ANF CstrUnboxed")
      };
    [@implicit_arity] BLetExport(Nonrecursive, [(cd_id, rhs)]);
  };
  List.map(bind_constructor, descrs);
};

let rec transl_anf_statement =
        ({ttop_desc, ttop_env: env, ttop_loc: loc} as s: toplevel_stmt)
        : (option(list(anf_bind)), list(import_spec)) =>
  switch (ttop_desc) {
  | [@implicit_arity] TTopLet(_, _, _, []) => (None, [])
  | [@implicit_arity]
    TTopLet(
      export_flag,
      Nonrecursive,
      mut_flag,
      [{vb_expr, vb_pat}, ...rest],
    ) =>
    let (exp_ans, exp_setup) = transl_comp_expression(vb_expr);
    let (rest_setup, rest_imp) =
      transl_anf_statement({
        ...s,
        ttop_desc:
          [@implicit_arity]
          TTopLet(export_flag, Nonrecursive, mut_flag, rest),
      });
    let rest_setup = Option.value(~default=[], rest_setup);
    let exported = export_flag == Exported;
    let setup =
      switch (vb_pat.pat_desc) {
      | [@implicit_arity] TPatVar(bind, _)
      | [@implicit_arity] TPatAlias({pat_desc: TPatAny}, bind, _) =>
        switch (mut_flag) {
        | Mutable =>
          let tmp = gensym("mut_bind_destructure");
          let boxed = Comp.prim1(Box, Imm.id(tmp));
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
        [[@implicit_arity] BLet(tmp, exp_ans), ...anf_patts];
      | [@implicit_arity] TPatAlias(patt, bind, _) =>
        let binds =
          switch (mut_flag) {
          | Mutable =>
            let tmp = gensym("mut_bind_destructure");
            let boxed = Comp.prim1(Box, Imm.id(tmp));
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
      | TPatAny => []
      | _ =>
        failwith(
          "NYI: transl_anf_statement: Non-record or non-tuple destructuring in let",
        )
      };
    (Some(exp_setup @ setup @ rest_setup), rest_imp);
  | [@implicit_arity] TTopLet(export_flag, Recursive, mut_flag, binds) =>
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
        | {pat_desc: [@implicit_arity] TPatVar(id, _)} => id
        | _ => failwith("Non-name not allowed on LHS of let rec."),
        binds,
      );

    switch (export_flag) {
    | Exported => (
        Some(
          List.concat(new_setup)
          @ [
            [@implicit_arity]
            BLetExport(Recursive, List.combine(names, new_binds)),
          ],
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
          | TDataRecord(_) => []
          },
        decls,
      );
    if (List.length(bindings) > 0) {
      (Some(bindings), []);
    } else {
      (None, []);
    };
  | TTopForeign(desc) =>
    let arity = Ctype.arity(desc.tvd_desc.ctyp_type);
    (
      None,
      [
        Imp.wasm_func(
          desc.tvd_id,
          desc.tvd_mod.txt,
          desc.tvd_name.txt,
          [@implicit_arity] FunctionShape(arity, 1),
        ),
      ],
    );
  | _ => (None, [])
  };

let linearize_builtins = (env, builtins) => {
  let bindings =
    List.concat @@
    List.map(
      decl =>
        switch (decl.type_kind) {
        | TDataVariant(_) when !decl.type_immediate =>
          linearize_decl(env, Location.dummy_loc, decl.type_path, decl)
        | _ => []
        },
      builtins,
    );
  (bindings, []);
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
  let void_comp = Comp.imm(Imm.const(Const_void));
  let void = AExp.comp(void_comp);
  let (last_bind, top_binds) =
    switch (top_binds) {
    | [bind, ...rest] => (bind, rest)
    | _ => (BSeq(void_comp), [])
    };
  let ans =
    switch (last_bind) {
    | BSeq(exp) => AExp.comp(exp)
    | [@implicit_arity] BLet(name, exp) =>
      AExp.let_(Nonrecursive, [(name, exp)], void)
    | BLetRec(names) => AExp.let_(Recursive, names, void)
    | [@implicit_arity] BLetExport(rf, binds) =>
      AExp.let_(~glob=Global, rf, binds, void)
    };
  let body =
    List.fold_left(
      (body, bind) =>
        switch (bind) {
        | BSeq(exp) => AExp.seq(exp, body)
        | [@implicit_arity] BLet(name, exp) =>
          AExp.let_(Nonrecursive, [(name, exp)], body)
        | BLetRec(names) => AExp.let_(Recursive, names, body)
        | [@implicit_arity] BLetExport(rf, binds) =>
          AExp.let_(~glob=Global, rf, binds, body)
        },
      ans,
      top_binds,
    );
  let imports = imports @ value_imports^;
  {body, env, imports, signature, analyses: ref([])};
};

let () = Matchcomp.compile_constructor_tag := compile_constructor_tag;
