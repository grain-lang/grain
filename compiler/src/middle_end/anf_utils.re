open Grain_parsing;
open Grain_typed;
open Anftree;

let rec anf_free_vars_help = (env, a: anf_expression) =>
  switch (a.anf_desc) {
  | [@implicit_arity] AESeq(fst, rest) =>
    Ident.Set.union(
      comp_free_vars_help(env, fst),
      anf_free_vars_help(env, rest),
    )
  | AEComp(c) => comp_free_vars_help(env, c)
  | [@implicit_arity] AELet(_, recflag, binds, body) =>
    let with_names =
      List.fold_left((acc, (id, _)) => Ident.Set.add(id, acc), env, binds);
    let free_binds =
      switch (recflag) {
      | Recursive =>
        List.fold_left(
          (acc, (_, body)) =>
            Ident.Set.union(acc, comp_free_vars_help(with_names, body)),
          Ident.Set.empty,
          binds,
        )
      | Nonrecursive =>
        List.fold_left(
          (acc, (_, body)) =>
            Ident.Set.union(acc, comp_free_vars_help(env, body)),
          Ident.Set.empty,
          binds,
        )
      };
    Ident.Set.union(free_binds, anf_free_vars_help(with_names, body));
  }

and comp_free_vars_help = (env, c: comp_expression) =>
  switch (c.comp_desc) {
  | [@implicit_arity] CLambda(args, body) =>
    anf_free_vars_help(Ident.Set.union(env, Ident.Set.of_list(args)), body)
  | [@implicit_arity] CIf(cond, thn, els) =>
    Ident.Set.union(imm_free_vars_help(env, cond)) @@
    Ident.Set.union(
      anf_free_vars_help(env, thn),
      anf_free_vars_help(env, els),
    )
  | [@implicit_arity] CWhile(cond, body) =>
    Ident.Set.union(
      anf_free_vars_help(env, cond),
      anf_free_vars_help(env, body),
    )
  | [@implicit_arity] CSwitch(arg, branches) =>
    List.fold_left(
      (acc, (_, b)) => Ident.Set.union(anf_free_vars_help(env, b), acc),
      imm_free_vars_help(env, arg),
      branches,
    )
  | [@implicit_arity] CPrim1(_, arg) => imm_free_vars_help(env, arg)
  | [@implicit_arity] CPrim2(_, arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | [@implicit_arity] CBoxAssign(arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | [@implicit_arity] CAssign(arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | [@implicit_arity] CApp(fn, args) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      imm_free_vars_help(env, fn),
      args,
    )
  | [@implicit_arity] CAppBuiltin(_, _, args) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      args,
    )
  | CTuple(args)
  | CArray(args)
  | [@implicit_arity] CAdt(_, _, args) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      args,
    )
  | [@implicit_arity] CArrayGet(arg1, arg2) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      [arg1, arg2],
    )
  | [@implicit_arity] CArraySet(arg1, arg2, arg3) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      [arg1, arg2, arg3],
    )
  | [@implicit_arity] CRecord(_, args) =>
    List.fold_left(
      (acc, (_, a)) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      args,
    )
  | [@implicit_arity] CGetTupleItem(_, arg)
  | [@implicit_arity] CGetAdtItem(_, arg)
  | CGetAdtTag(arg)
  | [@implicit_arity] CGetRecordItem(_, arg) => imm_free_vars_help(env, arg)
  | [@implicit_arity] CSetRecordItem(_, arg1, arg2)
  | [@implicit_arity] CSetTupleItem(_, arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | CInt32(_)
  | CInt64(_)
  | CString(_) => Ident.Set.empty
  | CImmExpr(i) => imm_free_vars_help(env, i)
  }

and imm_free_vars_help = (env, i: imm_expression) =>
  switch (i.imm_desc) {
  | ImmId(x) when !Ident.Set.mem(x, env) => Ident.Set.singleton(x)
  | _ => Ident.Set.empty
  };

let anf_free_vars = anf_free_vars_help(Ident.Set.empty);
let comp_free_vars = comp_free_vars_help(Ident.Set.empty);
let imm_free_vars = imm_free_vars_help(Ident.Set.empty);

let rec anf_count_vars = a =>
  switch (a.anf_desc) {
  | [@implicit_arity] AELet(_, recflag, binds, body) =>
    let max_binds =
      List.fold_left(max, 0) @@
      List.map(((_, c)) => comp_count_vars(c), binds);
    switch (recflag) {
    | Recursive => List.length(binds) + max(max_binds, anf_count_vars(body))
    | Nonrecursive =>
      max(max_binds, List.length(binds) + anf_count_vars(body))
    };
  | [@implicit_arity] AESeq(hd, tl) =>
    max(comp_count_vars(hd), anf_count_vars(tl))
  | AEComp(c) => comp_count_vars(c)
  }

and comp_count_vars = c =>
  switch (c.comp_desc) {
  | [@implicit_arity] CIf(_, t, f) =>
    max(anf_count_vars(t), anf_count_vars(f))
  | [@implicit_arity] CWhile(c, b) => anf_count_vars(c) + anf_count_vars(b)
  | [@implicit_arity] CSwitch(_, bs) =>
    List.fold_left(max, 0) @@ List.map(((_, b)) => anf_count_vars(b), bs)
  | [@implicit_arity] CApp(_, args) => List.length(args)
  | [@implicit_arity] CAppBuiltin(_, _, args) => List.length(args)
  | _ => 0
  };

module ClearLocationsArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_imm_expression = i => {...i, imm_loc: Location.dummy_loc};

  let leave_comp_expression = c => {...c, comp_loc: Location.dummy_loc};

  let leave_anf_expression = a => {...a, anf_loc: Location.dummy_loc};
};

module ClearLocations = Anf_mapper.MakeMap(ClearLocationsArg);

let clear_locations = ClearLocations.map_anf_program;
