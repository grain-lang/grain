open Grain_parsing;
open Grain_typed;
open Anftree;

let rec anf_free_vars_help = (env, a: anf_expression) =>
  switch (a.anf_desc) {
  | AESeq(fst, rest) =>
    Ident.Set.union(
      comp_free_vars_help(env, fst),
      anf_free_vars_help(env, rest),
    )
  | AEComp(c) => comp_free_vars_help(env, c)
  | AELet(_, recflag, binds, body) =>
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
  | CLambda(args, body) =>
    anf_free_vars_help(Ident.Set.union(env, Ident.Set.of_list(args)), body)
  | CIf(cond, thn, els) =>
    Ident.Set.union(imm_free_vars_help(env, cond)) @@
    Ident.Set.union(
      anf_free_vars_help(env, thn),
      anf_free_vars_help(env, els),
    )
  | CWhile(cond, body) =>
    Ident.Set.union(
      anf_free_vars_help(env, cond),
      anf_free_vars_help(env, body),
    )
  | CSwitch(arg, branches) =>
    List.fold_left(
      (acc, (_, b)) => Ident.Set.union(anf_free_vars_help(env, b), acc),
      imm_free_vars_help(env, arg),
      branches,
    )
  | CPrim1(_, arg) => imm_free_vars_help(env, arg)
  | CPrim2(_, arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | CBoxAssign(arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | CAssign(arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | CApp(fn, args, _) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      imm_free_vars_help(env, fn),
      args,
    )
  | CAppBuiltin(_, _, args) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      args,
    )
  | CTuple(args)
  | CArray(args)
  | CAdt(_, _, args) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      args,
    )
  | CArrayGet(arg1, arg2) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      [arg1, arg2],
    )
  | CArraySet(arg1, arg2, arg3) =>
    List.fold_left(
      (acc, a) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      [arg1, arg2, arg3],
    )
  | CRecord(_, args) =>
    List.fold_left(
      (acc, (_, a)) => Ident.Set.union(imm_free_vars_help(env, a), acc),
      Ident.Set.empty,
      args,
    )
  | CGetTupleItem(_, arg)
  | CGetAdtItem(_, arg)
  | CGetAdtTag(arg)
  | CGetRecordItem(_, arg) => imm_free_vars_help(env, arg)
  | CSetRecordItem(_, arg1, arg2)
  | CSetTupleItem(_, arg1, arg2) =>
    Ident.Set.union(
      imm_free_vars_help(env, arg1),
      imm_free_vars_help(env, arg2),
    )
  | CNumber(_)
  | CInt32(_)
  | CInt64(_)
  | CFloat32(_)
  | CFloat64(_)
  | CString(_)
  | CChar(_) => Ident.Set.empty
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
  | AELet(_, recflag, binds, body) =>
    let max_binds =
      List.fold_left(max, 0) @@
      List.map(((_, c)) => comp_count_vars(c), binds);
    switch (recflag) {
    | Recursive => List.length(binds) + max(max_binds, anf_count_vars(body))
    | Nonrecursive =>
      max(max_binds, List.length(binds) + anf_count_vars(body))
    };
  | AESeq(hd, tl) => max(comp_count_vars(hd), anf_count_vars(tl))
  | AEComp(c) => comp_count_vars(c)
  }

and comp_count_vars = c =>
  switch (c.comp_desc) {
  | CIf(_, t, f) => max(anf_count_vars(t), anf_count_vars(f))
  | CWhile(c, b) => anf_count_vars(c) + anf_count_vars(b)
  | CSwitch(_, bs) =>
    List.fold_left(max, 0) @@ List.map(((_, b)) => anf_count_vars(b), bs)
  | CApp(_, args, _) => List.length(args)
  | CAppBuiltin(_, _, args) => List.length(args)
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
