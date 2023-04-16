open Anftree;
open Anf_iterator;
open Grain_typed;

type analysis +=
  | FreeVars(Ident.Set.t);

let rec get_free_vars = lst =>
  switch (lst) {
  | [] => None
  | [FreeVars(x), ..._] => Some(x)
  | [_, ...tl] => get_free_vars(tl)
  };

let imm_expression_free_vars = ({imm_analyses}) =>
  get_free_vars(imm_analyses^);

let comp_expression_free_vars = ({comp_analyses}) =>
  get_free_vars(comp_analyses^);
let anf_expression_free_vars = ({anf_analyses}) =>
  get_free_vars(anf_analyses^);

/* Quick accessors for known-existing values */
let imm_free_vars = c => Option.get(imm_expression_free_vars(c));
let comp_free_vars = c => Option.get(comp_expression_free_vars(c));
let anf_free_vars = a => Option.get(anf_expression_free_vars(a));

let push_free_vars = (lref, p) => lref := [FreeVars(p), ...lref^];

module FreeVarsArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_imm_expression = ({imm_desc: desc, imm_analyses: analyses}) =>
    push_free_vars(analyses) @@
    (
      switch (desc) {
      | ImmId(x) => Ident.Set.singleton(x)
      | _ => Ident.Set.empty
      }
    );

  let leave_comp_expression = ({comp_desc: desc, comp_analyses: analyses}) =>
    push_free_vars(analyses) @@
    (
      switch (desc) {
      | CLambda(_, args, (body, _), _) =>
        Ident.Set.diff(
          anf_free_vars(body),
          Ident.Set.of_list(List.map(((arg, _)) => arg, args)),
        )
      | CIf(cond, thn, els) =>
        Ident.Set.union(imm_free_vars(cond)) @@
        Ident.Set.union(anf_free_vars(thn), anf_free_vars(els))
      | CFor(cond, inc, body) =>
        let cond =
          Option.fold(~none=Ident.Set.empty, ~some=anf_free_vars, cond);
        let inc =
          Option.fold(~none=Ident.Set.empty, ~some=anf_free_vars, inc);
        let body = anf_free_vars(body);
        Ident.Set.union(cond, Ident.Set.union(inc, body));
      | CContinue
      | CBreak => Ident.Set.empty
      | CReturn(expr) =>
        Option.fold(~none=Ident.Set.empty, ~some=imm_free_vars, expr)
      | CSwitch(arg, branches, _) =>
        List.fold_left(
          (acc, (_, b)) => Ident.Set.union(anf_free_vars(b), acc),
          imm_free_vars(arg),
          branches,
        )
      | CPrim0(_) => Ident.Set.empty
      | CPrim1(_, arg) => imm_free_vars(arg)
      | CPrim2(_, arg1, arg2) =>
        Ident.Set.union(imm_free_vars(arg1), imm_free_vars(arg2))
      | CPrimN(_, args) =>
        List.fold_left(
          (acc, a) => Ident.Set.union(imm_free_vars(a), acc),
          Ident.Set.empty,
          args,
        )
      | CBoxAssign(arg1, arg2) =>
        Ident.Set.union(imm_free_vars(arg1), imm_free_vars(arg2))
      | CAssign(arg1, arg2) =>
        Ident.Set.union(imm_free_vars(arg1), imm_free_vars(arg2))
      | CLocalAssign(arg1, arg2) => imm_free_vars(arg2)
      | CApp((fn, _), args, _) =>
        List.fold_left(
          (acc, a) => Ident.Set.union(imm_free_vars(a), acc),
          imm_free_vars(fn),
          args,
        )
      | CTuple(args)
      | CArray(args)
      | CAdt(_, _, _, args) =>
        List.fold_left(
          (acc, a) => Ident.Set.union(imm_free_vars(a), acc),
          Ident.Set.empty,
          args,
        )
      | CArrayGet(arg1, arg2) =>
        List.fold_left(
          (acc, a) => Ident.Set.union(imm_free_vars(a), acc),
          Ident.Set.empty,
          [arg1, arg2],
        )
      | CArraySet(arg1, arg2, arg3) =>
        List.fold_left(
          (acc, a) => Ident.Set.union(imm_free_vars(a), acc),
          Ident.Set.empty,
          [arg1, arg2, arg3],
        )
      | CRecord(_, _, args) =>
        List.fold_left(
          (acc, (_, a)) => Ident.Set.union(imm_free_vars(a), acc),
          Ident.Set.empty,
          args,
        )
      | CGetTupleItem(_, arg)
      | CGetAdtItem(_, arg)
      | CGetAdtTag(arg)
      | CGetRecordItem(_, arg) => imm_free_vars(arg)
      | CSetRecordItem(_, arg1, arg2)
      | CSetTupleItem(_, arg1, arg2) =>
        Ident.Set.union(imm_free_vars(arg1), imm_free_vars(arg2))
      | CNumber(_)
      | CInt32(_)
      | CInt64(_)
      | CUint32(_)
      | CUint64(_)
      | CFloat32(_)
      | CFloat64(_)
      | CBytes(_)
      | CString(_) => Ident.Set.empty
      | CImmExpr(i) => imm_free_vars(i)
      }
    );

  let leave_anf_expression = ({anf_desc: desc, anf_analyses: analyses}) =>
    push_free_vars(analyses) @@
    (
      switch (desc) {
      | AESeq(fst, rest) =>
        Ident.Set.union(comp_free_vars(fst), anf_free_vars(rest))
      | AEComp(c) => comp_free_vars(c)
      | AELet(_, recflag, _, binds, body) =>
        let bind_env =
          List.fold_left(
            (acc, (id, _)) => Ident.Set.add(id, acc),
            Ident.Set.empty,
            binds,
          );
        let free_in_binds =
          switch (recflag) {
          | Recursive =>
            List.fold_left(
              (acc, (_, bind_body)) =>
                Ident.Set.union(
                  acc,
                  Ident.Set.diff(comp_free_vars(bind_body), bind_env),
                ),
              Ident.Set.empty,
              binds,
            )
          | Nonrecursive =>
            List.fold_left(
              (acc, (_, bind_body)) =>
                Ident.Set.union(acc, comp_free_vars(bind_body)),
              Ident.Set.empty,
              binds,
            )
          };
        Ident.Set.union(
          free_in_binds,
          Ident.Set.diff(anf_free_vars(body), bind_env),
        );
      }
    );
};

module FreeVarsIterator = Anf_iterator.MakeIter(FreeVarsArg);

let analyze = anfprog => {
  FreeVarsIterator.iter_anf_program(anfprog);
};
