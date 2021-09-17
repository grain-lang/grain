open Grain_typed;
open Anftree;

let escaped_vars = ref(Ident.Set.empty);

let var_escapes = id => {
  Ident.Set.mem(id, escaped_vars^);
};

/*
 Variable escape analysis (only makes sense in the context of boxes).
 Variables escape if one of the following is true:
 - A function is called with that variable
 - That variable is returned from a function

 [TODO] Add optimization pass which converts unbox()/box() CApps into CPrim1s
        (they will be incorrectly missed by this analysis, but this analysis is sufficient for let mut)
 */

let rec get_returned_imm_ids = anf_expr => {
  let rec get_comp_tail_imm_ids = ({comp_desc}) => {
    switch (comp_desc) {
    // NOTE: CFor is always void
    | CIf(_, tru, fals) =>
      Ident.Set.union(get_returned_imm_ids(tru), get_returned_imm_ids(fals))
    | CImmExpr({imm_desc: ImmId(id)}) => Ident.Set.singleton(id)
    | _ => Ident.Set.empty
    };
  };
  switch (anf_expr.anf_desc) {
  | AEComp(comp) => get_comp_tail_imm_ids(comp)
  | AESeq(_, tl) => get_returned_imm_ids(tl)
  | AELet(_, _, _, _, tl) => get_returned_imm_ids(tl)
  };
};

module EscapeAnalysisArg = {
  include Anf_iterator.DefaultIterArgument;

  let leave_comp_expression = ({comp_desc}) => {
    switch (comp_desc) {
    | CApp(_, args, _) =>
      List.iter(
        ({imm_desc}) => {
          switch (imm_desc) {
          | ImmId(id) => escaped_vars := Ident.Set.add(id, escaped_vars^)
          | _ => ()
          }
        },
        args,
      )
    | CLambda(_, _, (body, _)) =>
      escaped_vars :=
        Ident.Set.union(escaped_vars^, get_returned_imm_ids(body))
    | _ => ()
    };
  };
};

module EscapeAnalysis = Anf_iterator.MakeIter(EscapeAnalysisArg);

let analyze: Analysis_pass.t =
  prog => {
    escaped_vars := Ident.Set.empty;
    EscapeAnalysis.iter_anf_program(prog);
    Printf.eprintf("escaped_vars: %s\n", Ident.Set.to_string(escaped_vars^));
  };
