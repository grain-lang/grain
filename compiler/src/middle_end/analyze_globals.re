open Anftree;
open Anf_iterator;
open Grain_typed;

let global_vars = ref(Ident.Set.empty);

let get_globals = () => global_vars^;

module AnalyzeGlobalsArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_anf_expression = ({anf_desc: desc}) =>
    switch (desc) {
    | AELet(Global, _, _, binds, _) =>
      List.iter(
        ((id, _)) => {global_vars := Ident.Set.add(id, global_vars^)},
        binds,
      )
    | _ => ()
    };

  let leave_anf_program = ({imports}) => {
    List.iter(
      ({imp_use_id}) => {
        global_vars := Ident.Set.add(imp_use_id, global_vars^)
      },
      imports.specs,
    );
  };
};

module AnalyzeGlobalsIterator = Anf_iterator.MakeIter(AnalyzeGlobalsArg);

let analyze = anfprog => {
  global_vars := Ident.Set.empty;
  AnalyzeGlobalsIterator.iter_anf_program(anfprog);
};
