open Anftree;
open Grain_typed;

let mutable_vars = ref(Ident.Set.empty);

let is_mutable = id => Ident.Set.mem(id, mutable_vars^);

module MutableVarsArg = {
  include Anf_iterator.DefaultIterArgument;

  let enter_anf_expression = ({anf_desc}) => {
    switch (anf_desc) {
    | AELet(_, _, Mutable, binds, _) =>
      List.iter(
        ((id, _)) => {mutable_vars := Ident.Set.add(id, mutable_vars^)},
        binds,
      )
    | _ => ()
    };
  };
};

module MutableVarsIter = Anf_iterator.MakeIter(MutableVarsArg);

let analyze = prog => {
  mutable_vars := Ident.Set.empty;
  MutableVarsIter.iter_anf_program(prog);
};
