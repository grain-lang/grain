open Anftree;
open Anf_iterator;
open Grain_typed;

let closure_scoped_vars = ref(Ident.Set.empty);

let is_closure_scoped_var = id => {
  Ident.Set.mem(id, closure_scoped_vars^);
};

module CSVArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) =>
    switch (desc) {
    | CLambda(_) =>
      closure_scoped_vars :=
        Ident.Set.union(closure_scoped_vars^, Anf_utils.comp_free_vars(c))
    | _ => ()
    };

  let leave_anf_expression = ({anf_desc: desc}) => {
    switch (desc) {
    | AELet(Global({exported: true}), _, _, binds, _) =>
      /* Assume that all exported globals are closure scope, since globals could
         appear in a closure scope in another module */
      let ids = List.map(fst, binds);
      closure_scoped_vars :=
        Ident.Set.union(closure_scoped_vars^, Ident.Set.of_list(ids));
    | _ => ()
    };
  };
};

module CSVIterator = Anf_iterator.MakeIter(CSVArg);

let analyze = anfprog => {
  closure_scoped_vars := Ident.Set.empty;
  CSVIterator.iter_anf_program(anfprog);
};
