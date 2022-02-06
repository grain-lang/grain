open Anftree;
open Anf_iterator;
open Grain_typed;

let closure_scoped_vars = ref(Anf_utils.IdentAllocationSet.empty);

let is_closure_scoped_var = id => {
  Anf_utils.IdentAllocationSet.mem(
    // The allocation_type is not compared in an IdentAllocationSet, so we can
    // fake it for convenience
    (id, Types.HeapAllocated),
    closure_scoped_vars^,
  );
};

module CSVArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) =>
    switch (desc) {
    | CLambda(_) =>
      closure_scoped_vars :=
        Anf_utils.IdentAllocationSet.union(
          closure_scoped_vars^,
          Anf_utils.comp_free_vars(c),
        )
    | _ => ()
    };

  let leave_anf_expression = ({anf_desc: desc}) => {
    switch (desc) {
    | AELet(Global, _, _, binds, _) =>
      /* Assume that all globals are closure scope, since globals could
         appear in a closure scope in another module */
      let ids = List.map(((id, c)) => (id, c.comp_allocation_type), binds);
      closure_scoped_vars :=
        Anf_utils.IdentAllocationSet.union(
          closure_scoped_vars^,
          Anf_utils.IdentAllocationSet.of_list(ids),
        );
    | _ => ()
    };
  };
};

module CSVIterator = Anf_iterator.MakeIter(CSVArg);

let analyze = anfprog => {
  closure_scoped_vars := Anf_utils.IdentAllocationSet.empty;
  CSVIterator.iter_anf_program(anfprog);
};
