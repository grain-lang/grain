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
        Ident.Set.union(
          closure_scoped_vars^,
          Analyze_free_vars.comp_free_vars(c),
        )
    | _ => ()
    };

  let leave_anf_program = ({signature: {cmi_sign}}) => {
    Types.(
      List.iter(
        fun
        | TSigValue(_, {val_fullpath: PIdent(id)})
        | TSigTypeExt(_, {ext_name: id}, _) =>
          closure_scoped_vars := Ident.Set.add(id, closure_scoped_vars^)
        | TSigType(_, {type_kind: TDataVariant(cds)}, _) =>
          List.iter(
            ({cd_id}) =>
              closure_scoped_vars :=
                Ident.Set.add(cd_id, closure_scoped_vars^),
            cds,
          )
        | TSigType(_) => ()
        | TSigValue(_) => failwith("NYI: external val_fullpath")
        | TSigModule(_)
        | TSigModType(_) => failwith("NYI: modules in module signatures"),
        cmi_sign,
      )
    );
  };
};

module CSVIterator = Anf_iterator.MakeIter(CSVArg);

let analyze = anfprog => {
  closure_scoped_vars := Ident.Set.empty;
  CSVIterator.iter_anf_program(anfprog);
};
