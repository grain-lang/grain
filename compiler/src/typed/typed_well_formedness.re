open Grain_parsing;
open Types;
open Typedtree;
open TypedtreeIter;

/*
 * This module provides well-formedness checks which are type/value resolution-dependent.
 */

let wasm_unsafe_types = [
  Builtin_types.path_wasmi32,
  Builtin_types.path_wasmi64,
  Builtin_types.path_wasmf32,
  Builtin_types.path_wasmf64,
];

let exp_is_wasm_unsafe = ({exp_type: {desc}}) => {
  switch (desc) {
  | TTyConstr(path, _, _) => List.mem(path, wasm_unsafe_types)
  | _ => false
  };
};

module WellFormednessArg: TypedtreeIter.IteratorArgument = {
  include TypedtreeIter.DefaultIteratorArgument;

  let enter_expression: expression => unit =
    ({exp_desc, exp_loc}) => {
      switch (exp_desc) {
      | TExpApp(
          {
            exp_desc:
              TExpIdent(
                Path.PExternal(Path.PIdent({name: "Pervasives"}), "==", _),
                _,
                _,
              ),
          },
          args,
        ) =>
        if (List.exists(exp_is_wasm_unsafe, args)) {
          let warning = Grain_utils.Warnings.EqualWasmUnsafe;
          if (Grain_utils.Warnings.is_active(warning)) {
            Grain_parsing.Location.prerr_warning(exp_loc, warning);
          };
        }
      | _ => ()
      };
    };
};

module WellFormednessIterator = TypedtreeIter.MakeIterator(WellFormednessArg);

let check_well_formedness = ({statements}) => {
  List.iter(WellFormednessIterator.iter_toplevel_stmt, statements);
};
