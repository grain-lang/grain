open Grain_parsing;
open Types;
open Typedtree;
open TypedtreeIter;

/*
 * This module provides well-formedness checks which are type/value resolution-dependent.
 * For the moment, this is just a warning on (==), but feel free to refactor once more checks
 * are desired.
 */

let wasm_unsafe_types = ["WasmI32", "WasmI64", "WasmF32", "WasmF64"];

let exp_is_wasm_unsafe = ({exp_type: {desc}}) => {
  switch (desc) {
  | TTyConstr(path, [], _) => List.mem(Path.name(path), wasm_unsafe_types)
  | _ => false
  };
};

module WellFormednessArg: TypedtreeIter.IteratorArgument = {
  include TypedtreeIter.DefaultIteratorArgument;

  let enter_expression: expression => unit =
    ({exp_desc, exp_loc}) => {
      switch (exp_desc) {
      | TExpApp({exp_desc: TExpIdent(val_fullpath, _, _)}, args)
          when Path.name(val_fullpath) == "Pervasives.==" =>
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
