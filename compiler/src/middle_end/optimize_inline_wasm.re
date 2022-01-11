// This module still exists to support the --no-bulk-memory flag.

open Anftree;
open Grain_typed;
open Analyze_inline_wasm;

module IWArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) => {
    switch (desc) {
    | CApp(({imm_desc: ImmId(id)}, _), args, _)
        when has_inline_wasm_type(id) =>
      let primn =
        switch (get_inline_wasm_type(id)) {
        | WasmPrimN(primn) => primn
        };
      {...c, comp_desc: CPrimN(primn, args)};
    | _ => c
    };
  };
};

module IWMapper = Anf_mapper.MakeMap(IWArg);

let optimize = anfprog =>
  if (mod_has_inlineable_wasm^) {
    IWMapper.map_anf_program(anfprog);
  } else {
    anfprog;
  };
