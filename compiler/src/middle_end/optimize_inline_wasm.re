open Anftree;
open Grain_typed;
open Analyze_inline_wasm;

module IWArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) => {
    switch (desc) {
    | CApp({imm_desc: ImmId(id)}, [arg1])
        when has_inline_wasm_type(id) =>
      let prim1 =
        switch (get_inline_wasm_type(id)) {
        | WasmPrim1(prim1) => prim1
        | _ => failwith("internal: WasmPrim2 was not found")
        };
      {...c, comp_desc: CPrim1(prim1, arg1)};
    | CApp({imm_desc: ImmId(id)}, [arg1, arg2])
        when has_inline_wasm_type(id) =>
      let prim2 =
        switch (get_inline_wasm_type(id)) {
        | WasmPrim2(prim2) => prim2
        | _ => failwith("internal: WasmPrim2 was not found")
        };
      {...c, comp_desc: CPrim2(prim2, arg1, arg2)};
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
