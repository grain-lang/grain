open Grain_typed;
open Mashtree;

type compiled_program = {
  asm: Binaryen.Module.t,
  signature: Cmi_format.cmi_infos,
};

let compile_wasm_module = (~name=?, {Linkedtree.signature} as linked_program) => {
  let asm = Compcore.compile_wasm_module(~name?, linked_program);
  {
    asm,
    signature,
  };
};
