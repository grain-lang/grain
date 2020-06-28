open Grain_typed;
open Mashtree;

type compiled_program = {
  asm: Binaryen.Module.t,
  signature: Cmi_format.cmi_infos,
};

let compile_wasm_module = (~name=?, {Mashtree.signature} as mashprog) => {
  let asm = Compcore.compile_wasm_module(~name?, mashprog);
  {asm, signature};
};
