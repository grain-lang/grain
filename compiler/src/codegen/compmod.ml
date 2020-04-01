open Grain_typed
open Mashtree

type compiled_program = {
  asm: Wasm.Ast.module_;
  signature: Cmi_format.cmi_infos;
}

let compile_wasm_module ({Mashtree.signature; } as mashprog) =
  let asm = Compcore.compile_wasm_module mashprog in
  {
    asm;
    signature;
  }
