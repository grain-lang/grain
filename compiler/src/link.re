open Grain_codegen;

let link = (~main_object, ~outfile, dependencies) => {
  let linked_program = Linkedtree.link(~main_object, dependencies);
  let compiled_program = Compmod.compile_wasm_module(linked_program);
  Emitmod.emit_binary(
    compiled_program.asm,
    compiled_program.signature,
    outfile,
  );
};
