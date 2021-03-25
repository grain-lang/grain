open Grain_codegen;

let statically_link_wasm_module = prog => {
  let linked_asm = Link.link_modules(prog);
  {...prog, asm: linked_asm};
};
