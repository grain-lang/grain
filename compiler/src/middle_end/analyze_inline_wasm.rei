open Anftree;
open Grain_typed;

type inline_type =
  | WasmPrimN(primn);

let mod_has_inlineable_wasm: ref(bool);

let get_inline_wasm_type: Ident.t => inline_type;
let has_inline_wasm_type: Ident.t => bool;

let analyze: Analysis_pass.t;
