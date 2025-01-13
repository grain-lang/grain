open Grain_typed;
open Grain_middle_end;
open Mashtree;
open Binaryen;

exception WasmRunnerError(Module.t, option(string), string);

let validate_module: (~name: string=?, Module.t) => unit;

let compile_wasm_module:
  (~name: string=?, Linkedtree.linked_program) => Module.t;

let module_to_bytes: Module.t => bytes;
