open Grain_typed;
open Grain_middle_end;
open Mashtree;
open Binaryen;

type codegen_env = {
  name: option(string),
  num_args: int,
  num_closure_args: int,
  stack_size,
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Expression.t, closure_data))),
  required_imports: list(import),
};

let init_codegen_env: option(string) => codegen_env;

exception WasmRunnerError(Module.t, option(string), string);

let validate_module: (~name: string=?, Module.t) => unit;

let compile_wasm_module:
  (~env: codegen_env=?, ~name: string=?, Mashtree.mash_program) => Module.t;

let module_to_bytes: Module.t => bytes;
