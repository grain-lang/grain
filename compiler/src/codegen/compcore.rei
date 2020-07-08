open Grain_typed;
open Grain_middle_end;
open Mashtree;
open Binaryen;

type codegen_env = {
  num_args: int,
  func_offset: int,
  global_offset: int,
  stack_size: int,
  import_global_offset: int,
  import_func_offset: int,
  import_offset: int,
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Expression.t, closure_data))),
  imported_funcs: Ident.tbl(Ident.tbl(int32)),
  imported_globals: Ident.tbl(Ident.tbl(string)),
};

let init_codegen_env: unit => codegen_env;

exception WasmRunnerError(Module.t, option(string), string);

let validate_module: (~name: string=?, Module.t) => unit;

let compile_wasm_module:
  (~env: codegen_env=?, ~name: string=?, Mashtree.mash_program) => Module.t;

let module_to_bytes: Module.t => bytes;
