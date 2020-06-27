open Grain_typed;
open Grain_middle_end;
open Mashtree;
open Wasm;

type codegen_env = {
  num_args: int,
  func_offset: int,
  global_offset: int,
  stack_size: int,
  import_global_offset: int,
  import_func_offset: int,
  import_offset: int,
  func_types: ref(BatDeque.t(Wasm.Types.func_type)),
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Concatlist.t(Wasm.Ast.instr'), closure_data))),
  imported_funcs: Ident.tbl(Ident.tbl(int32)),
  imported_globals: Ident.tbl(Ident.tbl(int32)),
};

let init_codegen_env: unit => codegen_env;

exception
  WasmRunnerError(
    option(string),
    Wasm.Source.region,
    string,
    Wasm.Ast.module_,
  );

let reparse_module: Wasm.Ast.module_ => Wasm.Ast.module_;

let validate_module: (~name: string=?, Wasm.Ast.module_) => unit;

let compile_wasm_module:
  (~env: codegen_env=?, ~name: string=?, Mashtree.mash_program) =>
  Wasm.Ast.module_;

let module_to_string: Wasm.Ast.module_ => string;
