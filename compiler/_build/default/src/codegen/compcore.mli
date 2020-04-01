open Grain_typed
open Grain_middle_end
open Mashtree
open Wasm

type codegen_env = {
  (* Pointer to top of heap (needed until GC is implemented) *)
  heap_top: Wasm.Ast.var;
  num_args: int;
  func_offset: int;
  global_offset: int;
  import_global_offset: int;
  import_func_offset: int;
  import_offset: int;
  func_types: Wasm.Types.func_type BatDeque.t ref;
  (* Allocated closures which need backpatching *)
  backpatches: (Wasm.Ast.instr' Concatlist.t * closure_data) list ref;
  imported_funcs: (int32 Ident.tbl) Ident.tbl;
  imported_globals: (int32 Ident.tbl) Ident.tbl;
}

val init_codegen_env : unit -> codegen_env

exception WasmRunnerError of Wasm.Source.region * string * Wasm.Ast.module_

val reparse_module : Wasm.Ast.module_ -> Wasm.Ast.module_

val validate_module : Wasm.Ast.module_ -> unit

val compile_wasm_module : ?env:codegen_env -> Mashtree.mash_program -> Wasm.Ast.module_

val module_to_string : Wasm.Ast.module_ -> string
