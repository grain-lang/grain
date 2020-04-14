open Grain_typed
open Grain_middle_end
open Mashtree
open Wasm
open Computils

val reparse_module : Wasm.Ast.module_ -> Wasm.Ast.module_

val validate_module : Wasm.Ast.module_ -> unit

val compile_wasm_module : ?env:codegen_env -> Mashtree.mash_program -> Wasm.Ast.module_

val module_to_string : Wasm.Ast.module_ -> string
