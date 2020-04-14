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

type bind_action = BindGet | BindSet | BindTee

val init_codegen_env : unit -> codegen_env

exception WasmRunnerError of Wasm.Source.region * string * Wasm.Ast.module_

val store : ?ty:Wasm.Types.value_type -> ?align:int -> ?offset:int -> ?sz:Wasm.Memory.pack_size option -> unit -> Wasm.Ast.instr'
val load : ?ty:Wasm.Types.value_type -> ?align:int -> ?offset:int -> ?sz:(Wasm.Memory.pack_size * Wasm.Memory.extension) option -> unit -> Wasm.Ast.instr'

val const_int32 : int -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase
val const_int64 : int -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase
val const_float32 : float -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase
val const_float64 : float -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase

val encoded_const_int32 : int -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase

val const_true : Wasm.Values.value Wasm.Source.phrase
val const_false : Wasm.Values.value Wasm.Source.phrase
val const_void : Wasm.Values.value Wasm.Source.phrase

val wrap_int32 : Wasm.I32.t -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase
val wrap_int64 : Wasm.I64.t -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase
val wrap_float32 : Wasm.F32.t -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase
val wrap_float64 : Wasm.F64.t -> (Wasm.I32.t, Wasm.I64.t, Wasm.F32.t, Wasm.F64.t) Wasm.Values.op Wasm.Source.phrase

val error_if_true : codegen_env -> Runtime_errors.grain_error -> immediate list -> Wasm.Ast.instr'

val check_overflow : codegen_env -> Wasm.Ast.instr' Concatlist.t

val round_allocation_size : int -> int
val heap_check_memory : codegen_env -> int -> Wasm.Ast.instr' Concatlist.t
val heap_allocate : codegen_env -> int -> Wasm.Ast.instr' Concatlist.t
val heap_allocate_imm : ?additional_words:int -> codegen_env -> immediate -> Wasm.Ast.instr' Concatlist.t

val dummy_err_val : immediate
val add_dummy_loc : 'a -> 'a Wasm.Source.phrase

val compile_imm : codegen_env -> immediate -> Wasm.Ast.instr' Concatlist.t
val compile_bind : action:bind_action -> codegen_env -> binding -> Wasm.Ast.instr' Concatlist.t

val untag : Value_tags.tag_type -> Wasm.Ast.instr' Concatlist.t
val untag_number : Wasm.Ast.instr' Concatlist.t

val encode_bool : Wasm.Ast.instr' Concatlist.t
val decode_bool : Wasm.Ast.instr' Concatlist.t

val encode_string : string -> int list

val call_runtime_check_memory : codegen_env -> Wasm.Ast.instr'
val call_runtime_throw_error : codegen_env -> Wasm.Ast.instr'
val call_console_log : codegen_env -> Wasm.Ast.instr'
val call_malloc : codegen_env -> Wasm.Ast.instr'
val call_error_handler : codegen_env -> Runtime_errors.grain_error -> immediate list -> Wasm.Ast.instr' Concatlist.t

val get_func_type_idx : codegen_env -> Wasm.Types.func_type -> int
val get_arity_func_type_idx : codegen_env -> int -> int

val get_swap : ?ty: Wasm.Types.value_type -> codegen_env -> int -> Wasm.Ast.instr' Concatlist.t
val set_swap : ?ty: Wasm.Types.value_type -> codegen_env -> int -> Wasm.Ast.instr' Concatlist.t
val tee_swap : ?ty: Wasm.Types.value_type -> codegen_env -> int -> Wasm.Ast.instr' Concatlist.t

val var_of_ext_global : codegen_env -> Ident.t -> Ident.t -> int32 Wasm.Source.phrase
val var_of_ext_func : codegen_env -> Ident.t -> Ident.t -> int32 Wasm.Source.phrase

val swap_slots : Wasm.Types.value_type list

val module_runtime_id : Ident.t
val reloc_base : Ident.t
val table_size : Ident.t
val runtime_mod : Ident.t
val console_mod : Ident.t
val check_memory_ident : Ident.t
val throw_error_ident : Ident.t
val log_ident : Ident.t
val malloc_ident : Ident.t

val runtime_global_imports : import list
val runtime_function_imports : import list
val runtime_imports : import list
