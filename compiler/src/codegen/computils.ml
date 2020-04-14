open Grain_typed
open Mashtree
open Value_tags
open Wasm
open Concatlist (* NOTE: This import shadows (@) and introduces (@+) and (+@) *)

let add_dummy_loc (x : 'a) : 'a Source.phrase = Source.(x @@ no_region)

(** Environment *)
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


(* Number of swap variables to allocate *)
let swap_slots_i32 = [Types.I32Type; Types.I32Type]
let swap_slots_i64 = [Types.I64Type]
let swap_i32_offset = 0
let swap_i64_offset = List.length swap_slots_i32
let swap_slots = List.append swap_slots_i32 swap_slots_i64


(* These are the bare-minimum imports needed for basic runtime support *)
let module_runtime_id = Ident.create_persistent "moduleRuntimeId"
let reloc_base = Ident.create_persistent "relocBase"
let table_size = Ident.create_persistent "GRAIN$TABLE_SIZE"
let runtime_mod = Ident.create_persistent "grainRuntime"
let console_mod = Ident.create_persistent "console"
let check_memory_ident = Ident.create_persistent "checkMemory"
let throw_error_ident = Ident.create_persistent "throwError"
let log_ident = Ident.create_persistent "log"
let malloc_ident = Ident.create_persistent "malloc"

let runtime_global_imports = [
  {
    mimp_mod=runtime_mod;
    mimp_name=reloc_base;
    mimp_type=MGlobalImport I32Type;
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  };
  {
    mimp_mod=runtime_mod;
    mimp_name=module_runtime_id;
    mimp_type=MGlobalImport I32Type;
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  }
]

let runtime_function_imports = [
  {
    mimp_mod=console_mod;
    mimp_name=log_ident;
    mimp_type=MFuncImport([I32Type], [I32Type]);
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  };
  {
    mimp_mod=runtime_mod;
    mimp_name=check_memory_ident;
    mimp_type=MFuncImport([I32Type], []);
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  };
  {
    mimp_mod=runtime_mod;
    mimp_name=malloc_ident;
    mimp_type=MFuncImport([I32Type], [I32Type]);
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  };
  {
    mimp_mod=runtime_mod;
    mimp_name=throw_error_ident;
    mimp_type=MFuncImport((BatList.init (Runtime_errors.max_arity + 1) (fun _ -> I32Type)), []);
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  };
]

let runtime_imports = List.append runtime_global_imports runtime_function_imports


(* [TODO]: [philip] Delete remnants of heap_top *)
let init_codegen_env() = {
  heap_top=add_dummy_loc (Int32.of_int (List.length runtime_global_imports));
  num_args=0;
  func_offset=0;
  global_offset=2;
  import_global_offset=0;
  import_func_offset=0;
  import_offset=0;
  func_types=ref BatDeque.empty;
  backpatches=ref [];
  imported_funcs=Ident.empty;
  imported_globals=Ident.empty;
}

exception WasmRunnerError of Wasm.Source.region * string * Wasm.Ast.module_

(* Seems a little silly when named this way, but it makes a little more sense in the
   context of this file, since we are encoding the given OCaml string in a WASM-compatible format
   (its UTF-8 byte sequence). *)
let encode_string : string -> int list = Utf8.decode

let encoded_int32 n = n * 2

let const_int32 n = add_dummy_loc (Values.I32Value.to_value @@ Int32.of_int n)
let const_int64 n = add_dummy_loc (Values.I64Value.to_value @@ Int64.of_int n)
let const_float32 n = add_dummy_loc (Values.F32Value.to_value @@ (Wasm.F32.of_float n))
let const_float64 n = add_dummy_loc (Values.F64Value.to_value @@ (Wasm.F64.of_float n))

(* These are like the above 'const' functions, but take inputs
   of the underlying types instead *)
let wrap_int32 n = add_dummy_loc (Values.I32Value.to_value n)
let wrap_int64 n = add_dummy_loc (Values.I64Value.to_value n)
let wrap_float32 n = add_dummy_loc (Values.F32Value.to_value n)
let wrap_float64 n = add_dummy_loc (Values.F64Value.to_value n)

(** Constant compilation *)
let rec compile_const c : Wasm.Values.value =
  let identity : 'a. 'a -> 'a = fun x -> x in
  let conv_int32 = Int32.(mul (of_int 2)) in
  let conv_int64 = Int64.(mul (of_int 2)) in
  let conv_float32 = identity in
  let conv_float64 = identity in
  begin
    match c with
    | MConstLiteral ((MConstLiteral _) as c) -> compile_const c
    | MConstI32 n -> Values.I32Value.to_value (conv_int32 n)
    | MConstI64 n -> Values.I64Value.to_value (conv_int64 n)
    | MConstF32 n -> Values.F32Value.to_value (Wasm.F32.of_float @@ conv_float32 n)
    | MConstF64 n -> Values.F64Value.to_value (Wasm.F64.of_float @@ conv_float64 n)
    | MConstLiteral (MConstI32 n) -> Values.I32Value.to_value n
    | MConstLiteral (MConstI64 n) -> Values.I64Value.to_value n
    | MConstLiteral (MConstF32 n) -> Values.F32Value.to_value (Wasm.F32.of_float n)
    | MConstLiteral (MConstF64 n) -> Values.F64Value.to_value (Wasm.F64.of_float n)
  end

(* Translate constants to WASM *)
let const_true = add_dummy_loc @@ compile_const const_true
let const_false = add_dummy_loc @@ compile_const const_false
let const_void = add_dummy_loc @@ compile_const const_void

(* WebAssembly helpers *)

(* These instructions get helpers due to their verbosity *)
let store
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0)
    ?sz:(sz=None)
    () =
  let open Wasm.Ast in
  Store({
      ty;
      align;
      sz;
      offset=Int32.of_int offset;
    })

let load
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0)
    ?sz:(sz=None)
    () =
  let open Wasm.Ast in
  Load({
      ty;
      align;
      sz;
      offset=Int32.of_int offset;
    })

let lookup_ext_global env modname itemname =
  Ident.find_same itemname (Ident.find_same modname (env.imported_globals))

let var_of_ext_global env modname itemname =
  add_dummy_loc @@ lookup_ext_global env modname itemname

let lookup_ext_func env modname itemname =
  Ident.find_same itemname (Ident.find_same modname (env.imported_funcs))

let var_of_ext_func env modname itemname =
  add_dummy_loc @@ lookup_ext_func env modname itemname


let call_runtime_check_memory env = Ast.Call(var_of_ext_func env runtime_mod check_memory_ident)
let call_runtime_throw_error env = Ast.Call(var_of_ext_func env runtime_mod throw_error_ident)
let call_console_log env = Ast.Call(var_of_ext_func env console_mod log_ident)

let call_malloc env = Ast.Call(var_of_ext_func env runtime_mod malloc_ident)

let get_func_type_idx env typ =
  match BatDeque.find ((=) typ) !(env.func_types) with
  | None ->
    env.func_types := BatDeque.snoc !(env.func_types) typ;
    BatDeque.size !(env.func_types) - 1
  | Some((i, _)) -> i


let get_arity_func_type_idx env arity =
  let has_arity (Types.FuncType(args, _)) = (List.length args) = arity in
  match BatDeque.find has_arity !(env.func_types) with
  | None ->
    let args = BatList.init arity (fun _ -> Types.I32Type) in
    let ftype = (Types.FuncType(args, [Types.I32Type])) in
    env.func_types := BatDeque.snoc !(env.func_types) ftype;
    BatDeque.size !(env.func_types) - 1
  | Some((i, _)) -> i


let untag_number = Concatlist.t_of_list [
    Ast.Const(const_int32 1);
    Ast.Binary(Values.I32 Ast.IntOp.ShrS)
  ]

let untag tag = Concatlist.t_of_list [
  Ast.Const(const_int32 (tag_val_of_tag_type tag));
  Ast.Binary(Values.I32 Ast.IntOp.Xor);
]

let encode_bool = Concatlist.t_of_list [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.Shl);
  Ast.Const(const_false);
  Ast.Binary(Values.I32 Ast.IntOp.Or);
]

let decode_bool = Concatlist.t_of_list [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.ShrU);
]

let encoded_const_int32 n = const_int32 (encoded_int32 n)

type bind_action = BindGet | BindSet | BindTee

let compile_bind ~action (env : codegen_env) (b : binding) : Wasm.Ast.instr' Concatlist.t =
  let (++) a b = Int32.(add (of_int a) b) in
  match b with
  | MArgBind(i) ->
    (* No adjustments are needed for argument bindings *)
    let slot = add_dummy_loc i in
    begin match action with
      | BindGet ->
        singleton (Ast.LocalGet(slot))
      | BindSet ->
        singleton (Ast.LocalSet(slot))
      | BindTee ->
        singleton (Ast.LocalTee(slot))
    end
  | MLocalBind(i) ->
    (* Local bindings need to be offset to account for arguments and swap variables *)
    let slot = add_dummy_loc ((env.num_args + (List.length swap_slots)) ++ i) in
    begin match action with
      | BindGet ->
        singleton (Ast.LocalGet(slot))
      | BindSet ->
        singleton (Ast.LocalSet(slot))
      | BindTee ->
        singleton (Ast.LocalTee(slot))
    end
  | MSwapBind(i) ->
    (* Swap bindings need to be offset to account for arguments *)
    let slot = add_dummy_loc (env.num_args ++ i) in
    begin match action with
      | BindGet ->
        singleton (Ast.LocalGet(slot))
      | BindSet ->
        singleton (Ast.LocalSet(slot))
      | BindTee ->
        singleton (Ast.LocalTee(slot))
    end
  | MGlobalBind(i) ->
    (* Global bindings need to be offset to account for any imports *)
    let slot = add_dummy_loc (env.global_offset ++ i) in
    begin match action with
      | BindGet ->
        singleton (Ast.GlobalGet(slot))
      | BindSet ->
        singleton (Ast.GlobalSet(slot))
      | BindTee ->
        Concatlist.t_of_list [
          Ast.GlobalSet(slot);
          Ast.GlobalGet(slot);
        ]
    end
  | MClosureBind(i) ->
    (* Closure bindings need to be calculated *)
    begin
      if not(action = BindGet) then
        failwith "Internal error: attempted to emit instruction which would mutate closure contents"
    end;
    cons
      (Ast.LocalGet(add_dummy_loc Int32.zero))
      (singleton (load ~offset:(4 * (3 + Int32.to_int i)) ()))
  | MImport(i) ->
    begin
      if not(action = BindGet) then
        failwith "Internal error: attempted to emit instruction which would mutate an import"
    end;
    (* Adjust for runtime functions *)
    let slot = add_dummy_loc (env.import_offset ++ i) in
    singleton (Ast.GlobalGet(slot))

let get_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~action:BindGet env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~action:BindGet env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found

let set_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~action:BindSet env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~action:BindSet env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found

let tee_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~action:BindTee env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~action:BindTee env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found


let compile_imm (env : codegen_env) (i : immediate) : Wasm.Ast.instr' Concatlist.t =
  match i with
  | MImmConst c -> singleton (Ast.Const(add_dummy_loc @@ compile_const c))
  | MImmBinding b ->
    compile_bind ~action:BindGet env b


(** Heap allocations. *)
let round_up (num : int) (multiple : int) : int =
  num - (num mod multiple) + multiple

(** Rounds the given number of words to be aligned correctly *)
let round_allocation_size (num_words : int) : int =
  round_up num_words 4

let heap_check_memory env (num_words : int) =
  let words_to_allocate = round_allocation_size num_words in
  Concatlist.t_of_list [
    Ast.Const(const_int32 (4 * words_to_allocate));
    call_runtime_check_memory env;
  ]

let heap_allocate env (num_words : int) =
  let words_to_allocate = round_allocation_size num_words in
  Concatlist.t_of_list [
    Ast.Const(const_int32 (4 * words_to_allocate));
    call_malloc env;
  ]

let heap_allocate_imm ?(additional_words=0) env (num_words : immediate) =
  let num_words = compile_imm env num_words in
  (* 
    Normally, this would be:
      Untag num_words
      Find remainder by 4
      Untag num_words
      Add (to get rounded value)
      Multiply by 4 (to get bytes)

    Instead, we do:
      Find remainder by 8
      Add (to get rounded value)
      Multiply by 2 (to get bytes)

    Proof:
      This is an exercise left to the reader.
   *)
  if additional_words = 0 then
    num_words @ num_words +@ [
      (* Round up to nearest multiple of 8 *)
      Ast.Const(const_int32 8);
      Ast.Binary(Values.I32 Ast.IntOp.RemS);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      (* Get number of bytes *)
      Ast.Const(const_int32 2);
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
      call_malloc env;
    ]
  else
    num_words @ num_words +@ [
      (* Add in additional words (tagged) before rounding *)
      Ast.Const(const_int32 (additional_words * 2));
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      (* Round up to nearest multiple of 8 *)
      Ast.Const(const_int32 8);
      Ast.Binary(Values.I32 Ast.IntOp.RemS);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      (* Add in the additional words (tagged) again *)
      Ast.Const(const_int32 (additional_words * 2));
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      (* Get number of bytes *)
      Ast.Const(const_int32 2);
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
      call_malloc env;
    ]


let call_error_handler env err args =
  let pad_val = MImmConst(MConstI32(Int32.zero)) in
  let args = Runtime_errors.pad_args pad_val args in
  let compiled_args = Concatlist.flatten @@ List.map (compile_imm env) args in
  (singleton (Ast.Const(const_int32 @@ Runtime_errors.code_of_error err))) @
  compiled_args +@ [
    call_runtime_throw_error env;
    Ast.Unreachable;
  ]
  

let error_if_true env err args =
  Ast.If([],
         Concatlist.mapped_list_of_t add_dummy_loc (call_error_handler env err args),
         [add_dummy_loc Ast.Nop])


let dummy_err_val = MImmConst(MConstI32(Int32.zero))

(* Checks whether the two Int64s at the top of the stack overflowed *)
let check_overflow env = Concatlist.t_of_list [
  (* WASM has no concept of overflows, so we have to check manually *)
  Ast.Const(const_int64 (Int32.to_int Int32.max_int));
  Ast.Compare(Values.I64 Ast.IntOp.GtS);
  error_if_true env OverflowError [(dummy_err_val); (dummy_err_val)];
  Ast.Const(const_int64 (Int32.to_int Int32.min_int));
  Ast.Compare(Values.I64 Ast.IntOp.LtS);
  error_if_true env OverflowError [(dummy_err_val); (dummy_err_val)];
]