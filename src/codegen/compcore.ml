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

let init_codegen_env() = {
  heap_top=add_dummy_loc (Int32.of_int 1);
  num_args=0;
  func_offset=0;
  global_offset=1;
  import_global_offset=0;
  import_func_offset=0;
  import_offset=0;
  func_types=ref BatDeque.empty;
  backpatches=ref [];
  imported_funcs=Ident.empty;
  imported_globals=Ident.empty;
}


(* Number of swap variables to allocate *)
let swap_slots_i32 = [Types.I32Type]
let swap_slots_i64 = [Types.I64Type]
let swap_i32_offset = 0
let swap_i64_offset = List.length swap_slots_i32
let swap_slots = List.append swap_slots_i32 swap_slots_i64


(* These are the bare-minimum imports needed for basic runtime support *)
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


(** Untags the number at the top of the stack *)
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

let compile_bind ~is_get (env : codegen_env) (b : binding) : Wasm.Ast.instr' Concatlist.t =
  let (++) a b = Int32.(add (of_int a) b) in
  match b with
  | MArgBind(i) ->
    (* No adjustments are needed for argument bindings *)
    let slot = add_dummy_loc i in
    if is_get then
      singleton (Ast.GetLocal(slot))
    else
      singleton (Ast.SetLocal(slot))
  | MLocalBind(i) ->
    (* Local bindings need to be offset to account for arguments and swap variables *)
    let slot = add_dummy_loc ((env.num_args + (List.length swap_slots)) ++ i) in
    if is_get then
      singleton (Ast.GetLocal(slot))
    else
      singleton (Ast.SetLocal(slot))
  | MSwapBind(i) ->
    (* Swap bindings need to be offset to account for arguments *)
    let slot = add_dummy_loc (env.num_args ++ i) in
    if is_get then
      singleton (Ast.GetLocal(slot))
    else
      singleton (Ast.SetLocal(slot))
  | MGlobalBind(i) ->
    (* Global bindings need to be offset to account for any imports *)
    let slot = add_dummy_loc (env.global_offset ++ i) in
    if is_get then
      singleton (Ast.GetGlobal(slot))
    else
      singleton (Ast.SetGlobal(slot))
  | MClosureBind(i) ->
    (* Closure bindings need to be calculated *)
    begin
      if not(is_get) then
        failwith "Internal error: attempted to emit instruction which would mutate closure contents"
    end;
    cons
      (Ast.GetLocal(add_dummy_loc Int32.zero))
      (singleton (load ~offset:(4 * (3 + Int32.to_int i)) ()))
  | MImport(i) ->
    begin
      if not(is_get) then
        failwith "Internal error: attempted to emit instruction which would mutate an import"
    end;
    (* Adjust for runtime functions *)
    let slot = add_dummy_loc (env.import_offset ++ i) in
    singleton (Ast.GetGlobal(slot))

let get_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~is_get:true env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~is_get:true env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found

let set_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~is_get:false env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~is_get:false env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found


let compile_imm (env : codegen_env) (i : immediate) : Wasm.Ast.instr' Concatlist.t =
  match i with
  | MImmConst c -> singleton (Ast.Const(add_dummy_loc @@ compile_const c))
  | MImmBinding b ->
    compile_bind ~is_get:true env b


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


let compile_prim1 env p1 arg : Wasm.Ast.instr' Concatlist.t =
  let compiled_arg = compile_imm env arg in
  (* TODO: Overflow checks? *)
  match p1 with
  | Add1 -> compiled_arg @+ [
      Ast.Const(encoded_const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
    ]
  | Sub1 -> compiled_arg @+ [
      Ast.Const(encoded_const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]
  | Not -> compiled_arg @+ [
      Ast.Const(const_int32 0x80000000);
      Ast.Binary(Values.I32 Ast.IntOp.Xor);
    ]
  | IsBool -> failwith "Unsupported (compcore): IsBool"
  | IsNum -> failwith "Unsupported (compcore): IsNum"
  | IsTuple -> failwith "Unsupported (compcore): IsTuple"


let compile_prim2 (env : codegen_env) p2 arg1 arg2 : Wasm.Ast.instr' Concatlist.t =
  let compiled_arg1 = compile_imm env arg1 in
  let compiled_arg2 = compile_imm env arg2 in
  let overflow_safe instrs =
    let compiled_swap_get = get_swap ~ty:Types.I64Type env 0 in
    let compiled_swap_set = set_swap ~ty:Types.I64Type env 0 in
    instrs @
    compiled_swap_set @
    compiled_swap_get @
    compiled_swap_get @
    (check_overflow env) @
    compiled_swap_get +@ [
      Ast.Convert(Values.I32 Ast.IntOp.WrapI64);
    ] in
                                                                          
  (* TODO: Overflow checks? *)
  match p2 with
  | Plus ->
    overflow_safe @@
    compiled_arg1 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Add);
    ]
  | Minus ->
    overflow_safe @@
    compiled_arg1 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Sub);
    ]
  | Times ->
    (* Untag one of the numbers:
       ((a * 2) / 2) * (b * 2) = (a * b) * 2
    *)
    overflow_safe @@
    compiled_arg1 @
    untag_number +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Mul);
    ]
  | And ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Binary(Values.I32 Ast.IntOp.And)
    ]
  | Or ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Binary(Values.I32 Ast.IntOp.Or)
    ]
  | Greater ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.GtS)
    ] @ encode_bool
  | GreaterEq ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.GeS)
    ] @ encode_bool
  | Less ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.LtS)
    ] @ encode_bool
  | LessEq ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.LeS)
    ] @ encode_bool
  | Eq ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.Eq)
    ] @ encode_bool


(** Heap allocations. *)
let round_up (num : int) (multiple : int) : int =
  multiple * (((num - 1) / multiple) + 1)

(** Rounds the given number of words to be aligned correctly *)
let round_allocation_size (num_words : int) : int =
  round_up num_words 4

let heap_allocate env (num_words : int) =
  let words_to_allocate = round_allocation_size num_words in
  Concatlist.t_of_list [
    Ast.Const(const_int32 (4 * words_to_allocate));
    call_malloc env;
  ]

let heap_check_memory env (num_words : int) =
  let words_to_allocate = round_allocation_size num_words in
  Concatlist.t_of_list [
    Ast.Const(const_int32 (4 * words_to_allocate));
    call_runtime_check_memory env;
  ]


let buf_to_ints (buf : Buffer.t) : int64 list =
  let num_bytes = Buffer.length buf in
  let num_ints = round_up num_bytes 8 in
  let out_ints = ref [] in

  let byte_buf = Bytes.create (num_ints * 8) in
  Buffer.blit buf 0 byte_buf 0 num_bytes;
  let bytes_to_int = if Sys.big_endian then
      Stdint.Uint64.of_bytes_big_endian
    else
      Stdint.Uint64.of_bytes_little_endian in
  for i = 0 to (num_ints - 1) do
    out_ints := (bytes_to_int byte_buf (i * 8))::!out_ints
  done;
  List.rev @@ List.map Stdint.Uint64.to_int64 !out_ints

let allocate_string env str =
  let str_as_bytes = Bytes.of_string str in
  let num_bytes = Bytes.length str_as_bytes in
  let num_ints = round_up num_bytes 8 in
  let buf = Buffer.create num_ints in
  BatUTF8.iter (BatUTF8.Buf.add_char buf) str;

  let ints_to_push : int64 list = buf_to_ints buf in
  let get_swap = get_swap env 0 in
  let set_swap = set_swap env 0 in
  let preamble = Concatlist.t_of_list [
      Ast.Const(const_int32 @@ (4 * (2 + (2 * (List.length ints_to_push)))));
      call_malloc env;
    ] @ set_swap @ get_swap +@ [
      Ast.Const(const_int32 @@ String.length str);
    ] @ get_swap +@ [
      Ast.Const(const_int32 (tag_val_of_heap_tag_type StringType));
      store ~offset:0 ();
      store ~offset:4 ();
    ] in
  let elts = List.flatten @@ List.mapi (fun idx (i : int64) ->
      Concatlist.list_of_t
        (get_swap +@
         [
           Ast.Const(add_dummy_loc (Values.I64 i));
           store ~ty:Types.I64Type ~offset:(8 * (idx + 1)) ();
         ])) ints_to_push in
  preamble +@ elts @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType(Some StringType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_closure env ?lambda ({func_idx; arity; variables} as closure_data) =
  let num_free_vars = List.length variables in
  let closure_size = num_free_vars + 3 in
  let get_swap = get_swap env 0 in
  let set_swap = set_swap env 0 in
  let access_lambda = Option.default (get_swap @+ [
      Ast.Const(const_int32 @@ 4 * round_allocation_size closure_size);
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]) lambda in
  env.backpatches := (access_lambda, closure_data)::!(env.backpatches);
  (heap_allocate env closure_size) @ set_swap @ get_swap +@ [
    Ast.Const(const_int32 num_free_vars);
  ] @ get_swap +@ [
    Ast.GetGlobal(add_dummy_loc Int32.zero); (* <- relocation offset *)
    Ast.Const(wrap_int32 (Int32.(add func_idx (of_int env.func_offset))));
    Ast.Binary(Values.I32 Ast.IntOp.Add);
  ] @ get_swap +@ [
    Ast.Const(add_dummy_loc (Values.I32Value.to_value arity));

    store ~offset:0 ();
    store ~offset:4 ();
    store ~offset:8 ();
  ] @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]


let allocate_tuple env elts =
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let set_swap = set_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) +@ [
      store ~offset:(4 * (idx + 1)) ();
    ] in

  (heap_allocate env (num_elts + 1)) @ set_swap @ get_swap +@ [
    Ast.Const(const_int32 num_elts);
    store ~offset:0 ();
  ] @ (Concatlist.flatten @@ List.mapi compile_elt elts) @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type TupleTagType);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]


let compile_allocation env alloc_type =
  match alloc_type with
  | MClosure(cdata) -> allocate_closure env cdata
  | MTuple(elts) -> allocate_tuple env elts
  | MString(str) -> allocate_string env str


let compile_tuple_op env tup_imm op =
  let tup = compile_imm env tup_imm in
  match op with
  | MTupleGet(idx) ->
    let idx_int = Int32.to_int idx in
    (* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. *)
    tup @ (untag TupleTagType) +@ [
        load ~offset:(4 * (idx_int + 1)) ();
      ]
  | MTupleSet(idx, imm) ->
    let idx_int = Int32.to_int idx in
    tup @ (untag TupleTagType) @ (compile_imm env imm) +@ [
        store ~offset:(4 * (idx_int + 1)) ();
      ]


let collect_backpatches env f =
  let nested_backpatches = ref [] in
  let res = f {env with backpatches=nested_backpatches} in
  res, !nested_backpatches

let do_backpatches env backpatches =
  let do_backpatch (lam, {variables}) =
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    let preamble = lam +@ [
        Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
        Ast.Binary(Values.I32 Ast.IntOp.Xor);
      ] @ set_swap in
    let backpatch_var idx var =
      get_swap @ (compile_imm env var) +@ [
          store ~offset:(4 * (idx + 3)) ();
        ] in
    preamble @
    (Concatlist.flatten @@ List.mapi backpatch_var variables) in
  (Concatlist.flatten @@ List.map do_backpatch backpatches)

let rec compile_store env binds =
  let process_binds env =
    let process_bind (b, instr) acc =
      let store_bind = compile_bind ~is_get:false env b in
      let get_bind = compile_bind ~is_get:true env b in
      let compiled_instr = match instr with
        | MAllocate(MClosure(cdata)) ->
          allocate_closure env ~lambda:get_bind cdata
        | _ -> compile_instr env instr in
      (compiled_instr @ store_bind) @ acc in
    List.fold_right process_bind binds empty in
  let instrs, backpatches = collect_backpatches env process_binds in
  instrs @ (do_backpatches env backpatches)

and compile_switch env arg branches default =
  (* Constructs the jump table. Assumes that branch 0 is the default *)
  let create_table ?default:(default=0) ?offset:(offset=0) stack =
    let max_label = List.fold_left max 0 stack in
    let label_blocks = (List.mapi (fun i l -> (i + offset, l)) stack
                        |> List.sort (fun (_, l1) (_, l2) -> compare l1 l2)) in
    let default_const = add_dummy_loc @@ Int32.of_int default in
    let matching i (block, lbl) = lbl = i in
    let get_slot i =
      match List.find_opt (matching i) label_blocks with
      | None ->
        default_const
      | Some((b, _)) ->
        add_dummy_loc @@ Int32.of_int b in
    BatList.init (max_label + 1) get_slot in
  let rec process_branches count stack bs =
    match bs with
    | [] ->
      let inner_block_body = (singleton (Ast.Const(const_int32 0))) @
                             (compile_imm env arg) @
                             (* Tag check elided *)
                             untag_number +@
                             [
                               Ast.BrTable((create_table ~offset:1 stack),
                                           (add_dummy_loc (Int32.of_int 0)));
                             ] in
      let default_block_body = compile_block env default in
      Concatlist.t_of_list
              [Ast.Block([Types.I32Type],
                         Concatlist.mapped_list_of_t add_dummy_loc (
                           (singleton (Ast.Block([Types.I32Type],
                                                 Concatlist.mapped_list_of_t add_dummy_loc
                                                   inner_block_body
                                                ))) @
                           default_block_body
                           ))]
    | (lbl, hd)::tl ->
      Concatlist.t_of_list
        [Ast.Block([Types.I32Type],
                   Concatlist.mapped_list_of_t add_dummy_loc
                     ((process_branches (count + 1) ((Int32.to_int lbl)::stack) tl) @
                      (compile_block env hd) +@
                      [
                        Ast.Br(add_dummy_loc @@ Int32.of_int count)
                      ]))] in
  let processed = process_branches 0 [] branches in
  singleton (Ast.Block([Types.I32Type],
                      Concatlist.mapped_list_of_t add_dummy_loc processed))


and compile_block env block =
  Concatlist.flatten (List.map (compile_instr env) block)

and compile_instr env instr =
  match instr with
  | MImmediate(imm) -> compile_imm env imm
  | MAllocate(alloc) -> compile_allocation env alloc
  | MTupleOp(tuple_op, tup) -> compile_tuple_op env tup tuple_op
  | MPrim1(p1, arg) -> compile_prim1 env p1 arg
  | MPrim2(p2, arg1, arg2) -> compile_prim2 env p2 arg1 arg2
  | MSwitch(arg, branches, default) -> compile_switch env arg branches default
  | MStore(binds) -> compile_store env binds

  | MCallIndirect(func, args) ->
    let compiled_func = compile_imm env func in
    let compiled_args = Concatlist.flatten @@ List.map (compile_imm env) args in
    let ftype = add_dummy_loc @@ Int32.of_int (get_arity_func_type_idx env (1 + List.length args)) in
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    let all_args =
      compiled_func @
      untag LambdaTagType @
      set_swap @
      get_swap @
      compiled_args in

    all_args @
    get_swap +@ [
      load ~offset:4 ();
      Ast.CallIndirect(ftype);
    ]

  | MIf(cond, thn, els) ->
    let compiled_cond = compile_imm env cond in
    let compiled_thn = Concatlist.mapped_list_of_t
        add_dummy_loc
        (compile_block env thn) in
    let compiled_els = Concatlist.mapped_list_of_t
        add_dummy_loc
        (compile_block env els) in
    compiled_cond @
    decode_bool +@ [
      Ast.If([Types.I32Type],
             compiled_thn,
             compiled_els);
    ]

  | MError(err, args) ->
    call_error_handler env err args
  | MCallKnown(func_idx, args) ->
    let compiled_args = Concatlist.flatten @@ List.map (compile_imm env) args in
    compiled_args +@ [
      Ast.Call(add_dummy_loc (Int32.of_int (env.import_func_offset + (Int32.to_int func_idx))));
    ]
  | MArityOp _ -> failwith "NYI: (compile_instr): MArityOp"
  | MTagOp _ -> failwith "NYI: (compile_instr): MTagOp"


let compile_function env {index; arity; stack_size; body=body_instrs} =
  let arity_int = Int32.to_int arity in
  let body_env = {env with num_args=arity_int} in
  let body = Concatlist.mapped_list_of_t add_dummy_loc @@
    (compile_block body_env body_instrs) +@ [Ast.Return] in
  let open Wasm.Ast in
  let ftype_idx = get_arity_func_type_idx env arity_int in
  let ftype = add_dummy_loc Int32.(of_int ftype_idx) in
  let locals = List.append swap_slots @@ BatList.init (stack_size) (fun n -> Types.I32Type) in
  add_dummy_loc {
    ftype;
    locals;
    body;
  }

let compute_table_size env {imports; exports; functions} =
  (List.length functions) + ((List.length imports) - (List.length runtime_global_imports)) + 2

let compile_imports env ({imports} as prog) =
  let compile_asm_type t =
    match t with
    | I32Type -> Types.I32Type
    | I64Type -> Types.I64Type
    | F32Type -> Types.F32Type
    | F64Type -> Types.F64Type
  in

  let compile_import_name name = function
    | MImportWasm -> Ident.name name
    | MImportGrain -> "GRAIN$EXPORT$GET$" ^ (Ident.name name)
  in

  let compile_import {mimp_mod; mimp_name; mimp_type; mimp_kind} =
    (* TODO: When user import become a thing, we'll need to worry about hygiene *)
    let module_name = encode_string @@ Ident.name mimp_mod in
    let item_name = encode_string @@ compile_import_name mimp_name mimp_kind in
    let idesc = match mimp_kind, mimp_type with
      | MImportGrain, MGlobalImport typ ->
        let typ = compile_asm_type typ in
        let func_type = Types.FuncType([], [typ]) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
      | _, MFuncImport(args, ret) ->
        let proc_list = List.map compile_asm_type in
        let func_type = Types.FuncType(proc_list args, proc_list ret) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
      | _, MGlobalImport typ ->
        let typ = compile_asm_type typ in
        let imptyp = Types.GlobalType(typ, Types.Immutable) in
        add_dummy_loc @@ Ast.GlobalImport(imptyp)
    in
    let open Wasm.Ast in
    add_dummy_loc {
      module_name;
      item_name;
      idesc;
    } in
  let table_size = compute_table_size env prog in
  let imports = List.map compile_import imports in
  (List.append
    imports
    [
      add_dummy_loc {
        Ast.module_name=encode_string (Ident.name runtime_mod);
        Ast.item_name=encode_string "mem";
        Ast.idesc=add_dummy_loc (Ast.MemoryImport (Types.MemoryType({
            Types.min=Int32.zero;
            Types.max=None;
          })));
      };
      add_dummy_loc {
        Ast.module_name=encode_string (Ident.name runtime_mod);
        Ast.item_name=encode_string "tbl";
        Ast.idesc=add_dummy_loc (Ast.TableImport (Types.TableType({
            Types.min=Int32.of_int table_size;
            Types.max=None;
          }, Types.AnyFuncType)));
      };
    ])

let compile_exports env {functions; imports; exports; num_globals} =
  let compile_getter i {ex_name; ex_global_index; ex_getter_index} =
    let exported_name = "GRAIN$EXPORT$GET$" ^ (Ident.name ex_name) in
    let name = encode_string exported_name in
    let fidx = (Int32.to_int ex_getter_index) + env.func_offset in
    let export =
      let open Wasm.Ast in
      add_dummy_loc {
        name;
        edesc=add_dummy_loc (Ast.FuncExport (add_dummy_loc @@ Int32.of_int fidx));
      } in
    export
  in
  let compile_lambda_export i _ =
    let name = encode_string ("GRAIN$LAM_" ^ (string_of_int i)) in
    let edesc = add_dummy_loc (Ast.FuncExport(add_dummy_loc @@ Int32.of_int (i + env.func_offset))) in
    let open Wasm.Ast in
    add_dummy_loc { name; edesc } in
  let heap_adjust_idx = env.func_offset + (List.length functions) in
  let main_idx = heap_adjust_idx + 1 in
  let heap_adjust_idx = add_dummy_loc @@ Int32.of_int heap_adjust_idx in
  let main_idx = add_dummy_loc @@ Int32.of_int main_idx in
  let compiled_lambda_exports = List.mapi compile_lambda_export functions in
  let compiled_exports = List.mapi compile_getter exports in
  (List.append
     compiled_lambda_exports
     (List.append
        compiled_exports
        [
          add_dummy_loc {
            Ast.name=encode_string "GRAIN$HEAP_ADJUST";
            Ast.edesc=add_dummy_loc (Ast.FuncExport heap_adjust_idx);
          };
          add_dummy_loc {
            Ast.name=encode_string "GRAIN$MAIN";
            Ast.edesc=add_dummy_loc (Ast.FuncExport main_idx);
          };
          add_dummy_loc {
            Ast.name=encode_string (Ident.name table_size);
            Ast.edesc=add_dummy_loc (Ast.GlobalExport (add_dummy_loc @@ Int32.of_int (num_globals + 2)));
          };
        ]))

let compile_tables env prog =
  (*let table_size = compute_table_size env prog in*)
  [
    (*add_dummy_loc {
      Ast.ttype=Types.TableType({
          Types.min=Int32.of_int table_size;
          Types.max=None;
        }, Types.AnyFuncType)
    };*)
  ]


let compile_elems env prog =
  let table_size = compute_table_size env prog in
  let open Wasm.Ast in
  [
    add_dummy_loc {
      index=add_dummy_loc (Int32.zero);
      offset=add_dummy_loc [
        add_dummy_loc (Ast.GetGlobal(add_dummy_loc @@ Int32.of_int 0));
      ];
      init=BatList.init table_size (fun n -> (add_dummy_loc (Int32.of_int n)));
    };
  ]

let compile_globals env ({num_globals} as prog) =
  List.append
    (BatList.init (1 + num_globals) (fun _ -> add_dummy_loc {
         Ast.gtype=Types.GlobalType(Types.I32Type, Types.Mutable);
         Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 0)]);
       }))
    [
      add_dummy_loc {
        Ast.gtype=Types.GlobalType(Types.I32Type, Types.Immutable);
        Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 (compute_table_size env prog))]);
      }
    ]


let heap_adjust env = add_dummy_loc {
  Ast.ftype = add_dummy_loc Int32.(of_int (get_func_type_idx env (Types.FuncType([Types.I32Type], [Types.I32Type]))));
  Ast.locals = [];
  Ast.body = List.map add_dummy_loc [
      (*Ast.GetLocal(add_dummy_loc @@ Int32.of_int 0);
      call_runtime_check_memory env;*)
      Ast.GetGlobal(env.heap_top);
      Ast.GetLocal(add_dummy_loc @@ Int32.of_int 0);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.SetGlobal(env.heap_top);
      Ast.GetGlobal(env.heap_top);
    ]
}

let compile_main env prog =
  compile_function env
    {
      index=Int32.of_int (-99);
      arity=Int32.zero;
      body=prog.main_body;
      stack_size=prog.main_body_stack_size;
    }

let compile_functions env ({functions} as prog) =
  let compiled_funcs = List.map (compile_function env) functions in
  let heap_adjust = heap_adjust env in
  let main = compile_main env prog in
  List.append
    compiled_funcs
    [
      heap_adjust;
      main;
    ]

exception WasmRunnerError of Wasm.Source.region * string * Wasm.Ast.module_

let reparse_module (module_ : Wasm.Ast.module_) =
  let open Wasm.Source in
  let as_str = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ module_) in
  let {it=script} = Wasm.Parse.string_to_module as_str in
  match script with
  | Wasm.Script.Textual(m) -> m
  | Encoded _ -> failwith "Internal error: reparse_module: Returned Encoded (should be impossible)"
  | Quoted _ -> failwith "Internal error: reparse_module: Returned Quoted (should be impossible)"

let validate_module (module_ : Wasm.Ast.module_) =
  try
    Valid.check_module module_
  with
  | Wasm.Valid.Invalid(region, msg) ->
    (* Re-parse module in order to get actual locations *)
    let reparsed = reparse_module module_ in
    (try
       Valid.check_module reparsed
     with
     | Wasm.Valid.Invalid(region, msg) ->
       raise (WasmRunnerError(region, msg, reparsed)));
    raise (WasmRunnerError(region, Printf.sprintf "WARNING: Did not re-raise after reparse: %s" msg, module_))


let prepare env ({imports} as prog) =
  let process_import ?dynamic_offset:(dynamic_offset=0) ?is_runtime_import:(is_runtime_import=false)
      (acc_env) idx {mimp_mod; mimp_name; mimp_type; mimp_kind} =
    let rt_idx = if is_runtime_import then idx + dynamic_offset else idx in
    let register tbl =
      let tbl = begin
        match Ident.find_same_opt mimp_mod tbl with
        | None -> Ident.add mimp_mod Ident.empty tbl
        | Some _ -> tbl
      end in
      Ident.add mimp_mod (Ident.add mimp_name (Int32.of_int rt_idx) (Ident.find_same mimp_mod tbl)) tbl
    in
    let imported_funcs, imported_globals = begin
      match mimp_type with
      | MFuncImport _ ->
        (register acc_env.imported_funcs), acc_env.imported_globals
      | MGlobalImport _ ->
        acc_env.imported_funcs, (register acc_env.imported_globals)
    end in
    {acc_env with imported_funcs; imported_globals} in
  let import_offset = List.length runtime_imports in
  let import_func_offset = List.length runtime_function_imports in
  let import_global_offset = import_offset + (List.length imports) in

  let new_imports = List.append runtime_imports imports in
  let new_env = BatList.fold_lefti (process_import ~is_runtime_import:true) env runtime_global_imports in
  let new_env = BatList.fold_lefti (process_import ~is_runtime_import:true) new_env runtime_function_imports in
  let new_env = BatList.fold_lefti (process_import ~dynamic_offset:import_global_offset) new_env imports in
  let global_offset = import_global_offset in
  let func_offset = global_offset - (List.length runtime_global_imports) in
  {
    new_env with
    import_offset;
    import_func_offset;
    import_global_offset;
    global_offset;
    func_offset;
  }, {
    prog with
    imports=List.append runtime_imports imports;
    num_globals=prog.num_globals + (List.length new_imports) + (List.length imports);
  }


let compile_wasm_module ?env prog =
  let open Wasm.Ast in
  let env = match env with
    | None -> init_codegen_env()
    | Some(e) -> e in
  let env, prog = prepare env prog in
  let funcs = compile_functions env prog in
  let imports = compile_imports env prog in
  let exports = compile_exports env prog in
  let globals = compile_globals env prog in
  let tables = compile_tables env prog in
  let elems = compile_elems env prog in
  let types = List.map add_dummy_loc (BatDeque.to_list !(env.func_types)) in
  let ret = add_dummy_loc {
    empty_module with
    funcs;
    imports;
    exports;
    globals;
    tables;
    elems;
    types;
    start=None;
  } in
  validate_module ret;
  ret

let module_to_string compiled_module =
  (* Print module to string *)
  Wasm.Sexpr.to_string 80 @@ Wasm.Arrange.module_ compiled_module


let () =
  Printexc.register_printer (fun exc ->
      match exc with
      | WasmRunnerError(region, str, module_) ->
        let fmt_module _ m = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m) in
        let s = Printf.sprintf "WASM Runner Exception at %s: '%s'\n%a\n"
          (Wasm.Source.string_of_region region) str
          fmt_module module_ in
        Some(s)
      | _ -> None)

