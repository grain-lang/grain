open Printf
open Ast_utils
open Legacy_types
open Expr
open Runtime_errors
open Value_tags
open Wasm

module Deque = Batteries.Deque
module Queue = Batteries.Queue


let rec repeat n value =
  if n == 0 then
    []
  else
    value::(repeat (n - 1) value)

let repeat_f n value =
  let rec help n value =
    if n == 0 then
      []
    else
      (value n)::(help (n - 1) value) in
  List.rev @@ help n value

type binding =
  | ArgBind of int32
  | LocalBind of int32
  | GlobalBind of int32
  | ClosureBind of int32

type compiler_env = {
  bindings: binding envt;
  heap_top: int32 Wasm.Source.phrase;
  compiled_lambdas: Ast.func' Deque.dq ref;
  stack_index: int;
  stack_size: int;
  lift_index: int ref;
  num_args: int;
  is_tail: bool;
  func_types: Types.func_type Deque.dq ref;
}

let add_dummy_loc (x : 'a) : 'a Source.phrase = Source.(x @@ no_region)

type fully_typed_import = {
  module_name : string;
  item_name : string;
  ikind : Types.func_type;
}

let external_funcs =
  [
    {
      module_name="console";
      item_name="log";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type])
    };
    {
      module_name="console";
      item_name="debug";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type])
    };
    {
      module_name="console";
      item_name="printClosure";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type])
    };
    {
      module_name="js";
      item_name="throwError";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type; Types.I32Type], [])
    };
    {
      module_name="js";
      item_name="checkMemory";
      ikind=Types.FuncType([Types.I32Type], [])
    };
    {
      module_name="grainBuiltins";
      item_name="print";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="equal";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="toString";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type]);
    };
    {
      module_name="grainBuiltins";
      item_name="stringAppend";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="stringLength";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="stringSlice";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type; Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="DOMQuery";
      ikind=Types.FuncType([Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="DOMSetText";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="DOMDangerouslySetInnerHTML";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type], [Types.I32Type])
    };
    {
      module_name="grainBuiltins";
      item_name="DOMAddEventListener";
      ikind=Types.FuncType([Types.I32Type; Types.I32Type; Types.I32Type], [Types.I32Type]);
    };
  ]

let lookup_ext_func modname itemname =
  let rec do_lookup elts idx =
    match elts with
    | [] -> raise Not_found
    | {module_name; item_name; _}::tl ->
      if module_name = modname && item_name = itemname then
        idx
      else
        do_lookup tl (idx + 1) in
  do_lookup external_funcs 0

let var_of_ext_func modname itemname =
  add_dummy_loc @@ Int32.of_int @@ lookup_ext_func modname itemname

let call_console_log = Ast.Call(add_dummy_loc @@ Int32.of_int 0)
let call_console_debug = Ast.Call(add_dummy_loc @@ Int32.of_int 1)
let call_console_print_closure = Ast.Call(add_dummy_loc @@ Int32.of_int 2)
let call_js_throw_error = Ast.Call(add_dummy_loc @@ Int32.of_int 3)

let initial_env = {
  bindings=[];
  heap_top=add_dummy_loc (Int32.of_int 0);
  compiled_lambdas=ref Deque.empty;
  stack_index=0;
  stack_size=0;
  lift_index=ref (List.length external_funcs);
  num_args=0;
  is_tail=true;
  func_types=ref Deque.empty;
}

let get_func_type_idx typ env =
  match Deque.find ((=) typ) !(env.func_types) with
  | None ->
    env.func_types := Deque.snoc !(env.func_types) typ;
    Deque.size !(env.func_types) - 1
  | Some((i, _)) -> i

let get_arity_func_type_idx arity env =
  let has_arity (Types.FuncType(args, _)) = (List.length args) = arity in
  match Deque.find has_arity !(env.func_types) with
  | None ->
    env.func_types := Deque.snoc !(env.func_types)
        (Types.FuncType((repeat arity Types.I32Type), [Types.I32Type]));
    Deque.size !(env.func_types) - 1
  | Some((i, _)) -> i

(* Seems a little silly when named this way, but it makes a little more sense in the
   context of this file, since we are encoding the given OCaml string in a WASM-compatible format
   (its UTF-8 byte sequence). *)
let encode_string : string -> int list = Utf8.decode

let resolve_func_import env ({module_name;item_name;ikind} : fully_typed_import) : Ast.import' =
  let open Ast in
  {
    module_name=encode_string module_name;
    item_name=encode_string item_name;
    idesc=add_dummy_loc @@
      Ast.FuncImport(add_dummy_loc
                     @@ Int32.of_int
                     @@ get_func_type_idx ikind env)
  }

let const_int32 n = add_dummy_loc (Values.I32Value.to_value @@ Int32.of_int n)
let const_int64 n = add_dummy_loc (Values.I64Value.to_value @@ Int64.of_int n)

let const_true  = const_int32 (0xFFFFFFFF)
let const_false = const_int32 (0x7FFFFFFF)
let bool_mask   = const_int32 (0x80000000)
let tag_as_bool = const_int32 (0x00000001)

let str_of_env env =
  sprintf "{bindings=%s}" (ExtLib.dump env.bindings)


(** Maps `f` over `lst`, appending the results. *)
let flat_map (f : 'a -> 'b list) (lst : 'a list) : 'b list = List.fold_left List.append [] (List.rev_map f lst)

let rec find ls x =
  match ls with
  | [] -> failwith (sprintf "Name %s not found" x)
  | (y,v)::rest ->
     if y = x then v else find rest x



let reserve_stack num_words =
  failwith "NYI: reserve_stack"

(** Calls the given function with the given list of arguments *)
let func_apply (name : string) (args : 'a list) (stack_size : int) =
  failwith "NYI: func_apply"

let lambda_apply (lambda : 'a) (args : 'b list) (stack_size : int) =
  failwith "NYI: lambda_apply"

let call_error_handler (code : int) val1 val2 =
  (* stack_size doesn't matter here, since this call won't return *)
  [
    Ast.Const(const_int32 code);
  ] @ val1 @ val2 @ [
    call_js_throw_error;
    Ast.Unreachable;
  ]

let dummy_err_val = [Ast.Const(const_int32 0)]

let error_if_true err val1 val2 =
  Ast.If([],
         List.map add_dummy_loc
           (call_error_handler (code_of_error err) val1 val2),
         [add_dummy_loc Ast.Nop])

let error_if_false err val1 val2 =
  Ast.If([],
         [add_dummy_loc Ast.Nop],
         List.map add_dummy_loc
           (call_error_handler (code_of_error err) val1 val2))

(*  label_of_error code_of_error *)
let create_error_handlers (defs : grain_error list) =
  failwith "NYI: create_error_handlers"

let heap_allocate (num_words : int) env =
  let words_to_allocate = 4 * (((num_words - 1) / 4) + 1) in
  [
    Ast.GetGlobal(env.heap_top);
    Ast.Const(const_int32 (4 * words_to_allocate));
    Ast.Binary(Values.I32 Ast.IntOp.Add);
    Ast.SetGlobal(env.heap_top);
  ]

let heap_check_memory (num_words : int) =
  let words_to_allocate = 4 * (((num_words - 1) / 4) + 1) in
  [
    Ast.Const(const_int32 (4 * words_to_allocate));
    Ast.Call(var_of_ext_func "js" "checkMemory");
  ]

(* Checks that the given value has the given tag (value ends up in dest) *)
let check_tag (tag : tag_type) (value : Ast.instr' list) =
  [
    Ast.Const(const_int32 1);
  ] @ value @ [
    Ast.Const(const_int32 (and_mask_of_tag_type tag));
    Ast.Binary(Values.I32 Ast.IntOp.And);
    Ast.Binary(Values.I32 Ast.IntOp.Shl);
    Ast.Const(const_int32 (shift_amount_of_tag_type tag));
    Ast.Binary(Values.I32 Ast.IntOp.Shl);
    Ast.Const(const_false);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let check_generic_tag (tag : heap_tag_type) (value : Ast.instr' list) =
  [
    Ast.Const(const_int32 1);
  ] @ value @ [
    Ast.Const(const_int32 (and_mask_of_tag_type GenericHeapType));
    Ast.Binary(Values.I32 Ast.IntOp.And);
    Ast.Binary(Values.I32 Ast.IntOp.Shl);
    Ast.Const(const_int32 (shift_amount_of_tag_type GenericHeapType));
    Ast.Binary(Values.I32 Ast.IntOp.Shl);
    Ast.Const(const_false);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ] @ value @ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type GenericHeapType);
    Ast.Binary(Values.I32 Ast.IntOp.Xor);
    Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
    Ast.Const(const_int32 (tag_val_of_heap_tag_type tag));
    Ast.Compare(Values.I32 Ast.IntOp.Eq);
  ]

(* Assert that the given value has the given tag *)
let assert_tag value err val_tag =
  [
    Ast.Const(const_int32 1);
  ] @ value @ [
    Ast.Const(const_int32 (and_mask_of_tag_type val_tag));
    Ast.Binary(Values.I32 Ast.IntOp.And);
    Ast.Binary(Values.I32 Ast.IntOp.Shl);
    Ast.Const(const_int32 (1 lsl (tag_val_of_tag_type val_tag)));
    Ast.Compare(Values.I32 Ast.IntOp.Eq);
    error_if_false err value dummy_err_val;
  ]

let assert_num arg err =
  assert_tag arg err NumberTagType

let assert_bool arg err =
  assert_tag arg err BooleanTagType

let assert_tuple arg err =
  assert_tag arg err TupleTagType

let assert_lambda arg err =
  assert_tag arg err LambdaTagType

let assert_generic gentype value err =
  [
    Ast.Const(const_int32 1);
  ] @ value @ [
    Ast.Const(const_int32 (and_mask_of_tag_type GenericHeapType));
    Ast.Binary(Values.I32 Ast.IntOp.And);
    Ast.Binary(Values.I32 Ast.IntOp.Shl);
    Ast.Const(const_int32 (1 lsl (tag_val_of_tag_type GenericHeapType)));
    Ast.Compare(Values.I32 Ast.IntOp.Eq);
    error_if_false err value dummy_err_val;
  ] @ value @ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type GenericHeapType);
    Ast.Binary(Values.I32 Ast.IntOp.Xor);
    Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
    Ast.Const(const_int32 (tag_val_of_heap_tag_type gentype));
    Ast.Compare(Values.I32 Ast.IntOp.Eq);
    error_if_false err value dummy_err_val;
  ]

let assert_string = assert_generic StringType

let get_arity tup_or_lambda tag =
  tup_or_lambda @ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type tag);
    Ast.Binary(Values.I32 Ast.IntOp.Xor);
    Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
  ]

let check_tuple_idx arg tup is_get =
  let non_num = if is_get then GetItemIndexNotNumber else SetItemIndexNotNumber in
  let too_small = if is_get then GetItemIndexTooSmall else SetItemIndexTooSmall in
  let too_large = if is_get then GetItemIndexTooLarge else SetItemIndexTooLarge in
  (assert_num arg non_num) @ arg @ [
    Ast.Const(const_int32 0);
    Ast.Compare(Values.I32 Ast.IntOp.LtS);
    error_if_true too_small arg dummy_err_val;
  ] @ arg @ [
    (* Untag index *)
    Ast.Const(const_int32 1);
    Ast.Binary(Values.I32 Ast.IntOp.ShrS);
  ] @ get_arity tup TupleTagType @ [
    Ast.Compare(Values.I32 Ast.IntOp.GeS);
    error_if_true too_large arg (get_arity tup TupleTagType)
  ]

let check_arity arg called_arity =
  [
    Ast.Const(const_int32 called_arity);
  ] @ get_arity arg LambdaTagType @ [
    Ast.Compare(Values.I32 Ast.IntOp.Ne);
    error_if_true ArityMismatch ([Ast.Const(const_int32 called_arity)]) (get_arity arg LambdaTagType)
  ]

(* Checks whether the two Int64s at the top of the stack overflowed *)
let check_overflow = [
  (* WASM has no concept of overflows, so we have to check manually *)
  Ast.Const(const_int64 (Int32.to_int Int32.max_int));
  Ast.Compare(Values.I64 Ast.IntOp.GtS);
  error_if_true OverflowError (dummy_err_val) (dummy_err_val);
  Ast.Const(const_int64 (Int32.to_int Int32.min_int));
  Ast.Compare(Values.I64 Ast.IntOp.LtS);
  error_if_true OverflowError (dummy_err_val) (dummy_err_val);
]

let get_tag = function
  | ImmNum(_, t)
  | ImmBool(_, t)
  | ImmId(_, t) -> t

let buf_to_ints (buf : Buffer.t) : int64 list =
  let num_bytes = Buffer.length buf in
  let num_ints = (((num_bytes - 1) / 8) + 1) in
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

let compile_string str env =
  let str_as_bytes = Bytes.of_string str in
  let num_bytes = Bytes.length str_as_bytes in
  let num_ints = 8 * (((num_bytes - 1) / 8) + 1) in
  let buf = Buffer.create num_ints in
  BatUTF8.iter (BatUTF8.Buf.add_char buf) str;


  let ints_to_push : int64 list = buf_to_ints buf in
  let preamble = (heap_check_memory (2 + (2 * (List.length ints_to_push)))) @ [
    Ast.GetGlobal(env.heap_top);
    Ast.GetGlobal(env.heap_top);
    Ast.Const(const_int32 @@ String.length str);
    Ast.GetGlobal(env.heap_top);
    Ast.Const(const_int32 (tag_val_of_heap_tag_type StringType));
    Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
    Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 4; Ast.sz=None});
    Ast.GetGlobal(env.heap_top);
    Ast.Const(const_int32 8);
    Ast.Binary(Values.I32 Ast.IntOp.Add);
    Ast.SetGlobal(env.heap_top);
  ] in
  let elts = List.flatten @@ List.map (fun (i : int64) -> [
        Ast.GetGlobal(env.heap_top);
        Ast.Const(add_dummy_loc (Values.I64 i));
        Ast.Store({Ast.ty=Types.I64Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
        Ast.GetGlobal(env.heap_top);
        Ast.Const(const_int32 8);
        Ast.Binary(Values.I32 Ast.IntOp.Add);
        Ast.SetGlobal(env.heap_top);
      ]) ints_to_push in
  preamble @ elts @ [
    (* Original heap top should now be on the stack *)
    Ast.Const(const_int32 @@ tag_val_of_tag_type GenericHeapType);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]


(** Untags the number at the top of the stack *)
let untag_number = [Ast.Const(const_int32 1); Ast.Binary(Values.I32 Ast.IntOp.ShrS)]

let untag tag = [
  Ast.Const(const_int32 (tag_val_of_tag_type tag));
  Ast.Binary(Values.I32 Ast.IntOp.Xor);
]

let encode_bool = [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.Shl);
  Ast.Const(const_false);
  Ast.Binary(Values.I32 Ast.IntOp.Or);
]

let decode_bool = [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.ShrU);
]

let tag_value tagtype = []

let encode n = n * 2

let flat_map_i2 (f : 'a -> int -> int -> 'b list) (start1 : int) (start2 : int) (lst : 'a list) : 'b list =
  let (res, _, _) = List.fold_left
      (fun (acc_lst, i1, i2) x -> ((f x i1 i2)::acc_lst), i1 + 1, i2 + 1) ([], start1, start2) lst in
  List.flatten res

let map_i (f : 'a -> int -> 'b) (xs : 'a list) : 'b list =
  let rec helper xs i acc =
    match xs with
    | [] -> acc
    | hd::tl -> helper tl (i + 1) ((f hd i)::acc) in
  List.rev @@ helper xs 0 []


let rec compile_fun (fun_name : string) (args : string list) env callee_restore extra_stack body : (Wasm.Ast.instr list * Wasm.Ast.instr list * compiler_env * int) =
  failwith "NYI: compile_fun"

and backpatch ?lambda:(lambda_opt=None) (free_vars : string list) (env : compiler_env) =
  let lambda = Option.default [Ast.GetGlobal(env.heap_top)] lambda_opt in
  let backpatch_var var idx =
    lambda @ (compile_imm (ImmId(var, -1)) env) @ [
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int (4 * (idx + 3)); Ast.sz=None});
    ] in
  List.flatten @@ map_i backpatch_var free_vars

and create_closure arity compiled_body env stack_size free_vars =
  let type_idx = get_arity_func_type_idx arity env in
  let lift_index = !(env.lift_index) in
  env.lift_index := lift_index + 1;
  env.compiled_lambdas := Deque.snoc !(env.compiled_lambdas) {
      Ast.ftype = add_dummy_loc @@ Int32.of_int type_idx;
      Ast.locals = repeat stack_size Types.I32Type;
      Ast.body = List.map add_dummy_loc compiled_body;
    };
  lift_index, (3 + (List.length free_vars)), free_vars

and compile_lambda (args : string list) body env : (int * int * string list) =
  let used_var_set = Ast_utils.free_vars body in
  let free_var_set = BindingSet.diff used_var_set @@ BindingSet.of_list args in
  let free_vars = Ast_utils.BindingSet.elements free_var_set in
  let free_env = map_i (fun var closure_idx ->
      (var, ClosureBind(Int32.of_int closure_idx))) free_vars in
  let new_args = "$self"::args in
  let arg_env = map_i (fun var arg_idx ->
      (var, ArgBind(Int32.of_int arg_idx))) new_args in
  let bindings = free_env @ arg_env in
  let stack_size = count_vars body in
  let preamble = [] in
  let postamble = [Ast.Return] in
  let compiled_func =
    preamble @ (compile_aexpr body {
        env with
        bindings=bindings;
        stack_index=0;
        stack_size=stack_size;
        num_args=(List.length new_args);
        is_tail=true;
      }) @ postamble in
  create_closure (List.length new_args) compiled_func env stack_size free_vars


and compile_aexpr (e : tag aexpr) (env : compiler_env) =
  match e with
  | ALet(name, rhs, body, t) ->
    let new_stack_idx = (Int32.of_int @@ env.stack_index) in
    let local_idx = (Int32.of_int @@ env.num_args + env.stack_index) in
    let new_loc = LocalBind(new_stack_idx) in
    let new_env = { env with
                    bindings = (name, new_loc)::env.bindings;
                    stack_index = env.stack_index + 1;
                    is_tail = false } in
    (compile_cexpr rhs env) @
    [Ast.SetLocal(add_dummy_loc local_idx)] @
    (compile_aexpr body new_env)
  | ALetRec(binds, body, t) ->
    let new_env = {
      env with
      bindings = (map_i (fun (name, _) idx ->
          let new_stack_idx = (Int32.of_int @@ env.stack_index + idx) in
          (name, LocalBind(new_stack_idx))) binds) @ env.bindings;
      stack_index = env.stack_index + (List.length binds);
    } in
    let compiled_binds, free_vars = List.split @@ map_i (fun (name, bind) idx ->
        let local_idx = (Int32.of_int @@ env.num_args + env.stack_index + idx) in
        match bind with
        | CLambda(args, body, t) ->
          let func_index, closure_size, free_vars = compile_lambda args body env in
          ((heap_check_memory closure_size) @ [
            Ast.GetGlobal(env.heap_top);
            Ast.Const(const_int32 (closure_size - 3));

            Ast.GetGlobal(env.heap_top);
            Ast.Const(const_int32 func_index);

            Ast.GetGlobal(env.heap_top);
            Ast.Const(const_int32 (List.length args));

            Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
            Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 4; Ast.sz=None});
            Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 8; Ast.sz=None});
            Ast.GetGlobal(env.heap_top);
            Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
            Ast.Binary(Values.I32 Ast.IntOp.Or);
            Ast.SetLocal(add_dummy_loc local_idx);
          ] @ (heap_allocate closure_size env), free_vars)
        | _ ->
          ((compile_cexpr bind {env with is_tail=false}) @
           [Ast.SetLocal(add_dummy_loc local_idx)], [])) binds in
    (List.flatten (compiled_binds @ (map_i (fun free_vars idx ->
         match free_vars with
         | [] -> []
         | _ ->
           let local_idx = (Int32.of_int @@ env.num_args + env.stack_index + idx) in
           (backpatch ~lambda:(Some(
                (* The local is stored with its tag, so we need to
                   untag it in order to get the pointer when
                   backpatching *)
                [
                  Ast.GetLocal(add_dummy_loc local_idx);
                  Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
                  Ast.Binary(Values.I32 Ast.IntOp.Xor);
                ])) free_vars new_env)) free_vars))) @
    (compile_aexpr body new_env)
  | ASeq(hd, tl, _) ->
    (compile_cexpr hd env) @ [Ast.Drop] @ (compile_aexpr tl env)
  | ACExpr(e) -> compile_cexpr e env
and compile_cexpr (e : tag cexpr) env =
  match e with
  | CSwitch(arg, branches, t) ->
    (* Constructs the jump table. Assumes that branch 0 is the default *)
    let create_table ?default:(default=0) ?offset:(offset=0) stack =
      let max_label = List.fold_left max 0 stack in
      let label_blocks = ref (List.mapi (fun i l -> (i + offset, l)) stack
                              |> List.sort (fun (_, l1) (_, l2) -> compare l1 l2)) in
      let default_const = add_dummy_loc @@ Int32.of_int default in
      let get_slot i =
        match !label_blocks with
        | (block, lbl)::tl when lbl = i ->
          label_blocks := tl;
          add_dummy_loc @@ Int32.of_int block
        | _ -> default_const in
      BatList.init max_label get_slot in
    let rec process_branches count stack bs =
      match bs with
      | [] -> [Ast.Block([Types.I32Type],
                         List.map add_dummy_loc (
                           [Ast.Const(const_int32 0)] @
                           (compile_imm arg env) @
                           (assert_num (compile_imm arg env) IfError) @ (* FIXME: Should be a different error *)
                           untag_number @
                           [
                             Ast.BrTable((create_table ~offset:1 stack),
                                         (add_dummy_loc (Int32.of_int 0)));
                           ]))] @
                           (call_error_handler (code_of_error GenericNumberError) [Ast.Const const_true] [Ast.Const const_true])
      | (lbl, hd)::tl ->
        [Ast.Block([Types.I32Type],
                   List.map add_dummy_loc
                     ((process_branches (count + 1) (lbl::stack) tl) @
                      (compile_aexpr hd env) @
                      [Ast.Br(add_dummy_loc @@ Int32.of_int count)]))] in
    let processed = process_branches 0 [] branches in
    [Ast.Block([Types.I32Type],
               List.map add_dummy_loc processed)]
  | CIf(cond, thn, els, t) ->
    (compile_imm cond env) @
    (assert_bool (compile_imm cond env) IfError) @
    decode_bool @
    [Ast.If([Types.I32Type],
            List.map add_dummy_loc (compile_aexpr thn env),
            List.map add_dummy_loc (compile_aexpr els env))]

  | CPrim1(Add1, arg, _) ->
    (compile_imm arg env) @
    (* We have to call 'compile_imm' again here because there is no
       instruction to duplicate the top of the stack. *)
    (assert_num (compile_imm arg env) ArithmeticError) @
    [Ast.Const(const_int32 @@ encode 1); Ast.Binary(Values.I32 Ast.IntOp.Add)]
  | CPrim1(Sub1, arg, _) ->
    (compile_imm arg env) @
    (assert_num (compile_imm arg env) ArithmeticError) @
    [Ast.Const(const_int32 @@ encode 1); Ast.Binary(Values.I32 Ast.IntOp.Sub)]

  | CPrim1(IsBool, arg, _) ->
    check_tag BooleanTagType (compile_imm arg env)
  | CPrim1(IsNum, arg, _) ->
    check_tag NumberTagType (compile_imm arg env)
  | CPrim1(IsTuple, arg, _) ->
    check_tag TupleTagType (compile_imm arg env)

  | CPrim1(Not, arg, _) ->
    (compile_imm arg env) @
    (assert_bool (compile_imm arg env) LogicError) @
    [
      Ast.Const(const_int32 0x80000000);
      Ast.Binary(Values.I32 Ast.IntOp.Xor);
    ]

  | CPrim2(Plus, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ArithmeticError) @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ArithmeticError) @
    [Ast.Binary(Values.I32 Ast.IntOp.Add);] @
    (compile_imm arg1 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    (compile_imm arg2 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    [Ast.Binary(Values.I64 Ast.IntOp.Add)] @
    (compile_imm arg1 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    (compile_imm arg2 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    [Ast.Binary(Values.I64 Ast.IntOp.Add)] @
    check_overflow
  | CPrim2(Minus, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ArithmeticError) @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ArithmeticError) @
    [Ast.Binary(Values.I32 Ast.IntOp.Sub)] @
    (compile_imm arg1 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    (compile_imm arg2 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    [Ast.Binary(Values.I64 Ast.IntOp.Sub)] @
    (compile_imm arg1 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    (compile_imm arg2 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    [Ast.Binary(Values.I64 Ast.IntOp.Sub)] @
    check_overflow
  | CPrim2(Times, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ArithmeticError) @
    untag_number @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ArithmeticError) @
    [Ast.Binary(Values.I32 Ast.IntOp.Mul)] @
    (compile_imm arg1 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    (compile_imm arg2 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    [Ast.Binary(Values.I64 Ast.IntOp.Mul)] @
    (compile_imm arg1 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    (compile_imm arg2 env) @
    [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)] @
    [Ast.Binary(Values.I64 Ast.IntOp.Mul)] @
    check_overflow

  | CPrim2(And, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_bool (compile_imm arg1 env) LogicError) @
    (compile_imm arg2 env) @
    (assert_bool (compile_imm arg2 env) LogicError) @
    [Ast.Binary(Values.I32 Ast.IntOp.And)]
  | CPrim2(Or, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_bool (compile_imm arg1 env) LogicError) @
    (compile_imm arg2 env) @
    (assert_bool (compile_imm arg2 env) LogicError) @
    [Ast.Binary(Values.I32 Ast.IntOp.Or)]

  | CPrim2(Greater, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ComparisonError) @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ComparisonError) @
    [Ast.Compare(Values.I32 Ast.IntOp.GtS)] @
    encode_bool
  | CPrim2(GreaterEq, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ComparisonError) @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ComparisonError) @
    [Ast.Compare(Values.I32 Ast.IntOp.GeS)] @
    encode_bool
  | CPrim2(Less, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ComparisonError) @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ComparisonError) @
    [Ast.Compare(Values.I32 Ast.IntOp.LtS)] @
    encode_bool
  | CPrim2(LessEq, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (assert_num (compile_imm arg1 env) ComparisonError) @
    (compile_imm arg2 env) @
    (assert_num (compile_imm arg2 env) ComparisonError) @
    [Ast.Compare(Values.I32 Ast.IntOp.LeS)] @
    encode_bool

  | CPrim2(Eq, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    (compile_imm arg2 env) @
    [Ast.Compare(Values.I32 Ast.IntOp.Eq)] @
    encode_bool

  | CString(s, t) -> compile_string s env

  | CTuple(elts, t) ->
    (* TODO: Perform any GC before *)
    let num_elts = List.length elts in
    (heap_check_memory (num_elts + 1)) @ [
      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 num_elts);
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
    ] @
    (List.flatten @@ map_i (fun e i -> [
           Ast.GetGlobal(env.heap_top)
         ] @ (compile_imm e env) @ [
             Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int (4 * (i + 1)); Ast.sz=None});
           ]) elts) @
    [
      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 @@ tag_val_of_tag_type TupleTagType);
      Ast.Binary(Values.I32 Ast.IntOp.Or);
    ] @
    (heap_allocate (num_elts + 1) env)

  | CGetItem(tup, index, tag) ->
    (assert_tuple (compile_imm tup env) GetItemNotTuple) @
    compile_imm tup env @ [
      Ast.Const(const_int32 @@ tag_val_of_tag_type TupleTagType);
      Ast.Binary(Values.I32 Ast.IntOp.Xor)
    ]
    @ (check_tuple_idx (compile_imm index env) (compile_imm tup env) true) @
    compile_imm index env @
    [
      Ast.Const(const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Shl);
      Ast.Const(const_int32 4);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None})
    ]
  | CSetItem(tup, index, value, tag) ->
    (assert_tuple (compile_imm tup env) GetItemNotTuple) @
    compile_imm tup env @ [
      Ast.Const(const_int32 @@ tag_val_of_tag_type TupleTagType);
      Ast.Binary(Values.I32 Ast.IntOp.Xor)
    ]
    @ (check_tuple_idx (compile_imm index env) (compile_imm tup env) false) @
    compile_imm index env @
    [
      Ast.Const(const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Shl);
      Ast.Const(const_int32 4);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
    ] @ compile_imm value env @ [
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
    ] @ compile_imm value env

  | CLambda(args, body, t) ->
    let idx, closure_size, free_vars = compile_lambda args body env in
    (heap_check_memory closure_size) @ [
      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 (closure_size - 3));

      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 idx);

      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 (List.length args));

      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 4; Ast.sz=None});
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 8; Ast.sz=None});
    ] @ (backpatch free_vars env) @ [
      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
      Ast.Binary(Values.I32 Ast.IntOp.Or);
    ] @ (heap_allocate closure_size env)

  | CApp(func, args, t) ->
    (* TODO: Tag & arity checks *)
    let compiled_func = compile_imm func env in
    let compiled_args = List.flatten @@ List.map (fun x -> compile_imm x env) args in
    let ftype = add_dummy_loc @@ Int32.of_int @@ get_arity_func_type_idx (1 + List.length args) env in
    (*compiled_func @ untag LambdaTagType @ [call_console_print_closure; Ast.Drop] @*)
    (assert_lambda compiled_func CalledNonFunction) @
    check_arity compiled_func (List.length args) @
    compiled_func @
    untag LambdaTagType @
    compiled_args @
    compiled_func @
    untag LambdaTagType @
    [
      Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 4; Ast.sz=None});
      Ast.CallIndirect(ftype);
    ]

  | CImmExpr(i) -> compile_imm i env
and compile_imm (i : tag immexpr) env : Ast.instr' list =
  (* TODO: Decide if this should push the value onto the stack or not *)
  match i with
  | ImmNum(n, _) -> [Ast.Const(const_int32 @@ encode n)]
  | ImmBool(b, _) ->
    if b then
      [Ast.Const(const_true)]
    else
      [Ast.Const(const_false)]
  | ImmId(name, _) ->
    match find env.bindings name with
    | ArgBind(n) -> [Ast.GetLocal(add_dummy_loc n)]
    | LocalBind(n) -> [Ast.GetLocal(add_dummy_loc @@ Int32.of_int (Int32.to_int n + env.num_args))]
    | GlobalBind(n) -> [Ast.GetGlobal(add_dummy_loc n)]
    | ClosureBind(n) -> [
        Ast.GetLocal(add_dummy_loc @@ Int32.of_int 0);
        Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int (4 * (3 + (Int32.to_int n))); Ast.sz=None})
      ]

let builtins = [
  ("print", 1, lookup_ext_func "grainBuiltins" "print");
  ("equal", 2, lookup_ext_func "grainBuiltins" "equal");
  ("toString", 1, lookup_ext_func "grainBuiltins" "toString");
  ("strcat", 2, lookup_ext_func "grainBuiltins" "stringAppend");
  ("strlen", 1, lookup_ext_func "grainBuiltins" "stringLength");
  ("strslice", 3, lookup_ext_func "grainBuiltins" "stringSlice");
  ("DOM::query", 1, lookup_ext_func "grainBuiltins" "DOMQuery");
  ("DOM::setText", 2, lookup_ext_func "grainBuiltins" "DOMSetText");
  ("DOM::dangerouslySetInnerHTML", 2, lookup_ext_func "grainBuiltins" "DOMDangerouslySetInnerHTML");
  ("DOM::addEventListener", 3, lookup_ext_func "grainBuiltins" "DOMAddEventListener");
]

let create_single_builtin_closure fidx arity env =
  let body =
    (repeat_f arity (fun i -> Ast.GetLocal(add_dummy_loc @@ Int32.of_int i))) @ [
      Ast.Call(add_dummy_loc @@ Int32.of_int fidx);
      Ast.Return;
    ] in

  let idx, closure_size, free_vars = create_closure (arity + 1) body env 0 [] in
  (heap_check_memory closure_size) @ [
      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 (closure_size - 3));

      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 idx);

      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 arity);

      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None});
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 4; Ast.sz=None});
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 8; Ast.sz=None});
    ] @ (backpatch free_vars env) @ [
      Ast.GetGlobal(env.heap_top);
      Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
      Ast.Binary(Values.I32 Ast.IntOp.Or);
    ] @ (heap_allocate closure_size env)

let heap_adjust env = {
  Ast.ftype = add_dummy_loc Int32.(of_int (get_func_type_idx (Types.FuncType([Types.I32Type], [Types.I32Type])) env));
  Ast.locals = [];
  Ast.body = List.map add_dummy_loc [
      Ast.GetLocal(add_dummy_loc @@ Int32.of_int 0);
      Ast.Call(var_of_ext_func "js" "checkMemory");
      Ast.GetGlobal(env.heap_top);
      Ast.GetLocal(add_dummy_loc @@ Int32.of_int 0);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.SetGlobal(env.heap_top);
      Ast.GetGlobal(env.heap_top);
    ]
}

let make_lambda_export i = add_dummy_loc {
  Ast.name=encode_string ("GRAIN$LAM_" ^ (string_of_int i));
  Ast.edesc=add_dummy_loc (Ast.FuncExport(add_dummy_loc @@ Int32.of_int i));
}

let create_builtin_closures to_create init_env =
  let env, preamble, _ =
    List.fold_left (fun (env, preamble, idx) (name, arity, fidx) ->
        let closure = create_single_builtin_closure fidx arity env in
        let setup =
          closure @ [
            Ast.SetGlobal(add_dummy_loc @@ Int32.of_int idx);
          ] in
        ({env with
          bindings = (name, GlobalBind(Int32.of_int idx))::env.bindings
         }, preamble @ setup, idx + 1)
      ) (init_env, [], 1) to_create in
  env, preamble

let compile_aprog (anfed : tag aprogram) =
  let stack_size = count_vars anfed in
  let heap_top = add_dummy_loc (Int32.of_int 0) in
  let env, builtin_setup = create_builtin_closures builtins {initial_env with stack_size = stack_size; heap_top=heap_top} in
  let compiled = List.map add_dummy_loc @@
    builtin_setup @
    compile_aexpr anfed env
    @ [Ast.Return] in

  (* Type of main function *)
  let ftype = add_dummy_loc
      Int32.(of_int (get_func_type_idx (Types.FuncType([], [Types.I32Type])) env)) in

  (* List of imports for the module (functions + memory) *)
  let imports = List.map add_dummy_loc @@
      (List.map (resolve_func_import env) external_funcs) @ [
        {
          Ast.module_name=encode_string "js";
          Ast.item_name=encode_string "mem";
          Ast.idesc=add_dummy_loc (Ast.MemoryImport(Types.MemoryType({Types.min=Int32.of_int 0; Types.max=None})))
        }
      ] in

  (* Main function *)
  let func = add_dummy_loc {Ast.ftype = ftype;
                            Ast.locals = repeat_f stack_size (fun n -> Types.I32Type);
                            Ast.body = compiled} in

  (* Collected lambdas *)
  let lambdas = (List.map add_dummy_loc @@ Deque.to_list !(env.compiled_lambdas)) in

  let heap_adjust_idx = add_dummy_loc Int32.(of_int @@ (List.length external_funcs) + List.length lambdas) in
  let main_idx = add_dummy_loc Int32.(of_int @@ (List.length external_funcs) + List.length lambdas + 1) in

  (* List of exports for the module *)
  let exports = List.map add_dummy_loc [
      {
        Ast.name=encode_string "GRAIN$HEAP_ADJUST";
        Ast.edesc=add_dummy_loc (Ast.FuncExport heap_adjust_idx);
      };
      {
        Ast.name=encode_string "GRAIN$MAIN";
        Ast.edesc=add_dummy_loc (Ast.FuncExport main_idx);
      }
    ] @ (repeat_f (List.length lambdas) (fun i -> make_lambda_export (i + (List.length external_funcs)))) in

  (* List of the module's global variables *)
  let globals = List.map add_dummy_loc @@ repeat (1 + (List.length builtins))
      {
        Ast.gtype=Types.GlobalType(Types.I32Type, Types.Mutable);
        Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 0)]);
      }
     in



  (* Table (used to call lambdas) *)
  let table_size = ((List.length lambdas) + 2 + (List.length external_funcs)) in
  let table = List.map add_dummy_loc [
      {
        Ast.ttype=Types.TableType({
            Types.max=None;
            Types.min=Int32.of_int table_size
          }, Types.AnyFuncType)
      }
    ] in

  (* Population of table elements *)
  let elems = List.map add_dummy_loc [
    {
      Ast.index=add_dummy_loc (Int32.zero);
      Ast.offset=add_dummy_loc @@ List.map add_dummy_loc [
          Ast.Const(const_int32 0);
        ];
      Ast.init=repeat_f table_size (fun n -> (add_dummy_loc (Int32.of_int (n - 1))))
    }
  ] in

  (* Returned module *)
  let ret = add_dummy_loc {
    Ast.empty_module with
    Ast.imports=imports;
    Ast.exports=exports;
    Ast.globals=globals;
    Ast.tables=table;
    Ast.elems=elems;
    Ast.funcs=lambdas@[add_dummy_loc @@ heap_adjust env; func];
    Ast.types=List.map add_dummy_loc (Deque.to_list !(env.func_types));
    Ast.start=None;
  } in
  Wasm_runner.validate_module ret;
  ret

let module_to_string compiled_module =
  (* Print module to string *)
  Wasm.Sexpr.to_string 80 @@ Wasm.Arrange.module_ compiled_module
