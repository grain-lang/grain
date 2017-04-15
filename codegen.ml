open Printf
open Ast_utils
open Types
open Expr
open Errors
open Value_tags
open Wasm

module Deque = Batteries.Deque
module Queue = Batteries.Queue

let num_imported_functions = 1

let rec repeat n value =
  if n == 0 then
    []
  else
    value::(repeat (n - 1) value)

let rec repeat_f n value =
  if n == 0 then
    []
  else
    (value n)::(repeat_f (n - 1) value)

type binding =
  | ArgBind of int32
  | LocalBind of int32
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

let initial_env = {
  bindings=[];
  heap_top=add_dummy_loc (Int32.of_int 0);
  compiled_lambdas=ref Deque.empty;
  stack_index=0;
  stack_size=0;
  lift_index=ref num_imported_functions;
  num_args=0;
  is_tail=true;
  func_types=ref Deque.empty;
}

let get_func_type_idx arity env =
  let has_arity (Types.FuncType(args, _)) = (List.length args) = arity in
  match Deque.find has_arity !(env.func_types) with
  | None ->
    env.func_types := Deque.snoc !(env.func_types)
        (Types.FuncType((repeat arity Types.I32Type), [Types.I32Type]));
    Deque.size !(env.func_types) - 1
  | Some((i, _)) -> i

let const_int32 n = add_dummy_loc (Values.I32Value.to_value @@ Int32.of_int n)

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

let call_error_handler (code : int) =
  (* stack_size doesn't matter here, since this call won't return *)
  failwith "NYI: call_error_handler"

(*  label_of_error code_of_error *)
let create_error_handlers (defs : snek_error list) =
  failwith "NYI: create_error_handlers"

let heap_allocate (num_words : int) env =
  let words_to_allocate = 4 * (((num_words - 1) / 4) + 1) in
  [
    Ast.GetGlobal(env.heap_top);
    Ast.Const(const_int32 (4 * words_to_allocate));
    Ast.Binary(Values.I32 Ast.IntOp.Add);
    Ast.SetGlobal(env.heap_top);
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


let assert_tag arg err t val_tag fmt =
  failwith "NYI: assert_tag"

let assert_num arg err t =
  assert_tag arg err t NumberTagType "$assert_num_%d"

let assert_bool arg err t =
  assert_tag arg err t BooleanTagType "$assert_bool_%d"

let assert_tuple arg err t =
  assert_tag arg err t TupleTagType "$assert_tuple_%d"

let assert_lambda arg err t =
  assert_tag arg err t LambdaTagType "$assert_lambda_%d"

let check_tuple_idx arg tup t is_get =
  failwith "NYI: check_tuple_idx"

let check_arity arg called_arity t =
  failwith "NYI: check_arity"

let get_tag = function
  | ImmNum(_, t)
  | ImmBool(_, t)
  | ImmId(_, t) -> t



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

(* Assumption: Tagged Lambda is in EAX *)




let rec compile_fun (fun_name : string) (args : string list) env callee_restore extra_stack body : (Wasm.Ast.instr list * Wasm.Ast.instr list * compiler_env * int) =
  failwith "NYI: compile_fun"

and backpatch (free_vars : string list) (env : compiler_env) =
  let backpatch_var var idx =
    [
      Ast.GetGlobal(env.heap_top);
    ] @ (compile_imm (ImmId(var, -1)) env) @ [
      Ast.Store({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int (4 * (idx + 3)); Ast.sz=None});
    ] in
  List.flatten @@ map_i backpatch_var free_vars

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
  let type_idx = get_func_type_idx (List.length new_args) env in
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
  let lift_index = !(env.lift_index) in
  eprintf "lift_index=%d\n" lift_index;
  env.lift_index := !(env.lift_index) + 1;
  env.compiled_lambdas := Deque.snoc !(env.compiled_lambdas) {
    Ast.ftype = add_dummy_loc @@ Int32.of_int type_idx;
    Ast.locals = repeat stack_size Types.I32Type;
    Ast.body = List.map add_dummy_loc compiled_func;
  };
  lift_index, (3 + (List.length free_vars)), free_vars


and compile_aexpr (e : tag aexpr) (env : compiler_env) =
  match e with
  | ALet(name, rhs, body, t) ->
    let new_stack_idx = (Int32.of_int env.stack_index) in
    let new_loc = LocalBind(new_stack_idx) in
    let new_env = { env with
                    bindings = (name, new_loc)::env.bindings;
                    stack_index = env.stack_index + 1;
                    is_tail = false } in
    (compile_cexpr rhs env) @
    [Ast.SetLocal(add_dummy_loc new_stack_idx)] @
    (compile_aexpr body new_env)
  | ACExpr(e) -> compile_cexpr e env
  | _ -> failwith "NYI: compile_aexpr"
and compile_cexpr (e : tag cexpr) env =
  match e with
  | CIf(cond, thn, els, t) ->
    (compile_imm cond env) @
    decode_bool @
    [Ast.If([Types.I32Type],
            List.map add_dummy_loc (compile_aexpr thn env),
            List.map add_dummy_loc (compile_aexpr els env))]

  | CPrim1(Add1, arg, _) ->
    (compile_imm arg env) @ [Ast.Const(const_int32 @@ encode 1); Ast.Binary(Values.I32 Ast.IntOp.Add)]
  | CPrim1(Sub1, arg, _) ->
    (compile_imm arg env) @ [Ast.Const(const_int32 @@ encode 1); Ast.Binary(Values.I32 Ast.IntOp.Sub)]

  | CPrim1(IsBool, arg, _) ->
    check_tag BooleanTagType (compile_imm arg env)
  | CPrim1(IsNum, arg, _) ->
    check_tag NumberTagType (compile_imm arg env)

  | CPrim2(Plus, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Binary(Values.I32 Ast.IntOp.Add)]
  | CPrim2(Minus, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Binary(Values.I32 Ast.IntOp.Sub)]
  | CPrim2(Times, arg1, arg2, _) ->
    (compile_imm arg1 env) @
    untag_number @
    (compile_imm arg2 env) @
    [Ast.Binary(Values.I32 Ast.IntOp.Mul)]

  | CPrim2(And, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Binary(Values.I32 Ast.IntOp.And)]
  | CPrim2(Or, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Binary(Values.I32 Ast.IntOp.Or)]

  | CPrim2(Greater, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Compare(Values.I32 Ast.IntOp.GtS)] @ encode_bool
  | CPrim2(GreaterEq, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Compare(Values.I32 Ast.IntOp.GeS)] @ encode_bool
  | CPrim2(Less, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Compare(Values.I32 Ast.IntOp.LtS)] @ encode_bool
  | CPrim2(LessEq, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Compare(Values.I32 Ast.IntOp.LeS)] @ encode_bool
  | CPrim2(Eq, arg1, arg2, _) ->
    (compile_imm arg1 env) @ (compile_imm arg2 env) @ [Ast.Compare(Values.I32 Ast.IntOp.Eq)] @ encode_bool

  | CTuple(elts, t) ->
    (* TODO: Perform any GC before *)
    let num_elts = List.length elts in
    [
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
    compile_imm tup env @ [
      Ast.Const(const_int32 @@ tag_val_of_tag_type TupleTagType);
      Ast.Binary(Values.I32 Ast.IntOp.Xor)
    ]
    @ compile_imm index env @
    [
      Ast.Const(const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Shl);
      Ast.Const(const_int32 4);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 0; Ast.sz=None})
    ]

  | CLambda(args, body, t) ->
    let idx, closure_size, free_vars = compile_lambda args body env in
    (* TODO: construct closure *)
    [
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
    let ftype = add_dummy_loc @@ Int32.of_int @@ get_func_type_idx (1 + List.length args) env in
    compiled_func @[ Ast.Call(add_dummy_loc (Int32.zero));] @
    untag LambdaTagType @
    [
      Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int 4; Ast.sz=None});
      Ast.Call(add_dummy_loc (Int32.zero));
    ] @
    compiled_func @
    untag LambdaTagType @
    compiled_args @
    [
      Ast.CallIndirect(ftype);
    ]

  | CImmExpr(i) -> compile_imm i env
  | _ -> failwith "NYI: compile_cexpr"
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
    | LocalBind(n) -> [Ast.GetLocal(add_dummy_loc @@ Int32.of_int (Int32.to_int n + env.num_args + 1))]
    | ClosureBind(n) -> [
        Ast.GetLocal(add_dummy_loc @@ Int32.of_int 0);
        Ast.Load({Ast.ty=Types.I32Type; Ast.align=2; Ast.offset=Int32.of_int (4 * (2 + (Int32.to_int n))); Ast.sz=None})
      ]

let builtins = [
  ("print", 1);
  ("input", 0);
  ("equal", 2);
]

let compile_aprog (anfed : tag aprogram) =
  let stack_size = count_vars anfed in
  let heap_top = add_dummy_loc (Int32.of_int 0) in
  let env = {initial_env with stack_size = stack_size; heap_top=heap_top} in
  let compiled = List.map add_dummy_loc @@
    compile_aexpr anfed env
    @ [Ast.Call(add_dummy_loc (Int32.zero)); Ast.Return] in

  let ftype = add_dummy_loc Int32.(of_int (Deque.size !(env.func_types))) in (* <- see inline_type in parser.mly in WASM spec *)
  let print_type = add_dummy_loc Int32.(of_int ((Deque.size !(env.func_types)) + 1)) in
  eprintf "ftype=%d\nprint_type=%d\n" (Deque.size !(env.func_types)) ((Deque.size !(env.func_types)) + 1);
  let imports = List.map add_dummy_loc [
    {
      Ast.module_name="console";
      Ast.item_name="log";
      Ast.ikind=add_dummy_loc (Ast.FuncImport(print_type));
    };
    {
      Ast.module_name="js";
      Ast.item_name="mem";
      Ast.ikind=add_dummy_loc (Ast.MemoryImport(Types.MemoryType({Types.min=Int32.of_int 0; Types.max=None})))
    }
  ] in
  let exports = List.map add_dummy_loc [] in
  let globals = List.map add_dummy_loc [
      {
        Ast.gtype=Types.GlobalType(Types.I32Type, Types.Mutable);
        Ast.value=(add_dummy_loc ([add_dummy_loc @@ Ast.Const(const_int32 0)]));
      }
    ] in
  
  let func = add_dummy_loc {Ast.ftype = ftype;
                            Ast.locals = repeat_f stack_size (fun n -> Types.I32Type);
                            Ast.body = compiled} in
  let lambdas = (List.map add_dummy_loc @@ Deque.to_list !(env.compiled_lambdas)) in
  let table_size = ((List.length lambdas) + 1 + num_imported_functions) in
  let table = List.map add_dummy_loc [
      {
        Ast.ttype=Types.TableType({Types.max=None; Types.min=Int32.of_int table_size}, Types.AnyFuncType)
      }
    ] in
  let elems = List.map add_dummy_loc [
    {
      Ast.index=add_dummy_loc (Int32.zero);
      Ast.offset=add_dummy_loc @@ List.map add_dummy_loc [
          Ast.Const(const_int32 0);
        ];
      Ast.init=List.rev @@ repeat_f table_size (fun n -> (add_dummy_loc (Int32.of_int (n - 1))))
    }
  ] in
  let compiled_module = {Ast.empty_module with
                         Ast.imports=imports;
                         Ast.exports=exports;
                         Ast.globals=globals;
                         Ast.tables=table;
                         Ast.elems=elems;
                         Ast.funcs=lambdas@[func];
                         Ast.types=(Deque.to_list !(env.func_types)) @
                                   [Types.FuncType([], []);
                                    Types.FuncType([Types.I32Type], [Types.I32Type])];
                         Ast.start=Some(add_dummy_loc Int32.(of_int @@ num_imported_functions + List.length lambdas));} in
  let (in_fd, out_fd) = Unix.pipe() in
  let (in_channel, out_channel) = (Unix.in_channel_of_descr in_fd, Unix.out_channel_of_descr out_fd) in
  Pervasives.set_binary_mode_in in_channel false;
  Pervasives.set_binary_mode_out out_channel false;
  Wasm.Print.module_ out_channel 80 (add_dummy_loc compiled_module);
  Unix.close out_fd;
  let str = BatStream.of_channel in_channel
            |> BatStream.to_string in
  Unix.close in_fd;
  str
