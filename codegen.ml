open Printf
open Ast_utils
open Types
open Expr
open Errors
open Value_tags
open Instruction

type compiler_env = {
  bindings: arg envt;
}

let const_true  = HexConst(0xFFFFFFFF)
let const_false = HexConst(0x7FFFFFFF)
let bool_mask   = HexConst(0x80000000)
let tag_as_bool = HexConst(0x00000001)

let str_of_env env =
  sprintf "{bindings=%s}" (ExtLib.dump @@ List.map (fun (n, i) -> (n, Instruction.arg_to_asm i)) env.bindings)

(** Maps `f` over `lst`, appending the results. *)
let flat_map (f : 'a -> 'b list) (lst : 'a list) : 'b list = List.fold_left List.append [] (List.rev_map f lst)

let rec find ls x =
  match ls with
  | [] -> failwith (sprintf "Name %s not found" x)
  | (y,v)::rest ->
     if y = x then v else find rest x

let lambda_get_arity (reg : reg) = RegOffset(0, reg);;
let lambda_get_ptr (reg : reg) = RegOffset(4, reg);;
let lambda_get_closure_size (reg : reg) = RegOffset(8, reg);;
let lambda_get_arg_offset arg = 12 + (arg * 4)

let rec repeat n value =
  if n == 0 then
    []
  else
    value::(repeat (n - 1) value)

(*IAdd(Reg(ESP), Const(~-4 * stack_size))*)
let reserve_stack num_words =
  repeat num_words (IPush(HexConst(0xDEADBEEF)))

(** Calls the given function with the given list of arguments *)
let func_apply (name : string) (args : arg list) (stack_size : int) : instruction list =
  (List.fold_left (fun acc arg -> (IPush(Sized(DWORD_PTR, arg)))::acc)
     [
       ICall(Label(name));
       IMov(Reg(ESP), Reg(EBP));
       IAdd(Reg(ESP), HexConst(~-4 * stack_size));
     ] args)

let lambda_apply (lambda : arg) (args : arg list) (stack_size : int) : instruction list =
  let preamble = [
    IMov(Reg(ECX), lambda);
    IMov(Reg(EDX), Reg(ECX));
    IOr(Reg(EDX), HexConst(tag_val_of_tag_type LambdaTagType));
  ] in
  preamble @
  (List.fold_left (fun acc arg -> (IPush(Sized(DWORD_PTR, arg)))::acc) [] ((Reg(EDX))::args)) @
  [
    IMov(Reg(ECX), lambda_get_ptr ECX);
    ICall(Reg(ECX));
    IMov(Reg(ESP), Reg(EBP));
    IAdd(Reg(ESP), HexConst(~-4 * stack_size));
  ]

let call_error_handler (code : int) : instruction list =
  (* stack_size doesn't matter here, since this call won't return *)
  func_apply "error" [Const(code); Reg(EAX)] 0

(*  label_of_error code_of_error *)
let create_error_handlers (defs : snek_error list) : instruction list =
  let handlers = List.map (fun e ->
      (ILabel(label_of_error e))::(call_error_handler (code_of_error e))) defs in
  List.flatten handlers

let heap_allocate (num_words : int) (dest : arg) tag : instruction list =
  let words_to_allocate = 4 * (((num_words - 1) / 4) + 1) in
  let ok_label = sprintf "$memcheck_%s" tag in
  [
    IMov(Reg(EAX), LabelContents("HEAP_END"));
    ISub(Reg(EAX), Const(4 * words_to_allocate));
    ICmp(Reg(EAX), Reg(ESI));
    IJge(ok_label);
    IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType));
    IPush(Reg(EDI));
    IPush(Reg(ESP)); (* stack_top in C *)
    IPush(Reg(EBP)); (* first_frame in C *)
    IPush(Const(4 * words_to_allocate)); (* bytes_needed in C *)
    IPush(Reg(ESI)); (* alloc_ptr in C *)
    ICall(Label("try_gc"));
    IAdd(Reg(ESP), Const(16)); (* clean up after call *)
    (* assume gc success if returning here, so EAX holds the new ESI value *)
    IPop(Reg(EDI));
    IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType));
    IMov(Reg(ESI), Reg(EAX));
    ILabel(ok_label);
    IMov(dest, Reg(ESI));
    IAdd(Reg(ESI), HexConst(4 * words_to_allocate));
  ]

(* Checks that the value has the given tag (value ends up in dest) *)
let check_tag (tag : tag_type) (arg : arg) (dest : arg) =
  [
    IMov(Reg(ECX), arg);
    IAnd(Reg(ECX), HexConst(and_mask_of_tag_type tag));
    IMov(dest, Const(1));
    IShl(dest, Reg(CL));
    IShl(dest, Const(shift_amount_of_tag_type tag));
    IOr(dest, const_false);
  ]


let assert_tag arg err t val_tag fmt =
  let label = sprintf fmt t in
  [
    ILineComment("start assert_tag");
    IMov(Reg(ECX), arg);
    IAnd(Reg(ECX), HexConst(and_mask_of_tag_type val_tag));
    IMov(Reg(EBX), Const(1));
    IShl(Reg(EBX), Reg(CL));
    ICmp(Reg(EBX), HexConst(1 lsl (tag_val_of_tag_type val_tag)));
    IJe(label);
    IMov(Reg(EAX), arg);
    IJmp(label_of_error err);
    ILabel(label);
    ILineComment("end assert_tag");
  ]

let assert_num arg err t : instruction list =
  assert_tag arg err t NumberTagType "$assert_num_%d"

let assert_bool arg err t : instruction list =
  assert_tag arg err t BooleanTagType "$assert_bool_%d"

let assert_tuple arg err t : instruction list =
  assert_tag arg err t TupleTagType "$assert_tuple_%d"

let assert_lambda arg err t : instruction list =
  assert_tag arg err t LambdaTagType "$assert_lambda_%d"

let check_tuple_idx arg tup t is_get : instruction list =
  let non_num = if is_get then GetItemIndexNotNumber else SetItemIndexNotNumber in
  let too_small = if is_get then GetItemIndexTooSmall else SetItemIndexTooSmall in
  let too_large = if is_get then GetItemIndexTooLarge else SetItemIndexTooLarge in
  let small_tag = sprintf "$check_tuple_small_%d" t in
  let large_tag = sprintf "$check_tuple_large_%d" t in
  let good_tag = sprintf "$check_tuple_good_%d" t in
  (assert_num arg non_num t) @ [
    IMov(Reg(EBX), tup);
    IXor(Reg(EBX), HexConst(1));
    IMov(Reg(ECX), arg);
    ICmp(Reg(ECX), HexConst(0));
    IJl(small_tag);
    IShr(Reg(ECX), HexConst(1));
    ICmp(Reg(ECX), RegOffset(0, EBX));
    IJge(large_tag);
    IJmp(good_tag);
    ILabel(small_tag);
    IMov(Reg(EAX), arg);
    IJmp(label_of_error too_small);
    ILabel(large_tag);
    IMov(Reg(EAX), arg);
    IJmp(label_of_error too_large);
    ILabel(good_tag);
  ]

let check_arity arg called_arity t : instruction list =
  let good_tag = sprintf "$check_arity_good_%d" t in
  [
    ILineComment("start check_arity");
    IMov(Reg(EBX), arg);
    ICmp(lambda_get_arity EBX, Sized(DWORD_PTR, Const(called_arity)));
    IJe(good_tag);
    IMov(Reg(EAX), Reg(EBX));
    IJmp(label_of_error ArityMismatch);
    ILabel(good_tag);
    ILineComment("end check_arity");
  ]

let check_overflow : instruction list =
  [
    IJo(label_of_error OverflowError);
  ]

let get_tag = function
  | ImmNum(_, t)
  | ImmBool(_, t)
  | ImmId(_, t) -> t

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
let backpatch free_vars env =
  let backpatch_var var idx =
    let closure_idx = lambda_get_arg_offset idx in
    (* Index into closure, offset by tag *)
    let offset_idx = closure_idx - (tag_val_of_tag_type LambdaTagType) in
    [
      ILineComment(sprintf "Loading closure index %d (%s) into offset %d (%x)" idx var closure_idx closure_idx);
      IMov(Reg(EDX), find env.bindings var);
      IMov(RegOffset(offset_idx, EAX), Reg(EDX));
    ] in
  let ret = [
    ILineComment("Starting backpatching");
    ILineComment(sprintf "Environment: %s" (str_of_env env));
  ] @ (List.flatten @@ map_i backpatch_var free_vars) @
  [
    ILineComment("Done with backpatching");
  ] in
  ret

let rec compile_fun (fun_name : string) (args : string list) env callee_restore extra_stack body : (instruction list * instruction list * compiler_env * int) =
  let stack_size = (count_vars body) + extra_stack in
  let maybe_xor = fun xor_first r f ->
    match r with
    | Reg(EDI) ->
      if xor_first then
        [IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType)); (f r)]
      else
        [(f r); IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType))]
    | _ -> [(f r)] in
  let preamble = [
    ILabel(fun_name);
  ] @ (List.flatten @@ List.map (fun r -> maybe_xor true r (fun x -> IPush(x))) callee_restore) @ [
    IPush(Reg(EBP));
    IMov(Reg(EBP), Reg(ESP));
    (*IAdd(Reg(ESP), Const(~-4 * stack_size))*)
  ] @ (reserve_stack stack_size) in
  let postamble = [
    IMov(Reg(ESP), Reg(EBP));
    IPop(Reg(EBP));
  ] @ (List.flatten @@ List.map (fun r -> maybe_xor false r (fun x -> IPop(x))) @@ List.rev callee_restore) @ [
    IRet;
  ] in
  let new_bindings = snd @@ List.fold_left (fun (idx, acc) arg ->
      (idx + 1, (arg, RegOffset((4 * (2 + idx + (List.length callee_restore))), EBP))::acc)) (0, env.bindings) args in
  let env = { bindings=new_bindings } in
  (preamble, postamble, env, stack_size)

and compile_lambda label (args : string list) body : (instruction list * string list) =
  let used_var_set = Ast_utils.free_vars body in
  let free_var_set = BindingSet.diff used_var_set @@ BindingSet.of_list args in
  let free_vars = Ast_utils.BindingSet.elements free_var_set in
  let free_env = map_i (fun var closure_idx ->
      (var, RegOffset(lambda_get_arg_offset closure_idx, EDI))) free_vars in
  let new_args = "$self"::args in
  let arg_env = map_i (fun var arg_idx ->
      (var, RegOffset((4 * (3 + arg_idx)), EBP))) new_args in
  let bindings = free_env @ arg_env in
  let end_label = sprintf "%s$end" label in
  
  let stack_size = count_vars body in
  let preamble = [
    IJmp(end_label);
    ILineComment("Compiling lambda with env:");
    ILineComment(sprintf "used_vars: %s" @@ (ExtLib.dump @@ BindingSet.elements used_var_set));
    ILineComment(sprintf "free_vars: %s" @@ ExtLib.dump free_vars);
    ILabel(label);
    (* We tag old EDI in order to allow GC to work correctly *)
    IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType));
    IPush(Reg(EDI));
    IPush(Reg(EBP));
    IMov(Reg(EBP), Reg(ESP));
    IMov(Reg(EDI), RegOffset(12, EBP));
    IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType));
  ] @ (reserve_stack stack_size) in
  let postamble = [
    IMov(Reg(ESP), Reg(EBP));
    IPop(Reg(EBP));
    IPop(Reg(EDI));
    (* Untag old EDI *)
    IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType));
    IRet;
    ILabel(end_label);
  ] @ (heap_allocate (3 + (List.length free_vars)) (Reg(EAX)) label) @ [
      IMov(lambda_get_arity EAX, Sized(DWORD_PTR, Const(List.length args)));
      IMov(lambda_get_ptr EAX, Sized(DWORD_PTR, Label(label)));
      IMov(lambda_get_closure_size EAX, Sized(DWORD_PTR, Const(List.length free_vars)));
      ILineComment("Tagging Lambda");
      IOr(Reg(EAX), HexConst(tag_val_of_tag_type LambdaTagType));
    ] in
  ((preamble @
    (compile_aexpr body 1 stack_size {bindings}
       (List.length new_args) true) @
    postamble),
   free_vars)
  

and compile_aexpr (e : tag aexpr) (si : int) (stack_size : int) (env : compiler_env) (num_args : int) (is_tail : bool) : instruction list =
  match e with
  | ALet(name, rhs, body, t) ->
    let new_env = { bindings = (name, RegOffset(~-4 * si, EBP))::env.bindings } in
    (compile_cexpr rhs si stack_size env num_args false) @
    [IMov(RegOffset(~-4 * si, EBP), Reg(EAX))] @
    (compile_aexpr body (si + 1) stack_size new_env num_args is_tail)
  | ALetRec(binds, body, t) ->
    let compiled_binds, free_vars = List.split @@ map_i (fun (name, bind) idx ->
        match bind with
        | CLambda(args, body, t) ->
          let name = sprintf "$letrec_lambda_%d" t in
          let compiled, free_vars = compile_lambda name args body in
          (compiled @ [
              IMov(RegOffset(~-4 * (si + idx), EBP), Reg(EAX));
            ]), free_vars
        | _ ->
          ((compile_cexpr bind si stack_size env num_args false) @
           [IMov(RegOffset(~-4 * (si + idx), EBP), Reg(EAX))]), []) binds in
    let new_env = {
      bindings = (map_i (fun (name, _) idx ->
          (name, RegOffset(~-4 * (si + idx), EBP))) binds) @
                 env.bindings
    } in
    (* Lambdas have been placed into stack, so we just need to fill in the closures *)
    (List.flatten (compiled_binds @ (map_i (fun free_vars idx ->
         match free_vars with
         | [] -> []
         | _ ->
           [
             IMov(Reg(EAX), RegOffset(~-4 * (si + idx), EBP));
           ] @
           (backpatch free_vars new_env)
       ) free_vars))) @
    (compile_aexpr body (si + (List.length binds)) stack_size new_env num_args is_tail)
  | ACExpr(ce) -> compile_cexpr ce si stack_size env num_args is_tail
  | ASeq(hd, tl, _) ->
    (* Stack indexes should be fine here *)
    (compile_cexpr hd si stack_size env num_args false) @
    (compile_aexpr tl si stack_size env num_args is_tail)
and compile_cexpr (e : tag cexpr) si stack_size env num_args is_tail =
  match e with
  | CIf(cond, thn, els, t) ->
    let label_then = sprintf "$if_then_%d" t in
    let label_els = sprintf "$if_else_%d" t in
    let label_end = sprintf "$if_end_%d" t in
    [IMov(Reg(EAX), compile_imm cond env)] @
    assert_bool (Reg(EAX)) IfError t @ [
      ICmp(Reg(EAX), const_true);
      IJe(label_then);
      IJmp(label_els);
      ILabel(label_then)
    ] @ compile_aexpr thn si stack_size env num_args is_tail @ [
      IJmp(label_end);
      ILabel(label_els)
    ] @ compile_aexpr els si stack_size env num_args is_tail @ [
      ILabel(label_end)
    ]
  | CPrim1(name, arg, t) ->
    [IMov(Reg(EAX), compile_imm arg env)] @
    (match name with
     | Add1  ->
       assert_num (Reg(EAX)) ArithmeticError t @ [
         IAdd(Reg(EAX), Const(2))
       ] @ (check_overflow)
     | Sub1  ->
       assert_num (Reg(EAX)) ArithmeticError t @ [
         ISub(Reg(EAX), Const(2))
       ] @ (check_overflow)
     | IsBool  ->
       check_tag BooleanTagType (Reg(EAX)) (Reg(EAX))
     | IsNum  ->
       check_tag NumberTagType (Reg(EAX)) (Reg(EAX))
     | IsTuple ->
       check_tag TupleTagType (Reg(EAX)) (Reg(EAX))
     | Not  ->
       assert_bool (Reg(EAX)) LogicError t @ [
         IXor(Reg(EAX), HexConst(0x80000000));
       ]
     | PrintStack  ->
       assert_num (Reg(EAX)) GenericNumberError t @
       func_apply "print_stack" [Reg(EAX); Reg(EBP); Reg(ESP)] stack_size)
  | CPrim2(name, arg1, arg2, t) ->
    let arg1, tag1 = compile_imm arg1 env, get_tag arg1 in
    let arg2, tag2 = compile_imm arg2 env, get_tag arg2 in
    [IMov(Reg(EAX), arg1); IMov(Reg(EDX), arg2)] @
    (match name with
     | Plus  ->
       assert_num (Reg(EAX)) ArithmeticError tag1 @
       assert_num (Reg(EDX)) ArithmeticError tag2 @ [
         IAdd(Reg(EAX), Reg(EDX))
       ] @ (check_overflow)
     | Minus  ->
       assert_num (Reg(EAX)) ArithmeticError tag1 @
       assert_num (Reg(EDX)) ArithmeticError tag2 @ [
         ISub(Reg(EAX), Reg(EDX))
       ] @ (check_overflow)
     | Times  ->
       assert_num (Reg(EAX)) ArithmeticError tag1 @
       assert_num (Reg(EDX)) ArithmeticError tag2 @ [
         ISar(Reg(EAX), Const(1));
         IMul(Reg(EAX), Reg(EDX))
       ] @ (check_overflow)
     | And  ->
       assert_bool (Reg(EAX)) LogicError tag1 @
       assert_bool (Reg(EDX)) LogicError tag2 @ [
         IAnd(Reg(EAX), Reg(EDX));
       ]
     | Or  ->
       assert_bool (Reg(EAX)) LogicError tag1 @
       assert_bool (Reg(EDX)) LogicError tag2 @ [
         IOr(Reg(EAX), Reg(EDX));
       ]
     | Greater  ->
       let label_false = sprintf "$gt_false_%d" t in
       let label_end = sprintf "$gt_end_%d" t in
       assert_num (Reg(EAX)) ComparisonError tag1 @
       assert_num (Reg(EDX)) ComparisonError tag2 @ [
         ICmp(Reg(EAX), Reg(EDX));
         IJle(label_false);
         IMov(Reg(EAX), const_true);
         IJmp(label_end);
         ILabel(label_false);
         IMov(Reg(EAX), const_false);
         ILabel(label_end);
       ]
     | GreaterEq  ->
       let label_false = sprintf "$ge_false_%d" t in
       let label_end = sprintf "$ge_end_%d" t in
       assert_num (Reg(EAX)) ComparisonError tag1 @
       assert_num (Reg(EDX)) ComparisonError tag2 @ [
         ICmp(Reg(EAX), Reg(EDX));
         IJl(label_false);
         IMov(Reg(EAX), const_true);
         IJmp(label_end);
         ILabel(label_false);
         IMov(Reg(EAX), const_false);
         ILabel(label_end);
       ]
     | Less  ->
       let label_false = sprintf "$lt_false_%d" t in
       let label_end = sprintf "$lt_end_%d" t in
       assert_num (Reg(EAX)) ComparisonError tag1 @
       assert_num (Reg(EDX)) ComparisonError tag2 @ [
         ICmp(Reg(EAX), Reg(EDX));
         IJge(label_false);
         IMov(Reg(EAX), const_true);
         IJmp(label_end);
         ILabel(label_false);
         IMov(Reg(EAX), const_false);
         ILabel(label_end);
       ]
     | LessEq  ->
       let label_false = sprintf "$le_false_%d" t in
       let label_end = sprintf "$le_end_%d" t in
       assert_num (Reg(EAX)) ComparisonError tag1 @
       assert_num (Reg(EDX)) ComparisonError tag2 @ [
         ICmp(Reg(EAX), Reg(EDX));
         IJg(label_false);
         IMov(Reg(EAX), const_true);
         IJmp(label_end);
         ILabel(label_false);
         IMov(Reg(EAX), const_false);
         ILabel(label_end);
       ]
     | Eq  ->
       let label_false = sprintf "$eq_false_%d" t in
       let label_end = sprintf "$eq_end_%d" t in
       [
         ICmp(Reg(EDX), Reg(EAX));
         IJne(label_false);
         IMov(Reg(EAX), const_true);
         IJmp(label_end);
         ILabel(label_false);
         IMov(Reg(EAX), const_false);
         ILabel(label_end);
       ])
  | CApp(func, args, t) ->
    let compiled_func = compile_imm func env in
    let mapped_args = List.map (fun a -> compile_imm a env) args in
    if is_tail then
      (*
         Basic algorithm for arbitrary-arity tail calls
         (While still following the C callee convention!):

         1. Push arguments and RET+Old EBP+Old EDI onto stack
         2. Copy arguments and RET+Old EBP+Old EDI (in reverse order) to
            their new locations (where the last argument to the current
            stack frame and the new stack frame go in the same address)
         3. Set EDI to align with new location of old EDI
         4. Set EBP to align with new location of old EBP
         5. Set ESP to align with new location of EBP from (3)
         6. Pop the top of the stack (old EBP) into EBP
         7. Jump to the function. At this point, the callee cannot
            distinguish the current state of the stack from a typical
            call

         There are a couple of tricks to make this work:
         1. The current stack size is statically wired through the
            compiler, so that the caller can set ESP to a value directly
            offset from EBP (as opposed to popping off a number of stack arguments)
         2. The `our_code_starts_here` function cannot make a call in tail
            position (since the C code which called it does not follow the
            callee convention).
      *)
      let new_num_args = 1 + List.length args in
      let new_args = List.rev_map (fun arg -> (IPush(Sized(DWORD_PTR, arg)))) (compiled_func::mapped_args) @ [
          IPush(Sized(DWORD_PTR, RegOffset(8, EBP)));
          IPush(Sized(DWORD_PTR, RegOffset(4, EBP)));
          IPush(Sized(DWORD_PTR, RegOffset(0, EBP)))
        ] in
      let copied_args = flat_map_i2 (fun arg src_off dest_off ->
          [
            IMov(Reg(EAX), RegOffset(4 * src_off, ESP));
            IMov(RegOffset(4 * (dest_off - (new_num_args - num_args)), EBP), Reg(EAX));
          ])
          0 0 new_args in
      [ILineComment("Starting tail call")] @
      (assert_lambda compiled_func CalledNonFunction t) @ [
        IMov(Reg(EDX), compiled_func);
        IXor((Reg(EDX)), HexConst(tag_val_of_tag_type LambdaTagType));
      ] @
      (check_arity (Reg(EDX)) (List.length args) t) @
      new_args @
      copied_args @ [
        IAdd(Reg(EBP), Const(~-4 * (new_num_args - num_args)));
        IMov(Reg(ESP), Reg(EBP));
        IPop(Reg(EBP));
        IPop(Reg(EDI));
        (* Untag the pushed EDI *)
        IXor(Reg(EDI), HexConst(tag_val_of_tag_type LambdaTagType));
        IJmpArg(RegOffset(4, EDX));
        ILineComment("Ending tail call")
      ]
    else
      [
        ILineComment(sprintf "Starting non-tail call (stack_size: %d)" stack_size);
        ILineComment(sprintf "Environment: %s" (ExtLib.dump @@ List.map (fun (n, i) -> (n, Instruction.arg_to_asm i)) env.bindings));
        IMov(Reg(EDX), compiled_func);
      ] @
      (assert_lambda (Reg(EDX)) CalledNonFunction t) @
      [
        IXor((Reg(EDX)), HexConst(tag_val_of_tag_type LambdaTagType));
      ] @
      (check_arity (Reg(EDX)) (List.length args) t) @
      (lambda_apply (Reg(EDX)) mapped_args stack_size) @ [
        ILineComment("Ending non-tail call")
      ]
  | CGetItem(tup, idx, tag) ->
    let tup_imm = compile_imm tup env in
    let idx_imm = compile_imm idx env in
    (assert_tuple tup_imm GetItemNotTuple tag) @
    (check_tuple_idx idx_imm tup_imm tag true) @ [
      IMov(Reg(EAX), tup_imm);
      IXor(Reg(EAX), HexConst(1));
      IMov(Reg(EBX), idx_imm);
      IMov(Reg(EAX), RegOffsetReg(EAX, EBX, 2, 4));
    ]
  | CSetItem(tup, idx, value, tag) ->
    let tup_imm = compile_imm tup env in
    let idx_imm = compile_imm idx env in
    let value_imm = compile_imm value env in
    (assert_tuple tup_imm SetItemNotTuple tag) @
    (check_tuple_idx idx_imm tup_imm tag false) @ [
      IMov(Reg(ECX), tup_imm);
      IXor(Reg(ECX), HexConst(1));
      IMov(Reg(EBX), idx_imm);
      IMov(Reg(EAX), value_imm);
      IMov(RegOffsetReg(ECX, EBX, 2, 4), Sized(DWORD_PTR, Reg(EAX)));
    ]
  | CTuple(elts, t) ->
    let num_elts = List.length elts in
    let imm_elts = (map_i (fun x i -> [
          IMov(Reg(ECX), Sized(DWORD_PTR, compile_imm x env));
          IMov(RegOffset((i + 1) * 4, EAX), Sized(DWORD_PTR, Reg(ECX)))
        ])
        (elts)) in
    [ILineComment("Heap-allocating tuple");] @
    (heap_allocate num_elts (Reg(EAX)) (sprintf "%d" t)) @ [
      ILineComment("Tagging Tuple");
      IMov(RegOffset(0, EAX), Sized(DWORD_PTR, Const(num_elts)));
    ] @ (List.flatten imm_elts) @ [
      IOr(Reg(EAX), HexConst(tag_val_of_tag_type TupleTagType));
    ]
  | CLambda(args, body, t) ->
    let name = sprintf "$lambda_compiled_%d" t in
    let compiled, free_vars = compile_lambda name args body in
    compiled @ (backpatch free_vars env)
  | CImmExpr(i) ->
    [IMov(Reg(EAX), compile_imm i env)]
and compile_imm e env : arg =
  match e with
  | ImmNum(n, _) -> HexConst((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env.bindings x)

let data_segment_closure name =
  let label = sprintf "$builtin_closure_%s" name in
  [
    ILineComment(sprintf "Builtin Closure: %s" name);
    ILabel(label);
    IDw(4);
  ], label

let make_builtin_data_closure offset (name, arity) idx =
  let bss, label = data_segment_closure name in
  let prelude = [
    ILineComment(sprintf "Making builtin closure for %s" name);
    IMov(LabelOffset(0, label), Sized(DWORD_PTR, Const(arity)));
    IMov(LabelOffset(4, label), Sized(DWORD_PTR, Label(name)));
    IMov(LabelOffset(8, label), Sized(DWORD_PTR, Const(0)));
    IMov(Reg(EAX), Sized(DWORD_PTR, Label(label)));
    IOr(Reg(EAX), HexConst(tag_val_of_tag_type LambdaTagType));
    IMov(RegOffset(~-4 * (offset + idx), EBP), Reg(EAX));
    ILineComment(sprintf "End builtin closure for %s" name);
  ] in
  bss, prelude

let make_builtin_closure offset (name, arity) idx =
  [ILineComment(sprintf "Making builtin closure for %s" name);] @
  [
    IMov(Reg(EAX), LabelContents("CORE_HEAP"));
    IAdd(Reg(EAX), HexConst(16 * idx));
    IMov(lambda_get_arity EAX, Sized(DWORD_PTR, Const(arity)));
    IMov(lambda_get_ptr EAX, Sized(DWORD_PTR, Label(name)));
    IMov(lambda_get_closure_size EAX, Sized(DWORD_PTR, Const(0)));
    IOr(Reg(EAX), HexConst(tag_val_of_tag_type LambdaTagType));
    IMov(RegOffset(~-4 * (offset + idx), EBP), Reg(EAX));
    ILineComment(sprintf "End builtin closure for %s" name);
  ]

let builtins = [
  ("print", 1);
  ("input", 0);
  ("equal", 2);
]

let compile_aprog anfed =
  let prog_preamble ="
section .text
extern error
extern print_stack
extern try_gc
extern HEAP_END
extern STACK_BOTTOM
extern CORE_HEAP
" ^ (ExtString.String.join "\n" @@
     List.map (fun (n, _) -> "extern " ^ n) builtins) ^ "
global our_code_starts_here" in
  let (preamble, postamble, env, stack_size) =
    compile_fun
      "our_code_starts_here"
      ["heap"]
      {bindings=[]}
      [Reg(ESI); Reg(EBX); Reg(EDI)]
      (List.length builtins)
      anfed in
  let new_env = {
    bindings=(map_i (fun (name, _) idx -> (name, RegOffset(~-4 * (1 + idx), EBP))) builtins) @ env.bindings;
    } in
  (*let new_env = {
    bindings=(List.map (fun (name, _) -> (name, Label("builtin_closure_" ^ name))) builtins) @ env.bindings;
  } in*)
  
  let compiled =
    preamble @ [
      ILineComment("Loading ESI");
      IMov(LabelContents("STACK_BOTTOM"), Sized(DWORD_PTR, Reg(EBP)));
      IMov(Reg(ESI), compile_imm (ImmId("heap", 0)) env);
      ILineComment("Loading closures")
    ] @
    (List.flatten @@ map_i (make_builtin_closure 1) builtins) @ [
      ILineComment("Done loading closures");
      IMov(Reg(EDI), HexConst(0));
    ] @
    (compile_aexpr anfed (1 + (List.length builtins)) stack_size new_env 0 false) @
    postamble in
  let handlers = create_error_handlers Errors.all_snek_errors in
  prog_preamble ^ to_asm (compiled @ handlers)
