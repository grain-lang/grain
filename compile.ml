open Types
open Printf
open Instruction
open Expr
open Pretty

(* Add or change these constants as needed *)
let err_COMP_NOT_NUM   = 1
let err_ARITH_NOT_NUM  = 2
let err_LOGIC_NOT_BOOL = 3
let err_IF_NOT_BOOL    = 4
let err_OVERFLOW       = 5
let err_GET_NOT_TUPLE  = 6
let err_GET_LOW_INDEX  = 7
let err_GET_HIGH_INDEX = 8
let err_INDEX_NOT_NUM  = 9


let const_true = HexConst (0xFFFFFFFF)
let const_false = HexConst(0x7FFFFFFF)
let bool_mask = HexConst(0x80000000)
let tag_as_bool = HexConst(0x00000001)


type 'a envt = (string * 'a) list

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;


(* FINISH THIS FUNCTION WITH THE WELL-FORMEDNESS FROM FER-DE-LANCE *)
let well_formed (p : (Lexing.position * Lexing.position) program) builtins : exn list =
  let rec wf_E e (env : sourcespan envt) =
    match e with
    | EBool _ -> []
    | ENumber(n, loc) ->
       if n > 1073741823 || n < -1073741824 then [Overflow(n, loc)] else []
    | EId (x, loc) ->
       (try ignore (List.assoc x env); []
        with Not_found ->
             [UnboundId(x, loc)])
    | EPrim1(_, e, _) -> wf_E e env
    | EPrim2(_, l, r, _) -> wf_E l env @ wf_E r env
    | EIf(c, t, f, _) -> wf_E c env @ wf_E t env @ wf_E f env
    | ETuple(vals, _) -> List.concat (List.map (fun e -> wf_E e env) vals)
    | EGetItem(tup, idx, _) -> wf_E tup env @ wf_E idx env
    | ESetItem(tup, idx, rhs, _) -> wf_E tup env @ wf_E idx env @ wf_E rhs env
    | ESeq(stmts, _) -> List.flatten (List.map (fun s -> wf_E s env) stmts)
    | ELet(binds, body, _) ->
       let rec dupe x binds =
         match binds with
         | [] -> None
         | (y, _, loc)::_ when x = y -> Some loc
         | _::rest -> dupe x rest in
       let rec process_binds rem_binds env =
         match rem_binds with
         | [] -> (env, [])
         | (x, e, loc)::rest ->
            let shadow =
              match dupe x rest with
              | Some where -> [DuplicateId(x, where, loc)]
              | None ->
                 try
                   let existing = List.assoc x env in [ShadowId(x, loc, existing)]
                 with Not_found -> [] in
            let errs_e = wf_E e env in
            let new_env = (x, loc)::env in
            let (newer_env, errs) = process_binds rest new_env in
            (newer_env, (shadow @ errs_e @ errs)) in              
       let (env2, errs) = process_binds binds env in
       errs @ wf_E body env2
    | ELetRec(binds, body, _) -> failwith "NYI: well_formed letrec"
    | ELambda(args, body, _) ->
       wf_E body (args @ env)
    | EApp(func, args, loc) ->
       (wf_E func env) @ List.concat (List.map (fun e -> wf_E e env) args)
  in
  wf_E p builtins
;;


type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list
  
let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram = helpA p
  and helpC (e : tag expr) : (unit cexpr * unit anf_bind list) = 
    match e with
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (CPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (CPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (CIf(cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ESeq([], _) -> failwith "Impossible by syntax"
    | ESeq([stmt], _) -> helpC stmt
    | ESeq(fst :: rest, tag) ->
       let (fst_ans, fst_setup) = helpC fst in
       let (rest_ans, rest_setup) = helpC (ESeq(rest, tag)) in
       (rest_ans, fst_setup @ [BSeq fst_ans] @ rest_setup)
    | ELet([], body, _) -> helpC body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELetRec(binds, body, _) ->
       let (names, new_binds_setup) = List.split (List.map (fun (name, rhs, _) -> (name, helpC rhs)) binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (body_ans, (BLetRec (List.combine names new_binds)) :: body_setup)
    | ELambda(args, body, _) ->
       (CLambda(List.map fst args, helpA body, ()), [])
    | EApp(func, args, _) ->
       let (new_func, func_setup) = helpI func in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(new_func, new_args, ()), func_setup @ List.concat new_setup)
    | ETuple(args, _) ->
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CTuple(new_args, ()), List.concat new_setup)
    | EGetItem(tup, idx, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (CGetItem(tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem(tup, idx, rhs, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (rhs_imm, rhs_setup) = helpI rhs in
       (CSetItem(tup_imm, idx_imm, rhs_imm, ()), tup_setup @ idx_setup @ rhs_setup)
    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * unit anf_bind list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])

    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (ImmId(tmp, ()), arg_setup @ [BLet(tmp, CPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [BLet(tmp, CPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (ImmId(tmp, ()), cond_setup @ [BLet(tmp, CIf(cond_imm, helpA _then, helpA _else, ()))])
    | EApp(func, args, tag) ->
       let tmp = sprintf "app_%d" tag in
       let (new_func, func_setup) = helpI func in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), (func_setup @ List.concat new_setup) @ [BLet(tmp, CApp(new_func, new_args, ()))])
    | ESeq([], _) -> failwith "Impossible by syntax"
    | ESeq([stmt], _) -> helpI stmt
    | ESeq(fst :: rest, tag) ->
       let (fst_ans, fst_setup) = helpC fst in
       let (rest_ans, rest_setup) = helpI (ESeq(rest, tag)) in
       (rest_ans, fst_setup @ [BSeq fst_ans] @ rest_setup)
    | ELet([], body, _) -> helpI body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet(bind, exp_ans)] @ body_setup)
    | ELetRec(binds, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let (names, new_binds_setup) = List.split (List.map (fun (name, rhs, _) -> (name, helpC rhs)) binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (ImmId(tmp, ()), (List.concat new_setup)
                        @ [BLetRec (List.combine names new_binds)]
                        @ body_setup
                        @ [BLet(tmp, body_ans)])
    | ELambda(args, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       (ImmId(tmp, ()), [BLet(tmp, CLambda(List.map fst args, helpA body, ()))])
    | ETuple(args, tag) ->
       let tmp = sprintf "tup_%d" tag in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), (List.concat new_setup) @ [BLet(tmp, CTuple(new_args, ()))])
    | EGetItem(tup, idx, tag) ->
       let tmp = sprintf "get_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (ImmId(tmp, ()), tup_setup @ idx_setup @ [BLet(tmp, CGetItem(tup_imm, idx_imm, ()))])
    | ESetItem(tup, idx, rhs, tag) ->
       let tmp = sprintf "set_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (rhs_imm, rhs_setup) = helpI rhs in
       (ImmId(tmp, ()), tup_setup @ idx_setup @ rhs_setup @ [BLet(tmp, CSetItem(tup_imm, idx_imm, rhs_imm, ()))])
  and helpA e : unit aexpr = 
    let (ans, ans_setup) = helpC e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq(exp) -> ASeq(exp, body, ())
        | BLet(name, exp) -> ALet(name, exp, body, ())
        | BLetRec(names) -> ALetRec(names, body, ()))
      ans_setup (ACExpr ans)
  in
  helpP p
;;


let free_vars (e : 'a aexpr) : string list =
  let rec helpA (bound : string list) (e : 'a aexpr) : string list =
     match e with
     | ASeq(fst, rest, _) ->
        helpC bound fst @ helpA bound rest
     | ALet(name, binding, body, _) ->
       (helpC bound binding) (* all the free variables in the binding, plus *)
       (* all the free variables in the body, except the name itself *)
       @ (helpA (name :: bound) body)
     | ALetRec(bindings, body, _) ->
        let names = List.map fst bindings in
        let new_bound = (names @ bound) in
        (helpA new_bound body) @ List.flatten (List.map (fun binding -> helpC new_bound (snd binding)) bindings)
     | ACExpr c -> helpC bound c
  and helpC (bound : string list) (e : 'a cexpr) : string list =
    match e with
    | CLambda(args, body, _) ->
      helpA (args @ bound) body
    | CIf(cond, thn, els, _) ->
      helpI bound cond @ helpA bound thn @ helpA bound els
    | CPrim1(_, arg, _) -> helpI bound arg
    | CPrim2(_, left, right, _) -> helpI bound left @ helpI bound right
    | CApp(fn, args, _) ->
      (helpI bound fn) @ (List.flatten (List.map (fun arg -> helpI bound arg) args))
    | CTuple(vals, _) -> List.flatten (List.map (fun v -> helpI bound v) vals)
    | CGetItem(tup, idx, _) -> helpI bound tup @ helpI bound idx
    | CSetItem(tup, idx, rhs, _) -> helpI bound tup @ helpI bound idx @ helpI bound rhs
    | CImmExpr i -> helpI bound i
  and helpI (bound : string list) (e : 'a immexpr) : string list =
    match e with
    | ImmId(name, _) ->
      (* a name is free if it is not bound *)
      if List.mem name bound then [] else [name]
    | _ -> []
  in List.sort_uniq String.compare (helpA [] e)
;;

let reserve size tag =
  let ok = sprintf "$memcheck_%d" tag in
  [
    IMov(Reg(EAX), LabelContents("HEAP_END"));
    ISub(Reg(EAX), Const(size));
    ICmp(Reg(EAX), Reg(ESI));
    IJge(ok);
    IPush(Reg(ESP)); (* stack_top in C *)
    IPush(Reg(EBP)); (* first_frame in C *)
    IPush(Const(size)); (* bytes_needed in C *)
    IPush(Reg(ESI)); (* alloc_ptr in C *)
    ICall(Label("try_gc"));
    IAdd(Reg(ESP), Const(16)); (* clean up after call *)
    (* assume gc success if returning here, so EAX holds the new ESI value *)
    IMov(Reg(ESI), Reg(EAX));
    ILabel(ok);
  ]

(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT -- THE ONLY NEW CODE IS CSetItem and ALet *)
let compile_fun name args body = failwith "NYI: compile_fun"

(* UPDATE THIS TO HANDLE FIRST-CLASS FUNCTIONS AS NEEDED *)
let call (label : arg) args =
  let setup = List.rev_map (fun arg ->
                  match arg with
                  | Sized _ -> IPush(arg)
                  | _ -> IPush(Sized(DWORD_PTR, arg))) args in
  let teardown =
    let len = List.length args in
    if len = 0 then []
    else [ IInstrComment(IAdd(Reg(ESP), Const(4 * len)), sprintf "Popping %d arguments" len) ] in
  setup @ [ ICall(label) ] @ teardown

let compile_prog anfed =
  let prelude =
    "section .text
extern error
extern print
extern equal
extern try_gc
extern HEAP_END
extern STACK_BOTTOM
global our_code_starts_here" in
  let suffix = sprintf "
err_comp_not_num:%s
err_arith_not_num:%s
err_logic_not_bool:%s
err_if_not_bool:%s
err_overflow:%s
err_get_not_tuple:%s
err_get_low_index:%s
err_get_high_index:%s
err_index_not_num:%s"
                       (* If you modified `call` above, then fix these up, too *)
                       (to_asm (call (Label "error") [Const(err_COMP_NOT_NUM)]))
                       (to_asm (call (Label "error") [Const(err_ARITH_NOT_NUM)]))
                       (to_asm (call (Label "error") [Const(err_LOGIC_NOT_BOOL)]))
                       (to_asm (call (Label "error") [Const(err_IF_NOT_BOOL)]))
                       (to_asm (call (Label "error") [Const(err_OVERFLOW)]))
                       (to_asm (call (Label "error") [Const(err_GET_NOT_TUPLE)]))
                       (to_asm (call (Label "error") [Const(err_GET_LOW_INDEX)]))
                       (to_asm (call (Label "error") [Const(err_GET_HIGH_INDEX)]))
                       (to_asm (call (Label "error") [Const(err_INDEX_NOT_NUM)]))
  in
  (* $heap is a mock parameter name, just so that compile_fun knows our_code_starts_here takes in 1 parameter *)
     let (prologue, comp_main, epilogue) = compile_fun "our_code_starts_here" ["$heap"] anfed in
     let heap_start = [
         IInstrComment(IMov(LabelContents("STACK_BOTTOM"), Reg(EBP)), "This is the bottom of our Garter stack");
         ILineComment("heap start");
         IInstrComment(IMov(Reg(ESI), RegOffset(8, EBP)), "Load ESI with our argument, the heap pointer");
         IInstrComment(IAdd(Reg(ESI), Const(7)), "Align it to the nearest multiple of 8");
         IInstrComment(IAnd(Reg(ESI), HexConst(0xFFFFFFF8)), "by adding no more than 7 to it")
       ] in
     let main = (prologue @ heap_start @ comp_main @ epilogue) in
     
     sprintf "%s%s%s\n" prelude (to_asm main) suffix


let compile_to_string prog : (exn list, string) either =
  let env = [ (* DBuiltin("equal", 2) *) ] in
  let errors = well_formed prog env in
  match errors with
  | [] ->
     let tagged : tag program = tag prog in
     let anfed : tag aprogram = atag (anf tagged) in
     (* printf "Prog:\n%s\n" (ast_of_prog prog); *)
     (* printf "Tagged:\n%s\n" (format_prog tagged string_of_int); *)
     (* printf "ANFed/tagged:\n%s\n" (string_of_aprogram anfed); *)
     Right(compile_prog anfed)
  | _ -> Left(errors)

