open Printf
open Types
open Expr

let rec pre_anf (({body; statements} : tag program) as p) : tag program =
  (* FIXME: Use a more principled approach *)
  let new_tags = ref 9999999 in
  let new_tag str =
    new_tags := !new_tags + 1;
    !new_tags in
  let make_statement_thunk acc cur =
    match cur with
    | SDataDecl(name, _, branches, tag) ->
      let counter = ref 0 in
      let mk_gensym() =
        let c = ref 0 in
        fun s ->
          begin
            c := !c + 1;
            sprintf "%s_%d" s !c
          end in
      let binds = List.map (fun cur ->
          counter := !counter + 1;
          match cur with
          | DDataSingleton(name, _) -> LetBind(name, None, ETuple([ENumber(!counter, new_tag())], new_tag()), new_tag())
          | DDataConstructor(name, args, _) ->
            let gensym = mk_gensym() in
            let func_args = List.map (fun a -> (gensym "arg", new_tag())) args in
            let func_arg_ids = List.map (fun a -> EId(fst a, new_tag())) func_args in
            LetBind(name, None, ELambda(func_args, ETuple((ENumber(!counter, new_tag()))::func_arg_ids, new_tag()), new_tag()), new_tag())) branches in
      (fun b -> ELet(binds, acc b, new_tag()))
    | _ -> failwith "Desugaring left non-data decl statement" in
  let statement_thunk = List.fold_left make_statement_thunk (fun b -> b) statements in
  let rec pre_anf_help (b : tag expr) : tag expr =
    match (b : tag expr) with
    | ELet (binds, body, t) ->
      ELet (List.flatten @@ List.map (function
          | LetBind(name, topt, rhs, tag) ->
            [LetBind(name, topt, pre_anf_help rhs, tag)]
          | TupDestr(ids, topt, rhs, tag) ->
            let tup_name = sprintf "tupdestr_%d" tag in
            LetBind(tup_name, None, pre_anf_help rhs, tag) ::
            (List.mapi (fun i (name, t) ->
                 LetBind(name, None, EGetItemExact(EId(tup_name, t), i, t), t)) ids)
        ) binds, pre_anf_help body, t)
    | ELetRec (binds, body, t) ->
      ELetRec (List.map (function
          | LetBind(name, topt, rhs, tag) ->
            LetBind(name, topt, pre_anf_help rhs, tag)
          | TupDestr(_, _, _, _) ->
            failwith "Not well-formed"
        ) binds, pre_anf_help body, t)
    | EPrim1 (op, e, t) -> EPrim1(op, pre_anf_help e, t)
    | EPrim2 (op, e1, e2, t) -> EPrim2(op, pre_anf_help e1, pre_anf_help e2, t)
    | EIf (cond, thn, els, t) -> EIf(pre_anf_help cond, pre_anf_help thn, pre_anf_help els, t)
    | ETuple (exprs, t) -> ETuple(List.map pre_anf_help exprs, t)
    | EString (_,_) -> b
    | EGetItem (_,_,_) -> failwith "NYI"
    | ESetItem (_,_,_,_) -> failwith "NYI"
    | EGetItemExact (e, n, t) -> EGetItemExact(pre_anf_help e, n, t)
    | ESetItemExact (l, n, r, t) -> ESetItemExact(pre_anf_help l, n, pre_anf_help r, t)
    | ENumber (_,_)
    | EBool (_,_)
    | ENull
    | EId (_,_) -> b
    | EApp (f, args, t) -> EApp(pre_anf_help f, List.map pre_anf_help args, t)
    | ELambda (args, b, t) -> ELambda(args, pre_anf_help b, t)
    | ESeq (exprs, t) -> ESeq(List.map pre_anf_help exprs, t)
    | EEllipsis _ -> failwith "you cannot"
    | EInclude (_,_,_) -> failwith "you cannot" in
  {statements=[]; body=statement_thunk @@ pre_anf_help body}

type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag program) : unit aprogram =
  (* At the point of ANF-ing, we drop the data information (for now) *)
  let rec helpP ({body; _} : tag program) : unit aprogram = helpA body
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
    | ELet(LetBind(bind, _, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELet(_::_, _, _) ->
       failwith "Impossible by pre_anf"
    | ELetRec(binds, body, _) ->
       let (names, new_binds_setup) = List.split (List.map (function
         | LetBind(name, _, rhs, _) -> (name, helpC rhs)
         | _ -> failwith "Impossible by pre_anf"
      ) binds) in
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
    | EString(s, _) ->
       (CString(s, ()), [])
    | EEllipsis(_) -> failwith "Cannot ANF library directly."
    | EGetItem(tup, idx, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (CGetItem(tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem(tup, idx, rhs, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (rhs_imm, rhs_setup) = helpI rhs in
       (CSetItem(tup_imm, idx_imm, rhs_imm, ()), tup_setup @ idx_setup @ rhs_setup)
    | EGetItemExact(tup, idx, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       (CGetItem(tup_imm, ImmNum(idx, ()), ()), tup_setup)
    | ESetItemExact(tup, idx, rhs, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (rhs_imm, rhs_setup) = helpI rhs in
       (CSetItem(tup_imm, ImmNum(idx, ()), rhs_imm, ()), tup_setup @ rhs_setup)
    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * unit anf_bind list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | ENull -> (ImmBool(false, ()), [])
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
    | ELet(LetBind(bind, _, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet(bind, exp_ans)] @ body_setup)
    | ELet(_, _, _) -> failwith "Impossible by pre_anf"
    | ELetRec(binds, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let (names, new_binds_setup) = List.split (List.map (function
         | LetBind(name, _, rhs, _) -> (name, helpC rhs)
         | _ -> failwith "Impossible by pre_anf"
       ) binds) in
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
    | EString(s, tag) ->
       let tmp = sprintf "str_%d" tag in
       (ImmId(tmp, ()), [BLet(tmp, CString(s, ()))])
    | EEllipsis(_) -> failwith "Cannot ANF library directly."
    | EInclude(_, _, _) -> failwith "Cannot ANF include"
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
    | EGetItemExact(tup, idx, tag) ->
       let tmp = sprintf "get_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       (ImmId(tmp, ()), tup_setup @ [BLet(tmp, CGetItem(tup_imm, ImmNum(idx, ()), ()))])
    | ESetItemExact(tup, idx, rhs, tag) ->
       let tmp = sprintf "set_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (rhs_imm, rhs_setup) = helpI rhs in
       (ImmId(tmp, ()), tup_setup @ rhs_setup @ [BLet(tmp, CSetItem(tup_imm, ImmNum(idx, ()), rhs_imm, ()))])
  and helpA (e : tag expr) : unit aexpr =
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
