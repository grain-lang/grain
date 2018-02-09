open Printf
open Expr
open Legacy_types

type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf_typed (p : Grain_typed.Typedtree.typed_program) : unit aprogram =
  let open Grain_parsing in
  let open Grain_typed in
  let open Types in
  let open Typedtree in

  let gensym name =
    (* FIXME: This seems a bit delicate. *)
    Ident.unique_name (Ident.create name) in
  let helpIConst (c : Types.constant) : ((unit immexpr), (string * unit cexpr)) either =
    match c with
     | Const_int i -> Left(ImmNum(i, ()))
     | Const_string s -> Right("str", (CString(s, ())))
     | Const_int32 i -> Left(ImmNum(Int32.to_int i, ()))
     | Const_int64 i -> Left(ImmNum(Int64.to_int i, ()))
     | Const_bool b -> Left(ImmBool(b, ()))
     | Const_float _ -> failwith "NYI: helpIConst: float" in
  let bindPatts (exp_id : string) (patts : pattern list) : unit anf_bind list =
    let postprocess_item cur acc =
      match cur with
      | None -> acc
      | Some(ident, (src, idx), extras) ->
        [BLet(ident, CGetItem(ImmId(src, ()), ImmNum(idx, ()), ()))] @ extras @ acc in
    let postprocess items = List.fold_right postprocess_item items [] in
    (* Pass one: Some(<identifier>, <access path>, <extra binds>)*)
    let rec anf_patts_pass_one src i {pat_desc; pat_extra} =
      begin match pat_extra with
        | [] -> ()
        | _ -> failwith "NYI: anf_patts_pass_one: TPatConstraint"
      end;
      match pat_desc with
      | TPatVar(bind, _) -> Some((Ident.unique_name bind), (src, i), [])
      | TPatAny -> None
      | TPatTuple(patts) ->
        let tmp = gensym "tup_patt" in
        Some(tmp, (src, i), postprocess @@ List.mapi (anf_patts_pass_one tmp) patts)
      | TPatConstant _ -> failwith "NYI: anf_patts_pass_one: TPatConstant"
      | TPatConstruct _ -> failwith "NYI: anf_patts_pass_one: TPatConstruct"
      | TPatOr _ -> failwith "NYI: anf_patts_pass_one: TPatOr"
      | TPatAlias _ -> failwith "NYI: anf_patts_pass_one: TPatAlias" in
    postprocess @@ List.mapi (anf_patts_pass_one exp_id) patts in
  let rec helpIExpr (({exp_desc; _} as e) : expression) : (unit immexpr * unit anf_bind list) =
    match exp_desc with
    | TExpIdent(_, _, {val_kind=TValUnbound _}) -> failwith "Impossible: val_kind was unbound"
    | TExpIdent(Path.PExternal _, _, _) -> failwith "NYI: helpIExpr: TExpIdent with PExternal"
    | TExpIdent(Path.PIdent ident, _, _) -> (ImmId(Ident.unique_name ident, ()), [])
    | TExpConstant c ->
      begin match helpIConst c with
        | Left imm -> (imm, [])
        | Right (name, cexpr) ->
          let tmp = gensym name in
          (ImmId(tmp, ()), [BLet(tmp, cexpr)])
      end
    | TExpNull -> (ImmBool(false, ()), [])
    | TExpPrim1(op, arg) ->
      let tmp = gensym "unary" in
      let (arg_imm, arg_setup) = helpIExpr arg in
      (ImmId(tmp, ()), arg_setup @ [BLet(tmp, CPrim1(op, arg_imm, ()))])
    | TExpPrim2(op, left, right) ->
       let tmp = gensym "binop" in
       let (left_imm, left_setup) = helpIExpr left in
       let (right_imm, right_setup) = helpIExpr right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [BLet(tmp, CPrim2(op, left_imm, right_imm, ()))])
    | TExpIf(cond, _then, _else) ->
       let tmp = gensym "if" in
       let (cond_imm, cond_setup) = helpIExpr cond in
       (ImmId(tmp, ()), cond_setup @ [BLet(tmp, CIf(cond_imm, helpAExpr _then, helpAExpr _else, ()))])
    | TExpApp(func, args) ->
       let tmp = gensym "app" in
       let (new_func, func_setup) = helpIExpr func in
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (ImmId(tmp, ()), (func_setup @ List.concat new_setup) @ [BLet(tmp, CApp(new_func, new_args, ()))])
    | TExpBlock([]) -> failwith "Impossible by syntax"
    | TExpBlock([stmt]) -> helpIExpr stmt
    | TExpBlock(fst :: rest) ->
       let (fst_ans, fst_setup) = helpCExpr fst in
       let (rest_ans, rest_setup) = helpIExpr {e with exp_desc=(TExpBlock(rest))} in
       (rest_ans, fst_setup @ [BSeq fst_ans] @ rest_setup)
    | TExpLet(Nonrecursive, [], body) -> helpIExpr body
    | TExpLet(Nonrecursive, {vb_expr; vb_pat={pat_desc=TPatVar(bind, _)}}::rest, body) ->
      let (exp_ans, exp_setup) = helpCExpr vb_expr in
       let (body_ans, body_setup) = helpIExpr ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
       (body_ans, exp_setup @ [BLet(Ident.unique_name bind, exp_ans)] @ body_setup)
    | TExpLet(Nonrecursive, {vb_expr; vb_pat={pat_desc=TPatTuple(patts)}}::rest, body) ->
      let (exp_ans, exp_setup) = helpCExpr vb_expr in
      let tmp = gensym "let_tup" in

      (* Extract items from tuple *)
      
      let anf_patts = bindPatts tmp patts in

      let (body_ans, body_setup) = helpIExpr ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
      (body_ans, exp_setup @ ((BLet(tmp, exp_ans))::anf_patts) @ body_setup)

    | TExpLet(Nonrecursive, _, _) -> failwith "Impossible by pre_anf"
    | TExpLet(Recursive, binds, body) ->
       let tmp = gensym "lam" in
       let (binds, new_binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, helpCExpr vb_expr)) binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in

       let names = List.map (function
           | {pat_desc=TPatVar(id, _)} -> Ident.unique_name id
           | _ -> failwith "Non-name not allowed on LHS of let rec.") binds in

       let (body_ans, body_setup) = helpCExpr body in
       (ImmId(tmp, ()), (List.concat new_setup)
                        @ [BLetRec (List.combine names new_binds)]
                        @ body_setup
                        @ [BLet(tmp, body_ans)])
    | TExpLambda({mb_pat; mb_body=body}::[], _) ->
      let tmp = gensym "lam" in
      let rec args mb_pat =
        begin match mb_pat.pat_desc with
          | TPatTuple(args) ->
            List.map (function
                | {pat_desc=TPatVar(id, _)} -> Ident.unique_name id
                | _ -> failwith "NYI: helpIExpr: Destructuring in lambda argument") args
          | TPatVar(v, _) -> [Ident.unique_name v]
          | TPatConstruct({txt=ident}, _, []) when Identifier.equal ident (Identifier.IdentName "()") -> []
          | _ -> failwith "Impossible: helpIExpr: Lambda contained non-tuple/var pattern"
        end in
      let args = args mb_pat in
      (ImmId(tmp, ()), [BLet(tmp, CLambda(args, helpAExpr body, ()))])
    | TExpLambda([], _) -> failwith "Impossible: helpIExpr: Empty lambda"
    | TExpLambda(_, _) -> failwith "NYI: helpIExpr: Multi-branch lambda"
    | TExpTuple(args) ->
       let tmp = gensym "tup" in
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (ImmId(tmp, ()), (List.concat new_setup) @ [BLet(tmp, CTuple(new_args, ()))])
    | TExpMatch _ -> failwith "NYI: helpIExpr: Match"
    | TExpConstruct _ -> failwith "NYI: helpIExpr: Construct"
  and helpCExpr (({exp_desc; _} as e) : expression) : (unit cexpr * unit anf_bind list) =
    match exp_desc with
    | TExpPrim1(op, arg) ->
       let (arg_imm, arg_setup) = helpIExpr arg in
       (CPrim1(op, arg_imm, ()), arg_setup)
    | TExpPrim2(op, left, right) ->
       let (left_imm, left_setup) = helpIExpr left in
       let (right_imm, right_setup) = helpIExpr right in
       (CPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | TExpIf(cond, _then, _else) ->
       let (cond_imm, cond_setup) = helpIExpr cond in
       (CIf(cond_imm, helpAExpr _then, helpAExpr _else, ()), cond_setup)
    | TExpBlock([]) -> failwith "Impossible by syntax"
    | TExpBlock([stmt]) -> helpCExpr stmt
    | TExpBlock(fst :: rest) ->
       let (fst_ans, fst_setup) = helpCExpr fst in
       let (rest_ans, rest_setup) = helpCExpr ({e with exp_desc=TExpBlock(rest)}) in
       (rest_ans, fst_setup @ [BSeq fst_ans] @ rest_setup)
    | TExpLet(Nonrecursive, [], body) -> helpCExpr body

    | TExpLet(Nonrecursive, {vb_expr; vb_pat={pat_desc=TPatVar(bind, _)}}::rest, body) ->
      let (exp_ans, exp_setup) = helpCExpr vb_expr in
       let (body_ans, body_setup) = helpCExpr ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
       (body_ans, exp_setup @ [BLet(Ident.unique_name bind, exp_ans)] @ body_setup)
    | TExpLet(Nonrecursive, {vb_expr; vb_pat={pat_desc=TPatTuple(patts)}}::rest, body) ->
      let (exp_ans, exp_setup) = helpCExpr vb_expr in
      let tmp = gensym "let_tup" in

      (* Extract items from tuple *)
      
      let anf_patts = bindPatts tmp patts in

      let (body_ans, body_setup) = helpCExpr ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
      (body_ans, exp_setup @ ((BLet(tmp, exp_ans))::anf_patts) @ body_setup)

    | TExpLet(Nonrecursive, _::_, _) ->
      failwith "Impossible by pre_anf"
    | TExpLet(Recursive, binds, body) ->
       let (binds, new_binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, helpCExpr vb_expr)) binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in

       let names = List.map (function
           | {pat_desc=TPatVar(id, _)} -> Ident.unique_name id
           | _ -> failwith "Non-name not allowed on LHS of let rec.") binds in

       let (body_ans, body_setup) = helpCExpr body in
       (body_ans, (List.concat new_setup)
                  @ ((BLetRec(List.combine names new_binds))::body_setup))
    | TExpLambda({mb_pat; mb_body=body}::[], _) ->
      let args =
        begin match mb_pat.pat_desc with
          | TPatTuple(args) ->
            List.map (function
                | {pat_desc=TPatVar(id, _)} -> Ident.unique_name id
                | _ -> failwith "NYI: helpIExpr: Destructuring in lambda argument") args
          | TPatVar(v, _) -> [Ident.unique_name v]
          | TPatConstruct({txt=ident}, _, []) when Identifier.equal ident (Identifier.IdentName "()") -> []
          | _ -> failwith "Impossible: helpIExpr: Lambda contained non-tuple/var pattern"
        end in
      (CLambda(args, helpAExpr body, ()), [])
    | TExpLambda([], _) -> failwith "helpCExpr: impossible: empty lambda"
    | TExpLambda(_, _) -> failwith "helpCExpr: NYI: multi-branch lambda"
    | TExpApp(func, args) ->
       let (new_func, func_setup) = helpIExpr func in
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (CApp(new_func, new_args, ()), func_setup @ List.concat new_setup)
    | TExpTuple(args) ->
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (CTuple(new_args, ()), List.concat new_setup)
    | _ -> let (imm, setup) = helpIExpr e in (CImmExpr imm, setup)
  and helpAExpr (({exp_desc; _} as e) : expression) : unit aexpr =
    let (ans, ans_setup) = helpCExpr e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq(exp) -> ASeq(exp, body, ())
        | BLet(name, exp) -> ALet(name, exp, body, ())
        | BLetRec(names) -> ALetRec(names, body, ()))
      ans_setup (ACExpr ans)
  in

  let rec helpAStmt (({ttop_desc; _} as s) : toplevel_stmt) : (unit anf_bind list) option =
    match ttop_desc with
    | TTopLet(_, []) -> None
    | TTopLet(Nonrecursive, {vb_expr; vb_pat}::rest) ->
      let (exp_ans, exp_setup) = helpCExpr vb_expr in
      let rest_setup = Option.default [] (helpAStmt {s with ttop_desc=TTopLet(Nonrecursive, rest)}) in
      let setup = begin match vb_pat.pat_desc with
        | TPatVar(bind, _) -> [BLet(Ident.unique_name bind, exp_ans)]
        | TPatTuple(patts) ->
          let tmp = gensym "let_tup" in
          let anf_patts = bindPatts tmp patts in
          (BLet(tmp, exp_ans))::anf_patts
        | _ -> failwith "NYI: helpAStmt: Non-tuple destructuring in let"
      end in
      Some(exp_setup @ setup @ rest_setup)
    | TTopLet(Recursive, binds) ->
      let (binds, new_binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, helpCExpr vb_expr)) binds) in
      let (new_binds, new_setup) = List.split new_binds_setup in

      let names = List.map (function
          | {pat_desc=TPatVar(id, _)} -> Ident.unique_name id
          | _ -> failwith "Non-name not allowed on LHS of let rec.") binds in

      Some((List.concat new_setup) @ [BLetRec(List.combine names new_binds)])
    | _ -> None in

  let helpAProg ({statements; body} : typed_program) : unit aexpr =
    let top_binds = List.fold_right (fun cur acc ->
        match cur with
        | None -> acc
        | Some(b) -> b @ acc) (List.map helpAStmt statements) [] in
    let (ans, ans_setup) = helpCExpr body in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq(exp) -> ASeq(exp, body, ())
        | BLet(name, exp) -> ALet(name, exp, body, ())
        | BLetRec(names) -> ALetRec(names, body, ()))
      (top_binds @ ans_setup) (ACExpr ans) in

  helpAProg p
