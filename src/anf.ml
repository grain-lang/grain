open Printf
open Expr
open Legacy_types

type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let compile_constructor_tag =
  let open Grain_typed.Types in
  function
  | CstrConstant i -> i lsl 1
  | CstrBlock i -> (i lsl 1) lor 1
  | CstrUnboxed -> failwith "compile_constructor_tag: cannot compile CstrUnboxed"

let anf_typed (p : Grain_typed.Typedtree.typed_program) : unit aprogram =
  let open Grain_parsing in
  let open Grain_typed in
  let open Types in
  let open Typedtree in

  let gensym name =
    (* FIXME: This seems a bit delicate. *)
    Ident.unique_name (Ident.create name) in

  let module MatchCompiler = Matchcomp.MatchTreeCompiler(struct
      let gensym = gensym
    end) in

  let extract_bindings pat cexpr =
    List.map (fun (name, e) -> BLet(name, e)) (MatchCompiler.extract_bindings pat cexpr) in

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
    | TExpLet(Nonrecursive, {vb_expr; vb_pat}::rest, body) ->
      (* TODO: Destructuring on letrec *)
      let (exp_ans, exp_setup) = helpCExpr vb_expr in
      let binds_setup = extract_bindings vb_pat exp_ans in
      let (body_ans, body_setup) = helpIExpr ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
      (body_ans, exp_setup @ binds_setup @ body_setup)
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
                | _ -> failwith "NYI: helpIExpr: Destructuring in lambda argument") args, []
          | TPatVar(v, _) -> [Ident.unique_name v], []
          | TPatConstruct({txt=ident}, _, []) when Identifier.equal ident (Identifier.IdentName "()") -> [], []
          | TPatAny -> [gensym "func_arg"], []
          | TPatAlias(pat, id, _) ->
            let arglist, extras = args pat in
            (* NOTE: This is assuming that arglist has length one.
               When full pattern matching is supported, that will
               handle this instead *)
            arglist, (List.hd arglist, (Ident.unique_name id))::extras
          | _ -> failwith "Impossible: helpIExpr: Lambda contained non-tuple/var pattern"
        end in
      let args, extras = args mb_pat in
      let anf_body = helpAExpr body in
      let anf_body = List.fold_left (fun body (a, b) -> ALet(b, CImmExpr(ImmId(a, ())), body, ())) anf_body extras in
      (ImmId(tmp, ()), [BLet(tmp, CLambda(args, anf_body, ()))])
    | TExpLambda([], _) -> failwith "Impossible: helpIExpr: Empty lambda"
    | TExpLambda(_, _) -> failwith "NYI: helpIExpr: Multi-branch lambda"
    | TExpTuple(args) ->
       let tmp = gensym "tup" in
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (ImmId(tmp, ()), (List.concat new_setup) @ [BLet(tmp, CTuple(new_args, ()))])
    | TExpMatch(exp, branches, _) ->
      let tmp = gensym "match" in
      let exp_ans, exp_setup = helpIExpr exp in
      let ans, setup = MatchCompiler.compile_result (Matchcomp.convert_match_branches branches) helpAExpr exp_ans in
      (ImmId(tmp, ())), (exp_setup @ (List.map (fun (n, e) -> BLet(n, e)) setup)) @ [(BLet(tmp, ans))]
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
      let rec args mb_pat = begin match mb_pat.pat_desc with
          | TPatTuple(args) ->
            List.map (function
                | {pat_desc=TPatVar(id, _)} -> Ident.unique_name id
                | _ -> failwith "NYI: helpIExpr: Destructuring in lambda argument") args, []
          | TPatVar(v, _) -> [Ident.unique_name v], []
          | TPatConstruct({txt=ident}, _, []) when Identifier.equal ident (Identifier.IdentName "()") -> [], []
          | TPatAny -> [gensym "func_arg"], []
          | TPatAlias(pat, id, _) ->
            let arglist, extras = args pat in
            (* NOTE: This is assuming that arglist has length one.
               When full pattern matching is supported, that will
               handle this instead *)
            arglist, (List.hd arglist, (Ident.unique_name id))::extras
          | _ -> failwith "Impossible: helpIExpr: Lambda contained non-tuple/var pattern"
        end in
      let args, extras = args mb_pat in
      let anf_body = helpAExpr body in
      let anf_body = List.fold_left (fun body (a, b) -> ALet(b, CImmExpr(ImmId(a, ())), body, ())) anf_body extras in
      (CLambda(args, anf_body, ()), [])
    | TExpLambda([], _) -> failwith "helpCExpr: impossible: empty lambda"
    | TExpLambda(_, _) -> failwith "helpCExpr: NYI: multi-branch lambda"
    | TExpApp(func, args) ->
       let (new_func, func_setup) = helpIExpr func in
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (CApp(new_func, new_args, ()), func_setup @ List.concat new_setup)
    | TExpTuple(args) ->
       let (new_args, new_setup) = List.split (List.map helpIExpr args) in
       (CTuple(new_args, ()), List.concat new_setup)
    | TExpMatch(expr, branches, _) ->
      let exp_ans, exp_setup = helpIExpr expr in
      let ans, setup = MatchCompiler.compile_result (Matchcomp.convert_match_branches branches) helpAExpr exp_ans in
      ans, (exp_setup @ (List.map (fun (n, e) -> BLet(n, e)) setup))
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

  let rec helpAStmt (({ttop_desc; ttop_env; _} as s) : toplevel_stmt) : (unit anf_bind list) option =
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
    | TTopData(decl) ->
      let open Types in
      let typath = Path.PIdent (decl.data_id) in
      let descrs = Datarepr.constructors_of_type typath (decl.data_type) in
      begin match descrs with
        | [] -> failwith "Impossible: TTopData TDataAbstract"
        | descrs ->
          let bind_constructor (cd_id, {cstr_name; cstr_tag; cstr_args}) =
            let rhs = match cstr_tag with
              | CstrConstant _ ->
                let compiled_tag = compile_constructor_tag cstr_tag in
                CTuple([ImmNum(compiled_tag, ())], ())
              | CstrBlock _ ->
                let compiled_tag = compile_constructor_tag cstr_tag in
                let args = List.map (fun _ -> gensym "constr_arg") cstr_args in
                let arg_ids = List.map (fun a -> ImmId(a, ())) args in
                CLambda(args, ACExpr(CTuple((ImmNum(compiled_tag, ()))::arg_ids, ())), ())
              | CstrUnboxed -> failwith "NYI: ANF CstrUnboxed" in
            BLet(Ident.unique_name cd_id, rhs) in
          Some(List.map bind_constructor descrs)
      end
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

let () =
  Matchcomp.compile_constructor_tag := compile_constructor_tag
