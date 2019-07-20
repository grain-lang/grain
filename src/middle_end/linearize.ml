open Grain_parsing
open Grain_typed
open Types
open Typedtree
open Anftree
open Anf_helper

module MatchCompiler = Matchcomp.MatchTreeCompiler

let compile_constructor_tag =
  let open Grain_typed.Types in
  function
  | CstrConstant i -> i lsl 1
  | CstrBlock i -> (i lsl 1) lor 1
  | CstrUnboxed -> failwith "compile_constructor_tag: cannot compile CstrUnboxed"

let gensym = Ident.create

let value_imports = ref []
(* At the linearization phase, we lift all imports *)
let symbol_table = ref (Ident.empty : (Ident.t Ident.tbl) Ident.tbl)
module PathMap = Hashtbl.Make(struct 
    type t = Path.t
    let hash x = Hashtbl.hash (Path.name x)
    let equal a b = (Path.compare a b) == 0
  end)
let type_map = PathMap.create 10

let lookup_symbol mod_ mod_decl name original_name =
  begin
    match Ident.find_same_opt mod_ (!symbol_table) with
    | Some _ -> ()
    | None -> symbol_table := Ident.add mod_ Ident.empty (!symbol_table)
  end;
  let modtbl = Ident.find_same mod_ (!symbol_table) in
  match Ident.find_name_opt name modtbl with
  | Some(_, ident) -> ident
  | None ->
    let fresh = gensym name in
    begin match mod_decl.md_filepath with
      | Some filepath ->
        value_imports := (Imp.grain_value fresh filepath original_name GlobalShape)::(!value_imports)
      | None -> ()
    end;
    symbol_table := Ident.add mod_ (Ident.add fresh fresh modtbl) (!symbol_table);
    fresh

type anf_bind =
  | BSeq of comp_expression
  | BLet of Ident.t * comp_expression
  | BLetRec of (Ident.t * comp_expression) list
  | BLetExport of rec_flag * (Ident.t * comp_expression) list

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let extract_bindings pat cexpr =
  List.map (fun (name, e) -> BLet(name, e)) (MatchCompiler.extract_bindings pat cexpr)

let transl_const (c : Types.constant) : (imm_expression, string * comp_expression) either =
  match c with
  | Const_string s -> Right("str", Comp.string s)
  | Const_float _ -> failwith "NYI: helpIConst: float"
  | _ -> Left(Imm.const c)


let rec transl_imm (({exp_desc; exp_loc=loc; exp_env=env; _} as e) : expression) : (imm_expression * anf_bind list) =
  match exp_desc with
  | TExpIdent(_, _, {val_kind=TValUnbound _}) -> failwith "Impossible: val_kind was unbound"
  | TExpIdent(Path.PExternal((Path.PIdent mod_) as p, ident, _), _, {val_fullpath=Path.PExternal(_, original_name, _)}) ->
    let mod_decl = Env.find_module p None env in
    (Imm.id ~loc ~env (lookup_symbol mod_ mod_decl ident original_name), [])
  | TExpIdent(Path.PExternal((Path.PIdent mod_) as p, ident, _), _, _) ->
    let mod_decl = Env.find_module p None env in
    (Imm.id ~loc ~env (lookup_symbol mod_ mod_decl ident ident), [])
  | TExpIdent(Path.PExternal _, _, _) -> failwith "NYI: transl_imm: TExpIdent with multiple PExternal"
  | TExpIdent((Path.PIdent ident) as path, _, _) ->
    begin match Env.find_value path env with
      | {val_fullpath=Path.PIdent _} -> (Imm.id ~loc ~env ident, [])
      | {val_fullpath=Path.PExternal((Path.PIdent mod_) as p, ident, _)} -> 
        let mod_decl = Env.find_module p None env in
        (Imm.id ~loc ~env (lookup_symbol mod_ mod_decl ident ident), [])
      | {val_fullpath=Path.PExternal _} -> failwith "NYI: transl_imm: TExpIdent with multiple PExternal"
    end
  | TExpConstant c ->
    begin match transl_const c with
      | Left imm -> (imm, [])
      | Right (name, cexpr) ->
        let tmp = gensym name in
        (Imm.id ~loc ~env tmp, [BLet(tmp, cexpr)])
    end
  | TExpNull -> (Imm.const ~loc ~env (Const_bool false)), []
  | TExpPrim1(op, arg) ->
    let tmp = gensym "unary" in
    let (arg_imm, arg_setup) = transl_imm arg in
    (Imm.id ~loc ~env tmp, arg_setup @ [BLet(tmp, Comp.prim1 ~loc ~env op arg_imm)])
  | TExpPrim2(And, left, right) ->
    let tmp = gensym "boolBinop" in
    let (left_imm, left_setup) = transl_imm left in
    (Imm.id ~loc ~env tmp, left_setup @ [BLet(tmp, Comp.if_ ~loc ~env left_imm (transl_anf_expression right) (AExp.comp ~loc ~env (Comp.imm ~loc ~env left_imm)))])
  | TExpPrim2(Or, left, right) ->
    let tmp = gensym "boolBinop" in
    let (left_imm, left_setup) = transl_imm left in
    (Imm.id ~loc ~env tmp, left_setup @ [BLet(tmp, Comp.if_ ~loc ~env left_imm (AExp.comp ~loc ~env (Comp.imm ~loc ~env left_imm)) (transl_anf_expression right))])
  | TExpPrim2(op, left, right) ->
    let tmp = gensym "binop" in
    let (left_imm, left_setup) = transl_imm left in
    let (right_imm, right_setup) = transl_imm right in
    (Imm.id ~loc ~env tmp, left_setup @ right_setup @ [BLet(tmp, Comp.prim2 ~loc ~env op left_imm right_imm)])
  | TExpAssign(left, right) ->
    let tmp = gensym "assign" in
    let (left_imm, left_setup) = transl_imm left in
    let (right_imm, right_setup) = transl_imm right in
    (Imm.id ~loc ~env tmp, left_setup @ right_setup @ [BLet(tmp, Comp.assign ~loc ~env left_imm right_imm)])
  | TExpIf(cond, _then, _else) ->
    let tmp = gensym "if" in
    let (cond_imm, cond_setup) = transl_imm cond in
    (Imm.id ~loc ~env tmp, cond_setup @ [BLet(tmp, Comp.if_ ~loc ~env cond_imm (transl_anf_expression _then) (transl_anf_expression _else))])
  | TExpWhile(cond, body) ->
    let tmp = gensym "while" in
    (Imm.id ~loc ~env tmp, [BLet(tmp, Comp.while_ ~loc ~env (transl_anf_expression cond) (transl_anf_expression body))])
  | TExpApp(func, args) ->
    let tmp = gensym "app" in
    let (new_func, func_setup) = transl_imm func in
    let (new_args, new_setup) = List.split (List.map transl_imm args) in
    (Imm.id ~loc ~env tmp, (func_setup @ List.concat new_setup) @ [BLet(tmp, Comp.app ~loc ~env new_func new_args)])
  | TExpBlock([]) -> failwith "Impossible by syntax"
  | TExpBlock([stmt]) -> transl_imm stmt
  | TExpBlock(fst :: rest) ->
    let (fst_ans, fst_setup) = transl_comp_expression fst in
    let (rest_ans, rest_setup) = transl_imm {e with exp_desc=(TExpBlock(rest))} in
    (rest_ans, fst_setup @ [BSeq fst_ans] @ rest_setup)
  | TExpLet(Nonrecursive, [], body) -> transl_imm body
  | TExpLet(Nonrecursive, {vb_expr; vb_pat}::rest, body) ->
    (* TODO: Destructuring on letrec *)
    let (exp_ans, exp_setup) = transl_comp_expression vb_expr in
    let binds_setup = extract_bindings vb_pat exp_ans in
    let (body_ans, body_setup) = transl_imm ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
    (body_ans, exp_setup @ binds_setup @ body_setup)
  | TExpLet(Recursive, binds, body) ->
    let tmp = gensym "lam" in
    let (binds, new_binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, transl_comp_expression vb_expr)) binds) in
    let (new_binds, new_setup) = List.split new_binds_setup in

    let names = List.map (function
        | {pat_desc=TPatVar(id, _)} -> id
        | _ -> failwith "Non-name not allowed on LHS of let rec.") binds in

    let (body_ans, body_setup) = transl_comp_expression body in
    (Imm.id ~loc ~env tmp, (List.concat new_setup)
                           @ [BLetRec (List.combine names new_binds)]
                           @ body_setup
                           @ [BLet(tmp, body_ans)])
  | TExpLambda({mb_pat; mb_body=body}::[], _) ->
    let tmp = gensym "lam" in
    let rec args mb_pat =
      begin match mb_pat.pat_desc with
        | TPatTuple(args) ->
          List.map (function
              | {pat_desc=TPatVar(id, _)} -> id
              | _ -> failwith "NYI: transl_imm: Destructuring in lambda argument") args, []
        | TPatVar(v, _) -> [v], []
        | TPatConstruct({txt=ident}, _, []) when Identifier.equal ident (Identifier.IdentName "()") -> [], []
        | TPatAny -> [gensym "func_arg"], []
        | TPatAlias(pat, id, _) ->
          let arglist, extras = args pat in
          (* NOTE: This is assuming that arglist has length one.
             When full pattern matching is supported, that will
             handle this instead *)
          arglist, (List.hd arglist, id)::extras
        | _ -> failwith "Impossible: transl_imm: Lambda contained non-tuple/var pattern"
      end in
    let args, extras = args mb_pat in
    let anf_body = transl_anf_expression body in
    let anf_body = List.fold_left (fun body (a, b) ->
        AExp.let_ ~loc ~env Nonrecursive
          [(b, Comp.imm ~loc ~env (Imm.id ~loc ~env a))]
          body) anf_body extras in
    (Imm.id ~loc ~env tmp, [BLet(tmp, Comp.lambda ~loc ~env args anf_body)])
  | TExpLambda([], _) -> failwith "Impossible: transl_imm: Empty lambda"
  | TExpLambda(_, _) -> failwith "NYI: transl_imm: Multi-branch lambda"
  | TExpTuple(args) ->
    let tmp = gensym "tup" in
    let (new_args, new_setup) = List.split (List.map transl_imm args) in
    (Imm.id ~loc ~env tmp, (List.concat new_setup) @ [BLet(tmp, Comp.tuple ~loc ~env new_args)])
  | TExpMatch(exp, branches, _) ->
    let tmp = gensym "match" in
    let exp_ans, exp_setup = transl_imm exp in
    let ans, setup = MatchCompiler.compile_result (Matchcomp.convert_match_branches branches) transl_anf_expression exp_ans in
    (Imm.id ~loc ~env tmp), (exp_setup @ (List.map (fun (n, e) -> BLet(n, e)) setup)) @ [(BLet(tmp, ans))]
  | TExpConstruct _ -> failwith "NYI: transl_imm: Construct"


and bind_patts ?exported:(exported=false) (exp_id : Ident.t) (patts : pattern list) : anf_bind list =
  let postprocess_item cur acc =
    match cur with
    | None -> acc
    | Some(ident, (src, idx), extras) ->
      let bind = if exported then
          BLetExport(Nonrecursive, [ident, Comp.tuple_get (Int32.of_int idx) (Imm.id src)])
        else
          BLet(ident, Comp.tuple_get (Int32.of_int idx) (Imm.id src)) in
      [bind] @ extras @ acc in
  let postprocess items = List.fold_right postprocess_item items [] in
  (* Pass one: Some(<identifier>, <access path>, <extra binds>)*)
  let rec anf_patts_pass_one src i {pat_desc; pat_extra} =
    begin match pat_extra with
      | [] -> ()
      | _ -> failwith "NYI: anf_patts_pass_one: TPatConstraint"
    end;
    match pat_desc with
    | TPatVar(bind, _) -> Some(bind, (src, i), [])
    | TPatAny -> None
    | TPatTuple(patts) ->
      let tmp = gensym "tup_patt" in
      Some(tmp, (src, i), postprocess @@ List.mapi (anf_patts_pass_one tmp) patts)
    | TPatConstant _ -> failwith "NYI: anf_patts_pass_one: TPatConstant"
    | TPatConstruct _ -> failwith "NYI: anf_patts_pass_one: TPatConstruct"
    | TPatOr _ -> failwith "NYI: anf_patts_pass_one: TPatOr"
    | TPatAlias _ -> failwith "NYI: anf_patts_pass_one: TPatAlias" in
  postprocess @@ List.mapi (anf_patts_pass_one exp_id) patts

and transl_comp_expression (({exp_desc; exp_loc=loc; exp_env=env; _} as e) : expression) : (comp_expression * anf_bind list) =
  match exp_desc with
  | TExpPrim1(op, arg) ->
    let (arg_imm, arg_setup) = transl_imm arg in
    (Comp.prim1 ~loc ~env op arg_imm, arg_setup)
  | TExpPrim2(And, left, right) ->
    let (left_imm, left_setup) = transl_imm left in
    (Comp.if_ ~loc ~env left_imm (transl_anf_expression right) (AExp.comp ~loc ~env (Comp.imm ~loc ~env left_imm)), left_setup)
  | TExpPrim2(Or, left, right) ->
    let (left_imm, left_setup) = transl_imm left in
    (Comp.if_ ~loc ~env left_imm (AExp.comp ~loc ~env (Comp.imm ~loc ~env left_imm)) (transl_anf_expression right), left_setup)
  | TExpPrim2(op, left, right) ->
    let (left_imm, left_setup) = transl_imm left in
    let (right_imm, right_setup) = transl_imm right in
    (Comp.prim2 ~loc ~env op left_imm right_imm, left_setup @ right_setup)
  | TExpIf(cond, _then, _else) ->
    let (cond_imm, cond_setup) = transl_imm cond in
    (Comp.if_ ~loc ~env cond_imm (transl_anf_expression _then) (transl_anf_expression _else), cond_setup)
  | TExpBlock([]) -> failwith "Impossible by syntax"
  | TExpBlock([stmt]) -> transl_comp_expression stmt
  | TExpBlock(fst :: rest) ->
    let (fst_ans, fst_setup) = transl_comp_expression fst in
    let (rest_ans, rest_setup) = transl_comp_expression ({e with exp_desc=TExpBlock(rest)}) in
    (rest_ans, fst_setup @ [BSeq fst_ans] @ rest_setup)
  | TExpLet(Nonrecursive, [], body) -> transl_comp_expression body

  | TExpLet(Nonrecursive, {vb_expr; vb_pat={pat_desc=TPatVar(bind, _)}}::rest, body) ->
    let (exp_ans, exp_setup) = transl_comp_expression vb_expr in
    let (body_ans, body_setup) = transl_comp_expression ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
    (body_ans, exp_setup @ [BLet(bind, exp_ans)] @ body_setup)
  | TExpLet(Nonrecursive, {vb_expr; vb_pat={pat_desc=TPatTuple(patts)}}::rest, body) ->
    let (exp_ans, exp_setup) = transl_comp_expression vb_expr in
    let tmp = gensym "let_tup" in

    (* Extract items from tuple *)

    let anf_patts = bind_patts tmp patts in

    let (body_ans, body_setup) = transl_comp_expression ({e with exp_desc=TExpLet(Nonrecursive, rest, body)}) in
    (body_ans, exp_setup @ ((BLet(tmp, exp_ans))::anf_patts) @ body_setup)

  | TExpLet(Nonrecursive, _::_, _) ->
    failwith "Impossible by pre_anf"
  | TExpLet(Recursive, binds, body) ->
    let (binds, new_binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, transl_comp_expression vb_expr)) binds) in
    let (new_binds, new_setup) = List.split new_binds_setup in

    let names = List.map (function
        | {pat_desc=TPatVar(id, _)} -> id
        | _ -> failwith "Non-name not allowed on LHS of let rec.") binds in

    let (body_ans, body_setup) = transl_comp_expression body in
    (body_ans, (List.concat new_setup)
               @ ((BLetRec(List.combine names new_binds))::body_setup))
  | TExpLambda({mb_pat; mb_body=body}::[], _) ->
    let rec args mb_pat = begin match mb_pat.pat_desc with
      | TPatTuple(args) ->
        List.map (function
            | {pat_desc=TPatVar(id, _)} -> id
            | _ -> failwith "NYI: transl_imm: Destructuring in lambda argument") args, []
      | TPatVar(v, _) -> [v], []
      | TPatConstruct({txt=ident}, _, []) when Identifier.equal ident (Identifier.IdentName "()") -> [], []
      | TPatAny -> [gensym "func_arg"], []
      | TPatAlias(pat, id, _) ->
        let arglist, extras = args pat in
        (* NOTE: This is assuming that arglist has length one.
           When full pattern matching is supported, that will
           handle this instead *)
        arglist, (List.hd arglist, id)::extras
      | _ -> failwith "Impossible: transl_imm: Lambda contained non-tuple/var pattern"
    end in
    let args, extras = args mb_pat in
    let anf_body = transl_anf_expression body in
    let anf_body = List.fold_left (fun body (a, b) -> AExp.let_ ~loc ~env Nonrecursive [(b, Comp.imm ~loc ~env (Imm.id ~loc ~env a))] body) anf_body extras in
    (Comp.lambda ~loc ~env args anf_body, [])
  | TExpLambda([], _) -> failwith "transl_comp_expression: impossible: empty lambda"
  | TExpLambda(_, _) -> failwith "transl_comp_expression: NYI: multi-branch lambda"
  | TExpApp(func, args) ->
    let (new_func, func_setup) = transl_imm func in
    let (new_args, new_setup) = List.split (List.map transl_imm args) in
    (Comp.app ~loc ~env new_func new_args, func_setup @ List.concat new_setup)
  | TExpTuple(args) ->
    let (new_args, new_setup) = List.split (List.map transl_imm args) in
    (Comp.tuple ~loc ~env new_args, List.concat new_setup)
  | TExpMatch(expr, branches, _) ->
    let exp_ans, exp_setup = transl_imm expr in
    let ans, setup = MatchCompiler.compile_result (Matchcomp.convert_match_branches branches) transl_anf_expression exp_ans in
    ans, (exp_setup @ (List.map (fun (n, e) -> BLet(n, e)) setup))
  | _ -> let (imm, setup) = transl_imm e in (Comp.imm ~loc ~env imm, setup)

and transl_anf_expression (({exp_desc; exp_loc=loc; exp_env=env; _} as e) : expression) : anf_expression =
  let (ans, ans_setup) = transl_comp_expression e in
  List.fold_right
    (fun bind body ->
       match bind with
       | BSeq(exp) -> AExp.seq ~loc ~env exp body
       | BLet(name, exp) -> AExp.let_ ~loc ~env Nonrecursive [(name, exp)] body
       | BLetRec(names) -> AExp.let_ ~loc ~env Recursive names body
       | BLetExport(name, exp) -> failwith "Global bind at non-toplevel")
    ans_setup (AExp.comp ~loc ~env ans)


let rec transl_anf_statement (({ttop_desc; ttop_env=env; ttop_loc=loc} as s) : toplevel_stmt) : (anf_bind list) option * import_spec list =
  match ttop_desc with
  | TTopLet(_, _, []) -> None, []
  | TTopLet(export_flag, Nonrecursive, {vb_expr; vb_pat}::rest) ->
    let (exp_ans, exp_setup) = transl_comp_expression vb_expr in
    let rest_setup, rest_imp = transl_anf_statement {s with ttop_desc=TTopLet(export_flag, Nonrecursive, rest)} in
    let rest_setup = Option.default [] rest_setup in
    let exported = export_flag = Exported in
    let setup = begin match vb_pat.pat_desc with
      | TPatVar(bind, _) -> 
        if exported 
        then [BLetExport(Nonrecursive, [bind, exp_ans])]
        else [BLet(bind, exp_ans)]
      | TPatTuple(patts) ->
        let tmp = gensym "let_tup" in
        let anf_patts = bind_patts ~exported tmp patts in
        (BLet(tmp, exp_ans))::anf_patts
      | _ -> failwith "NYI: transl_anf_statement: Non-tuple destructuring in let"
    end in
    Some(exp_setup @ setup @ rest_setup), rest_imp
  | TTopLet(export_flag, Recursive, binds) ->
    let (binds, new_binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, transl_comp_expression vb_expr)) binds) in
    let (new_binds, new_setup) = List.split new_binds_setup in

    let names = List.map (function
        | {pat_desc=TPatVar(id, _)} -> id
        | _ -> failwith "Non-name not allowed on LHS of let rec.") binds in

    begin match export_flag with
      | Exported ->
        Some((List.concat new_setup) @ [BLetExport(Recursive, List.combine names new_binds)]), []
      | Nonexported ->
        Some((List.concat new_setup) @ [BLetRec(List.combine names new_binds)]), [] 
    end
  | TTopData(decls) ->
    let open Types in
    let bindings = List.concat @@ List.map (fun decl ->
      let typath = Path.PIdent (decl.data_id) in
      (* FIXME: [philip] This is kind of hacky...would be better to store this in the Env directly...not to mention, 
        I think this'll be much more fragile than if it were in the static info *)
      let ty_id = begin match PathMap.find_opt type_map typath with
        | Some(id) -> id
        | None ->
          let id = PathMap.length type_map in
          PathMap.add type_map typath id;
          id
      end in
      let descrs = Datarepr.constructors_of_type typath (decl.data_type) in
      begin match descrs with
        | [] -> failwith "Impossible: TTopData TDataAbstract"
        | descrs ->
          let bind_constructor (cd_id, {cstr_name; cstr_tag; cstr_args}) =
            let rhs = match cstr_tag with
              | CstrConstant _ ->
                let compiled_tag = compile_constructor_tag cstr_tag in
                Comp.adt ~loc ~env (Imm.const ~loc ~env (Const_int ty_id)) (Imm.const ~loc ~env (Const_int compiled_tag)) []
              | CstrBlock _ ->
                let compiled_tag = compile_constructor_tag cstr_tag in
                let args = List.map (fun _ -> gensym "constr_arg") cstr_args in
                let arg_ids = List.map (fun a -> Imm.id ~loc ~env a) args in
                let imm_tytag = Imm.const ~loc ~env (Const_int ty_id) in
                let imm_tag = Imm.const ~loc ~env (Const_int compiled_tag) in
                Comp.lambda ~loc ~env args (AExp.comp ~loc ~env (Comp.adt ~loc ~env imm_tytag imm_tag arg_ids))
              | CstrUnboxed -> failwith "NYI: ANF CstrUnboxed" in
            BLetExport(Nonrecursive, [cd_id, rhs]) in
          List.map bind_constructor descrs
      end) decls in
      Some(bindings), []
  | TTopForeign(desc) ->
    let arity = Ctype.arity (desc.tvd_desc.ctyp_type) in
    None, [Imp.wasm_func desc.tvd_id desc.tvd_mod.txt desc.tvd_name.txt (FunctionShape(arity, 1))]
  | _ -> None, []

let transl_anf_module ({statements; body; env; signature} : typed_program) : anf_program =
  PathMap.clear type_map;
  value_imports := [];
  symbol_table := Ident.empty;
  let top_binds, imports = List.fold_right (fun cur (acc_bind, acc_imp) ->
      match cur with
      | None, lst -> acc_bind, (lst @ acc_imp)
      | Some(b), lst -> (b @ acc_bind), (lst @ acc_imp)) (List.map transl_anf_statement statements) ([], []) in
  let (ans, ans_setup) = transl_comp_expression body in
  let body = List.fold_right
      (fun bind body ->
         match bind with
         | BSeq(exp) -> AExp.seq exp body
         | BLet(name, exp) -> AExp.let_ Nonrecursive [(name, exp)] body
         | BLetRec(names) -> AExp.let_ Recursive names body
         | BLetExport(rf, binds) -> AExp.let_ ~glob:Global rf binds body)
      (top_binds @ ans_setup) (AExp.comp ans) in
  let imports = imports @ (!value_imports) in
  {
    body;
    env;
    imports;
    signature;
    analyses=ref [];
  }


let () =
  Matchcomp.compile_constructor_tag := compile_constructor_tag

