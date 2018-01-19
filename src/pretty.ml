open Types
open Printf
open Format
open Lexing

let rec intersperse (elts : 'a list) (sep : 'a) : 'a list =
  match elts with
  | [] -> []
  | [elt] -> [elt]
  | elt::rest -> elt::sep::(intersperse rest sep)

let string_of_op1 op =
  match op with
  | Add1 -> "add1"
  | Sub1 -> "sub1"
  | PrintStack -> "printStack"
  | Not -> "!"
  | IsNum -> "isnum"
  | IsBool -> "isbool"
  | IsTuple -> "istuple"

let name_of_op1 op =
  match op with
  | Add1 -> "Add1"
  | Sub1 -> "Sub1"
  | PrintStack -> "PrintStack"
  | Not -> "Not"
  | IsNum -> "IsNum"
  | IsBool -> "IsBool"
  | IsTuple -> "IsTuple"

let string_of_op2 op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | And -> "&&"
  | Or -> "||"
  | Greater -> ">"
  | Less -> "<"
  | GreaterEq -> ">="
  | LessEq -> "<="
  | Eq -> "=="
let name_of_op2 op =
  match op with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | And -> "And"
  | Or -> "Or"
  | Greater -> "Greater"
  | Less -> "Less"
  | GreaterEq -> "GreaterEq"
  | LessEq -> "LessEq"
  | Eq -> "Eq"

let rec string_of_typ t =
  match t with
  | TyCon s -> s
  | TyVar s -> sprintf "'%s" s
  | TyArr(args, ret) -> sprintf "(%s -> %s)" (ExtString.String.join " " (List.map string_of_typ args))
                          (string_of_typ ret)
  | TyTup args -> sprintf "(%s)" (ExtString.String.join ", " (List.map string_of_typ args))
  | TyGen(args, s) -> sprintf "(%s %s)" (ExtString.String.join ", " (List.map string_of_typ args)) s
and string_of_scheme (vars, typ) =
  sprintf "(Forall (%s), %s)" (ExtString.String.join ", " vars) (string_of_typ typ)
;;

let string_of_sopt (sopt : scheme option) : string =
  match sopt with
  | None -> ""
  | Some scheme -> " : " ^ (string_of_scheme scheme)

let rec string_of_a_list (p : (string * 'a) list) : string =
  let inner = match p with
  | (n, _)::rest -> n ^ ", " ^ string_of_a_list rest
  | [] -> ""
  in sprintf "(%s)" inner

let rec string_of_expr (e : 'a expr) : string =
  match e with
  | ENumber(n, _) -> string_of_int n
  | EBool(b, _) -> string_of_bool b
  | EId(x, _) -> x
  | ENull -> "<null>"
  | EEllipsis(_) -> "..."
  | EInclude(lib, body, _) ->
    sprintf "(include %s in %s)" lib (string_of_expr body)
  | EPrim1(op, e, _) ->
     sprintf "%s(%s)" (string_of_op1 op) (string_of_expr e)
  | EPrim2(op, left, right, _) ->
     sprintf "(%s %s %s)" (string_of_expr left) (string_of_op2 op) (string_of_expr right)
  | ELet(binds, body, _)
  | ELetRec(binds, body, _) ->
     let binds_strs = List.map (function
       | LetBind(x, sopt, e, _) -> sprintf "%s%s = %s" x (string_of_sopt sopt) (string_of_expr e)
       | TupDestr(x, sopt, e, _) -> sprintf "%s%s = %s" (string_of_a_list x) (string_of_sopt sopt) (string_of_expr e)
     ) binds in
     let binds_str = List.fold_left (^) "" (intersperse binds_strs ", ") in
     sprintf "(let %s in %s)" binds_str (string_of_expr body)
  | EIf(cond, thn, els, _) ->
     sprintf "(if %s: %s else: %s)"
             (string_of_expr cond)
             (string_of_expr thn)
             (string_of_expr els)
  | EApp(func, args, _) ->
    sprintf "(%s(%s))" (string_of_expr func) (ExtString.String.join ", " (List.map string_of_expr args))
  | EString(s, _) -> sprintf "\"%s\"" s
  | ETuple(vals, _) ->
     sprintf "(%s)" (ExtString.String.join ", " (List.map string_of_expr vals))
  | EGetItem(tup, idx, _) ->
     sprintf "%s[%s]" (string_of_expr tup) (string_of_expr idx)
  | ESetItem(tup, idx, rhs, _) ->
    sprintf "%s[%s] := %s" (string_of_expr tup) (string_of_expr idx) (string_of_expr rhs)
  | EGetItemExact(tup, idx, _) ->
     sprintf "%s[:%d:]" (string_of_expr tup) idx
  | ESetItemExact(tup, idx, rhs, _) ->
sprintf "%s[:%d:] := %s" (string_of_expr tup) idx (string_of_expr rhs)
  | ELambda(args, body, _) ->
     sprintf "(lambda(%s): %s)" (ExtString.String.join ", " (List.map fst args))
             (string_of_expr body)
  | ESeq(stmts, _) ->
     sprintf "(%s)" (ExtString.String.join "; " (List.map string_of_expr stmts))

(*TODO: Show data types*)
let string_of_prog {body; _} = string_of_expr body

let string_of_pos ((pstart, pend) : (Lexing.position * Lexing.position)) : string =
  sprintf "%s, %d:%d-%d:%d" pstart.pos_fname pstart.pos_lnum (pstart.pos_cnum - pstart.pos_bol)
          pend.pos_lnum (pend.pos_cnum - pend.pos_bol)

let rec string_of_aexpr (e : 'a aexpr) : string =
  match e with
  | ASeq(fst, snd, _) ->
     sprintf "(%s; %s)" (string_of_cexpr fst) (string_of_aexpr snd)
  | ALet(x, e, b, _) -> sprintf "(alet %s = %s in %s)" x (string_of_cexpr e) (string_of_aexpr b)
  | ALetRec(xes, b, _) ->
     sprintf "(aletrec %s in %s)"
             (ExtString.String.join
                ", "
                (List.map (fun (x, e) -> sprintf "%s = %s" x (string_of_cexpr e)) xes))
             (string_of_aexpr b)
  | ACExpr c -> string_of_cexpr c
and string_of_cexpr c =
  match c with
  | CPrim1(op, e, _) ->
     sprintf "%s(%s)" (string_of_op1 op) (string_of_immexpr e)
  | CPrim2(op, left, right, _) ->
     sprintf "(%s %s %s)" (string_of_immexpr left) (string_of_op2 op) (string_of_immexpr right)
  | CIf(cond, thn, els, _) ->
     sprintf "(if %s: %s else: %s)"
             (string_of_immexpr cond)
             (string_of_aexpr thn)
             (string_of_aexpr els)
  | CApp(func, args, _) ->
    sprintf "(%s(%s))" (string_of_immexpr func) (ExtString.String.join ", " (List.map string_of_immexpr args))
  | CString(s, _) -> sprintf "\"%s\"" s
  | CTuple(vals, _) ->
     sprintf "(%s)" (ExtString.String.join ", " (List.map string_of_immexpr vals))
  | CGetItem(tup, idx, _) ->
     sprintf "%s[%s]" (string_of_immexpr tup) (string_of_immexpr idx)
  | CSetItem(tup, idx, rhs, _) ->
     sprintf "%s[%s] := %s" (string_of_immexpr tup) (string_of_immexpr idx) (string_of_immexpr rhs)
  | CLambda(args, body, _) ->
     sprintf "(lambda(%s): %s)" (ExtString.String.join ", " args) (string_of_aexpr body)
  | CImmExpr i -> string_of_immexpr i
and string_of_immexpr i =
  match i with
  | ImmNum(n, _) -> string_of_int n
  | ImmBool(b, _) -> string_of_bool b
  | ImmId(x, _) -> x
and string_of_aprogram p = string_of_aexpr p

let rec format_prog_helper (p : 'a program) (print_a : 'a -> string) : string * string * string =
  let maybe_a a =
    let astr = print_a a in
    if astr = "" then "" else "<" ^ astr ^ ">" in
  let indent = 2 in
  let open_bracket fmt =
    pp_print_string fmt "[" in
  let close_bracket fmt =
    pp_print_string fmt "]" in
  let print_list ?brackets:(brackets=false) fmt p_item items p_sep =
    if brackets then
      open_bracket fmt;
    begin
      match items with
      | [] -> ();
      | [item] -> p_item item fmt
      | first::rest ->
        p_item first fmt;
        List.iter (fun item -> p_sep fmt; p_item item fmt) rest;
    end;
    if brackets then
      close_bracket fmt
  in
  let print_record fmt pairs p_kv_sep p_sep =
    pp_print_string fmt "{";
    let rec do_print items =
      match items with
      | [] -> ();
      | [(name, value)] ->
        pp_print_string fmt name;
        p_kv_sep fmt;
        pp_print_string fmt value
      | (name, value)::rest ->
        pp_print_string fmt name;
        p_kv_sep fmt;
        pp_print_string fmt value;
        List.iter (fun (name, value) -> p_sep fmt; pp_print_string fmt name; p_kv_sep fmt; pp_print_string fmt value) rest in
    do_print pairs;
    pp_print_string fmt "}" in
  let print_colon_sep fmt =
    pp_print_space fmt (); pp_print_string fmt ":"; pp_print_space fmt () in
  let print_comma_sep fmt =
    pp_print_string fmt ","; pp_print_space fmt () in
  let print_semi_sep fmt =
    pp_print_string fmt ";"; pp_print_space fmt () in
  let open_label fmt label a =
    pp_open_hvbox fmt indent; pp_print_string fmt label; pp_print_string fmt (maybe_a a);
    pp_print_string fmt "("; pp_print_cut fmt () in
  let open_paren fmt =
    pp_open_box fmt 2; pp_print_string fmt "("; pp_print_cut fmt () in
  let close_paren fmt =
    pp_print_break fmt 0 (~-indent); pp_close_box fmt (); pp_print_string fmt ")" in
  let quote x = "\"" ^ x ^ "\"" in
  let rec help_bind (b : 'a bind) fmt =
    match b with
    | LetBind(x, sopt, b, a) ->
      open_paren fmt;
      pp_print_string fmt (" " ^ (quote x)); pp_print_string fmt (string_of_sopt sopt);
      pp_print_string fmt (maybe_a a); print_comma_sep fmt; help b fmt;
      close_paren fmt
    | TupDestr(ids, sopt, b, a) ->
      open_paren fmt;
      pp_print_string fmt (" " ^ (quote (string_of_a_list ids))); pp_print_string fmt (string_of_sopt sopt);
      pp_print_string fmt (maybe_a a); print_comma_sep fmt; help b fmt;
      close_paren fmt
  and help_data_branch d fmt =
    match d with
    | DDataSingleton(name, a) ->
      open_label fmt "DDataSingleton" a;
      pp_print_string fmt (quote name);
      close_paren fmt
    | DDataConstructor(name, tyvars, a) ->
      open_label fmt "DDataConstructor" a;
      pp_print_string fmt (quote name);
      print_comma_sep fmt;
      print_list fmt (fun t fmt ->
          match t with
          | TyVar(v) -> pp_print_string fmt (quote v)
          | _ -> failwith "Impossible") tyvars print_comma_sep;
      close_paren fmt
  and help_stmt s fmt =
    match s with
    | SInclude(name, a) ->
      open_label fmt "SInclude" a;
      pp_print_string fmt (quote name);
      close_paren fmt
    | SLet(binds, a) ->
      open_label fmt "SLet" a;
      open_paren fmt; print_list fmt help_bind binds print_comma_sep; close_paren fmt;
      print_comma_sep fmt;
      close_paren fmt
    | SLetRec(binds, a) ->
      open_label fmt "SLetRec" a;
      open_paren fmt; print_list fmt help_bind binds print_comma_sep; close_paren fmt;
      print_comma_sep fmt;
      close_paren fmt
    | SDataDecl(name, tyvars, branches, a) ->
      open_label fmt "SDataDecl" a;
      open_paren fmt;
      pp_print_string fmt (quote name);
      print_comma_sep fmt;
      print_list fmt (fun t fmt ->
          match t with
          | TyVar(v) -> pp_print_string fmt (quote v)
          | _ -> failwith "Impossible") tyvars print_comma_sep;
      print_list fmt help_data_branch branches print_comma_sep;
      close_paren fmt
  and help e fmt =
    match e with
    | ENumber(n, a) ->
       open_label fmt "ENumber" a;
       pp_print_int fmt n;
       close_paren fmt
    | EBool(b, a) ->
       open_label fmt "EBool" a;
       pp_print_bool fmt b;
       close_paren fmt
    | EId(x, a) ->
       open_label fmt "EId" a;
       pp_print_string fmt (quote x);
       close_paren fmt
    | ENull ->
      pp_print_string fmt (quote "ENull")
    | EEllipsis(a) ->
       open_label fmt "EEllipsis" a;
       close_paren fmt
    | EInclude(lib, body, a) ->
      open_label fmt "EInclude" a;
      pp_print_string fmt (quote lib);
      help body fmt;
      close_paren fmt
    | EPrim1(op, e, a) ->
       open_label fmt "EPrim1" a;
       pp_print_string fmt (name_of_op1 op);
       print_comma_sep fmt; help e fmt;
       close_paren fmt
    | EPrim2(op, e1, e2, a) ->
       open_label fmt "EPrim2" a;
       pp_print_string fmt (name_of_op2 op);
       print_comma_sep fmt; help e1 fmt; print_comma_sep fmt; help e2 fmt;
       close_paren fmt
    | EIf(cond, thn, els, a) ->
       open_label fmt "EIf" a;
       help cond fmt; print_comma_sep fmt; help thn fmt; print_comma_sep fmt; help els fmt;
       close_paren fmt
    | EApp(func, args, a) ->
       open_label fmt "EApp" a;
       help func fmt;
       print_comma_sep fmt;
       (match args with
        | [] -> ()
        | [e] -> help e fmt
        | e1::rest -> help e1 fmt; List.iter (fun e -> print_comma_sep fmt; help e fmt) rest);
       close_paren fmt
    | EString(s, a) ->
      open_label fmt "EString" a;
      pp_print_string fmt (quote s);
      close_paren fmt
    | ETuple(vals, a) ->
       open_label fmt "ETuple" a;
       (match vals with
        | [] -> ()
        | [e] -> help e fmt
        | e1::rest -> help e1 fmt; List.iter (fun e -> print_comma_sep fmt; help e fmt) rest);
       close_paren fmt
    | EGetItem(tup, idx, a) ->
       open_label fmt "EGetItem" a;
       help tup fmt;
       print_comma_sep fmt; help idx fmt;
       close_paren fmt
    | ESetItem(tup, idx, rhs, a) ->
       open_label fmt "ESetItem" a;
       help tup fmt;
       print_comma_sep fmt; help idx fmt;
       print_comma_sep fmt; help rhs fmt;
       close_paren fmt
    | EGetItemExact(tup, idx, a) ->
       open_label fmt "EGetItemExact" a;
       help tup fmt;
       print_comma_sep fmt; pp_print_int fmt idx;
       close_paren fmt
    | ESetItemExact(tup, idx, rhs, a) ->
       open_label fmt "ESetItemExact" a;
       help tup fmt;
       print_comma_sep fmt; pp_print_int fmt idx;
       print_comma_sep fmt; help rhs fmt;
       close_paren fmt
    | ESeq(stmts, a) ->
       open_label fmt "ESeq" a;
       print_list fmt help stmts print_semi_sep;
       close_paren fmt
    | ELet(binds, body, a) ->
       open_label fmt "ELet" a;
       open_paren fmt; print_list fmt help_bind binds print_comma_sep; close_paren fmt;
       print_comma_sep fmt;
       help body fmt;
       close_paren fmt
    | ELetRec(binds, body, a) ->
       open_label fmt "ELetRec" a;
       open_paren fmt; print_list fmt help_bind binds print_comma_sep; close_paren fmt;
       print_comma_sep fmt;
       help body fmt;
       close_paren fmt
    | ELambda(args, body, a) ->
       let print_item (x, a) fmt =
         open_paren fmt;
         pp_print_string fmt (" " ^ (quote x)); pp_print_string fmt (maybe_a a); print_comma_sep fmt;
         close_paren fmt in
       open_label fmt "ELambda" a;
       open_paren fmt; print_list fmt print_item args print_comma_sep; close_paren fmt;
       print_comma_sep fmt;
       help body fmt;
       close_paren fmt
  in
  let help_stmts s fmt =
    print_list fmt help_stmt s print_comma_sep in
  help_stmts p.statements str_formatter;
  let statements_str = flush_str_formatter() in
  help p.body str_formatter;
  let body_str = flush_str_formatter() in
  print_record str_formatter [
    ("statements", statements_str);
    ("body", body_str)
  ] print_colon_sep print_comma_sep;
  let full_prog = flush_str_formatter () in
  (statements_str, body_str, full_prog)

let format_prog (p : 'a program) (print_a : 'a -> string) : string =
  let (_, _, p) = format_prog_helper p print_a in
  p
;;

let format_expr (e : 'a expr) (print_a : 'a -> string) : string =
  let (_, body, _) = format_prog_helper {statements=[]; body=e} print_a in
  body
;;

let ast_of_pos_prog (e : sourcespan program) : string =
  format_prog e string_of_pos
let ast_of_prog (e : 'a program) : string =
  format_prog e (fun _ -> "")

let rec ast_of_pos_expr (e : (Lexing.position * Lexing.position) expr) : string =
  format_expr e string_of_pos
let rec ast_of_expr (e : 'a expr) : string =
  format_expr e (fun _ -> "")
