open Legacy_types

type tag = int
let tag (p : 'a program) : tag program =
  let next = ref 0 in
  let tag () =
    next := !next + 1;
    !next in
  let rec help_bind (b : 'a bind) : tag bind =
    match b with
    | LetBind(x, topt, b, _) ->
      let t = tag() in LetBind(x, topt, helpE b, t)
    | TupDestr(ids, topt, b, _) ->
      let t = tag() in TupDestr(List.map (fun (n, _) -> (n, tag())) ids, topt, helpE b, t)
  and helpE (e : 'a expr) : tag expr =
    match e with
    | EId(x, _) -> EId(x, tag())
    | ENumber(n, _) -> ENumber(n, tag())
    | EBool(b, _) -> EBool(b, tag())
    | EInclude(lib, body, _) -> EInclude(lib, helpE body, tag())
    | EEllipsis(_) -> EEllipsis(tag())
    | EPrim1(op, e, _) ->
       let prim_tag = tag() in
       EPrim1(op, helpE e, prim_tag)
    | EPrim2(op, e1, e2, _) ->
       let prim_tag = tag() in
       EPrim2(op, helpE e1, helpE e2, prim_tag)
    | ESeq(stmts, _) ->
       let seq_tag = tag() in
       ESeq(List.map helpE stmts, seq_tag)
    | ELet(binds, body, _) ->
       let let_tag = tag() in
       ELet(List.map help_bind binds, helpE body, let_tag)
    | ELetRec(binds, body, _) ->
      let let_tag = tag() in
      ELetRec(List.map help_bind binds, helpE body, let_tag)
    | EIf(cond, thn, els, _) ->
       let if_tag = tag() in
       EIf(helpE cond, helpE thn, helpE els, if_tag)
    | ETuple(vals, _) ->
       let tuple_tag = tag() in
       ETuple(List.map helpE vals, tuple_tag)
    | EString(s, _) ->
       let string_tag = tag() in
       EString(s, string_tag)
    | EGetItem(tup, idx, _) ->
       let get_tag = tag() in
       EGetItem(helpE tup, helpE idx, get_tag)
    | ESetItem(tup, idx, rhs, _) ->
       let get_tag = tag() in
       ESetItem(helpE tup, helpE idx, helpE rhs, get_tag)
    | EGetItemExact(tup, idx, _) ->
       let get_tag = tag() in
       EGetItemExact(helpE tup, idx, get_tag)
    | ESetItemExact(tup, idx, rhs, _) ->
       let get_tag = tag() in
       ESetItemExact(helpE tup, idx, helpE rhs, get_tag)
    | EApp(func, args, _) ->
       let app_tag = tag() in
       EApp(helpE func, List.map helpE args, app_tag)
    | ELambda(args, body, _) ->
       let lam_tag = tag() in
       ELambda(List.map (fun (a, _) -> (a, tag())) args, helpE body, lam_tag)
    | ENull -> ENull
  and helpD (d : 'a data_branch) : tag data_branch =
    match d with
    | DDataSingleton(name, _) -> DDataSingleton(name, tag())
    | DDataConstructor(name, tyvars, _) -> DDataConstructor(name, tyvars, tag())
  and helpS (s : 'a stmt) : tag stmt =
    match s with
    | SInclude(lib, _) -> SInclude(lib, tag())
    | SLet(binds, _) ->
      let let_tag = tag() in
      SLet(List.map help_bind binds, let_tag)
    | SLetRec(binds, _) ->
      let let_tag = tag() in
      SLetRec(List.map help_bind binds, let_tag)
    | SDataDecl(name, vars, branches, _) ->
      let data_tag = tag() in
      SDataDecl(name, vars, List.map helpD branches, data_tag)
  and helpP (p : 'a program) : tag program =
    {statements = List.map helpS p.statements;
     body = helpE p.body}
  in helpP p

let rec untag : 'a. 'a program -> unit program = fun p ->
  let rec help_bind b : unit bind =
    match b with
    | LetBind(x, topt, b, _) ->
      LetBind(x, topt, helpE b, ())
    | TupDestr(ids, topt, b, _) ->
      TupDestr(List.map (fun (n, _) -> (n, ())) ids, topt, helpE b, ())
  and helpE e =
    match e with
    | EId(x, _) -> EId(x, ())
    | ENumber(n, _) -> ENumber(n, ())
    | EBool(b, _) -> EBool(b, ())
    | EInclude(lib, body, _) -> EInclude(lib, helpE body, ())
    | EEllipsis(_) -> EEllipsis(())
    | EPrim1(op, e, _) ->
      EPrim1(op, helpE e, ())
    | EPrim2(op, e1, e2, _) ->
      EPrim2(op, helpE e1, helpE e2, ())
    | ESeq(stmts, _) ->
      ESeq(List.map helpE stmts, ())
    | ELet(binds, body, _) ->
      ELet(List.map (function
      | LetBind(x, topt, b, _) ->
        LetBind(x, topt, helpE b, ())
      | TupDestr(ids, topt, b, _) ->
        TupDestr(List.map (fun (n, _) -> (n, ())) ids, topt, helpE b, ())
      ) binds, helpE body, ())
    | ELetRec(binds, body, _) ->
      ELetRec(List.map (function
      | LetBind(x, topt, b, _) ->
        LetBind(x, topt, helpE b, ())
      | TupDestr(ids, topt, b, _) ->
        TupDestr(List.map (fun (n, _) -> (n, ())) ids, topt, helpE b, ())
      ) binds, helpE body, ())
    | EIf(cond, thn, els, _) ->
      EIf(helpE cond, helpE thn, helpE els, ())
    | ETuple(vals, _) ->
      ETuple(List.map helpE vals, ())
    | EString(s, _) ->
      EString(s, ())
    | EGetItem(tup, idx, _) ->
      EGetItem(helpE tup, helpE idx, ())
    | ESetItem(tup, idx, rhs, _) ->
      ESetItem(helpE tup, helpE idx, helpE rhs, ())
    | EGetItemExact(tup, idx, _) ->
      EGetItemExact(helpE tup, idx, ())
    | ESetItemExact(tup, idx, rhs, _) ->
      ESetItemExact(helpE tup, idx, helpE rhs, ())
    | EApp(name, args, _) ->
      EApp(helpE name, List.map helpE args, ())
    | ELambda(args, body, _) ->
      ELambda(List.map (fun (x, _) -> (x, ())) args, helpE body, ())
    | ENull -> ENull
  and helpD d : unit data_branch =
    match d with
    | DDataSingleton(name, _) -> DDataSingleton(name, ())
    | DDataConstructor(name, tyvars, _) -> DDataConstructor(name, tyvars, ())
  and helpS s : unit stmt =
    match s with
    | SInclude(lib, _) -> SInclude(lib, ())
    | SLet(binds, _) ->
      SLet(List.map help_bind binds, ())
    | SLetRec(binds, _) ->
      SLetRec(List.map help_bind binds, ())
    | SDataDecl(name, vars, branches, _) ->
      SDataDecl(name, vars, List.map helpD branches, ())
  and helpP p =
    {statements = List.map helpS p.statements;
     body = helpE p.body}
  in helpP p

let atag (p : 'a aprogram) : tag aprogram =
  let next = ref 0 in
  let tag () =
    next := !next + 1;
    !next in
  let rec helpA (e : 'a aexpr) : tag aexpr =
    match e with
    | ASeq(fst, snd, _) ->
       let seq_tag = tag() in
       ASeq(helpC fst, helpA snd, seq_tag)
    | ALet(x, c, b, _) ->
       let let_tag = tag() in
       ALet(x, helpC c, helpA b, let_tag)
    | ALetRec(xcs, b, _) ->
       let let_tag = tag() in
       ALetRec(List.map (fun (x, c) -> (x, helpC c)) xcs, helpA b, let_tag)
    | ACExpr c -> ACExpr (helpC c)
  and helpC (c : 'a cexpr) : tag cexpr =
    match c with
    | CPrim1(op, e, _) ->
       let prim_tag = tag() in
       CPrim1(op, helpI e, prim_tag)
    | CPrim2(op, e1, e2, _) ->
       let prim_tag = tag() in
       CPrim2(op, helpI e1, helpI e2, prim_tag)
    | CSwitch(arg, branches, _) ->
       let switch_tag = tag() in
       CSwitch(helpI arg, List.map (fun (i, b) -> (i, helpA b)) branches, switch_tag)
    | CIf(cond, thn, els, _) ->
       let if_tag = tag() in
       CIf(helpI cond, helpA thn, helpA els, if_tag)
    | CTuple(vals, _) ->
       let tuple_tag = tag() in
       CTuple(List.map helpI vals, tuple_tag)
    | CString(s, _) ->
       let string_tag = tag() in
       CString(s, string_tag)
    | CGetItem(tup, idx, _) ->
       let get_tag = tag() in
       CGetItem(helpI tup, helpI idx, get_tag)
    | CSetItem(tup, idx, rhs, _) ->
       let get_tag = tag() in
       CSetItem(helpI tup, helpI idx, helpI rhs, get_tag)
    | CApp(name, args, _) ->
       let app_tag = tag() in
       CApp(helpI name, List.map helpI args, app_tag)
    | CLambda(args, body, _) ->
       let lam_tag = tag() in
       CLambda(args, helpA body, lam_tag)
    | CImmExpr i -> CImmExpr (helpI i)
  and helpI (i : 'a immexpr) : tag immexpr =
    match i with
    | ImmId(x, _) -> ImmId(x, tag())
    | ImmNum(n, _) -> ImmNum(n, tag())
    | ImmBool(b, _) -> ImmBool(b, tag())
  and helpP p = helpA p
  in helpP p

let auntag (p : 'a aprogram) : unit aprogram =
  let rec helpA (e : 'a aexpr) : unit aexpr =
    match e with
    | ASeq(fst, snd, _) ->
       ASeq(helpC fst, helpA snd, ())
    | ALet(x, c, b, _) ->
       ALet(x, helpC c, helpA b, ())
    | ALetRec(xcs, b, _) ->
       ALetRec(List.map (fun (x, c) -> (x, helpC c)) xcs, helpA b, ())
    | ACExpr c -> ACExpr (helpC c)
  and helpC (c : 'a cexpr) : unit cexpr =
    match c with
    | CPrim1(op, e, _) ->
       CPrim1(op, helpI e, ())
    | CPrim2(op, e1, e2, _) ->
       CPrim2(op, helpI e1, helpI e2, ())
    | CSwitch(arg, branches, _) ->
       CSwitch(helpI arg, List.map (fun (i, b) -> (i, helpA b)) branches, ())
    | CIf(cond, thn, els, _) ->
       CIf(helpI cond, helpA thn, helpA els, ())
    | CTuple(vals, _) ->
       CTuple(List.map helpI vals, ())
    | CString(s, _) ->
       CString(s, ())
    | CGetItem(tup, idx, _) ->
       CGetItem(helpI tup, helpI idx, ())
    | CSetItem(tup, idx, rhs, _) ->
       CSetItem(helpI tup, helpI idx, helpI rhs, ())
    | CApp(name, args, _) ->
       CApp(helpI name, List.map helpI args, ())
    | CLambda(args, body, _) ->
       CLambda(args, helpA body, ())
    | CImmExpr i -> CImmExpr (helpI i)
  and helpI (i : 'a immexpr) : unit immexpr =
    match i with
    | ImmId(x, _) -> ImmId(x, ())
    | ImmNum(n, _) -> ImmNum(n, ())
    | ImmBool(b, _) -> ImmBool(b, ())
  and helpP p = helpA p
  in helpP p
