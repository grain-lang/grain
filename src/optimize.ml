open Printf
open Pretty
open Expr
open Types
open Simple_expr
open Utils

type optimization_settings = {
  verbose: bool;
  sound: bool;
  initial_functions: initial_func list
}

let purity_env (prog : tag aprogram) (initial_funcs : initial_func list) : (string, bool) Hashtbl.t =
  let ans : (string, bool) Hashtbl.t = Hashtbl.create 0 in
  let pure_builtins, unpure_builtins =
    List.fold_left (fun (pure, unpure) (n, _, p) ->
        if p then
          ((n::pure), unpure)
        else
          (pure, (n::unpure))) ([], []) initial_funcs in
  let rec helpA (aexp : tag aexpr) : bool =
    (* is the given expression pure or not?
       Also, update ans with any bindings you encounter. *)
    match aexp with
    | ALet(bind, value, body, _) ->
      (let val_purity = helpC value in
       Hashtbl.add ans bind val_purity;
       (helpA body) && val_purity)
    | ALetRec(binds, body, _) ->
      (List.iter (fun (n, v) ->
           Hashtbl.add ans n true) binds;
       List.iter (fun (n, v) ->
           let res = helpC v in
           Hashtbl.add ans n res) binds;
       helpA body)
    | ASeq(hd, tl, _) ->
      let hd_purity = helpC hd in
      helpA tl && hd_purity
    | ACExpr e -> helpC e
  and helpC (cexp : tag cexpr) : bool =
    match cexp with
    | CIf(c,t,e,_) ->
      let c_purity = helpI c in
      let t_purity = helpA t in
      let e_purity = helpA e in
      c_purity && t_purity && e_purity
    | CPrim1(_,_,_) -> true
    | CPrim2(op, arg1, arg2, _) ->
      true
    | CApp(ImmId(f, _), _, _) when List.mem f unpure_builtins -> false
    | CApp(ImmId(f, _), _, _) when List.mem f pure_builtins -> true
    | CApp(ImmId(f, _), args,_) ->
      let f_purity = Hashtbl.find ans f in
      f_purity
    | CApp _ -> false
    | CString(s, _) -> true
    | CTuple(elts, _) ->
      true
    | CGetItem(tup, idx, _) ->
      true
    | CSetItem(tup, idx, v, _) ->
      false
    | CLambda(args, body, _) ->
      (List.iter (fun a -> Hashtbl.add ans a true) args;
       helpA body)
    | CImmExpr(v) -> helpI v
  and helpI (imm : tag immexpr) : bool =
    match imm with
    | ImmId(id, _) -> Hashtbl.find ans id
    | _ -> true
  in
  ignore(helpA prog);
  ans

let const_fold (prog : 'a aprogram) opts : unit aprogram =
  (* PRECONDITION: prog has been scope-resolved *)
  let open BindingMap in
  (* If false, enable optimizations which may mask errors *)
  let sound = opts.sound in
  let purity = purity_env prog opts.initial_functions in
  let add_imm_binding binds name value =
    match value with
    | CImmExpr(v) ->
      add name v binds
    | _ -> binds in
  let num_valid n =
    n < 1073741823 && n > -1073741824 in
  let rec helpA (binds : unit immexpr BindingMap.t) (aexp : 'b aexpr) =
    match aexp with
    | ALet(bind, value, body, _) ->
      let folded_rhs = helpC binds value in
      ALet(bind, folded_rhs, (helpA (add_imm_binding binds bind folded_rhs) body), ())
    | ALetRec(letrec_binds, body, _) ->
      (* Re-runs constant folding over letrec bindings until a fixed point is reached *)
      let rec fold_binds binds (letrec_binds : (string * unit cexpr) list) =
        let folded = List.map (fun (n, v) -> (n, helpC binds v)) letrec_binds in
        if folded = letrec_binds then
          (letrec_binds, binds)
        else
          let new_env = List.fold_left (fun b (n, v) -> add_imm_binding b n v) binds folded in
          fold_binds new_env folded in
      let initial_folded = List.map (fun (n, v) -> (n, helpC binds v)) letrec_binds in
      let folded_binds, new_env = fold_binds binds initial_folded in
      ALetRec(folded_binds, helpA new_env body, ())
    | ASeq(hd, tl, _) ->
      ASeq(helpC binds hd, helpA binds tl, ())
    | ACExpr e -> ACExpr(helpC binds e)
  and helpC binds (cexp : 'b cexpr) =
    match cexp with
    | CIf(c, t, e, _) ->
      let folded_c = helpI binds c in
      let folded_t = helpA binds t in
      let folded_e = helpA binds e in
      CIf(folded_c, folded_t, folded_e, ())
    | CPrim1(op, arg, _) ->
      let folded_arg = helpI binds arg in
      begin
        match (op, folded_arg) with
        | (IsBool, ImmBool(_, _))
        | (IsNum, ImmNum(_, _)) ->
          CImmExpr(ImmBool(true, ()))
        | (IsBool, ImmNum(_, _))
        | (IsNum, ImmBool(_, _)) ->
          CImmExpr(ImmBool(false, ()))
        | (Add1, ImmNum(arg, _)) when num_valid (arg + 1) ->
          CImmExpr(ImmNum(arg + 1, ()))
        | (Sub1, ImmNum(arg, _)) when num_valid (arg - 1) ->
          CImmExpr(ImmNum(arg - 1, ()))
        | (Not, ImmBool(arg, _)) ->
          CImmExpr(ImmBool(not arg, ()))
        | _ -> CPrim1(op, folded_arg, ())
      end
    | CPrim2(op, arg1, arg2, _) ->
      let folded_arg1 = helpI binds arg1 in
      let folded_arg2 = helpI binds arg2 in
      begin
        (* We are making the conscious decision to *NOT* _always_ optimize
           things like (0 * x) into 0, since we want to error if x is a
           non-number *)
        match (op, folded_arg1, folded_arg2) with
        (* Equality constant-valued expressions *)
        | (Eq, ImmNum _, ImmBool _)
        | (Eq, ImmBool _, ImmNum _) ->
          CImmExpr(ImmBool(false, ()))
        | (Eq, ImmNum(n, _), ImmNum(m, _)) when n = m ->
          CImmExpr(ImmBool(true, ()))
        | (Eq, ImmBool(a, _), ImmBool(b, _)) when a = b ->
          CImmExpr(ImmBool(true, ()))
        (* Boolean constant-valued expressions *)
        | (And, ImmBool(arg1, _), ImmBool(arg2, _)) ->
          CImmExpr(ImmBool(arg1 && arg2, ()))
        | (Or, ImmBool(arg1, _), ImmBool(arg2, _)) ->
          CImmExpr(ImmBool(arg1 || arg2, ()))
        (* Numeric constant-valued expressions *)
        | (Plus, ImmNum(arg1, _), ImmNum(arg2, _)) when num_valid (arg1 + arg2) ->
          CImmExpr(ImmNum(arg1 + arg2, ()))
        | (Minus, ImmNum(arg1, _), ImmNum(arg2, _)) when num_valid (arg1 - arg2) ->
          CImmExpr(ImmNum(arg1 - arg2, ()))
        | (Times, ImmNum(arg1, _), ImmNum(arg2, _)) when num_valid (arg1 * arg2) ->
          CImmExpr(ImmNum(arg1 * arg2, ()))
        | (Less, ImmNum(arg1, _), ImmNum(arg2, _)) ->
          CImmExpr(ImmBool(arg1 < arg2, ()))
        | (Greater, ImmNum(arg1, _), ImmNum(arg2, _)) ->
          CImmExpr(ImmBool(arg1 > arg2, ()))
        | (LessEq, ImmNum(arg1, _), ImmNum(arg2, _)) ->
          CImmExpr(ImmBool(arg1 <= arg2, ()))
        | (GreaterEq, ImmNum(arg1, _), ImmNum(arg2, _)) ->
          CImmExpr(ImmBool(arg1 >= arg2, ()))
        (** Less sound optimizations (may remove potential type errors) **)
        (* No-ops *)
        | (And, ImmBool(true, _), x)
        | (And, x, ImmBool(true, _))
        | (Or, ImmBool(false, _), x)
        | (Or, x, ImmBool(false, _))
        | (Plus, ImmNum(0, _), x)
        | (Plus, x, ImmNum(0, _))
        | (Minus, ImmNum(0, _), x)
        | (Minus, x, ImmNum(0, _))
        | (Times, ImmNum(1, _), x)
        | (Times, x, ImmNum(1, _)) when not sound ->
          CImmExpr(x)
        | (Times, ImmNum(0, _), ImmId(x, _))
        | (Times, ImmId(x, _), ImmNum(0, _)) when (not sound) && (Hashtbl.find purity x) ->
          CImmExpr(ImmNum(0, ()))
        | (And, ImmBool(false as b, _), ImmId(x, _))
        | (And, ImmId(x, _), ImmBool(false as b, _)) 
        | (Or, ImmBool(true as b, _), ImmId(x, _))
        | (Or, ImmId(x, _), ImmBool(true as b, _)) when (not sound) && (Hashtbl.find purity x) ->
          CImmExpr(ImmBool(b, ()))
        | _ -> CPrim2(op, folded_arg1, folded_arg2, ())
      end
    | CApp(f, args, _) ->
      let folded_f = helpI binds f in
      let folded_args = List.map (helpI binds) args in
      CApp(folded_f, folded_args, ())
    | CTuple(elts, _) ->
      CTuple(List.map (helpI binds) elts, ())
    | CString(s, _) ->
      CString(s, ())
    | CGetItem(tup, idx, _) ->
      let folded_tup = helpI binds tup in
      let folded_idx = helpI binds idx in
      CGetItem(folded_tup, folded_idx, ())
    | CSetItem(tup, idx, value, _) ->
      let folded_tup = helpI binds tup in
      let folded_idx = helpI binds idx in
      let folded_value = helpI binds value in
      CSetItem(folded_tup, folded_idx, folded_value, ())
    | CLambda(args, body, _) ->
      (* We remove the arguments here in order to make the compiler
         robust about any future decisions about shadowing *)
      CLambda(args, helpA (List.fold_right BindingMap.remove args binds) body, ())
    | CImmExpr(v) -> CImmExpr(helpI binds v)
  and helpI binds (imm : 'b immexpr) =
    match imm with
    | ImmNum(n,_) -> ImmNum(n, ())
    | ImmBool(b,_) -> ImmBool(b, ())
    | ImmId(i,_) ->
      try
        find i binds
      with
      | Not_found -> ImmId(i, ())
  in
  helpA empty (auntag prog)


let cse (prog : tag aprogram) opts : unit aprogram =
  (* PRECONDITION: Scope is resolved *)
  let initial_funcs = opts.initial_functions in
  let purity = purity_env prog initial_funcs in
  (* This table maps arbitrary simple expressions to simpler ones
     that are equal to them: for example, "let x = a + b in" should map (a + b)
     to x.  You will likely need the generality of simple_expr for both the
     keys and the values of this table, but if you're careful, you might
     be able to simplify it to map simpl_exprs to strings. *)
  let equiv_exprs : (simple_expr, simple_expr) Hashtbl.t = Hashtbl.create 0 in
  let add_identity i = Hashtbl.add equiv_exprs (Id(i)) (Id(i)) in
  List.iter add_identity @@
  List.map (fun (n, _, _) -> n) initial_funcs;
  let union expr new_bind =
    (* Check if simplified RHS is simple *)
    
    match cexpr_to_simple_opt expr with
    (* If it's not simple, then just add the new
       binding to the CSE dict as itself *)
    | Some(Num(_))
    | Some(Bool(_)) (* Don't substitute constants *)
    | None -> add_identity new_bind;
      expr
    (* If it _is_ simple, then attempt to
       reuse existing representative *)
    | Some(simple_expr) ->
      begin
        if Hashtbl.find purity new_bind then
          try
            (* If the binding is pure, look up the representative
               for the simple expression in the hashmap *)
            Hashtbl.add equiv_exprs (Id(new_bind)) @@ Hashtbl.find equiv_exprs simple_expr;
            simple_to_cexpr @@ Hashtbl.find equiv_exprs simple_expr
          with
          | Not_found ->
            (* This is the first instance of simple_expr.
               Add the new binding as its representative *)
            Hashtbl.add equiv_exprs simple_expr (Id(new_bind));
            add_identity new_bind;
            simple_to_cexpr simple_expr
        else
          (* If the binding is impure, don't add a
             substitution for its subexpression *)
          (add_identity new_bind;
           expr)
      end
  in
  let rec helpA (aexp : tag aexpr) =
    match aexp with
    | ALet(bind, value, body, _) ->
      (* Simplify the RHS as needed *)
      let cse_value = helpC value in
      let reduced = union cse_value bind in
      ALet(bind, reduced, helpA body, ())
    | ALetRec(binds, body, _) ->
      List.iter (fun (n, _) -> add_identity n) binds;
      let process_bind (name, v) =
        let rhs = helpC v in
        let reduced = union rhs name in
        (name, reduced) in
      ALetRec(List.map process_bind binds, helpA body, ())
    | ASeq(hd, tl, _) ->
      ASeq(helpC hd, helpA tl, ())
    | ACExpr e -> ACExpr(helpC e)
  and helpC (cexp : tag cexpr) =
    let simplified =
      match cexp with
      | CIf(c, t, e, _) ->
        CIf(helpI c, helpA t, helpA e, ())
      | CPrim1(op, arg, _) ->
        CPrim1(op, helpI arg, ())
      | CPrim2(op, arg1, arg2, _) ->
        CPrim2(op, helpI arg1, helpI arg2, ())
      | CApp(f, args, _) ->
        CApp(helpI f, List.map helpI args, ())
      | CTuple(elts, _) ->
        CTuple(List.map helpI elts, ())
      | CString(s, _) ->
        CString(s, ())
      | CGetItem(tup, idx, _) ->
        CGetItem(helpI tup, helpI idx, ())
      | CSetItem(tup, idx, value, _) ->
        CSetItem(helpI tup, helpI idx, helpI value, ())
      | CLambda(args, body, _) ->
        List.iter (fun a -> Hashtbl.add equiv_exprs (Id(a)) (Id(a))) args;
        CLambda(args, helpA body, ())
      | CImmExpr(value) -> CImmExpr(helpI value) in
    begin
      match cexpr_to_simple_opt simplified with
      | Some(Num(_))
      | Some(Bool(_)) (* Don't substitute constants *)
      | None -> simplified
      | Some(simple_expr) ->
        try
          simple_to_cexpr @@ Hashtbl.find equiv_exprs simple_expr
        with
        | Not_found -> simplified
    end
  and helpI (imm : tag immexpr) =
    match imm with
    | ImmNum(n, _) -> ImmNum(n, ())
    | ImmBool(b, _) -> ImmBool(b, ())
    | ImmId(id, _) ->
      simple_to_imm @@ Hashtbl.find equiv_exprs (Id(id)) in
  helpA prog

let dae (prog : tag aprogram) opts : unit aprogram =
  let initial_funcs = opts.initial_functions in
  let purity = purity_env prog initial_funcs in
  let used = Hashtbl.create 0 in
  let sound = opts.sound in
  let can_remove identifier =
    try
      not (Hashtbl.find used identifier) (* raises Not_found if not in map *)
    with
    | Not_found -> Hashtbl.find purity identifier in
  let rec helpA (aexp : tag aexpr) =
    match aexp with
    | ALet(bind, value, body, _) ->
      (* Simplify the RHS as needed *)
      let dae_body = helpA body in
      let dae_value = helpC value in
      begin
        match dae_body with
        (* Treat assignment as "dead" when its body just returns the RHS *)
        | ACExpr(CImmExpr(ImmId(id, _))) when id = bind ->
          ACExpr(dae_value)
        | _ ->
          
          let rhs_safe = (not sound) || match dae_value with
            | CImmExpr(ImmId _)
            | CImmExpr(ImmNum _)
            | CImmExpr(ImmBool _) -> true
            | _ -> false in
          if rhs_safe && can_remove bind then
            dae_body
          else
            ALet(bind, helpC value, dae_body, ())
      end
    | ALetRec(binds, body, _) ->
      let mapped_binds = List.map (fun (n, v) -> (n, helpC v)) binds in
      let dae_body = helpA body in
      let new_binds = List.fold_right (fun (n, v) tl ->
          if can_remove n then
            tl
          else
            (n, v)::tl) mapped_binds [] in
      (match new_binds with
       | [] -> dae_body
       | _ -> ALetRec(new_binds, dae_body, ()))
    | ASeq(hd, tl, _) ->
      ASeq(helpC hd, helpA tl, ())
    | ACExpr e -> ACExpr(helpC e)
  and helpC (cexp : tag cexpr) =
    match cexp with
    | CIf(c, t, e, _) ->
      CIf(helpI c, helpA t, helpA e, ())
    | CPrim1(op, arg, _) ->
      CPrim1(op, helpI arg, ())
    | CPrim2(op, arg1, arg2, _) ->
      CPrim2(op, helpI arg1, helpI arg2, ())
    | CApp(f, args, _) ->
      CApp(helpI f, List.map helpI args, ())
    | CTuple(elts, _) ->
      CTuple(List.map helpI elts, ())
    | CString(s, _) ->
      CString(s, ())
    | CGetItem(tup, idx, _) ->
      CGetItem(helpI tup, helpI idx, ())
    | CSetItem(tup, idx, value, _) ->
      CSetItem(helpI tup, helpI idx, helpI value, ())
    | CLambda(args, body, _) ->
      List.iter (fun a -> Hashtbl.add used a true) args;
      CLambda(args, helpA body, ())
    | CImmExpr(value) -> CImmExpr(helpI value)
  and helpI (imm : tag immexpr) =
    match imm with
    | ImmNum(n, _) -> ImmNum(n, ())
    | ImmBool(b, _) -> ImmBool(b, ())
    | ImmId(id, _) ->
      Hashtbl.add used id true;
      ImmId(id, ())
  in
  helpA prog

let optimize ?passes:(passes=4) (prog : tag aprogram) (opts : optimization_settings) : tag aprogram =
  let rec pass n prog =
    if n <= 0 then
      prog
    else
      let const_prog : tag aprogram = atag (const_fold prog opts) in
      let cse_prog = atag (cse const_prog opts) in
      let dae_prog = atag (dae cse_prog opts) in
      begin
        (if opts.verbose
         then begin
           printf "Const/tagged:\n%s\n" (string_of_aprogram const_prog);
           printf "CSE/tagged:\n%s\n" (string_of_aprogram cse_prog);
           printf "DAE/tagged:\n%s\n" (string_of_aprogram (atag dae_prog))
         end
         else ());
        if dae_prog = prog then
          dae_prog
        else
          pass (n - 1) dae_prog
      end in
  pass passes prog
