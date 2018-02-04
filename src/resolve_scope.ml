open Printf
open Expr
open Legacy_types
open Utils

let make_gensym () =
  let count = ref 0 in
  fun name ->
    count := !count + 1;
    sprintf "%s$%d" name !count

let resolve_scope (p : 'a aprogram) initial_env : 'a aprogram =
  (* PRECONDITION: p is well-formed *)
  let open BindingMap in
  let gensym = make_gensym() in
  let rec helpA binds (aexpr : 'a aprogram) =
    match aexpr with
    | ALet(name, bind, body, tag) ->
      let new_name = gensym name in
      let folded_bind = helpC binds bind in
      let new_binds = add name new_name binds in
      ALet(new_name, folded_bind, helpA new_binds body, tag)
    | ALetRec(letrec_binds, body, tag) ->
      let rec collect_names rem_names binds =
        match rem_names with
        | [] -> binds
        | (name, _)::rest ->
          let new_name = gensym name in
          collect_names rest (add name new_name binds) in
      let process_bind binds (name, value) =
        (find name binds, helpC binds value) in
      let new_binds = collect_names letrec_binds binds in
      ALetRec(List.map (process_bind new_binds) letrec_binds,
              helpA new_binds body, tag)
    | ASeq(hd, tl, tag) ->
      ASeq(helpC binds hd, helpA binds tl, tag)
    | ACExpr(c) -> ACExpr(helpC binds c)
  and helpC binds cexpr =
    match cexpr with
    | CIf(c, t, e, tag) ->
      let folded_c = helpI binds c in
      let folded_t = helpA binds t in
      let folded_e = helpA binds e in
      CIf(folded_c, folded_t, folded_e, tag)
    | CPrim1(op, arg, t) ->
      let folded_arg = helpI binds arg in
      CPrim1(op, folded_arg, t)
    | CPrim2(op, arg1, arg2, t) ->
      let folded_arg1 = helpI binds arg1 in
      let folded_arg2 = helpI binds arg2 in
      CPrim2(op, folded_arg1, folded_arg2, t)
    | CApp(f, args, t) ->
      let folded_f = helpI binds f in
      let folded_args = List.map (helpI binds) args in
      CApp(folded_f, folded_args, t)
    | CTuple(elts, t) ->
      CTuple(List.map (helpI binds) elts, t)
    | CString(s, t) ->
      CString(s, t)
    | CGetItem(tup, idx, t) ->
      let folded_tup = helpI binds tup in
      let folded_idx = helpI binds idx in
      CGetItem(folded_tup, folded_idx, t)
    | CSetItem(tup, idx, value, t) ->
      let folded_tup = helpI binds tup in
      let folded_idx = helpI binds idx in
      let folded_value = helpI binds value in
      CSetItem(folded_tup, folded_idx, folded_value, t)
    | CLambda(args, body, t) ->
      let add_bind (acc_binds, new_args) name =
        let new_name = gensym name in
        ((add name new_name acc_binds), new_name::new_args) in
      let acc_binds, new_args = List.fold_left add_bind (binds, []) args in
      CLambda(List.rev new_args, helpA acc_binds body, t)
    | CImmExpr(v) -> CImmExpr(helpI binds v)
  and helpI binds imm =
    match imm with
    | ImmId(i, t) -> ImmId(find i binds, t)
    | _ -> imm in
  let initial_binds = List.fold_left (fun b n -> add n n b) empty (List.map fst initial_env) in
  helpA initial_binds p
