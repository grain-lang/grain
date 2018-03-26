open Legacy_types
open Expr

module BindingSet = Set.Make(struct
    type t = string
    let compare = String.compare
end)

let free_vars (e : 'a aexpr) : BindingSet.t =
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
    | CSwitch(arg, branches, _) ->
      (helpI bound arg) @ (List.flatten (List.map (fun (_, a) -> helpA bound a) branches))
    | CPrim1(_, arg, _) -> helpI bound arg
    | CPrim2(_, left, right, _) -> helpI bound left @ helpI bound right
    | CApp(fn, args, _) ->
      (helpI bound fn) @ (List.flatten (List.map (fun arg -> helpI bound arg) args))
    | CString(_, _) -> []
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
  in BindingSet.of_list @@ List.sort_uniq String.compare (helpA [] e)
;;

let count_vars e =
  let rec helpA e =
    match e with
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ALetRec(binds, body, _) ->
      (List.length binds) + (max (List.fold_left max 0 @@ List.map (fun (x, c) -> helpC c) binds) (helpA body))
    | ACExpr e -> helpC e
    | ASeq(hd, tl, _) -> max (helpC hd) (helpA tl)
  and helpC e =
    match e with
    | CIf(_, t, f, _) -> max (helpA t) (helpA f)
    | CSwitch(_, bs, _) -> List.fold_left max 0 @@ List.map (fun (_, b) -> helpA b) bs
    | CApp(_, args, _) -> List.length args
    | _ -> 0
  in helpA e
;;
