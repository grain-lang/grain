open Types
open Expr


type 'a wf_env = {
  binds : 'a envt;
  is_tail : bool;
  includes_allowed : bool;
}

let safe_assoc v lst =
  try Some(List.assoc v lst)
  with
  | Not_found -> None

let well_formed (p : (Lexing.position * Lexing.position) program) (is_library : bool) builtins : exn list =
  let check_non_tail pos env errs =
    if is_library && env.is_tail then
      (EllipsisNotInLibrary(pos))::errs
    else
      errs in
  let rec wf_E e (env : sourcespan wf_env) =
    (* Includes only allowed at beginning of file *)
    let env = match e with
      | EInclude(_, _, _) -> env
      | _ -> {env with includes_allowed=false} in
    match e with
    | EBool(_, loc) -> check_non_tail loc env []
    | ENumber(n, loc) ->
       if n > 1073741823 || n < -1073741824 then (check_non_tail loc env [Overflow(n, loc)]) else []
    | EId (x, loc) ->
       (try ignore (List.assoc x env.binds); []
        with Not_found ->
          [UnboundId(x, loc)])
       |> check_non_tail loc env
    | EPrim1(_, e, loc) ->
      let new_env = {env with is_tail=false} in
      wf_E e new_env
      |> check_non_tail loc env
    | EPrim2(_, l, r, loc) ->
      let new_env = {env with is_tail=false} in
      (wf_E l new_env @ wf_E r new_env)
      |> check_non_tail loc env
    | EIf(c, t, f, loc) ->
      let new_env = {env with is_tail=false} in
      (wf_E c new_env @ wf_E t new_env @ wf_E f new_env)
      |> check_non_tail loc env
    | ETuple(vals, loc) ->
      let new_env = {env with is_tail=false} in
      List.concat (List.map (fun e -> wf_E e new_env) vals)
      |> check_non_tail loc env
    | EString(s, loc) ->
      begin try
          BatUTF8.validate s;
          check_non_tail loc env []
        with
        | BatUTF8.Malformed_code ->
          check_non_tail loc env [MalformedString(loc)]
      end
    | EGetItem(tup, idx, loc) ->
      let new_env = {env with is_tail=false} in
      (wf_E tup new_env @ wf_E idx new_env)
      |> check_non_tail loc env
    | ESetItem(tup, idx, rhs, loc) ->
      let new_env = {env with is_tail=false} in
      (wf_E tup new_env @ wf_E idx new_env @ wf_E rhs new_env)
      |> check_non_tail loc env
    | EGetItemExact(tup, idx, _) -> wf_E tup env
    | ESetItemExact(tup, idx, rhs, _) -> wf_E tup env @ wf_E rhs env
    | ESeq(exprs, loc) ->
      List.flatten (List.map (fun e -> wf_E e {env with is_tail=false}) exprs)
      |> check_non_tail loc env
    | ELet(binds, body, _) ->
       let rec dupe x binds =
         match binds with
         | [] -> None
         | (y, _, _, loc)::_ when x = y -> Some loc
         | _::rest -> dupe x rest in
       let rec process_binds rem_binds env =
         match rem_binds with
         | [] -> (env, [])
         | (x, topt, e, loc)::rest ->
            let shadow =
              match dupe x rest with
              | Some where -> [DuplicateId(x, where, loc)]
              | None ->
                 try
                   let existing = List.assoc x env.binds in [ShadowId(x, loc, existing)]
                 with Not_found -> [] in
            let errs_e = wf_E e {env with is_tail=false} in
            let new_env = {env with binds=(x, loc)::env.binds} in
            let (newer_env, errs) = process_binds rest new_env in
            (newer_env, (shadow @ errs_e @ errs)) in              
       let (env2, errs) = process_binds binds env in
       errs @ wf_E body {env2 with is_tail=env.is_tail}
    | ELetRec(binds, body, _) ->
      let rec dupe x binds =
        match binds with
        | [] -> None
        | (y, _, _, loc)::_ when x = y -> Some loc
        | _::rest -> dupe x rest in
      let rec collect_names rem_names env =
        match rem_names with
        | [] -> (env, [])
        | (x, _, _, loc)::rest ->
          let shadow =
            match dupe x rest with
            | Some where -> [DuplicateId(x, where, loc)]
            | None ->
              try
                let existing = List.assoc x env.binds in [ShadowId(x, loc, existing)]
              with Not_found -> [] in
          let new_env = {env with binds=(x, loc)::env.binds} in
          let newer_env, errs = collect_names rest new_env in
          (newer_env, (shadow @ errs)) in
      let rec process_rhses rem_rhs env =
        match rem_rhs with
        | [] -> []
        | (name, _, v, loc)::tl ->
          let non_lambda =
            match v with
            | ELambda _ -> []
            | _ -> [LetRecNonFunction(name, loc)] in
          let cur_errs = non_lambda @ (wf_E v {env with is_tail=false}) in
          cur_errs @ (process_rhses tl env) in
      let new_env, names_errs = collect_names binds env in
      let errs = process_rhses binds {new_env with is_tail=env.is_tail} in
      names_errs @ errs @ (wf_E body new_env)
    | ELambda(args, body, loc) ->
      let new_binds, new_errors = List.fold_left
          (* Fold over list, accumulating new list of bound ids and errors *)
          (fun (acc_binds, acc_errors) (name, pos) ->
             match (safe_assoc name acc_binds) with
               | Some(loc) -> acc_binds, (DuplicateId(name, pos, loc))::acc_errors
               | None ->
                 match (safe_assoc name env.binds) with
                 | Some(loc) -> acc_binds, (ShadowId(name, pos, loc))::acc_errors
                 | None -> ((name, pos)::acc_binds), acc_errors) ([], (check_non_tail loc env [])) args in
      new_errors @ (wf_E body ({env with binds=args @ env.binds; is_tail=false}))
    | EEllipsis(loc) ->
      if not is_library then
        [(EllipsisInNonLibrary(loc))]
      else if not env.is_tail then
        [(EllipsisNotInTailPosition(loc))]
      else
        []
    | EInclude(lib, body, loc) ->
      if not env.includes_allowed then
        [(IncludeNotAtBeginning(loc))] @ (wf_E body env)
      else
        wf_E body env
    | EApp(func, args, loc) ->
       (wf_E func env) @ List.concat (List.map (fun e -> wf_E e env) args)
  in
  wf_E p {binds=builtins; is_tail=true; includes_allowed=true}
;;
