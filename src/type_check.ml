open Printf
open BatUref
open Types
open Pretty



(** A type environment (mapping names to schemes) *)
type typ_env = scheme envt;;

(* A unification is a dictionary mapping type variable names to
   unifiables (from the BatUref library) containing types.
 *)
type unification = (typ uref) envt;;

type typ_constraint = typ * typ;;

type unifier = unification * (typ_constraint list);;

let typ_int = TyCon "Int"
let typ_bool = TyCon "Bool"
let typ_str = TyCon "String"

let counter = ref 0

let gen_typ_sym base =
  counter := !counter + 1;
  sprintf "%s$%d" base !counter

let rec free_typ_tyvars typ =
  match typ with
  | TyCon _ -> []
  | TyVar s -> [s]
  | TyArr(args, ret) -> List.concat (List.map free_typ_tyvars (args @ [ret]))
  | TyTup(args) -> List.concat (List.map free_typ_tyvars args)
and free_scheme_tyvars (args, typ) =
  List.fold_left ExtList.List.remove (List.sort_uniq String.compare (free_typ_tyvars typ)) args
;;

let rec free_scheme_tyvars ((args, typ) : scheme) =
  List.filter (fun n -> not (List.mem n args)) (free_typ_tyvars typ)
;;

let rec free_typs_tyvars (ts : typ list) =
  List.fold_left (fun acc cur -> List.append acc (free_typ_tyvars cur)) [] ts
;;

let rec free_tenv_tyvars (e : typ_env) =
  List.fold_left (fun acc (name, t) -> acc @ (free_scheme_tyvars t)) [] e
;;

let lookup (e : 'a envt) (name : string) : 'a =
  Printf.eprintf "%s\n" name; snd @@ List.find (fun (n, t) -> String.equal n name) e

let rec subst_type (s : unification) (t : typ) : typ =
  match t with
  | TyCon a -> t
  | TyVar a ->
    begin
      try
        uget (lookup s a)
      with Not_found -> t
    end
  | TyArr(args, ret) -> TyArr(subst_types s args, subst_type s ret)
  | TyTup args -> TyTup(subst_types s args)

and subst_types (s : unification) (ts : typ list) : typ list =
  List.map (subst_type s) ts

let subst_scheme (s : unification) ((tyargs, typ) : scheme) : scheme =
  (tyargs, subst_type (List.filter (fun (name, _) -> not (List.mem name tyargs)) s) typ)

let subst_env (s : unification) (env : typ_env) : typ_env =
  List.map (fun (name, scheme) -> (name, subst_scheme s scheme)) env

(* The function bind ensures that we can't have infinite types (e.g., by trying to equate
   X with [X -> Y]), and produces a small unification environment containing this single
   type variable, if this occurs check passes. *)
exception OccursCheck of string
let bind (tyvarname : string) (t : typ) : unification =
  match t with
  | TyVar name when tyvarname = name -> [] (* nothing to be done *)
  | _ ->
     if List.mem tyvarname (free_typ_tyvars t)
     then raise (OccursCheck (sprintf "Infinite types: %s occurs in %s" tyvarname (string_of_typ t)))
     else [tyvarname, uref t] (* make a unification containing just this one type variable, mapped to t *)

(* Unify takes two types, and a unification describing the known equalities among types,
   and tries to unify the two types.
   If the first type is a TyVar, unify it with the second type.
   If the second type is a TyVar, unify it with the first.
   If both types are TyCons, and the same type constant, then unification succeeds.
   If both are TyArrs of the same arity, recur on the pieces and unify them.
   If both are TyTups of the same arity, recur on the pieces and unify them.
   Otherwise, raise a type error explaining the mismatch.
   Return the unification, especially if it's been modified...
*)
exception TypeError of string
let rec unify (t1 : typ) (t2 : typ) (unif_env : unification) : unification =
  let rec unify_many ts1 ts2 unif_env =
    match ts1, ts2 with
    | [], [] -> unif_env
    | t1::ts1, t2::ts2 ->
      let unif_env = unify t1 t2 unif_env in
      unify_many (subst_types unif_env ts1) (subst_types unif_env ts2) unif_env
    | _ -> failwith "Impossible (different lengths in unify_many)" in
  match t1, t2 with
  | TyVar n1, _ when not(List.mem_assoc n1 unif_env) ->
     (bind n1 t2) @ unif_env
  | _, TyVar n2 when not(List.mem_assoc n2 unif_env) ->
     (bind n2 t1) @ unif_env
  | TyVar n1, TyVar n2 ->
     let n1_ref = List.assoc n1 unif_env in
     let n2_ref = List.assoc n2 unif_env in
     let choose_new_representative t1 t2 =
       (* When we get inside this function, we're effectively producing a
          new type equality asserting `t1 = t2`, but as we discussed in class,
          we need to pick a preferred direction to rewrite that equality,
          either saying "Rewrite t1 as t2", or "Rewrite t2 as t1".  If this function
          returns t2, for example, then we're choosing the first direction,
          "Rewrite t1 as t2".  You need to implement this function to choose the
          correct direction, depending on what the types are. *)
       t1 in
     unite ~sel:choose_new_representative n1_ref n2_ref;
     unif_env
  | (TyArr(args1, ret1)), (TyArr(args2, ret2)) when (List.length args1) == (List.length args2) ->
    unify_many (args1 @ [ret1]) (args2 @ [ret2]) unif_env
  | TyTup elts1, TyTup elts2 when (List.length elts1) == (List.length elts2) ->
    List.fold_left2 (fun uenv a1 a2 -> unify a1 a2 uenv) unif_env elts1 elts2
  | TyCon a, TyCon b when a == b -> unif_env
  | _ -> raise (TypeError (sprintf "Cannot unify types: %s, %s" (string_of_typ t1) (string_of_typ t2)));;


let instantiate ((tyvars, t) : scheme) =
  subst_type (List.map (fun tv -> (tv, uref (TyVar(gen_typ_sym tv)))) tyvars) t

let generalize (env : typ_env) (t : typ) : scheme =
  let free_env = free_tenv_tyvars env in
  let tyvars = List.filter (fun x -> not (List.mem x free_env)) (free_typ_tyvars t) in
  (tyvars, t)

let lookup_env (e : typ_env) (name : string) : typ =
  instantiate (lookup e name)

let prim1_types = function
  | Add1
  | Sub1
  | PrintStack -> (typ_int, typ_int)
  | Not -> (typ_bool, typ_bool)
  | IsNum
  | IsBool
  | IsTuple -> (TyVar(gen_typ_sym "prim1"), typ_bool)

let prim2_types = function
  | Plus
  | Minus
  | Times -> (typ_int, typ_int, typ_int)
  | Less
  | Greater
  | LessEq
  | GreaterEq -> (typ_int, typ_int, typ_bool)
  | And
  | Or -> (typ_bool, typ_bool, typ_bool)
  | Eq -> (TyVar(gen_typ_sym "prim2"), TyVar(gen_typ_sym "prim2"), typ_bool)


let infer (gamma : typ_env) exp : typ_constraint list =
  let constraints = ref [] in
  let uni a b = constraints := (a, b)::!constraints in
  let rec type_infer (gamma : typ_env) exp : typ =
    match exp with
    | ELet(binds, body, _) ->
      let new_env = List.fold_left (fun acc bind_type -> match bind_type with
      | LetBind(name, scheme, exp, _) ->
        (name, generalize gamma (type_infer acc exp))::acc
      | TupDestr(ids, scheme, exp, _) ->
        let t_exp = type_infer acc exp in
        let arg_vars = List.map (fun (name, _) -> gen_typ_sym name) ids in
        let t_args = List.map (fun (t_name) -> TyVar(t_name)) arg_vars in
        uni (TyTup(t_args)) t_exp;
        List.map2 (fun (name, _) t_arg -> (name, generalize gamma t_arg)) ids t_args @
        acc
      ) gamma binds
      in
      type_infer new_env body
    | ELetRec(binds, body, _) ->
      let bind_vars = List.map (function
        | LetBind(name, scheme, exp, _) -> gen_typ_sym name
        | TupDestr(_, _, _, _) -> failwith "Should have failed during well-formedness checking."
      ) binds in
      let first_pass_env : typ_env = List.map2 (fun bind bind_name -> match bind with
        | LetBind(name, scheme, exp, _) ->
          (name, ([], TyVar(bind_name)))
        | TupDestr(_, _, _, _) -> failwith "Should have failed during well-formedness checking."
        ) binds bind_vars @ gamma in
      let rec process binds bind_vars env =
        match binds with
        | [] -> env
        | LetBind(name, scheme, exp, _)::tl ->
          let t_exp = type_infer first_pass_env exp in
          (match scheme with
           | Some(s) -> uni (instantiate s) t_exp
           | None -> ());
          uni (TyVar(List.hd bind_vars)) t_exp;
          process tl (List.tl bind_vars) ((name, generalize gamma t_exp)::env)
        | TupDestr(_, _, _, _)::_ -> failwith "Should have failed during well-formedness checking."
      in
      type_infer (process binds bind_vars first_pass_env) body
    | EPrim1(op, exp, _) ->
      let (t_arg, t_out) = prim1_types op in
      uni t_arg (type_infer gamma exp);
      t_out
    | EPrim2(op, left, right, _) ->
      let (t_arg1, t_arg2, t_out) = prim2_types op in
      uni t_arg1 (type_infer gamma left);
      uni t_arg2 (type_infer gamma right);
      t_out
    | EIf(cond, thn, els, _) ->
      let t_cond = type_infer gamma cond
      and t_thn = type_infer gamma thn
      and t_els = type_infer gamma els in
      uni t_cond typ_bool;
      uni t_thn t_els;
      t_thn
    | ETuple(exps, _) -> TyTup(List.map (type_infer gamma) exps)
    | EGetItem(tup, idx, _) -> raise (TypeError "Non-constant tuple access not supported with type-checker enabled")
    | ESetItem(tup, idx, value, _) -> raise (TypeError "Non-constant tuple access not supported with type-checker enabled")
    | EGetItemExact(tup, idx, _) -> failwith "NYI"
    | ESetItemExact(tup, idx, value, _) -> failwith "NYI"
    | ENumber(_, _) -> typ_int
    | EBool(_, _) -> typ_bool
    | EString(_, _) -> typ_str
    | EId(name, _) -> lookup_env gamma name
    | EApp(func, args, _) ->
      let t_func = type_infer gamma func
      and t_args = List.map (type_infer gamma) args
      and t_var = TyVar(gen_typ_sym "func_result") in
      uni t_func (TyArr(t_args, t_var));
      t_var
    | ELambda(args, body, _) ->
      let arg_vars = List.map (fun (arg, _) -> gen_typ_sym arg) args in
      let ret = type_infer ((List.map2 (fun (arg, _) tv -> (arg, ([], TyVar(tv)))) args arg_vars) @ gamma) body in
      TyArr((List.map (fun (a) -> TyVar a) arg_vars), ret)
    | ESeq(exps, _) ->
      begin
        match exps with
        | [] -> failwith "Impossible: WF should have failed"
        (* We iterate through the list to populate the constraints as a side-effect *)
        | hd::tl -> List.fold_left (fun acc cur -> type_infer gamma cur) (type_infer gamma hd) tl
      end
    | _ -> failwith "Impossible"
  in
  ignore (type_infer gamma exp);
  List.rev !constraints

let rec solver (cs : typ_constraint list) (unif_env : unification) =
  match cs with
  | [] -> unif_env
  | (t1, t2)::tl ->
    let unif_env = unify t1 t2 unif_env in
    solver (List.map (fun (t1, t2) -> ((subst_type unif_env t1), (subst_type unif_env t2))) tl) unif_env

let initial_types_env = [
  (let print_sym = gen_typ_sym "a" in
  ("print", ([], TyArr([TyVar(print_sym)], TyVar(print_sym)))));
  (let sym_a = gen_typ_sym "a" in
  let sym_b = gen_typ_sym "b" in
  ("equal", ([], TyArr([TyVar(sym_a); TyVar(sym_b)], typ_bool))));
  ("toString", ([], TyArr([TyVar(gen_typ_sym "a")], typ_str)));
  ("strcat", ([], TyArr([typ_str; typ_str], typ_str)));
  ("strlen", ([], TyArr([typ_str], typ_int)));
  ("strslice", ([], TyArr([typ_str; typ_int; typ_int], typ_str)));
]

let type_check (prog : 'a Types.program) : unit =
  let {statements; body} = prog in
  let types = infer initial_types_env body in
  let lst = List.fold_right (^) (List.map ((fun (x, y) -> "(" ^  (string_of_typ x) ^ ", " ^ (string_of_typ y) ^ "); ")) types) "" in
  Printf.eprintf "%s\n" lst;
  ignore @@ solver types []

(*
let rec type_check (gamma : typ_env) exp typ : bool =
  match exp with
  | ELet(binds, body, _) -> failwith "NYI"
  | ELetRec(binds, body, _) -> failwith "NYI"
  | EPrim1(op, exp, _) -> failwith "NYI"
  | EPrim2(op, left, right, _) -> failwith "NYI"
  | EIf(cond, thn, els, _) -> failwith "NYI"
  | ETuple(exps, _) -> failwith "NYI"
  | EGetItem(tup, idx, _) -> failwith "NYI"
  | ESetItem(tup, idx, value, _) -> failwith "NYI"
  | EGetItemExact(tup, idx, _) -> failwith "NYI"
  | ESetItemExact(tup, idx, value, _) -> failwith "NYI"
  | ENumber(_, _) -> failwith "NYI"
  | EBool(_, _) -> failwith "NYI"
  | EId(name, _) -> failwith "NYI"
  | EApp(func, args, _) -> failwith "NYI"
  | ELambda(args, body, _) -> failwith "NYI"
  | ESeq(exps, _) -> failwith "NYI"
  | EEllipsis(_) -> failwith "NYI"
and type_infer (gamma : typ_env) exp : typ =
  match exp with
  | ELet(binds, body, _) -> failwith "NYI"
  | ELetRec(binds, body, _) -> failwith "NYI"
  | EPrim1(op, exp, _) -> failwith "NYI"
  | EPrim2(op, left, right, _) -> failwith "NYI"
  | EIf(cond, thn, els, _) -> failwith "NYI"
  | ETuple(exps, _) -> failwith "NYI"
  | EGetItem(tup, idx, _) -> failwith "NYI"
  | ESetItem(tup, idx, value, _) -> failwith "NYI"
  | EGetItemExact(tup, idx, _) -> failwith "NYI"
  | ESetItemExact(tup, idx, value, _) -> failwith "NYI"
  | ENumber(_, _) -> TyCon "Int"
  | EBool(_, _) -> TyCon "Bool"
  | EId(name, _) -> lookup_env gamma name
  | EApp(func, args, _) -> failwith "NYI"
  | ELambda(args, body, _) ->
    let arg_vars = List.map (fun (arg, _) -> gen_typ_sym arg) args in
    type_infer ((List.map2 (fun (arg, _) tv -> (arg, ([], TyVar(tv)))) args arg_vars) @ gamma) body
  | ESeq(exps, _) -> failwith "NYI"
  | EEllipsis(_) -> failwith "NYI"
;;
**)
