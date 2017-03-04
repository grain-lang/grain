open Printf
open BatUref
open Types
open Pretty

let rec free_typ_tyvars typ =
  match typ with
  | TyCon _ -> []
  | TyVar s -> [s]
  | TyArr(args, ret) -> List.concat (List.map free_typ_tyvars (args @ [ret]))
  | TyTup(args) -> List.concat (List.map free_typ_tyvars args)
and free_scheme_tyvars (args, typ) =
  List.fold_left ExtList.List.remove (List.sort_uniq String.compare (free_typ_tyvars typ)) args
;;

(* A unification is a dictionary mapping type variable names to
   unifiables (from the BatUref library) containing types.  
 *)
type unification = (typ uref) envt;;


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
let unify (t1 : typ) (t2 : typ) (unif_env : unification) : unification =
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
  | _ -> failwith "NYI"

  
let rec type_check gamma exp typ : bool =
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
and type_infer gamma exp : scheme =
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
;;
