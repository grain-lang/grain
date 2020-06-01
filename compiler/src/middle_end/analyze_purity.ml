open Anftree
open Anf_iterator
open Grain_typed

(*
   NOTE: Not every ANF node is guaranteed to have a purity analysis,
   but every node is guaranteed to either have one or be the child
   of a node which does.
*)

type analysis +=
  | Pure of bool
  | PurityTable of bool Ident.tbl

let purity_tbl : (bool Ident.tbl) ref = ref Ident.empty

let rec get_purity lst =
  match lst with
  | [] -> None
  | (Pure x)::_ -> Some(x)
  | _::tl -> get_purity tl

let rec get_purity_tbl lst =
  match lst with
  | [] -> raise Not_found
  | (PurityTable t)::_ -> t
  | _::tl -> get_purity_tbl tl

let set_id_purity id p =
  purity_tbl := Ident.add id p !purity_tbl

module StringHash = Hashtbl.Make(struct
  type t = string
  let hash x = Hashtbl.hash x
  let equal a b = (String.compare a b) == 0
end)

let pervasives_purity = StringHash.of_seq (List.to_seq [
  ("+", true);
  ("-", true);
  ("*", true);
  ("==", true);
  ("<", true);
  (">", true);
  ("<=", true);
  (">=", true);
  ("&&", true);
  ("||", true);

  ("void", true);
])

let get_id_purity (id : Ident.t) : bool =
  try
    StringHash.find pervasives_purity (Ident.name id)
  with Not_found ->
    Ident.find_same id !purity_tbl

let imm_expression_purity {imm_analyses} = get_purity !imm_analyses
let comp_expression_purity {comp_analyses} = get_purity !comp_analyses
let anf_expression_purity {anf_analyses} = get_purity !anf_analyses

let pure_identifiers {analyses} = get_purity_tbl !analyses

(* Quick accessors for known-existing values *)
let imm_expression_purity_internal i = Option.get (imm_expression_purity i)
let comp_expression_purity_internal c = Option.get (comp_expression_purity c)
let anf_expression_purity_internal a = Option.get (anf_expression_purity a)

let push_purity lref p =
  lref := (Pure p)::!lref

let analyze_imm_expression ({imm_desc; imm_analyses}) =
  match imm_desc with
  | ImmId(id) -> push_purity imm_analyses (get_id_purity id)
  | ImmConst(_) -> push_purity imm_analyses true

let rec analyze_comp_expression ({comp_desc = desc; comp_analyses = analyses}) =
  let purity = match desc with
    | CImmExpr(i) ->
      analyze_imm_expression i;
      imm_expression_purity_internal i
    | CPrim1(Box, _)
    | CPrim1(Unbox, _) ->
      false
    | CPrim1(_, a) ->
      analyze_imm_expression a;
      imm_expression_purity_internal a
    | CPrim2(_, a1, a2) ->
      analyze_imm_expression a1;
      analyze_imm_expression a2;
      imm_expression_purity_internal a1 &&
      imm_expression_purity_internal a2
    | CAssign(_, _) -> (* TODO: Would be nice if we could "scope" the purity analysis to local assignments *)
      false
    | CArrayGet(a, i) ->
      analyze_imm_expression a;
      analyze_imm_expression i;
      imm_expression_purity_internal a &&
      imm_expression_purity_internal i
    | CArraySet _ ->
      false
    | CTuple(args)
    | CArray(args)
    | CAdt(_, _, args) ->
      let arg_purities = List.map (fun arg -> analyze_imm_expression arg; imm_expression_purity_internal arg) args in
      List.for_all (fun x -> x) arg_purities
    | CRecord (_, args) ->
      let arg_purities = List.map (fun (_, arg) -> analyze_imm_expression arg; imm_expression_purity_internal arg) args in
      List.for_all (fun x -> x) arg_purities
    | CGetTupleItem(_, a) ->
      analyze_imm_expression a;
      imm_expression_purity_internal a
    | CSetTupleItem _ ->
      false
    | CGetAdtItem(_, a) ->
      analyze_imm_expression a;
      imm_expression_purity_internal a
    | CGetAdtTag _ ->
      true
    | CGetRecordItem(_, r) ->
      analyze_imm_expression r;
      imm_expression_purity_internal r
    | CIf(c, t, f) ->
      analyze_imm_expression c;
      analyze_anf_expression t;
      analyze_anf_expression f;
      (imm_expression_purity_internal c) && (anf_expression_purity_internal t) && (anf_expression_purity_internal f)
    | CWhile(c, body) ->
      analyze_anf_expression c;
      analyze_anf_expression body;
      (anf_expression_purity_internal c) && (anf_expression_purity_internal body)
    | CSwitch(exp, branches) ->
      analyze_imm_expression exp;
      let branches_purities = List.map (fun (t, b) -> analyze_anf_expression b; anf_expression_purity_internal b) branches in
      (imm_expression_purity_internal exp) && (List.for_all (fun x -> x) branches_purities)
    | CApp(f, args) ->
      let arg_purities = List.map (fun arg -> analyze_imm_expression arg; imm_expression_purity_internal arg) args in
      analyze_imm_expression f;
      imm_expression_purity_internal f && (List.for_all (fun x -> x) arg_purities)
    | CAppBuiltin(_module, f, args) ->
      List.iter (fun arg -> analyze_imm_expression arg) args;
      false
    | CLambda(args, body) ->
      List.iter (fun i -> set_id_purity i true) args;
      analyze_anf_expression body;
      anf_expression_purity_internal body
    | CInt32 _
    | CInt64 _
    | CString _ -> true
  in
  push_purity analyses purity

and analyze_anf_expression ({anf_desc = desc; anf_analyses = analyses}) =
  match desc with
  | AELet(g, Nonrecursive, binds, body) ->
    let process_bind (id, bind) =
      analyze_comp_expression bind;
      let purity = comp_expression_purity_internal bind in
      set_id_purity id purity;
      purity
    in
    let bind_purity = List.for_all (fun x -> x) (List.map process_bind binds) in
    analyze_anf_expression body;
    let purity = (anf_expression_purity_internal body) && bind_purity in
    push_purity analyses purity
  | AELet(g, Recursive, binds, body) ->
    (* Initialize purity to true, just so they're in scope *)
    List.iter (fun (id, _) -> set_id_purity id true) binds;
    (* Do the actual purity analysis *)
    let process_bind (id, bind) =
      analyze_comp_expression bind;
      let purity = comp_expression_purity_internal bind in
      set_id_purity id purity;
      purity
    in
    let bind_purity = List.for_all (fun x -> x) (List.map process_bind binds) in
    analyze_anf_expression body;
    let purity = (anf_expression_purity_internal body) && bind_purity in
    push_purity analyses purity
  | AESeq(hd, tl) ->
    analyze_comp_expression hd;
    analyze_anf_expression tl;
    let purity = (comp_expression_purity_internal hd) && (anf_expression_purity_internal tl) in
    push_purity analyses purity
  | AEComp(c) ->
    analyze_comp_expression c;
    let purity = comp_expression_purity_internal c in
    push_purity analyses purity


let analyze {imports; body; analyses} =
  purity_tbl := Ident.empty;
  let process_import {imp_use_id} =
    (* TODO: pure imports *)
    set_id_purity imp_use_id false
  in
  List.iter process_import imports;
  analyze_anf_expression body;
  analyses := (PurityTable !purity_tbl)::!analyses
