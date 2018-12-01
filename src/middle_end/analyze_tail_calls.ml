open Anftree
open Anf_iterator
open Grain_typed

type analysis +=
  | TailCall of bool
  | TailRecursive of bool

let tail_callable_names = Ident_tbl.create 50

let push_tail_callable_name id =
  Ident_tbl.add tail_callable_names id true

let pop_tail_callable_name id =
  Ident_tbl.remove tail_callable_names id

let is_tail_callable id =
  Ident_tbl.mem tail_callable_names id

let push_tail_call analysis =
  analysis := TailCall(true)::!analysis

let push_tail_recursive analysis =
  analysis := TailRecursive(true)::!analysis

(* Indicate whether or not this expression contails a tail call that needs optimization *)
let rec analyze_comp_expression ({comp_desc = desc; comp_analyses = analyses}) =
  match desc with
  | CIf(_, t, f) ->
    let t_branch = analyze_anf_expression t in
    let f_branch = analyze_anf_expression f in
    t_branch || f_branch
  | CSwitch(_, branches) ->
    List.fold_left (fun has_tail_call (_, b) -> analyze_anf_expression b || has_tail_call) false branches
  | CWhile(_, body) ->
    (* While this loop itself is not in tail position, we still want to analyze the body. *)
    ignore @@ analyze_anf_expression body; false
  | CApp({imm_desc=ImmId(id)}, _) ->
    push_tail_call analyses;
    is_tail_callable id
  | CAppBuiltin _
  | CApp _ ->
    push_tail_call analyses;
    false
  | CAssign _
  | CTuple _
  | CAdt _
  | CSetTupleItem _
  | CGetTupleItem _
  | CGetAdtItem _
  | CGetAdtTag _
  | CLambda _
  | CString _
  | CPrim1 _
  | CPrim2 _
  | CImmExpr _ -> false

(* Mark functions as tail-recursive *)
and analyze_anf_expression ({anf_desc = desc; anf_analyses = analyses}) =
  match desc with
  | AELet(_, Nonrecursive, binds, body) ->
    (* None of these binds are in tail position *)
    List.iter (fun (_, exp) -> ignore @@ analyze_comp_expression exp) binds;
    analyze_anf_expression body
  | AELet(_, Recursive, binds, body) ->
    List.iter (fun (id, _) -> push_tail_callable_name id) binds;
    List.iter (fun (_, ({comp_desc; comp_analyses} as bind)) ->
      match comp_desc with
      | CLambda(args, body) -> if analyze_anf_expression body then push_tail_recursive comp_analyses
      | _ -> ignore @@ analyze_comp_expression bind
    ) binds;
    List.iter (fun (id, _) -> pop_tail_callable_name id) binds;
    analyze_anf_expression body
  | AESeq(hd, tl) ->
    (* Only the AEComp at the end of a sequence is in tail position *)
    ignore @@ analyze_comp_expression hd;
    analyze_anf_expression tl
  | AEComp(c) ->
    analyze_comp_expression c


let comp_is_tail_recursive {comp_analyses} = 
  let rec is_tail_recursive analyses =
    match analyses with
    | TailRecursive(b)::_ -> Some(b)
    | _::tl -> is_tail_recursive tl
    | [] -> None in
  is_tail_recursive !comp_analyses

let comp_is_tail_call {comp_analyses} = 
  let rec is_tail_call analyses =
    match analyses with
    | TailCall(b)::_ -> Some(b)
    | _::tl -> is_tail_call tl
    | [] -> None in
  is_tail_call !comp_analyses

let mark_not_tail_recursive ({comp_analyses}) =
  let rec process_analyses = function
    | TailRecursive(true)::tl -> TailRecursive(false)::tl
    | hd::tl -> hd :: process_analyses tl
    | [] -> [] in
  comp_analyses := process_analyses !comp_analyses

let analyze {body} =
  ignore @@ analyze_anf_expression body
