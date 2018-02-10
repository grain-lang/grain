(** Pattern Matching compiler, based on "Compiling Pattern Matching to Good Decision Trees"
    by Luc Maranget (http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf) *)

(* NOTE: Constant patterns are currently unsupported, since
   they require logic for guard expressions. *)

open Grain_parsing
open Grain_typed
open Types
open Typedtree

(** The type for compiled match pattern decision trees.
    These trees are evaluated with respect to a stack of values. *)
type decision_tree =
  | Leaf of int
  (** Leaves of the decision tree. The int corresponds
      to the action number (i.e. switch branch) to be taken *)
  | Fail
  (** Represents a failed match. *)
  | Switch of (int * decision_tree) list
  (** Multi-way test for an occurence. The list is a
      non-empty list of pairs of
      (constructor_tag, nested_decision_tree) tuples. *)
  | Swap of int * decision_tree
  (** This is a control structure which swaps the item in the
      n'th position with the top of the value stack (where 'n' is the
      number saved in the node) *)


type conversion_result = {
  tree : decision_tree;
  last_label : int;
  branches : (int * expression) list;
}

(* Mapper argument which normalizes patterns via a postorder traversal *)
module PatternNormalizerArgument : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let is_any p =
    match p.pat_desc with
    | TPatAny -> true
    | _ -> false

  let leave_pattern (p : pattern) =
    match p.pat_desc with
    | TPatConstant _ -> failwith "Internal error: constant patterns are currently unsupported (should not be produced by parser)"
    | TPatTuple(args) when List.for_all is_any args ->
      (* Replace (_, _, _) with _ *)
      (* This is safe, since we have already done type-checking. *)
      {p with pat_desc=TPatAny}
    | TPatOr({pat_desc=TPatAny}, _) ->
      (* Replace (_ | patts...) with _ *)
      {p with pat_desc=TPatAny}
    | TPatAlias({pat_desc=TPatAny}, a, b) ->
      (* Replace (_ as name) with name *)
      {p with pat_desc=TPatVar(a, b)}
    | TPatOr(({pat_desc=TPatVar _} as p1), _) ->
      (* Replace (name | patts...) with name *)
      p1
    | _ -> p
end

module PatternNormalizer = TypedtreeMap.MakeMap(PatternNormalizerArgument)

let normalize_branch ({mb_pat; _} as mb) =
  let mb_pat = PatternNormalizer.map_pattern mb_pat in
  {mb with mb_pat}

let rec pattern_always_matches patt =
  (* Precondition: Normalized *)
  match patt.pat_desc with
  | TPatAny
  | TPatVar _ -> true
  | TPatAlias(p, _, _) -> pattern_always_matches p
  | TPatTuple(args) when List.for_all pattern_always_matches args -> true
  | _ -> false


let rec pattern_head_constructors_aux p acc =
  match p.pat_desc with
  | TPatConstruct(_, cd, _) when not (List.mem cd acc) -> cd::acc
  | TPatOr(p1, p2) -> pattern_head_constructors_aux p1 (pattern_head_constructors_aux p2 acc)
  | _ -> acc

let pattern_head_constructors patt =
  pattern_head_constructors_aux patt []

let matrix_head_constructors mtx =
  let rec help mtx acc =
    match mtx with
    | [] -> acc
    | ([], _)::_ -> failwith "Impossible: Empty pattern matrix"
    | (p::_, mb)::tl -> pattern_head_constructors_aux p acc in
  help mtx []

let normalize_branches match_branches =
  let match_branches = List.map normalize_branch match_branches in
  let rec help branches =
    match branches with
    | [] -> []
    | hd :: _ when pattern_always_matches hd.mb_pat -> [hd]
    | hd :: tl -> hd :: (help tl) in
  help match_branches

let make_matrix branches =
  List.map (fun ({mb_pat} as mb) ->
      match mb_pat.pat_desc with
      | TPatTuple(patts) -> patts, mb
      | _ -> ([mb_pat], mb)) branches

let matrix_width = function
  | [] -> raise Not_found
  | (pats, _) :: _ -> List.length pats

let rec col_is_wildcard mtx col =
  match mtx with
  | [] -> true
  | (pats, _) :: _ when not (pattern_always_matches (List.nth pats col)) -> false
  | _ :: tl -> col_is_wildcard tl col

let rec rotate_matrix mtx col =
  match mtx with
  | [] -> []
  | (pats, mb) :: tl ->
    let tl = rotate_matrix tl col in
    let before, after = Misc.Stdlib.List.split_at col pats in
    (after @ before, mb) :: tl

let rec rotate_if_needed mtx =
  if col_is_wildcard mtx 0 then
    (0, mtx)
  else begin
    (* Safe, since size 0 matrix is wildcard *)
    let width = matrix_width mtx in
    let rec find_rotation n =
      if n >= width then
        raise Not_found
      else if col_is_wildcard mtx n then
        (n, rotate_matrix mtx n)
      else
        find_rotation (n + 1) in
    find_rotation 1
  end

(* See Paper *)
let rec specialize_matrix cd mtx =
  match mtx with
  | [] -> []
  | ([], _)::_ -> failwith "Impossible: Empty pattern matrix in specialize_matrix"
  | ((p::_ as ps), mb)::tl when pattern_always_matches p ->
    (ps, mb)::(specialize_matrix cd tl)
  | (p::ptl, mb)::tl when List.mem cd (pattern_head_constructors p) ->
    (({p with pat_desc=TPatAny})::ptl, mb)::(specialize_matrix cd tl)
  | _::tl -> specialize_matrix cd tl

(* See Paper *)
let rec default_matrix mtx =
  match mtx with
  | [] -> []
  | ([], _)::_ -> failwith "Impossible: Empty pattern matrix in specialize_matrix"
  | ((p::[]), _)::_ -> failwith "default_matrix called with one-column matrix"
  | ((p::ptl), mb)::tl when pattern_always_matches p ->
    (ptl, mb)::(default_matrix tl)
  | _::tl -> default_matrix tl

let convert_match_branches (match_branches : Typedtree.match_branch list) : conversion_result =
  let rec help i matrix =
    match matrix with
    | [] -> {tree=Fail; last_label=i; branches=[]} (* No branches to match. *)
    | (pats, mb) :: _ when List.for_all pattern_always_matches pats ->
      {
        tree=Leaf i;
        last_label=i;
        branches=[i, mb.mb_body];
      }
    | _ ->
      let n, rotated = try rotate_if_needed matrix with
        | Not_found ->
          failwith "Internal error: convert_match_branches: no non-wildcard column found but not all patterns always match (should be impossible)" in
      ignore(rotated);
      failwith "NYI: convert_match_branches"

  in
  help 0 (make_matrix @@ normalize_branches match_branches)
