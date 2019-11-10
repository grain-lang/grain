(** Pattern Matching compiler, based on "Compiling Pattern Matching to Good Decision Trees"
    by Luc Maranget (http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf) *)

(* NOTE: Constant patterns are currently unsupported, since
   they require logic for guard expressions. *)

open Sexplib.Conv
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
  (* The following instruction(s) are extensions from the paper's ADT: *)
  | Explode of int * decision_tree
  (** This instruction indicates that the first argument should be
      interpreted as a tuple and expanded into its contents.
      Argument: (arity) *)
[@@deriving sexp]


type conversion_result = {
  tree : decision_tree;
  last_label : int;
  branches : (int * expression * pattern) list;
}

(** Utilities *)

(** Swaps the first and 'idx'-th elements of the given list *)
let swap_list idx lst =
  match lst with
  | [] -> failwith "Impossible (swap_list): Cannot swap empty list"
  | hd::tl ->
    let split_hd, split_tl = BatList.split_at (idx - 2) tl in
    match split_tl with
    | [] -> failwith "Impossible (swap_list): Invalid swap index"
    | new_hd::rem -> new_hd::(split_hd @ (hd::rem))

(** Decision tree to ANF compilation (compiled trees evaluate to the
    corresponding label to be used in the computed goto) *)

type compiled_tree = Anftree.anf_expression

(* Forward declaration to be populated by anf.ml *)
let compile_constructor_tag : (constructor_tag -> int) ref =
  ref (fun _ -> failwith "matchcomp: compile_constructor_tag: should be set by linearize.ml!")

module MatchTreeCompiler = struct
  open Anftree
  open Anf_helper

  let pattern_could_contain_binding patt =
    match patt.pat_desc with
    | TPatConstant _
    | TPatAny -> false
    | _ -> true

  let no_op x = x
  let compose_binding id cexp func x = AExp.let_ Nonrecursive [(id, cexp)] (func x)
  let with_binding id cexp = compose_binding id cexp no_op

  let extract_bindings (patt : pattern) (expr : Anftree.comp_expression) : (Ident.t * Anftree.comp_expression) list =
    (*Printf.eprintf "Extracting bindings from:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_pattern patt));*)
    let rec extract_bindings patt expr =
      let {pat_loc=loc; pat_env=env} = patt in
      match patt.pat_desc with
      | TPatConstant _
      | TPatAny -> []
      | TPatVar(id, _) ->
        [(id, expr)]
      | TPatAlias(p, id, _) ->
        let bindings_p = extract_bindings p expr in
        (id, expr)::bindings_p
      | TPatTuple(args) ->
        let tup_name = Ident.create "match_bind_tup" in
        let tup_id = Imm.id ~loc ~env tup_name in
        let process_nested other_binds idx nested_pat =
          let this_binds = if pattern_could_contain_binding nested_pat then
              begin
                let tup_arg_name = Ident.create "match_bind_tup_arg" in
                let tup_arg_imm = Imm.id ~loc ~env tup_arg_name in
                let arg_binds = extract_bindings nested_pat (Comp.imm ~loc ~env tup_arg_imm) in
                match arg_binds with
                | [] -> []
                | _ -> (tup_arg_name, Comp.tuple_get ~loc ~env (Int32.of_int idx) tup_id)::arg_binds
              end
            else [] in
          this_binds @ other_binds in
        let binds = BatList.fold_lefti process_nested [] args in
        begin match binds with
          | [] -> []
          | _ -> (tup_name, expr)::binds
        end
      | TPatRecord(fields) ->
        let rec_name = Ident.create "match_bind_rec" in
        let rec_id = Imm.id ~loc ~env rec_name in
        let process_nested other_binds (lid, ld, nested_pat) =
          let this_binds = if pattern_could_contain_binding nested_pat then
              begin
                let rec_field_name = Ident.create @@ "match_bind_rec_field_" ^ Identifier.last lid.txt in
                let rec_field_imm = Imm.id ~loc ~env rec_field_name in
                let field_binds = extract_bindings nested_pat (Comp.imm ~loc ~env rec_field_imm) in
                match field_binds with
                | [] -> []
                | _ -> (rec_field_name, Comp.record_get ~loc ~env (Int32.of_int ld.lbl_pos) rec_id)::field_binds
              end
            else [] in
          this_binds @ other_binds in
        let binds = BatList.fold_left process_nested [] fields in
        begin match binds with
          | [] -> []
          | _ -> (rec_name, expr)::binds
        end
      | TPatConstruct(_, _, args) ->
        let data_name = Ident.create "match_bind_data" in
        let data_id = Imm.id ~loc ~env data_name in
        let process_nested other_binds idx nested_pat =
          let this_binds =
            if pattern_could_contain_binding nested_pat then
              begin
                let cstr_arg_name = Ident.create "match_bind_cstr_arg" in
                let cstr_arg_imm = Imm.id ~loc ~env cstr_arg_name in
                let arg_binds = extract_bindings nested_pat (Comp.imm ~loc ~env cstr_arg_imm) in
                match arg_binds with
                | [] -> []
                | _ -> (cstr_arg_name, Comp.adt_get ~loc ~env (Int32.of_int (idx)) data_id)::arg_binds
              end
            else [] in
          this_binds @ other_binds in
        let binds = BatList.fold_lefti process_nested [] args in
        begin match binds with
          | [] -> []
          | _ -> (data_name, expr)::binds
        end
      | TPatOr _ -> failwith "NYI: extract_bindings > TPatOr" in
    let res = extract_bindings patt expr in
    (*Printf.eprintf "Bindings:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list (sexp_of_pair sexp_of_string (fun x -> (sexp_of_string (Pretty.string_of_cexpr x)))) res));*)
    res

  let fold_tree setup ans =
    List.fold_right (fun (name, exp) body -> AExp.let_ Nonrecursive [(name, exp)] body) setup (AExp.comp ans)

  let rec compile_tree_help tree values =

    let (cur_value, rest_values) = match values with
      | [] -> failwith "Impossible (compile_tree_help): Empty value stack"
      | hd::tl -> hd, tl in
    match tree with
    | Leaf(i) -> Comp.imm (Imm.const (Const_int i)), []
    | Fail ->
      (* FIXME: We need a "throw error" node in ANF *)
      Comp.imm (Imm.const (Const_int 0)), []
    (* Optimizations to avoid unneeded destructuring: *)
    | Explode(_, ((Leaf(_)) as inner))
    | Explode(_, (Fail as inner)) -> compile_tree_help inner values
    | Explode(arity, rest) ->
      (* Tack on the new bindings. We assume that the indices of 'rest'
         already account for the new indices. *)
      let bindings = BatList.init arity (fun idx ->
          let id = Ident.create "match_explode" in
          (id, Comp.tuple_get (Int32.of_int idx) cur_value)) in
      let new_values = (List.map (fun (id, _) -> Imm.id id) bindings) @ rest_values in
      let rest_ans, rest_setup = compile_tree_help rest new_values in
      rest_ans, (bindings @ rest_setup)
    | Swap(idx, rest_tree) ->
      compile_tree_help rest_tree (swap_list idx values)
    | Switch(branches) ->
      (* Runs when no branches match *)
      let base = compile_tree_help Fail values in
      let value_constr_name = Ident.create "match_constructor" in
      let value_constr_id = Imm.id value_constr_name in
      let value_constr = Comp.adt_get_tag cur_value in
      (* Fold left should be safe here, since there should be at most one branch
         per constructor *)
      let switch_body_ans, switch_body_setup = List.fold_left (fun (body_ans, body_setup) (tag, tree) ->
          let cmp_id_name = Ident.create "match_cmp_constructors" in
          let cmp_id = Imm.id cmp_id_name in
          (* If the constructor has the correct tag, execute this branch.
             Otherwise continue. *)
          let setup = [(cmp_id_name, Comp.prim2 Eq value_constr_id (Imm.const (Const_int tag)))] in
          let tree_ans, tree_setup = compile_tree_help tree values in
          let ans = Comp.if_
              cmp_id
              (fold_tree tree_setup tree_ans)
              (fold_tree body_setup body_ans) in
          ans, setup)
          base branches in
      switch_body_ans, (value_constr_name, value_constr)::(switch_body_setup)

  let compile_result ({tree; branches}) helpA expr : Anftree.comp_expression * (Ident.t * Anftree.comp_expression) list =
    (*prerr_string "Compiling tree:";
      prerr_string (Sexplib.Sexp.to_string_hum (sexp_of_decision_tree tree));
      prerr_newline();*)
    let ans, setup = compile_tree_help tree [expr] in
    let jmp_name = Ident.create "match_dest" in
    let setup = setup @ [(jmp_name, ans)] in
    let switch_branches = (List.map (fun (tag, branch, orig_pat) ->
        tag, List.fold_right
          (fun (name, exp) body -> AExp.let_ Nonrecursive [(name, exp)] body)
          (extract_bindings orig_pat (Comp.imm expr))
          (helpA branch))
        branches) in
    Comp.switch (Imm.id jmp_name) switch_branches, setup
end

let normalize_branch ({mb_pat; _} as mb) =
  let mb_pat = Parmatch.normalize_pat mb_pat in
  {mb with mb_pat}

let rec pattern_always_matches patt =
  (* Precondition: Normalized *)
  match patt.pat_desc with
  | TPatAny
  | TPatVar _ -> true
  | TPatAlias(p, _, _) -> pattern_always_matches p
  | TPatTuple(args) when List.for_all pattern_always_matches args -> true
  | TPatRecord(fields) when List.for_all (fun (_, _, p) -> pattern_always_matches p) fields -> true
  | _ -> false

let flatten_pattern size ({pat_desc} as p) =
  match pat_desc with
  | TPatTuple(args) -> args
  | TPatAny -> Parmatch.omegas size
  | _ -> [p]


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
    | (p::_, mb)::tl -> help tl (pattern_head_constructors_aux p acc) in
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
    let rotated_row = swap_list col pats in
    (rotated_row, mb) :: tl

let rec rotate_if_needed mtx =
  if not(col_is_wildcard mtx 0) then
    (0, mtx)
  else
    match mtx with
    | [] -> (0, mtx)
    | _ ->
      begin
        (* Safe, since size 0 matrix is wildcard *)
        let width = matrix_width mtx in
        let rec find_rotation n =
          if n >= width then
            raise Not_found
          else if not(col_is_wildcard mtx n) then
            (n, rotate_matrix mtx n)
          else
            find_rotation (n + 1) in
        find_rotation 1
      end

(* [See paper for details]

   Row: ([p_1; p_2; ...], a_j)

   Pattern p_1 (for row j)             Row(s) of S(c, P -> A)
   -------------------------           ----------------------
        c(q_1,...,q_n)           [([q_1; ...; q_n; p_2; ...], a_j)]
   c'(q_1,...,q_n) (c' <> c)                  No row
              _                    [([_; ...; _; p_2; ...], a_j)] [n wildcards]
         (q_1 | q_2)               [(S(c, ([q_1; p_2; ...], a_j)));
                                    (S(c, ([q_2; p_2; ...], a_j)))]
*)
let rec specialize_matrix cd mtx =
  let arity = cd.cstr_arity in
  let rec specialized_rows row =
    match row with
    | [] -> failwith "Impossible: Empty pattern row in specialize_matrix"
    | ({pat_desc} as p)::ptl ->
      match pat_desc with
      | _ when pattern_always_matches p ->
        let wildcards = BatList.init arity (fun _ -> {p with pat_desc=TPatAny}) in
        [wildcards @ ptl]
      | TPatConstruct(_, pcd, _) when cd <> pcd -> []
      | TPatConstruct(_, pcd, args) -> [args @ ptl]
      | TPatOr(p1, p2) -> (specialized_rows (p1::ptl)) @ (specialized_rows (p2::ptl))
      | _ -> [] (* TODO: Should this be possible? *) in

  List.fold_right (fun (row, mb) rem ->
      (List.map (fun patts -> (patts, mb)) (specialized_rows row)) @ rem) mtx []

(* [See paper for details]

   Row: ([p_1; p_2; ...], a_j)

   Pattern p_1 (for row j)       Row(s) of D(P)
   -----------------------       --------------
        c(q_1,...,q_n)               No row
              _                [([p_2; ...], a_j)]
         (q_1 | q_2)           [D(([q_1; p_2; ...], a_j));
                                D(([q_2; p_2; ...], a_j))]
*)
let rec default_matrix mtx =
  let rec default_rows row =
    match row with
    | [] -> failwith "Impossible: Empty pattern row in default_matrix"
    | _::[] -> failwith "default_matrix called with a one-column matrix"
    | ({pat_desc} as p)::ptl ->
      match pat_desc with
      | _ when pattern_always_matches p -> [ptl]
      | TPatConstruct _ -> []
      | TPatOr(p1, p2) -> (default_rows (p1::ptl)) @ (default_rows (p2::ptl))
      | _ -> [] (* TODO: Should this be possible? *) in

  List.fold_right (fun (row, mb) rem ->
      (List.map (fun patts -> (patts, mb)) (default_rows row)) @ rem) mtx []


let rec compile_matrix last_label branches mtx =
  match mtx with
  | [] -> {tree=Fail; last_label=last_label; branches=branches} (* No branches to match. *)
  | (pats, mb) :: _ when List.for_all pattern_always_matches pats ->
    {
      tree=Leaf (last_label + 1);
      last_label=last_label + 1;
      branches=(last_label + 1, mb.mb_body, mb.mb_pat)::branches;
    }
  | _ when not (col_is_wildcard mtx 0) ->
    (* If the first column contains a non-wildcard pattern *)
    let constructors = matrix_head_constructors mtx in
    (*Printf.eprintf "constructors:\n%s\n" (Sexplib.Sexp.to_string_hum ((Sexplib.Conv.sexp_of_list sexp_of_constructor_description) constructors));*)
    let handle_constructor ({last_label; branches}, switch_branches) cstr =
      let arity = cstr.cstr_arity in
      let specialized = specialize_matrix cstr mtx in
      let ({tree} as result) = compile_matrix last_label branches specialized in
      let final_tree = Explode(arity + 1, tree) in
      {result with tree=final_tree}, ((!compile_constructor_tag cstr.cstr_tag, final_tree)::switch_branches)
    in
    begin match constructors with
      | [] -> failwith "Internal error: compile_matrix: non-wildcard column returned no constructors"
      | _ ->
        let (result, switch_branches) =
          List.fold_left
            handle_constructor
            ({last_label; branches; tree=Fail}, [])
            constructors in
        {
          result with
          tree=Switch(switch_branches);
        }
    end
  | _ ->
    (* Adjust stack so non-wildcard column is on top *)
    let i, rotated  = try rotate_if_needed mtx with
      | Not_found ->
        failwith "Internal error: convert_match_branches: no non-wildcard column found but not all patterns always match (should be impossible)" in
    let ({tree; _} as result) = compile_matrix last_label branches rotated in
    {result with tree=Swap(i, tree)}
    

let convert_match_branches (match_branches : Typedtree.match_branch list) : conversion_result =
  let mtx = List.map2 (fun (ps, _) mb -> (ps, mb))
      (make_matrix @@ normalize_branches match_branches)
      (match_branches) in
  (*prerr_string "Initial matrix:\n";
  prerr_string (Sexplib.Sexp.to_string_hum (Sexplib.Conv.sexp_of_list
                                              (Sexplib.Conv.sexp_of_pair
                                                 (Sexplib.Conv.sexp_of_list sexp_of_pattern)
                                                 sexp_of_match_branch) mtx));
  prerr_newline();*)
  compile_matrix 0 [] mtx
