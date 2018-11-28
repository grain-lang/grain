open Anftree
open Grain_typed
open Types
open Analyze_tail_calls

type analysis +=
  | TailCallOptimized of bool

type transform_rule = {
  f: Ident.t; 
  f_args: Ident.t list; 
  dont_break_id: Ident.t; 
  next_id: Ident.t
}

module IdentHash =
  struct
    type t = Ident.t
    let equal i j = Ident.same i j
    let hash i = Hashtbl.hash i
  end

module IdentHashtbl = Hashtbl.Make(IdentHash)

let rewrite_rules = IdentHashtbl.create 50
let transform_rules = IdentHashtbl.create 50

let push_rewrite_rule a b =
  IdentHashtbl.add rewrite_rules a b

let pop_rewrite_rule a =
  IdentHashtbl.remove rewrite_rules a

let get_rewrite_rule id =
  try
    IdentHashtbl.find rewrite_rules id
  with
  | Not_found -> id

let transform_rule_stack = ref []

let push_transform_rule a b =
  transform_rule_stack := a :: !transform_rule_stack;
  IdentHashtbl.add transform_rules a b

let pop_transform_rule () =
  let current_rule = List.hd !transform_rule_stack in
  let rest_rules = List.tl !transform_rule_stack in
  transform_rule_stack := rest_rules;
  IdentHashtbl.remove transform_rules current_rule

let get_transform_rule id =
  IdentHashtbl.find transform_rules id

let has_transform_rule id =
  IdentHashtbl.mem transform_rules id

let comp_is_tail_call c =
  Option.default false @@ Analyze_tail_calls.comp_is_tail_call c

let comp_is_tail_recursive c =
  Option.default false @@ Analyze_tail_calls.comp_is_tail_recursive c

let comp_is_tail_call_optimized {comp_analyses} =
  let rec find_tail_call_optimized analyses =
    match analyses with
    | TailCallOptimized(b)::_ -> b
    | _::tl -> find_tail_call_optimized tl
    | [] -> false in
  find_tail_call_optimized !comp_analyses

let has_tail_recursive_binding binds =
  List.exists (fun (_, exp) -> Option.default false @@ Analyze_tail_calls.comp_is_tail_recursive exp) binds

let wrap_imm wrapper imm = 
  {
    imm_analyses=ref []; 
    imm_desc=imm;
    imm_loc=wrapper.comp_loc;
    imm_env=wrapper.comp_env
  }

let wrap_id wrapper imm_id = 
  wrap_imm wrapper @@ ImmId(imm_id)

let wrap_comp wrapper comp = 
  {wrapper with comp_analyses=ref []; comp_desc=comp}

let wrap_comp_with_anf wrapper comp = 
  {
    comp_analyses=ref []; 
    comp_desc=comp;
    comp_loc=wrapper.anf_loc;
    comp_env=wrapper.anf_env
  }

let wrap_anf_with_comp wrapper anf = 
  {
    anf_analyses=ref []; 
    anf_desc=anf;
    anf_loc=wrapper.comp_loc;
    anf_env=wrapper.comp_env
  }

let wrap_anf wrapper anf = 
  {wrapper with anf_analyses=ref []; anf_desc=anf}

let default_ref {anf_loc; anf_env} = 
  CPrim1(Box, {
    imm_desc=ImmConst(Const_int(0));
    imm_loc=anf_loc;
    imm_env=anf_env;
    imm_analyses=ref([]);
  })

let unbox_of id {comp_loc; comp_env} =
  CPrim1(Unbox, {
    imm_desc=ImmId(id);
    imm_loc=comp_loc;
    imm_env=comp_env;
    imm_analyses=ref([]);
  })

let mark_tail_call_optimized ({comp_analyses}) =
  comp_analyses := TailCallOptimized(true) :: !comp_analyses

module TailCallsArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  let enter_anf_expression ({anf_desc = desc} as a) =
    match desc with
    | AELet(global, Recursive, binds, body) when has_tail_recursive_binding binds ->
      let dont_break_id = Ident.create "break#tc" in
      let next_id = Ident.create "next#tc" in
      let iterator_binds = ref [
        (dont_break_id, wrap_comp_with_anf a @@ default_ref a);
        (next_id, wrap_comp_with_anf a @@ default_ref a)
      ] in
      let transform_rules = ref [] in
      let push_local_transform_rule id rule =
        transform_rules := (id, rule) :: !transform_rules in
      let new_binds = List.flatten @@ List.map (fun ((id : Ident.t), ({comp_desc} as bind)) -> 
        begin match comp_desc with
        | CLambda(args, body) when comp_is_tail_recursive bind ->
          let wrap_id = wrap_id bind
          and wrap_imm = wrap_imm bind
          and wrap_comp = wrap_comp bind
          and wrap_anf = wrap_anf body
          and default_ref = default_ref a in
          let arg_ref_ids = List.map (fun (arg : Ident.t) -> 
            Ident.create (arg.name ^ "#tc_ref")
          ) args in
          let arg_refs = List.map (fun id -> id, wrap_comp default_ref) arg_ref_ids in
          iterator_binds := arg_refs @ !iterator_binds;
          let arg_derefs = List.map (fun ((id : Ident.t), _) -> 
            Ident.create (id.name ^ "#deref"), wrap_comp @@ unbox_of id bind
          ) arg_refs in
          let iterative_lam_id = Ident.create (id.name ^ "#tc_iter") in
          let iterative_lam = wrap_comp @@ 
            CLambda([], wrap_anf @@ AELet(Nonglobal, Nonrecursive, arg_derefs, body)) in
          mark_tail_call_optimized iterative_lam; Analyze_tail_calls.mark_not_tail_recursive iterative_lam;
          let iterative_lam_pair = iterative_lam_id, iterative_lam in
          List.iter2 (fun arg (arg_deref, _) -> push_rewrite_rule arg arg_deref) args arg_derefs;
          push_local_transform_rule id {f=iterative_lam_id; f_args=arg_ref_ids; dont_break_id; next_id};
          let arg_refs_sets = List.map2 (fun (arg_ref, _) arg -> wrap_comp @@ CAssign(wrap_id arg_ref, wrap_id arg)) arg_refs args in
          let run_loop = wrap_anf @@ AESeq(
            wrap_comp @@ CAssign(wrap_id dont_break_id, wrap_imm @@ ImmConst(Const_bool(true))),
            let (next_func, _) = iterative_lam_pair in
            wrap_anf @@ AESeq(
              wrap_comp @@ CAssign(wrap_id next_id, wrap_id next_func),
              let return_val_id = Ident.create "return#tc" in
              wrap_anf @@ AELet(Nonglobal, Nonrecursive, [(return_val_id, wrap_comp default_ref)], wrap_anf @@ AESeq(
                wrap_comp @@ CWhile(
                  wrap_anf @@ AEComp(wrap_comp @@ unbox_of dont_break_id bind),
                  wrap_anf @@ AESeq(
                    wrap_comp @@ CAssign(wrap_id dont_break_id, wrap_imm @@ ImmConst(Const_bool(false))),
                    let unboxed_next_id = Ident.create "next#tc_unboxed" in
                    let next_result_id = Ident.create "next#tc_result" in
                    wrap_anf @@ AELet(Nonglobal, Nonrecursive, [(unboxed_next_id, wrap_comp @@ unbox_of next_id bind)],
                      wrap_anf @@ AELet(Nonglobal, Nonrecursive, [(next_result_id, wrap_comp @@ CApp(wrap_id unboxed_next_id, []))],
                        wrap_anf @@ AEComp(wrap_comp @@ CAssign(wrap_id return_val_id, wrap_id next_result_id))
                      )
                    )
                  )
                ),
                wrap_anf @@ AEComp(wrap_comp @@ unbox_of return_val_id bind)
              ))
            )
          ) in
          let lam_runner_body = List.fold_right (fun arg_ref_set anf_comp -> wrap_anf @@ AESeq(arg_ref_set, anf_comp)) arg_refs_sets run_loop in
          let lam_runner = wrap_comp @@ CLambda(args, lam_runner_body) in
          Analyze_tail_calls.mark_not_tail_recursive lam_runner;
          let lam_runner_pair = id, lam_runner in
          [iterative_lam_pair; lam_runner_pair]
        | _ -> [(id, bind)] end
      ) binds in
      List.iter (fun (id, rule) -> push_transform_rule id rule) !transform_rules;
      {a with anf_analyses=ref []; anf_desc=AELet(Nonglobal, Nonrecursive, !iterator_binds, 
        {a with anf_analyses=ref []; anf_desc=AELet(global, Recursive, new_binds, body)})};
    | _ -> a

  let enter_comp_expression ({comp_desc = desc} as c) =
    match desc with
    | CApp({imm_desc=ImmId(f)}, args) when has_transform_rule f && comp_is_tail_call c ->
      let {f=new_f; f_args=new_f_args; dont_break_id; next_id} = get_transform_rule f in
      let _true = wrap_imm c @@ ImmConst(Const_bool(true)) in
      let _false = wrap_imm c @@ ImmConst(Const_bool(false)) in
      let comp_false = {c with comp_desc=CImmExpr(_false)} in
      let set_break_and_next = wrap_anf_with_comp c @@ AESeq(
        wrap_comp c @@ CAssign(wrap_id c dont_break_id, _true),
        wrap_anf_with_comp c @@ AEComp(wrap_comp c @@ CAssign(wrap_id c next_id, wrap_id c new_f))
      ) in
      let replaced_tail_call = List.fold_right2 (fun new_f_arg arg seq ->
        wrap_anf_with_comp c @@ AESeq(
          wrap_comp c @@ CAssign(wrap_id c new_f_arg, arg),
          seq
        )
      ) new_f_args args set_break_and_next in
      {c with comp_desc=CIf(
        _false,
        wrap_anf_with_comp c @@ AEComp(comp_false),
        replaced_tail_call
      )}
    | CLambda(args, _) -> List.iter (fun arg -> pop_rewrite_rule arg) args; c
    | _ -> c

  let leave_comp_expression ({comp_desc = desc} as c) =
    begin match desc with
    | CLambda _ when comp_is_tail_call_optimized c -> pop_transform_rule ()
    | _ -> ()
    end; c

  let leave_imm_expression ({imm_desc = desc} as i) =
    match desc with 
    | ImmId(id) -> {i with imm_desc=ImmId(get_rewrite_rule id)}
    | _ -> i

end

module TailCallsMapper = Anf_mapper.MakeMap(TailCallsArg)

let optimize anfprog =
  (* Reset state *)
  IdentHashtbl.clear rewrite_rules;
  IdentHashtbl.clear transform_rules;
  transform_rule_stack := [];
  TailCallsMapper.map_anf_program anfprog
