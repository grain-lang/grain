(**
This module transforms tail-recursive directly and mutually recursive functions into loops.

Until WebAssembly supports tail calls, this module aims to transform this program:
```
let rec isEven = (x) => {
  if (x <= 1) {
    x == 0
  } else {
    isOdd(x - 1)
  }
}
and isOdd = (y) => {
  if (y <= 1) {
    y == 1
  } else {
    isEven(y - 1)
  }
}
```

Into this program:
```
let continue = box 0,
next = box 0,
_x = box 0,
_y = box 0;

let rec isEven = (x) => {
  _x := x;
  continue := true;
  next := _isEven;
  let returnValue = box 0;
  while (unbox continue) {
    continue := false;
    returnValue := (unbox next)()
  };
  unbox returnValue
}
and isOdd = (y) => {
  _y := y;
  continue := true;
  next := _isOdd;
  let returnValue = box 0;
  while (unbox continue) {
    continue := false;
    returnValue := (unbox next)()
  };
  unbox returnValue
}
and _isEven = () => {
  let x = unbox _x;
  if (x <= 1) {
    x == 0
  } else {
    _y := x - 1;
    continue := true;
    next := _isOdd
  }
}
and _isOdd = () => {
  let y = unbox _y;
  if (y <= 1) {
    y == 0
  } else {
    _x := y - 1;
    continue := true;
    next := _isEven
  }
}
```

Things to note:
  * Two references, `continue` and `next` are available to the entire set of recursive funtions.
  * References are created for all arguments to the functions. These are available to the entire set of recursive
    functions.
  * Each function is transformed into two functions-- an invocation function and an iteree function. The invocation 
    function is called when the function is called from an outside source, and controls the loop that calls the iteree.
    The iteree function is a thunk that first binds the would-be arguments from the references to names, then proceeds
    with the original function body with minor alterations, the most important being a replacement of tail calls with a
    setup to continue the iteration.

*)

open Anftree
open Grain_typed
open Types
open Analyze_tail_calls

type analysis +=
  | TailCallOptimized of bool

type transform_rule = {
  f_id: Ident.t; 
  f_args: Ident.t list; 
  continue_loop_id: Ident.t; 
  next_f_id: Ident.t
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

let mark_tail_call_optimized ({comp_analyses}) =
  comp_analyses := TailCallOptimized(true) :: !comp_analyses

module TailCallsArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

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

  type loop_bindings = {
    continue_id: Ident.t;
    next_id: Ident.t;
    first_f_id: Ident.t;
    arg_ref_ids: Ident.t list;
  }
  
  (* Creates the function that will run the loop for the (transformed) recursive function. *)
  let create_lambda_runner lambda {continue_id; next_id; first_f_id; arg_ref_ids} =
    let assign_id id value = wrap_comp lambda @@ CAssign(wrap_id lambda id, wrap_id lambda value) in
    let assign_imm id value = wrap_comp lambda @@ CAssign(wrap_id lambda id, wrap_imm lambda value) in
    let bind id value body = wrap_anf_with_comp lambda @@ AELet(Nonglobal, Nonrecursive, [(id, value)], body) in
    let box value = wrap_comp lambda @@ CPrim1(Box, wrap_imm lambda value) in
    let unbox id = wrap_comp lambda @@ unbox_of id lambda in
    let sequence comp_exprs anf_expr = 
      List.fold_right (fun comp seq -> wrap_anf_with_comp lambda @@ AESeq(comp, seq)) comp_exprs anf_expr in
    let to_anf value = wrap_anf_with_comp lambda @@ AEComp(value) in
  
    let lambda_args = match lambda.comp_desc with
    | CLambda(args, _) -> args
    | _ -> failwith "comp_expr was not a lambda" in

    (* We initialize the boxes representing the function arguments with the given function arguments. *)
    let arg_ref_assignments = List.map2 (fun arg_ref arg -> assign_id arg_ref arg) arg_ref_ids lambda_args in

    (* The value of the next function to call *)
    let unboxed_next_id = Ident.create "next#tc_unboxed" in
    (* The value of the result of calling that function *)
    let next_result_id = Ident.create "next#tc_result" in
    (* The box holding the result to calling the `next` function *)
    let return_val_id = Ident.create "return#tc" in

    let run_loop = sequence [
      assign_imm continue_id @@ ImmConst(Const_bool(true));
      assign_id next_id first_f_id;
    (* Doesn't matter what's in this box, as it is immediately overwritten *)
    ] @@ bind return_val_id (box @@ ImmConst(Const_int(0))) @@ sequence [
      wrap_comp lambda @@ CWhile(
        unbox continue_id |> to_anf,
        sequence [
          assign_imm continue_id @@ ImmConst(Const_bool(false))
        ] @@ 
        bind unboxed_next_id (unbox next_id) @@
        bind next_result_id (CApp(wrap_id lambda unboxed_next_id, []) |> wrap_comp lambda) @@
        (assign_id return_val_id next_result_id |> to_anf)
      )
    ] @@ (unbox return_val_id |> to_anf); in
    let lam_runner_body = sequence arg_ref_assignments run_loop in
    let lam_runner = wrap_comp lambda @@ CLambda(lambda_args, lam_runner_body) in
    (* Prevent re-optimizing this lambda *)
    Analyze_tail_calls.mark_not_tail_recursive lam_runner;
    lam_runner

  let enter_anf_expression ({anf_desc = desc} as a) =
    match desc with
    | AELet(global, Recursive, binds, body) when has_tail_recursive_binding binds ->
      let continue_loop_id = Ident.create "continue#tc" in
      let next_f_id = Ident.create "next#tc" in
      let iterator_binds = ref [
        (continue_loop_id, wrap_comp_with_anf a @@ default_ref a);
        (next_f_id, wrap_comp_with_anf a @@ default_ref a)
      ] in
      let new_binds = List.flatten @@ List.rev_map (fun ((id : Ident.t), ({comp_desc} as bind)) -> 
        begin match comp_desc with
        | CLambda(args, body) when comp_is_tail_recursive bind ->
          let wrap_comp = wrap_comp bind
          and wrap_anf = wrap_anf body
          and default_ref = default_ref a in

          let unbox id = wrap_comp @@ unbox_of id bind in

          (* IDs to the boxes representing the arguments to this function. *)
          let arg_ref_ids = List.map (fun (arg : Ident.t) -> Ident.create (arg.name ^ "#tc_ref")) args in
          (* The boxes representing the arguments to this function. *)
          let arg_refs = List.map (fun id -> id, wrap_comp default_ref) arg_ref_ids in
          (* Bindings containing the value received from unboxing those boxes. *)
          let arg_derefs = List.map (fun ((id : Ident.t), _) -> Ident.create (id.name ^ "#deref"), unbox id) arg_refs in

          (* Include the argument boxes in the bindings list available to all mutually recursive functions. *)
          iterator_binds := arg_refs @ !iterator_binds;

          let iterative_lam_id = Ident.create (id.name ^ "#tc_iter") in
          (* Make a new thunk that begins with new bindings to the would-be arguments *)
          let iterative_lam = wrap_comp @@ 
            CLambda([], wrap_anf @@ AELet(Nonglobal, Nonrecursive, arg_derefs, body)) in
          
          mark_tail_call_optimized iterative_lam; Analyze_tail_calls.mark_not_tail_recursive iterative_lam;

          List.iter2 (fun arg (arg_deref, _) -> push_rewrite_rule arg arg_deref) args arg_derefs;
          (* Flag internal tail calls to this function to be rewritten into setting the `next` function *)
          (* These are pushed in the correct order because of the rev_map *)
          push_transform_rule id {f_id=iterative_lam_id; f_args=arg_ref_ids; continue_loop_id; next_f_id};
          
          (* Create the function that gets called instead of the original *)
          let lam_runner = create_lambda_runner bind {continue_id=continue_loop_id; next_id=next_f_id; first_f_id=iterative_lam_id; arg_ref_ids} in

          [iterative_lam_id, iterative_lam; id, lam_runner]
        | _ -> [(id, bind)] end
      ) binds in
      {a with anf_analyses=ref []; anf_desc=AELet(Nonglobal, Nonrecursive, !iterator_binds, 
        {a with anf_analyses=ref []; anf_desc=AELet(global, Recursive, new_binds, body)})};
    | _ -> a

  let enter_comp_expression ({comp_desc = desc} as c) =
    match desc with
    (* Apply transformation to tail call within tail recursive function *)
    | CApp({imm_desc=ImmId(f)}, args) when has_transform_rule f && comp_is_tail_call c ->
      let {f_id=new_f; f_args=new_f_args; continue_loop_id; next_f_id} = get_transform_rule f in
      let _true = wrap_imm c @@ ImmConst(Const_bool(true)) in
      let _false = wrap_imm c @@ ImmConst(Const_bool(false)) in
      let comp_false = {c with comp_desc=CImmExpr(_false)} in
      let set_break_and_next = wrap_anf_with_comp c @@ AESeq(
        wrap_comp c @@ CAssign(wrap_id c continue_loop_id, _true),
        wrap_anf_with_comp c @@ AEComp(wrap_comp c @@ CAssign(wrap_id c next_f_id, wrap_id c new_f))
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
