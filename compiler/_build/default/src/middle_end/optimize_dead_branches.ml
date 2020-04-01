open Anftree
open Grain_typed
open Types

module BranchArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  let is_simple_case {anf_desc} =
    match anf_desc with
    | AEComp _ -> true
    | _ -> false

  let has_optimizable_conditional binds =
    List.exists (fun (_, {comp_desc}) -> 
      match comp_desc with
      | CIf({imm_desc=ImmConst(Const_bool(true))}, branch, _)
      | CIf({imm_desc=ImmConst(Const_bool(false))}, _, branch) -> true
      | _ -> false
    ) binds

  let has_simple_optimizable_conditional binds =
    List.for_all (fun (_, {comp_desc}) -> 
      match comp_desc with
      | CIf({imm_desc=ImmConst(Const_bool(true))}, branch, _)
      | CIf({imm_desc=ImmConst(Const_bool(false))}, _, branch) ->
        is_simple_case branch
      | _ -> true
    ) binds

  let extract_comp {anf_desc} =
    match anf_desc with
    | AEComp(comp) -> comp
    | _ -> failwith "No extractable comp"

  let rec relinearize id global ({anf_desc} as a) cont =
    match anf_desc with
    | AEComp(comp) -> 
      {a with anf_desc=AELet(global, Nonrecursive, [(id, comp)], cont)}
    | AESeq(comp, body) -> 
      {a with anf_desc=AESeq(comp, relinearize id global body cont)}
    | AELet(_global, _recursive, binds, body) -> 
      {a with anf_desc=AELet(_global, _recursive, binds, relinearize id global body cont)}

  let enter_anf_expression ({anf_desc = desc} as a) =
    match desc with
    | AEComp({comp_desc=CIf({imm_desc=ImmConst(Const_bool(true))}, branch, _)})
    | AEComp({comp_desc=CIf({imm_desc=ImmConst(Const_bool(false))}, _, branch)}) ->
      branch
    | AELet(global, recursive, binds, body) when has_simple_optimizable_conditional binds ->
      let binds = List.map (fun (id, ({comp_desc} as comp)) ->
        match comp_desc with
        | CIf({imm_desc=ImmConst(Const_bool(true))}, branch, _)
        | CIf({imm_desc=ImmConst(Const_bool(false))}, _, branch) ->
          id, extract_comp branch
        | _ -> id, comp
      ) binds in
      {a with anf_desc=AELet(global, recursive, binds, body)}
    | AELet(global, Nonrecursive, binds, body) when has_optimizable_conditional binds ->
      (* We can't relinearize recursive bindings since they depend on each other *)
      List.fold_right (fun (name, ({comp_desc} as comp)) cont ->
        match comp_desc with
        | CIf({imm_desc=ImmConst(Const_bool(true))}, _true, _) ->
          relinearize name global _true cont
        | CIf({imm_desc=ImmConst(Const_bool(false))}, _, _false) ->
          relinearize name global _false cont
        | _ -> {a with anf_desc=AELet(global, Nonrecursive, [(name, comp)], cont)}
      ) binds body
    | _ -> a

end

module BranchMapper = Anf_mapper.MakeMap(BranchArg)

let optimize anfprog =
  (* Reset state *)
  BranchMapper.map_anf_program anfprog
