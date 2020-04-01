open Anftree
open Grain_typed
open Types

let known_constants = ref (Ident.empty : constant Ident.tbl)

let add_constant id value =
  known_constants := Ident.add id value !known_constants

module ConstantPropagationArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  let enter_anf_expression ({anf_desc = desc} as a) =
    begin match desc with
      | AELet(g, r, binds, body) ->
      List.iter (fun (id, v) ->
        match v with
        | {comp_desc=CImmExpr({imm_desc=ImmConst(c)})} -> add_constant id c
        | _ -> ()
        ) binds
      | _ -> ()
    end;
    a

  let leave_imm_expression ({imm_desc = desc} as i) =
    match desc with
    | ImmId(id) -> begin
      try 
        let value = Ident.find_same id !known_constants in
        {i with imm_desc=ImmConst(value)}
      with
        Not_found -> i end
    | _ -> i

end

module ConstantPropagationMapper = Anf_mapper.MakeMap(ConstantPropagationArg)

let optimize anfprog =
  (* Reset state *)
  known_constants := Ident.empty;
  ConstantPropagationMapper.map_anf_program anfprog
