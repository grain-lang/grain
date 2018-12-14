open Anftree
open Grain_typed
open Types

let used_symbols = ref (Ident.empty : bool Ident.tbl)

let mark_used id =
  used_symbols := Ident.add id true !used_symbols

let get_comp_purity c =
  Option.default false @@ Analyze_purity.comp_expression_purity c

let can_remove ident value =
  try
    not (Ident.find_same ident !used_symbols)
  with
  | Not_found -> get_comp_purity value

module DAEArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  (* let enter_anf_program ({signature} as prog) =
    let signatures = signature.cmi_sign in
    List.iter (fun signature ->
      match signature with
      | TSigValue(_, vd) -> mark_used @@ Path.head vd.val_fullpath
      | _ -> ()
    ) signatures;
    prog *)

  let enter_imm_expression ({imm_desc = desc} as i) =
    begin match desc with
      | ImmId(i) -> mark_used i
      | _ -> ()
    end;
    i

  let leave_anf_expression ({anf_desc = desc} as a) =
    match desc with
    | AELet(Global, _, _, _)
    | AESeq _
    | AEComp _ -> a
    | AELet(g, Nonrecursive, [(bind, value)], body) ->
      begin match body.anf_desc with
        | AEComp({comp_desc=CImmExpr({imm_desc=ImmId(id)})}) when Ident.same id bind ->
          {a with anf_desc=AEComp(value)}
        | _ ->
          if can_remove bind value then
            body
          else
            a
      end
    | AELet(g, r, binds, body) ->
      let new_binds = List.fold_right (fun (id, v) tl ->
          if can_remove id v then
            tl
          else
            (id, v)::tl) binds [] in
      begin match new_binds with
        | [] -> body
        | _ -> {a with anf_desc=AELet(g, r, new_binds, body)}
      end
end

module DAEMapper = Anf_mapper.MakeMap(DAEArg)

let optimize anfprog =
  (* Reset state *)
  used_symbols := Ident.empty;
  DAEMapper.map_anf_program anfprog
