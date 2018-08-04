open Anftree
open Grain_typed

let pure_identifiers = ref (Ident.empty : bool Ident.tbl)
let used_symbols = ref (Ident.empty : bool Ident.tbl)

let mark_used id =
  used_symbols := Ident.add id true !used_symbols

let can_remove ident =
  try
    not (Ident.find_same ident !used_symbols)
  with
  | Not_found -> Ident.find_same ident !pure_identifiers

module DAEArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  let enter_comp_expression ({comp_desc = desc} as c) =
    begin match desc with
      | CLambda(args, _) ->
        List.iter mark_used args
      | _ -> ()
    end;
    c

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
          if can_remove bind then
            body
          else
            a
      end
    | AELet(g, r, binds, body) ->
      let new_binds = List.fold_right (fun (id, v) tl ->
          if can_remove id then
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
  pure_identifiers := Analyze_purity.pure_identifiers anfprog;
  used_symbols := Ident.empty;
  DAEMapper.map_anf_program anfprog
