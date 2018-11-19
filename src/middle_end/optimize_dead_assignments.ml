open Anftree
open Grain_typed

type analysis +=
  | Pure = Analyze_purity.Pure

let used_symbols = ref (Ident.empty : bool Ident.tbl)

let mark_used id =
  used_symbols := Ident.add id true !used_symbols

let get_purity {comp_analyses} =
  let rec find_purity : analysis list -> bool option = function
  | Pure(x)::_ -> Some(x)
  | _::tl -> find_purity tl
  | [] -> None in
  Option.default false @@ find_purity !comp_analyses

let can_remove ident value =
  try
    not (Ident.find_same ident !used_symbols)
  with
  | Not_found -> get_purity value

module DAEArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

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
