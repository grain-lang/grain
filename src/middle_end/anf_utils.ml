open Grain_parsing
open Grain_typed
open Anftree

let rec anf_free_vars_help env (a : anf_expression) =
  match a.anf_desc with
  | AESeq(fst, rest) ->
    Ident.Set.union
      (comp_free_vars_help env fst)
      (anf_free_vars_help env rest)
  | AEComp(c) -> comp_free_vars_help env c
  | AELet(_, recflag, binds, body) ->
    let with_names = List.fold_left (fun acc (id, _) -> Ident.Set.add id acc) env binds in
    let free_binds =
      match recflag with
      | Recursive -> List.fold_left
                       (fun acc (_, body) -> Ident.Set.union acc (comp_free_vars_help with_names body))
                       Ident.Set.empty
                       binds
      | Nonrecursive -> List.fold_left
                          (fun acc (_, body) -> Ident.Set.union acc (comp_free_vars_help env body))
                          Ident.Set.empty
                          binds in
    Ident.Set.union free_binds (anf_free_vars_help with_names body)

and comp_free_vars_help env (c : comp_expression) =
  match c.comp_desc with
  | CLambda(args, body) ->
    anf_free_vars_help (Ident.Set.union env (Ident.Set.of_list args)) body
  | CIf(cond, thn, els) ->
    Ident.Set.union (imm_free_vars_help env cond) @@
    Ident.Set.union (anf_free_vars_help env thn) (anf_free_vars_help env els)
  | CWhile(cond, body) ->
    Ident.Set.union (anf_free_vars_help env cond) (anf_free_vars_help env body)
  | CSwitch(arg, branches) ->
    List.fold_left (fun acc (_, b) -> Ident.Set.union (anf_free_vars_help env b) acc)
      (imm_free_vars_help env arg)
      branches
  | CPrim1(_, arg) -> imm_free_vars_help env arg
  | CPrim2(_, arg1, arg2) ->
    Ident.Set.union
      (imm_free_vars_help env arg1)
      (imm_free_vars_help env arg2)
  | CAssign(arg1, arg2) ->
    Ident.Set.union
      (imm_free_vars_help env arg1)
      (imm_free_vars_help env arg2)
  | CApp(fn, args) ->
    List.fold_left (fun acc a -> Ident.Set.union (imm_free_vars_help env a) acc)
      (imm_free_vars_help env fn)
      args
  | CAppBuiltin(_, _, args) ->
    List.fold_left (fun acc a -> Ident.Set.union (imm_free_vars_help env a) acc)
      Ident.Set.empty
      args
  | CTuple(args)
  | CAdt(_, args) ->
    List.fold_left (fun acc a -> Ident.Set.union (imm_free_vars_help env a) acc)
      Ident.Set.empty
      args
  | CGetTupleItem(_, arg)
  | CGetAdtItem(_, arg)
  | CGetAdtTag(arg) ->
    imm_free_vars_help env arg
  | CSetTupleItem(_, arg1, arg2) ->
    Ident.Set.union
      (imm_free_vars_help env arg1)
      (imm_free_vars_help env arg2)
  | CString(s) -> Ident.Set.empty
  | CImmExpr(i) -> imm_free_vars_help env i

and imm_free_vars_help env (i : imm_expression) =
  match i.imm_desc with
  | ImmId(x) when not(Ident.Set.mem x env) -> Ident.Set.singleton x
  | _ -> Ident.Set.empty


let anf_free_vars = anf_free_vars_help Ident.Set.empty
let comp_free_vars = comp_free_vars_help Ident.Set.empty
let imm_free_vars = imm_free_vars_help Ident.Set.empty


let rec anf_count_vars a =
  match a.anf_desc with
  | AELet(_, recflag, binds, body) ->
    let max_binds = List.fold_left max 0 @@ List.map (fun (_, c) -> comp_count_vars c) binds in
    begin match recflag with
      | Recursive -> (List.length binds) + (max max_binds (anf_count_vars body))
      | Nonrecursive -> max max_binds ((List.length binds) + (anf_count_vars body))
    end
  | AESeq(hd, tl) -> max (comp_count_vars hd) (anf_count_vars tl)
  | AEComp(c) -> comp_count_vars c

and comp_count_vars c =
  match c.comp_desc with
  | CIf(_, t, f) -> max (anf_count_vars t) (anf_count_vars f)
  | CWhile(c, b) -> (anf_count_vars c) + (anf_count_vars b)
  | CSwitch(_, bs) -> List.fold_left max 0 @@ List.map (fun (_, b) -> anf_count_vars b) bs
  | CApp(_, args) -> List.length args
  | CAppBuiltin(_, _, args) -> List.length args
  | _ -> 0


module ClearLocationsArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  let leave_imm_expression i =
    {i with imm_loc=Location.dummy_loc}

  let leave_comp_expression c =
    {c with comp_loc=Location.dummy_loc}

  let leave_anf_expression a =
    {a with anf_loc=Location.dummy_loc}
end

module ClearLocations = Anf_mapper.MakeMap(ClearLocationsArg)

let clear_locations = ClearLocations.map_anf_program
