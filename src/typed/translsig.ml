open Grain_utils
open Types

let used_type_variables = ref (Tbl.empty : (int, type_expr list ref) Tbl.t)

let reset_type_variables () =
  used_type_variables := Tbl.empty

let rec collect_type_vars typ =
  match typ.desc with
  | TTyVar _ ->
    begin try
      let type_exprs = Tbl.find typ.id !used_type_variables in
      type_exprs := typ :: !type_exprs
    with Not_found ->
      used_type_variables := Tbl.add typ.id (ref [typ]) !used_type_variables
    end
  | TTyArrow(ty_args, ty_res, _) ->
    List.iter collect_type_vars ty_args;
    collect_type_vars ty_res
  | TTyTuple ty_args ->
    List.iter collect_type_vars ty_args
  | TTyRecord ty_args ->
    List.iter (fun (_, ty_arg) -> collect_type_vars ty_arg) ty_args
  | TTyConstr(_, ty_args, _) ->
    List.iter collect_type_vars ty_args
  | TTyUniVar _ -> ()
  | TTyPoly(ty_arg, ty_args) ->
    collect_type_vars ty_arg;
    List.iter collect_type_vars ty_args
  | TTyLink ty_arg
  | TTySubst ty_arg ->
    collect_type_vars ty_arg

let link_type_vars ty =
  reset_type_variables ();
  collect_type_vars ty;
  let rec link_types texpr = 
    let desc = match texpr.desc with
      | TTyVar _ as ty -> 
        begin try 
          let vars = Tbl.find texpr.id !used_type_variables in
          if List.length !vars < 2 then raise Not_found;
          TTyLink (List.hd !vars)
        with Not_found ->
          ty
        end
      | TTyArrow(tyl, ret, c) -> TTyArrow(List.map link_types tyl, link_types ret, c)
      | TTyTuple l -> TTyTuple (List.map link_types l)
      | TTyRecord l -> TTyRecord (List.map (fun (name, arg) -> (name, link_types arg)) l)
      | TTyConstr(p, l, m) -> TTyConstr(p, List.map link_types l, m)
      | TTyUniVar _ as ty -> ty
      | TTySubst _ -> assert false
      | TTyLink ty -> TTyLink (link_types ty)
      | TTyPoly(ty, tyl) -> TTyPoly(link_types ty, List.map link_types tyl) in
    {texpr with desc} in
  link_types ty

let translate_signature sg =
  List.map (fun item ->
    match item with
    | TSigValue(id, d) ->
      TSigValue(id, {d with val_type=link_type_vars d.val_type})
    | TSigType _
    | TSigModule _
    | TSigModType _ -> item
  ) sg
