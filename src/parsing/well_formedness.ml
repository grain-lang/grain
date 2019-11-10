open Parsetree
open Ast_iterator
open Grain_utils

type wferr =
  | MalformedString of Location.t
  | MultipleModuleName of Location.t
  | TypeNameShouldBeUppercase of string * Location.t
  | IllegalAliasName of string * Location.t
  | ExternalAlias of string * Location.t
  | ModuleNameShouldBeUppercase of string * Location.t
  | ModuleImportNameShouldNotBeExternal of string * Location.t
  | TyvarNameShouldBeLowercase of string * Location.t
  | ExportAllShouldOnlyAppearOnce of Location.t
  | EmptyRecordPattern of Location.t

exception Error of wferr

let prepare_error =
  let open Printf in
  let open Location in
  function
  | MalformedString loc ->
    errorf ~loc "Malformed string literal"
  | MultipleModuleName loc ->
    errorf ~loc "Multiple modules in identifier"
  | TypeNameShouldBeUppercase(name, loc) ->
    errorf ~loc "Type '%s' should have an uppercase name." name
  | IllegalAliasName(name, loc) ->
    errorf ~loc "Alias '%s' should have proper casing." name
  | ExternalAlias(name, loc) ->
    errorf ~loc "Alias '%s' should be at most one level deep." name
  | ModuleNameShouldBeUppercase(name, loc) ->
    errorf ~loc "Module '%s' should have an uppercase name." name
  | ModuleImportNameShouldNotBeExternal(name, loc) ->
    errorf ~loc "Module name '%s' should contain only one module." name
  | TyvarNameShouldBeLowercase(var, loc) ->
    errorf ~loc "Type variable '%s' should be lowercase." var
  | ExportAllShouldOnlyAppearOnce loc ->
    errorf ~loc "An 'export *' statement should appear at most once."
  | EmptyRecordPattern loc ->
    errorf ~loc "A record pattern must contain at least one named field."


let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | _ -> None
    )

type well_formedness_checker = {
  errs : wferr list ref;
  iterator : iterator;
}

let malformed_strings errs super =
  let iter_expr self ({pexp_desc=desc; pexp_loc=loc} as e) =
    begin
      match desc with
      | PExpConstant(PConstString s) ->
        begin
          try
            BatUTF8.validate s
          with
          | BatUTF8.Malformed_code ->
            errs := (MalformedString loc)::!errs
        end
      | _ -> ()
    end;
    super.expr self e in
  let iterator = { super with
                   expr = iter_expr } in
  { errs; iterator }

let malformed_identifiers errs super =
  let open Identifier in
  let iter_expr self ({pexp_desc=desc; pexp_loc=loc} as e) =
    begin
      match desc with
      | PExpId {txt=(IdentExternal(IdentExternal _, _))} ->
        errs := (MultipleModuleName loc)::!errs
      | _ -> ()
    end;
    super.expr self e in
  let iter_import self imports =
    let xor p q = (p && (not q)) || ((not p) && q) in
    let casing_mismatch orig alias =
      let o = orig.[0] in
      let a = alias.[0] in
      xor (BatChar.is_uppercase o) (BatChar.is_uppercase a) in
    List.iter (fun {pimp_val} ->
      match pimp_val with
      | PImportValues values ->
        List.iter (fun (name, alias) ->
          match (name, alias) with
          | {txt=IdentName(orig)}, Some({txt=IdentName(alias); loc}) 
            when casing_mismatch orig alias -> 
            errs := (IllegalAliasName (alias, loc))::!errs
          | _, Some({txt=(IdentExternal _ as alias); loc}) ->
            errs := (ExternalAlias (Identifier.string_of_ident alias, loc))::!errs
          | _ -> ()
        ) values
      | _ -> ()
    ) imports;
    super.import self imports in
  let iterator = { super with expr = iter_expr; import = iter_import } in
  { errs; iterator }

let types_have_correct_case errs super =
  let check_uppercase loc s =
    let first_char = String.get s 0 in
    if first_char <> BatChar.uppercase first_char then
      errs := (TypeNameShouldBeUppercase(s, loc))::!errs in
  let iter_data self ({pdata_name={loc=name_loc; txt=name}; pdata_loc=loc; _} as d) =
    check_uppercase name_loc name;
    super.data self d in
  (* FIXME: The parser should read in uppercase types as PTyConstr instances *)
  let iterator = { super with data = iter_data } in
  { errs; iterator }

let modules_have_correct_case errs super =
  let check_uppercase loc s =
    let first_char = String.get s 0 in
    if first_char <> BatChar.uppercase first_char then
      errs := (ModuleNameShouldBeUppercase(s, loc))::!errs in
  let iter_mods self imports =
    List.iter (fun {pimp_mod_alias=alias} ->
      match alias with
      | Some {loc=name_loc; txt=(IdentName name)} ->
        check_uppercase name_loc name;
      | Some _ (* IdentExternal handled by another WF rule *)
      | None -> ()
    ) imports;
    super.import self imports in
  let iterator = { super with import = iter_mods } in
  { errs; iterator }

let module_imports_not_external errs super =
  let check_name loc id =
    match id with
    | Identifier.IdentName _ -> ()
    | Identifier.IdentExternal _ ->
      errs := (ModuleImportNameShouldNotBeExternal(Identifier.string_of_ident id, loc))::!errs in
  let iter_mods self imports =
    List.iter (fun {pimp_mod_alias=alias} ->
      match alias with
      | Some {loc=name_loc; txt=name} ->
        check_name name_loc name;
      | None -> ()
    ) imports;
    super.import self imports in
  let iterator = { super with import = iter_mods } in
  { errs; iterator }

let only_has_one_export_all errs super =
  let count_export = ref 0 in
  let iter_export_all self ({ptop_desc=desc; ptop_loc=loc} as e) =
    let check_export_count () =
      if !count_export > 1 then errs := (ExportAllShouldOnlyAppearOnce loc)::!errs in
    begin match desc with
    | PTopExportAll _ -> incr count_export; check_export_count ()
    | _ -> ()
    end;
    super.toplevel self e in
  let iterator = { super with toplevel = iter_export_all } in
  { errs; iterator }

let no_empty_record_patterns errs super =
  let iter_toplevel_binds self ({ptop_desc=desc; ptop_loc=loc} as e) =
    begin match desc with
    | PTopLet(_, _, vbs) ->
      List.iter (function 
        | {pvb_pat={ppat_desc=PPatRecord(fields, _)}} -> 
          if List.length fields = 0 then errs := (EmptyRecordPattern loc)::!errs
        | _ -> ()
      ) vbs
    | _ -> ()
    end;
    super.toplevel self e in
  let iter_binds self ({pexp_desc=desc; pexp_loc=loc} as e) =
    begin match desc with
    | PExpLet(_, vbs, _) ->
      List.iter (function 
        | {pvb_pat={ppat_desc=PPatRecord(fields, _)}} -> 
          if List.length fields = 0 then errs := (EmptyRecordPattern loc)::!errs
        | _ -> ()
      ) vbs
    | _ -> ()
    end;
    super.expr self e in
  let iterator = { super with toplevel = iter_toplevel_binds; expr = iter_binds } in
  { errs; iterator }

let compose_well_formedness { errs; iterator } cur =
  cur errs iterator

let well_formedness_checks = [
  malformed_strings;
  malformed_identifiers;
  types_have_correct_case;
  modules_have_correct_case;
  module_imports_not_external;
  only_has_one_export_all;
  no_empty_record_patterns;
]

let well_formedness_checker() =
  List.fold_left
    compose_well_formedness
    {errs=ref []; iterator=default_iterator}
    well_formedness_checks

let check_well_formedness {statements; body} =
  let checker = well_formedness_checker() in
  List.iter (checker.iterator.toplevel checker.iterator) statements;
  checker.iterator.expr checker.iterator body;
  (* FIXME: We should be able to raise _all_ errors at once *)
  List.iter (fun e -> raise (Error e)) !(checker.errs)
