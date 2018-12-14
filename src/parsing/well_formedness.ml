open Parsetree
open Ast_iterator
open Grain_utils

type wferr =
  | MalformedString of Location.t
  | MultipleModuleName of Location.t
  | TypeNameShouldBeUppercase of string * Location.t
  | TyvarNameShouldBeLowercase of string * Location.t
  | ExportAllShouldOnlyAppearOnce of Location.t
  | ExportDataAllShouldOnlyAppearOnce of Location.t

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
  | TyvarNameShouldBeLowercase(var, loc) ->
    errorf ~loc "Type variable '%s' should be lowercase." var
  | ExportAllShouldOnlyAppearOnce loc ->
    errorf ~loc "An 'export *' statement should appear at most once."
  | ExportDataAllShouldOnlyAppearOnce loc ->
    errorf ~loc "An 'export data *' statement should appear at most once."


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
  let iterator = { super with expr = iter_expr } in
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

let only_has_one_export_all errs super =
  let count_export = ref 0 in
  let count_export_data = ref 0 in
  let iter_export_all self ({ptop_desc=desc; ptop_loc=loc} as e) =
    let check_export_count () =
      if !count_export > 1 then errs := (ExportAllShouldOnlyAppearOnce loc)::!errs in
    let check_export_data_count () =
      if !count_export_data > 1 then errs := (ExportDataAllShouldOnlyAppearOnce loc)::!errs in
    begin match desc with
    | PTopExportAll _ -> incr count_export; check_export_count ()
    | PTopExportDataAll _ -> incr count_export_data; check_export_data_count ()
    | _ -> ()
    end;
    super.toplevel self e in
  let iterator = { super with toplevel = iter_export_all } in
  { errs; iterator }

let compose_well_formedness { errs; iterator } cur =
  cur errs iterator

let well_formedness_checks = [
  malformed_strings;
  malformed_identifiers;
  types_have_correct_case;
  only_has_one_export_all;
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
