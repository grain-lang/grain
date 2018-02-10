open Lexing
open Printf
open Legacy_types
open Grain_parsing

(*type grain_library = sourcespan program -> sourcespan program*)

let stdlib_directory() : string option =
  let open BatPathGen.OfString in
  let open Infix in
  !Grain_utils.Config.grain_root
  |> Option.map (fun root ->
      to_string @@ (of_string root) /: "lib" /: "grain" /: "stdlib")

let include_dirs() =
  (Option.map_default (fun x -> [x]) [] (stdlib_directory())) @ !Grain_utils.Config.include_dirs

let locate_module (lib : string) =
  let lib_path dir =
    let open BatPathGen.OfString in
    let open Infix in
    to_string @@ (of_string dir) /: (lib ^ ".grlib") in
  List.map lib_path (include_dirs())
  |> List.find_opt Sys.file_exists

let load_module import : Parsetree.parsed_program =
  let open Parsetree in
  let open Identifier in
  let id = match import.ptop_desc with
    | PTopImport({pimp_mod}) -> pimp_mod
    | _ -> failwith "Impossible" in
  let name = match id with
    | {txt=IdentName n} -> n
    | _ -> failwith "Bad module name" in
  match locate_module name with
  (* Close enough *)
  | None -> raise (Grain_typed.Typemod.Error(Location.dummy_loc, Grain_typed.Env.empty, Grain_typed.Typemod.Interface_not_compiled(name)))
  | Some(file) ->
    let inchan = open_in file in
    let lexbuf = Lexing.from_channel inchan in
    let loaded = Driver.parse ~name:name lexbuf in
    (* Not going to bother with a worklist since we're getting rid of this soon anyhow *)
    if List.exists (function | {Parsetree.ptop_desc=Parsetree.PTopImport _} -> true | _ -> false) (loaded.statements) then
      eprintf "WARNING: Library '%s' appears to import another library. This is currently unsupported!\n" name;
    if loaded.body.Parsetree.pexp_desc <> Parsetree.PExpNull then
      eprintf "WARNING: Library '%s' appears to have a non-empty body. This is currently unsupported!\n" name;

    loaded

let load_libraries (prog : Parsetree.parsed_program) =
  (* TODO: Ditch this altogether and use the facilities in 'typed' for separate compilation *)
  (* Currently hacked together. *)
  let open Parsetree in
  let imports, others = List.partition (function | {ptop_desc=PTopImport _} -> true | _ -> false) prog.statements in
  let loaded = List.map load_module imports in
  let new_stmts = List.fold_left (fun acc cur -> cur.statements @ acc) others loaded in
  {prog with statements=new_stmts}
