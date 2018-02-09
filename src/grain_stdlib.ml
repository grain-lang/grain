open Lexing
open Printf
open Legacy_types
open Grain_parsing

(*type grain_library = sourcespan program -> sourcespan program*)

let stdlib_directory() : string option =
  let open BatPathGen.OfString in
  let open Infix in
  Config.get_grain_root()
  |> Option.map (fun root ->
      to_string @@ (of_string root) /: "lib" /: "grain" /: "stdlib")


let load_libraries (initial_env : sourcespan envt) (include_dirs : string list) (prog : Parsetree.parsed_program) =
  prog
