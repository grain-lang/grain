open Lexing
open Printf
open Types
open Grain_parsing

type grain_library = sourcespan program -> sourcespan program

let parse name lexbuf : sourcespan program =
  let open Lexing in
  let string_of_position p =
    sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) in
  try
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    fst @@ List.hd @@ Parser.parse_program Lexer.token lexbuf
  with
  | Failure x when String.equal x "lexing: empty token" ->
    failwith (sprintf "lexical error at %s"
                (string_of_position lexbuf.lex_curr_p))
  | Parsing.Parse_error ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      failwith (sprintf "Parse error at line %d, col %d: token %s"
                  line cnum tok)
    end

(** Converts an library AST's ellipsis into a 'hole' to be filled
    by the returned function. The idea is that a loaded program
    can be applied to the returned function to wrap it with the
    given standard library *)
let convert_to_continuation ast : grain_library =
  let rec helpAst ast k =
    match ast with
    | ELet(binds, body, a) ->
      helpAst body (fun ({body; _} as p) -> k @@ {p with body=ELet(binds, body, a)})
    | ELetRec(binds, body, a) ->
      helpAst body (fun ({body; _} as p) -> k @@ {p with body=ELetRec(binds, body, a)})
    | EEllipsis(_) -> k
    | _ -> failwith "Invalid library AST passed well-formedness" in
  let helpStatements stmts k =
    fun ({statements; _} as p) -> k @@ {p with statements=(stmts @ statements)} in
  let help {statements; body} k =
    helpAst body (helpStatements statements k) in
  help ast (fun x -> x)

let load_library initial_env library =
  Printf.eprintf "going to load\n";
  let filename = library in
  let inchan = open_in filename in
  let lexbuf = Lexing.from_channel inchan in
  let lib = Desugar.desugar_program @@ parse filename lexbuf in
  let errors = Well_formedness.well_formed lib true initial_env in
  match errors with
  | [] -> Right(convert_to_continuation lib)
  | _ -> Left(errors)

let compose f g x = f (g x)

(** Lifts the given list of loaded libraries into a single either result *)
let lift_loaded_libraries (libs : (exn list, grain_library) either list) : (exn list, grain_library) either =
  List.fold_left (fun acc cur ->
      match cur with
      | Right(cont) ->
        (match acc with
         | Right(rest_cont) -> Right(compose cont rest_cont)
         | Left(_) -> acc)
      | Left(errs) ->
        (match acc with
         | Right(_) -> cur
         | Left(rest_errs) -> Left(errs @ rest_errs))) (Right(fun x -> x)) libs

(** Returns the path to the given library within the given list
    of include directories, if it exists. *)
let library_path (include_dirs : string list) (lib : string) =
  let lib_path dir =
    let open BatPathGen.OfString in
    let open Infix in
    to_string @@ (of_string dir) /: (lib ^ ".grlib") in
  List.map lib_path include_dirs
  |> List.find_opt Sys.file_exists


let rec extract_includes (include_dirs : string list) (({body; _} : sourcespan program) as prog) =
  match body with
  | EInclude(lib, body, loc) ->
    let rest = extract_includes include_dirs {prog with body=body} in
    let lib_file = library_path include_dirs lib in
    begin
      match (rest, lib_file) with
      | Left(errs), None -> Left((IncludeNotFound(lib, loc))::errs)
      | Left(_), _ -> rest
      | Right(_), None -> Left([IncludeNotFound(lib, loc)])
      | Right((libs, body)), Some(lib_path) -> Right((lib_path::libs), body)
    end
  | _ -> Right([], prog)


let stdlib_directory() : string option =
  let open BatPathGen.OfString in
  let open Infix in
  Config.get_grain_root()
  |> Option.map (fun root ->
      to_string @@ (of_string root) /: "lib" /: "grain" /: "stdlib")


let load_libraries (initial_env : sourcespan envt) (include_dirs : string list) (prog : sourcespan program) =
  let extracted = extract_includes include_dirs prog in
  match extracted with
  | Left(errs) -> Left(errs)
  | Right((libs, body)) ->
    let loaded = List.map (load_library initial_env) libs
                 |> lift_loaded_libraries in
    match loaded with
    | Left(errs) -> Left(errs)
    | Right(add_lib) -> Right(add_lib body)
