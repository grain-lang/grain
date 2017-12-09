open Lexing
open Printf
open Types

type grain_library = sourcespan program -> sourcespan program

let parse name lexbuf =
  let open Lexing in
  let string_of_position p =
    sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) in
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
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
  let rec help ast k =
    match ast with
    | ELet(binds, body, a) ->
      help body (fun b -> k @@ ELet(binds, b, a))
    | ELetRec(binds, body, a) ->
      help body (fun b -> k @@ ELetRec(binds, b, a))
    | EEllipsis(_) -> k
    | _ -> failwith "Invalid library AST passed well-formedness" in
  help ast (fun x -> x)

let load_library initial_env library =
  let filename = "lib/" ^ library ^ ".grlib" in
  let inchan = open_in filename in
  let lexbuf = Lexing.from_channel inchan in
  let lib = parse filename lexbuf in
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

let library_exists lib =
  let filename = "lib/" ^ lib ^ ".grlib" in
  Sys.file_exists filename

let rec extract_includes (prog : sourcespan program) =
  match prog with
  | EInclude(lib, body, loc) ->
    let rest = extract_includes body in
    (match rest with
     | Left(errs) ->
       if not (library_exists lib) then
         Left((IncludeNotFound(lib, loc))::errs)
       else
         Left(errs)
     | Right((libs, body)) ->
       if not (library_exists lib) then
         Left([(IncludeNotFound(lib, loc))])
       else
         Right((lib::libs), body))
  | _ -> Right([], prog)

let load_libraries (initial_env : sourcespan envt) (prog : sourcespan program) =
  let extracted = extract_includes prog in
  match extracted with
  | Left(errs) -> Left(errs)
  | Right((libs, body)) ->
    let loaded = List.map (load_library initial_env) libs
                 |> lift_loaded_libraries in
    match loaded with
    | Left(errs) -> Left(errs)
    | Right(add_lib) -> Right(add_lib body)

