open Lexing
open Printf
open Legacy_types
open Grain_parsing

(*type grain_library = sourcespan program -> sourcespan program*)

let parse name lexbuf : Parsetree.parsed_program =
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


let stdlib_directory() : string option =
  let open BatPathGen.OfString in
  let open Infix in
  Config.get_grain_root()
  |> Option.map (fun root ->
      to_string @@ (of_string root) /: "lib" /: "grain" /: "stdlib")


let load_libraries (initial_env : sourcespan envt) (include_dirs : string list) (prog : Parsetree.parsed_program) =
  Right(prog)
