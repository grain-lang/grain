(** External frontend for running the parser. *)
open Lexing
open Location

type error =
  | AmbiguousParse of Parsetree.parsed_program list
  | NoValidParse

let report_error ppf = function
  | AmbiguousParse parses ->
    if !Grain_utils.Config.verbose || (!Grain_utils.Config.parser_debug_level > 0) then begin
      Format.fprintf ppf "The Grain compiler had trouble parsing your program. Here were the potential parses:@\n@[<v>%a@]"
        (fun ppf -> List.iter (fun x ->
             Format.fprintf ppf "@[%s@]%," (Sexplib.Sexp.to_string_hum @@ Parsetree.sexp_of_parsed_program x)
           )) parses
    end else
      Format.fprintf ppf "The Grain compiler had trouble parsing your program."
  | NoValidParse ->
    Format.fprintf ppf "The Grain compiler was unable to parse your program. \
                        If you see this message, please file an issue at https://github.com/grain-lang/grain"

exception Error of Location.t * error

let () =
  Location.register_error_of_exn
    (function
      | Error(loc, err) -> Some(Location.error_of_printer loc report_error err)
      | _ -> None)

let parse ?name lexbuf : Parsetree.parsed_program =
  Option.may (fun n ->
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = n };
      Location.input_name := n;
    ) name;
  let loc_start = Option.map_default (fun n -> {lexbuf.lex_start_p with pos_fname = n}) lexbuf.lex_start_p name in
  let loc_end = lexbuf.lex_curr_p in
  let startpos = {loc_start; loc_end; loc_ghost=true} in
  match Parser.parse_program Lexer.token lexbuf with
  | [] -> raise (Error(startpos, NoValidParse))
  | [(x, _)] -> x
  | parses -> raise (Error(startpos, AmbiguousParse (List.map fst parses)))
