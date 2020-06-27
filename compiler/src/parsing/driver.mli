val parse : ?name:string -> Lexing.lexbuf -> Parsetree.parsed_program
(** Wrapper for the parser, including error handling and ambiguous parses. *)
