/** Wrapper for the parser, including error handling and ambiguous parses. */

let parse: (~name: string=?, Lexing.lexbuf) => Parsetree.parsed_program;
