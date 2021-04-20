/** Wrapper for the parser, including error handling and ambiguous parses. */

let parse: (~name: string=?, Lexing.lexbuf) => Parsetree.parsed_program;

let scan_for_imports: (~defer_errors: bool=?, string) => list(string);
