open Grain_utils;

/** Wrapper for the parser, including error handling and ambiguous parses. */

let parse:
  (~name: string=?, Lexing.lexbuf, unit => string) => Parsetree.parsed_program;

let scan_for_imports: (~defer_errors: bool=?, Filepath.t) => list(string);
