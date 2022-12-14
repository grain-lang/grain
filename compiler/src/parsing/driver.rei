/** Wrapper for the parser, including error handling and ambiguous parses. */

let parse:
  (~name: string=?, Sedlexing.lexbuf, unit => string) =>
  Parsetree.parsed_program;

let read_imports: Parsetree.parsed_program => list(Location.loc(string));

let scan_for_imports:
  (~defer_errors: bool=?, string) => list(Location.loc(string));

let reset: unit => unit;
