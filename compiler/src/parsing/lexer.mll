{
  open Lexing
  open Parser
  open Printf

  let lexbuf_loc {lex_start_p=loc_start; lex_curr_p=loc_end; _} =
    let open Location in
    {
      loc_start; loc_end; loc_ghost=false
    }

  type error =
    | UnrecognizedCharacter of char
    | IllegalStringCharacter of string
    | IllegalUnicodeCodePoint of string

  exception Error of Location.t * error

  let report_error ppf err =
    match err with
    | UnrecognizedCharacter c ->
      Format.fprintf ppf "Unrecognized character: %C" c
    | IllegalStringCharacter sc ->
      Format.fprintf ppf "Illegal string character: %S" sc
    | IllegalUnicodeCodePoint cp ->
      Format.fprintf ppf "Illegal unicode code point: %S" cp

  let () =
    Location.register_error_of_exn
      (function
        | Error(loc, err) -> Some(Location.error_of_printer loc report_error err)
        | _ -> None)

  let add_code_point buf str loc = begin
    let (esc, numstr) = ((String.sub str 1 1), (String.sub str 2 ((String.length str) - 2))) in
    let code_point = (match esc with
      | "u" when (numstr.[0] = '{') -> Scanf.sscanf (String.sub numstr 1 ((String.length numstr) - 1)) "%x" (fun x -> x)
      | "u"
      | "x" -> Scanf.sscanf numstr "%x" (fun x -> x)
      | _ -> Scanf.sscanf (esc^numstr) "%o" (fun x -> x)) in
    if (Uchar.is_valid code_point) then
      Buffer.add_utf_8_uchar buf (Uchar.of_int code_point)
    else
      raise (Error(loc, IllegalUnicodeCodePoint(str)));
  end

  let newline_regex = Str.regexp "\\(\r\\|\n\\)"

  let process_newlines lexbuf =
    let input = lexeme lexbuf in
    String.iter (fun c ->
      if Str.string_match newline_regex (String.make 1 c) 0
      then new_line lexbuf
    ) input

  let comments = ref []

  let consume_comments () =
    let out_comments = !comments in
    comments := [];
    out_comments

  let parse_line_comment comment_type lexbuf =
    let source = lexeme lexbuf in
    let loc = lexbuf_loc lexbuf in
    comments := comment_type source loc :: !comments

  let wrap_block_comment_lexer comment comment_type lexbuf =
    let start_loc = Location.curr lexbuf  in
    let (s, end_loc) = comment lexbuf in
    let loc = { start_loc with Location.loc_end = end_loc.Location.loc_end } in
    comments := comment_type s loc :: !comments

}

let dec_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let oct_digit = ['0'-'7']
let bin_digit = ['0'-'1']

let dec_int = dec_digit (dec_digit | '_')*
let hex_int = '0' ['x' 'X'] hex_digit (hex_digit | '_')*
let oct_int = '0' ['o' 'O'] oct_digit (oct_digit | '_')*
let bin_int = '0' ['b' 'B'] bin_digit (bin_digit | '_')*

let unsigned_int = dec_int | hex_int | oct_int | bin_int

let dec_float_exp = ['e' 'E'] ['+' '-']? dec_digit (dec_digit | '_')*
let dec_float_decimal = '.' (dec_digit | '_')*
let dec_float_decimal_explicit = '.' dec_digit (dec_digit | '_')*
let dec_float_integral = dec_digit (dec_digit | '_')*

let dec_float = dec_float_integral dec_float_decimal dec_float_exp? | dec_float_decimal_explicit dec_float_exp? | dec_float_integral dec_float_exp

let unsigned_float = dec_float

let ident = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ident_cap = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+

let std_escapes = (("\\" dec_digit dec_digit? dec_digit?)
                 | ("\\x" hex_digit hex_digit?)
                 | ("\\" oct_digit oct_digit? oct_digit?)
                 | ("\\u" hex_digit hex_digit hex_digit hex_digit)
                 | ("\\u{" hex_digit hex_digit? hex_digit? hex_digit? hex_digit? hex_digit? "}")
                 | ("\\" ['\\' 'n' 'r' 't' '"' '\''] ))

let tquot_str = "```" (std_escapes
                    | "\\`"
                    | "`"[^ '`']
                    | "``"[^ '`']
                    | [^ '`' '\\'])* "```"

let dquot_str = '"' (std_escapes
                   | [^ '\\' '"' '\n' '\r'])* '"'

let squot_str = '\'' (std_escapes
              | [^ '\\' '\'' '\n' '\r'])* '\''

let unicode_esc = "\\u{" hex_digit (hex_digit (hex_digit (hex_digit (hex_digit hex_digit?)?)?)?)? "}"
let unicode4_esc = "\\u" hex_digit hex_digit hex_digit hex_digit
let hex_esc = "\\x" hex_digit hex_digit?
let oct_esc = "\\" oct_digit (oct_digit oct_digit?)?
let num_esc = (unicode_esc | unicode4_esc | hex_esc | oct_esc)

let newline_char = ("\r\n"|"\n\r"|'\n'|'\r')
let newline_chars = (newline_char | blank)* newline_char

let line_comment = "//" (([^ '\r' '\n']*(newline_chars | eof)) | (newline_chars | eof))
let shebang_comment = "#!" (([^ '\r' '\n']*(newline_chars | eof)) | (newline_chars | eof))

rule token = parse
  | line_comment { parse_line_comment make_line_comment lexbuf; process_newlines lexbuf; EOL }
  | shebang_comment { parse_line_comment make_shebang_comment lexbuf; process_newlines lexbuf; EOL }
  | "/*" { wrap_block_comment_lexer read_block_comment make_block_comment lexbuf; token lexbuf }
  | "/**" { wrap_block_comment_lexer read_doc_comment make_doc_comment lexbuf; token lexbuf }
  | blank { token lexbuf }
  | newline_chars { process_newlines lexbuf; EOL }
  | (unsigned_float as x) 'f' { FLOAT32 x }
  | (unsigned_float as x) 'd' { FLOAT64 x }
  | unsigned_float as x { NUMBER_FLOAT x }
  | (unsigned_int as x) 'l' { INT32 x }
  | (unsigned_int as x) 'L' { INT64 x }
  | (unsigned_int as x) 'n' { WASMI32 x }
  | (unsigned_int as x) 'N' { WASMI64 x }
  | (unsigned_float as x) 'w' { WASMF32 x }
  | (unsigned_float as x) 'W' { WASMF64 x }
  | unsigned_int as x { NUMBER_INT x }
  | "primitive" { PRIMITIVE }
  | "foreign" { FOREIGN }
  | "wasm" { WASM }
  | "while" { WHILE }
  | "if" { IF }
  | "when" { WHEN }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "void" { VOID }
  | "import" { IMPORT }
  | "export" { EXPORT }
  | "except" { EXCEPT }
  | "from" { FROM }
  | "->" { ARROW }
  | "=>" { THICKARROW }
  | "enum" { ENUM }
  | "record" { RECORD }
  | "|" { PIPE }
  | "let" { LET }
  | "mut" { MUT }
  | "rec" { REC }
  | "match" { MATCH }
  | "assert" { ASSERT }
  | "fail" { FAIL }
  | "exception" { EXCEPTION }
  | "try" { TRY }
  | "raise" { RAISE }
  | "catch" { CATCH }
  | "..." { ELLIPSIS }
  | "." { DOT }
  | "::" { COLONCOLON }
  | ":=" { GETS }
  | ":" { COLON }
  | "is" { IS }
  | "isnt" { ISNT }
  | "==" { EQEQ }
  | "=" { EQUAL }
  | "," { COMMA }
  | ";" { SEMI }
  | "as" { AS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "^" { CARET }
  | "<" { LCARET }
  | ">" { RCARET }
  | "^" { CARET }
  | "++" { PLUSPLUS }
  | "+" { PLUS }
  | "-" { DASH }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "<=" { LESSEQ }
  | ">=" { GREATEREQ }
  | "&" { AMP }
  | "&&" { AMPAMP }
  | "|" { PIPE }
  | "||" { PIPEPIPE }
  | "!" { NOT }
  | "@" { AT }
  | '"'   { read_str (Buffer.create 16) lexbuf }
  | '\'' { read_char (Buffer.create 4) lexbuf }
  | "_" { UNDERSCORE }
  | ident as x { ID x }
  | ident_cap as x { TYPEID x }
  | eof { EOF }
  | _ as c { raise (Error(lexbuf_loc lexbuf, UnrecognizedCharacter c)) }

and read_str buf =
  parse
  | '\\' newline_char { read_str buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; read_str buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_str buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; read_str buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_str buf lexbuf }
  | num_esc { add_code_point buf (lexeme lexbuf) (lexbuf_loc lexbuf); read_str buf lexbuf }
  | [^ '"' '\\']+ { process_newlines lexbuf; Buffer.add_string buf (lexeme lexbuf); read_str buf lexbuf }
  | '"' { STRING (Buffer.contents buf) }
  | _ { raise (Error(lexbuf_loc lexbuf, IllegalStringCharacter(lexeme lexbuf))) }

and read_char buf =
  parse
  | "\\n'" { CHAR "\n" }
  | "\\r'" { CHAR "\r" }
  | "\\'" { CHAR "'" }
  | "\\\\'" { CHAR "\\" }
  | (num_esc as esc) ''' { add_code_point buf esc (lexbuf_loc lexbuf); CHAR (Buffer.contents buf) }
  | ([^ '\'' '\\']+ as _match) '\'' { process_newlines lexbuf; Buffer.add_string buf _match; CHAR (Buffer.contents buf) }
  | _ { raise (Error(lexbuf_loc lexbuf, IllegalStringCharacter(lexeme lexbuf))) }

and read_block_comment =
  shortest
  | _* "*/" { process_newlines lexbuf; "/*" ^ (lexeme lexbuf), Location.curr lexbuf }

and read_doc_comment =
  shortest
  | _* "*/" { process_newlines lexbuf; "/**" ^ (lexeme lexbuf), Location.curr lexbuf }
