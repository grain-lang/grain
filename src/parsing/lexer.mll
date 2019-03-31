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
    | UnicodeCharacter
    | IllegalStringCharacter of string

  exception Error of Location.t * error

  let report_error ppf err =
    match err with
    | UnrecognizedCharacter c ->
      Format.fprintf ppf "Unrecognized character: %C" c
    | UnicodeCharacter ->
      Format.fprintf ppf "Unicode characters are currently unsupported."
    | IllegalStringCharacter sc ->
      Format.fprintf ppf "Illegal string character: %S" sc

  let () =
    Location.register_error_of_exn
      (function
        | Error(loc, err) -> Some(Location.error_of_printer loc report_error err)
        | _ -> None)

  let add_char_code buf lexbuf = begin
    let str = lexeme lexbuf in
    let (esc, numstr) = ((String.sub str 1 1), (String.sub str 2 ((String.length str) - 2))) in
    let to_add = (match esc with
      | "u" -> Scanf.sscanf numstr "%x" (fun x -> x)
      | "x" -> Scanf.sscanf numstr "%x" (fun x -> x)
      | _ -> Scanf.sscanf (esc^numstr) "%o" (fun x -> x)) in
    if (to_add > 255) then
      raise (Error(lexbuf_loc lexbuf, UnicodeCharacter))
    else
      Buffer.add_char buf (Char.chr to_add);
  end

}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let oct_digit = ['0'-'7']

let ident = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ident_cap = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+

let std_escapes = (("\\" dec_digit dec_digit? dec_digit?)
                 | ("\\x" hex_digit hex_digit?)
                 | ("\\u" hex_digit hex_digit? hex_digit? hex_digit?)
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

let unicode_esc = "\\u" hex_digit (hex_digit (hex_digit hex_digit?)?)?
let hex_esc = "\\x" hex_digit hex_digit?
let oct_esc = "\\" oct_digit (oct_digit oct_digit?)?
let num_esc = (unicode_esc | hex_esc | oct_esc)

let newline_char = ("\r\n"|"\n\r"|'\n'|'\r')

let comment = '#' ((([^'|'])[^ '\r' '\n']*(newline_char | eof)) | (newline_char | eof))

rule token = parse
  | comment { new_line lexbuf; token lexbuf }
  | blank { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { NUM (int_of_string x) }
  | "foreign" { FOREIGN }
  | "wasm" { WASM }
  | "add1" { ADD1 }
  | "sub1" { SUB1 }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "isbool" { ISBOOL }
  | "isnum" { ISNUM }
  | "istuple" { ISTUPLE }
  | "import" { IMPORT }
  | "export" { EXPORT }
  | "except" { EXCEPT }
  | "box" { BOX }
  | "unbox" { UNBOX }
  | "->" { ARROW }
  | "=>" { THICKARROW }
  | "data" { DATA }
  | "|" { PIPE }
  | "let" { LET }
  | "rec" { REC }
  | "match" { MATCH }
  | "::" { COLONCOLON }
  | ":=" { GETS }
  | ":" { COLON }
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
  | "<" { LCARET }
  | ">" { RCARET }
  | "+" { PLUS }
  | "-" { DASH }
  | "*" { STAR }
  | "<=" { LESSEQ }
  | ">=" { GREATEREQ }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | '"'   { read_dquote_str (Buffer.create 16) lexbuf }
  | '\'' { read_squote_str (Buffer.create 16) lexbuf }
  | "_" { UNDERSCORE }
  | ident as x { ID x }
  | ident_cap as x { TYPEID x }
  | eof { EOF }
  | _ as c { raise (Error(lexbuf_loc lexbuf, UnrecognizedCharacter c)) }


and read_dquote_str buf =
  parse
  | '\\' newline_char { read_dquote_str buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; read_dquote_str buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_dquote_str buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; read_dquote_str buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_dquote_str buf lexbuf }
  | num_esc { add_char_code buf lexbuf; read_dquote_str buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (lexeme lexbuf);
    read_dquote_str buf lexbuf }
  | '"' { STRING (Buffer.contents buf) }
  | _ { raise (Error(lexbuf_loc lexbuf, IllegalStringCharacter(lexeme lexbuf))) }

and read_squote_str buf =
  parse
  | "\\" newline_char { read_squote_str buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; read_squote_str buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_squote_str buf lexbuf }
  | "\\'" { Buffer.add_char buf '\''; read_squote_str buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_squote_str buf lexbuf }
  | num_esc { add_char_code buf lexbuf; read_squote_str buf lexbuf }
  | [^ ''' '\\']+ { Buffer.add_string buf (lexeme lexbuf);
    read_squote_str buf lexbuf }
  | '\'' { STRING (Buffer.contents buf) }
  | _ { raise (Error(lexbuf_loc lexbuf, IllegalStringCharacter(lexeme lexbuf))) }
