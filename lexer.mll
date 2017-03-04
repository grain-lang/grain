{
  open Lexing
  open Parser
  open Printf
}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+

rule token = parse
  | '#' [^ '\n']+ { token lexbuf }
  | blank { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { NUM (int_of_string x) }
  | "def" { DEF }
  | "add1" { ADD1 }
  | "sub1" { SUB1 }
  | "print" { PRINT }
  | "printStack" { PRINTSTACK }
  | "begin" { BEGIN }
  | "end" { END }
  | "if" { IF }
  | "true" { TRUE }
  | "false" { FALSE }
  | "isbool" { ISBOOL }
  | "isnum" { ISNUM }
  | "istuple" { ISTUPLE }
  | "lambda" { LAMBDA }
  | "λ" { LAMBDA }
  | ":" { COLON }
  | "else:" { ELSECOLON }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | ":=" { GETS }
  | "==" { EQEQ }
  | "=" { EQUAL }
  | "," { COMMA }
  | ";" { SEMI }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "<" { LESS }
  | ">" { GREATER }
  | "<=" { LESSEQ }
  | ">=" { GREATEREQ }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | ident as x { ID x }
  | eof { EOF }
  | _ as c { failwith (sprintf "Unrecognized character: %c" c) }


