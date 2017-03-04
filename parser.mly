%{
open Types

%}

%token <int> NUM
%token <string> ID
%token LBRACK RBRACK DEF ADD1 SUB1 LPAREN RPAREN LET REC IN EQUAL COMMA PLUS MINUS TIMES IF COLON ELSECOLON TRUE FALSE ISBOOL ISNUM ISTUPLE LAMBDA EQEQ LESS GREATER PRINT PRINTSTACK EOF LESSEQ GREATEREQ AND OR NOT GETS BEGIN END SEMI

%left PLUS MINUS TIMES GREATER LESS EQEQ LESSEQ GREATEREQ AND OR


%type <(Lexing.position * Lexing.position) Types.program> program

%start program

%%

const :
  | NUM { ENumber($1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | TRUE { EBool(true, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | FALSE { EBool(false, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }

prim1 :
  | ADD1 { Add1 }
  | SUB1 { Sub1 }
  | NOT { Not }
  | PRINT { Print }
  | ISBOOL { IsBool }
  | ISNUM { IsNum }
  | ISTUPLE { IsTuple }
  | PRINTSTACK { PrintStack }

binds :
  | ID EQUAL expr { [($1, $3, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))] }
  | ID EQUAL expr COMMA binds { ($1, $3, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))::$5 }

ids :
  | ID { [$1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())] }
  | ID COMMA ids { ($1, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))::$3 }

exprs :
  | expr { [$1] }
  | expr COMMA exprs { $1::$3 }

tuple_exprs :
  | expr COMMA { [$1] }
  | expr COMMA exprs { $1::$3 }

simple_expr :
  | prim1 LPAREN expr RPAREN { EPrim1($1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | LPAREN tuple_exprs RPAREN { ETuple($2, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | LPAREN expr RPAREN { $2 }
  | simple_expr LPAREN exprs RPAREN { EApp($1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | simple_expr LPAREN RPAREN { EApp($1, [], (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | LPAREN LAMBDA ids COLON expr RPAREN { ELambda($3, $5, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | LPAREN LAMBDA COLON expr RPAREN { ELambda([], $4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | simple_expr LBRACK binop_expr RBRACK { EGetItem($1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | simple_expr LBRACK binop_expr RBRACK GETS binop_expr { ESetItem($1, $3, $6, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | BEGIN block_exprs END { ESeq($2, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | const { $1 }
  | ID { EId($1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }

block_exprs :
  | expr { [$1] }
  | expr SEMI { [$1] }
  | expr SEMI block_exprs { $1::$3 }

binop_expr :
  | binop_expr PLUS binop_expr { EPrim2(Plus, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr MINUS binop_expr { EPrim2(Minus, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr TIMES binop_expr { EPrim2(Times, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr EQEQ binop_expr { EPrim2(Eq, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr LESS binop_expr { EPrim2(Less, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr GREATER binop_expr { EPrim2(Greater, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr LESSEQ binop_expr { EPrim2(LessEq, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr GREATEREQ binop_expr { EPrim2(GreaterEq, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr AND binop_expr { EPrim2(And, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr OR binop_expr { EPrim2(Or, $1, $3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | simple_expr { $1 }

expr :
  | LET binds IN expr { ELet($2, $4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | LET REC binds IN expr { ELetRec($3, $5, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | IF expr COLON expr ELSECOLON expr { EIf($2, $4, $6, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) }
  | binop_expr { $1 }

program : expr EOF { $1 }

%%

