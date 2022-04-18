%{
(* These opens are used inside the actual parser *)
open Parsetree
open Ast_helper
open Asttypes

(* Including the Parser_extra file allows it to be written in Reason and have editor tooling *)
include Parser_header

(* https://github.com/ocaml/dune/issues/2450 *)
module Grain_parsing = struct end
%}


%token <string> NUMBER_INT
%token <string> NUMBER_FLOAT
%token <string> INT32
%token <string> INT64
%token <string> FLOAT32
%token <string> FLOAT64
%token <string> WASMI32
%token <string> WASMI64
%token <string> WASMF32
%token <string> WASMF64
%token <string> ID
%token <string> TYPEID
%token <string> STRING
%token <string> CHAR
%token LBRACK LBRACKRCARET RBRACK LPAREN RPAREN LBRACE RBRACE LCARET RCARET
%token LCARETLCARET RCARETRCARET RCARETRCARETRCARET
%token CARET
%token COMMA SEMI AS
%token THICKARROW ARROW
%token IS ISNT EQEQ LESSEQ GREATEREQ
%token NOTEQ EQUAL GETS
%token UNDERSCORE
%token COLON DOT ELLIPSIS

%token ASSERT FAIL EXCEPTION THROW

%token PLUS PLUSPLUS DASH STAR SLASH PERCENT
%token PLUSEQ DASHEQ STAREQ SLASHEQ PERCENTEQ
%token TRUE FALSE VOID

%token LET MUT REC IF WHEN ELSE MATCH WHILE FOR CONTINUE BREAK
%token AMPAMP PIPEPIPE NOT AT
%token AMP PIPE

%token ENUM RECORD TYPE IMPORT EXPORT FOREIGN WASM PRIMITIVE
%token EXCEPT FROM
%token EOL EOF

// reserved tokens
%token TRY CATCH COLONCOLON

// Not a real token, this is injected by the lexer
%token FUN

/* Operator precedence may be found in /docs/contributor/operator_precedence.md */

%nonassoc _below_infix

%left PIPEPIPE
%left AMPAMP
%left PIPE
%left CARET
%left AMP
%left EQEQ NOTEQ IS ISNT
%left LCARET LESSEQ RCARET GREATEREQ
%left LCARETLCARET
%left PLUS DASH PLUSPLUS
%left STAR SLASH PERCENT

%right SEMI EOL COMMA DOT COLON

%nonassoc _if
%nonassoc ELSE


%start <Parsetree.parsed_program> program

// This causes a syntax error to bubble up to a higher construct.
// This helps provide more useful error messages, e.g. with the
// expression `foo(bar baz` you'll get something like "Expected a
// closing paren for the function call" rather than "A plus sign
// could work here"
%on_error_reduce
  eol+
  eols
  record_get
  array_get
  non_assign_expr
  annotated_expr
  simple_expr
  binop_expr
  expr
  typ
  data_typ
  id
  id_expr
  lbrace
  lbrack
  lbrackrcaret
  lparen
  lcaret
  comma
  eos
  arrow
  thickarrow
  equal
  const
  pattern
  type_id
  value_binds

%%

// List helpers. These will parse a left-associated list.
// Solves the shift/reduce conflicts you get from Menhir's
// built-in lists.

lseparated_nonempty_list_inner(sep, X):
  | lseparated_nonempty_list_inner(sep, X) sep X { $3::$1 }
  | X { [$1] }

%inline lseparated_nonempty_list(sep, X):
  | lseparated_nonempty_list_inner(sep, X) { List.rev $1 }

%inline lseparated_list(sep, X):
  | { [] }
  | lseparated_nonempty_list(sep, X) { $1 }

// The Grain grammar.

%inline eol:
  | EOL {}

eols:
  | eol+ {}

%inline opt_eols:
  | ioption(eols) {}

eos:
  | eols {}
  | SEMI opt_eols {}

lbrack:
  | LBRACK opt_eols %prec EOL {}

lbrackrcaret:
  | LBRACKRCARET opt_eols %prec EOL {}

rbrack:
  | opt_eols RBRACK {}

lparen:
  | LPAREN opt_eols %prec EOL {}

rparen:
  | opt_eols RPAREN {}

lbrace:
  | LBRACE opt_eols %prec EOL {}

rbrace:
  | opt_eols RBRACE {}

lcaret:
  | LCARET opt_eols {}

rcaret:
  | opt_eols RCARET {}

comma:
  | COMMA opt_eols %prec COMMA {}

%inline colon:
  | COLON opt_eols {}

%inline dot:
  | DOT opt_eols {}

arrow:
  | ARROW opt_eols {}

thickarrow:
  | THICKARROW opt_eols {}

equal:
  | EQUAL opt_eols {}

const:
  // Rational literals are a special case of the division binop_expr.
  | dash_op? NUMBER_INT { Const.number (PConstNumberInt (if Option.is_some $1 then "-" ^ $2 else $2)), $sloc }
  | dash_op? NUMBER_FLOAT { Const.number (PConstNumberFloat (if Option.is_some $1 then "-" ^ $2 else $2)), $sloc }
  | dash_op? INT32 { Const.int32 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? INT64 { Const.int64 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? FLOAT32 { Const.float32 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? FLOAT64 { Const.float64 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? WASMI32 { Const.wasmi32 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? WASMI64 { Const.wasmi64 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? WASMF32 { Const.wasmf32 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | dash_op? WASMF64 { Const.wasmf64 (if Option.is_some $1 then "-" ^ $2 else $2), $sloc }
  | TRUE { Const.bool true, $loc }
  | FALSE { Const.bool false, $loc }
  | VOID { Const.void, $loc }
  | STRING { Const.string $1, $loc }
  | CHAR { Const.char $1, $loc }

expr:
  | stmt_expr { $1 }
  // allow infix operators to cause a shift
  | non_stmt_expr %prec _below_infix { $1 }

non_binop_expr:
  | lam_expr { $1 }
  | non_assign_expr { $1 }
  | assign_expr { $1 }

non_stmt_expr:
  | binop_expr { $1 }
  | annotated_expr { $1 }

annotated_expr:
  | non_binop_expr %prec COLON { $1 }
  | non_binop_expr colon typ { Exp.constraint_ ~loc:(to_loc $loc) $1 $3 }

binop_expr:
  | non_stmt_expr infix_op opt_eols non_stmt_expr { Exp.binop ~loc:(to_loc $loc) (mkid_expr $loc($2) [$2]) [$1; $4] }

ellipsis_prefix(X):
  | ELLIPSIS X {$2}

pattern:
  | pattern colon typ { Pat.constraint_ ~loc:(to_loc $loc) $1 $3 }
  | UNDERSCORE { Pat.any ~loc:(to_loc $loc) () }
  | const { Pat.constant ~loc:(to_loc (snd $1)) (fst $1) }
  // Allow rational numbers in patterns
  | dash_op? NUMBER_INT slash_op dash_op? NUMBER_INT { Pat.constant ~loc:(to_loc $sloc) @@ Const.number (PConstNumberRational ((if Option.is_some $1 then "-" ^ $2 else $2), (if Option.is_some $4 then "-" ^ $5 else $5))) }
  | ID { Pat.var ~loc:(to_loc $loc) (mkstr $loc $1) }
  | special_id { Pat.var ~loc:(to_loc $loc) (mkstr $loc $1) }
  | primitive_ { Pat.var ~loc:(to_loc $loc) (mkstr $loc $1) }
  | lparen tuple_patterns rparen { Pat.tuple ~loc:(to_loc $loc) $2 }
  | lbrackrcaret patterns rbrack { Pat.array ~loc:(to_loc $loc) $2 }
  | lbrackrcaret rbrack { Pat.array ~loc:(to_loc $loc) [] }
  | lparen pattern rparen { $2 }
  | lbrace record_patterns rbrace { Pat.record ~loc:(to_loc $loc) $2 }
  | type_id lparen patterns rparen { Pat.construct ~loc:(to_loc $loc) $1 $3 }
  | type_id { Pat.construct ~loc:(to_loc $loc) $1 [] }
  | lbrack rbrack { Pat.list ~loc:(to_loc $loc) [] }
  | lbrack lseparated_nonempty_list(comma, list_item_pat) comma? rbrack { Pat.list ~loc:(to_loc $loc) $2 }

list_item_pat:
  | ELLIPSIS pattern { ListSpread ($2, to_loc $loc) }
  | pattern { ListItem $1 }

patterns:
  | lseparated_nonempty_list(comma, pattern) comma? { $1 }

%inline tuple_pattern_ending:
  | ioption(eols) lseparated_nonempty_list(comma, pattern) ioption(comma) { $2 }

tuple_patterns:
  | pattern COMMA ioption(tuple_pattern_ending) { $1::(Option.value ~default:[] $3) }

record_patterns:
  | lseparated_nonempty_list(comma, record_pattern) comma? { $1 }

record_pattern:
  | UNDERSCORE { None, Open }
  | id colon pattern { Some($1, $3), Closed }
  | id { Some($1, Pat.var ~loc:(to_loc $loc) (mkstr $loc (Identifier.last $1.txt))), Closed }

data_typ:
  | type_id lcaret typs rcaret { Typ.constr ~loc:(to_loc $loc) $1 $3 }
  // Resolve Foo < n > abiguity in favor of the type vector
  | type_id %prec _below_infix { Typ.constr ~loc:(to_loc $loc) $1 [] }

typ:
  | data_typ arrow typ { Typ.arrow ~loc:(to_loc $loc) [$1] $3 }
  | FUN ID arrow typ { Typ.arrow ~loc:(to_loc $loc) [(Typ.var $2)] $4 }
  | FUN lparen typs? rparen arrow typ { Typ.arrow ~loc:(to_loc $loc) (Option.value ~default:[] $3) $6 }
  | lparen tuple_typs rparen { Typ.tuple ~loc:(to_loc $loc) $2 }
  | lparen typ rparen { $2 }
  | ID { Typ.var ~loc:(to_loc $loc) $1 }
  | data_typ { $1 }

typs:
  | lseparated_nonempty_list(comma, typ) comma? { $1 }

%inline tuple_typ_ending:
  | ioption(eols) lseparated_nonempty_list(comma, typ) ioption(comma) { $2 }

tuple_typs:
  | typ COMMA ioption(tuple_typ_ending) { $1::(Option.value ~default:[] $3) }

value_bind:
  | pattern equal expr { Vb.mk ~loc:(to_loc $loc) $1 $3 }

value_binds:
  | lseparated_nonempty_list(comma, value_bind) { $1 }

import_exception:
  | EXCEPT lbrace lseparated_nonempty_list(comma, id) comma? rbrace {$3}

as_prefix(X):
  | AS X {$2}

aliasable(X):
  | X as_prefix(X)? {($1, $2)}

import_ids:
  | lseparated_nonempty_list(comma, aliasable(id)) comma? {$1}

import_shape:
  | id { PImportModule $1 }
  | STAR import_exception? { PImportAllExcept (Option.value ~default:[] $2) }
  | lbrace import_ids? rbrace { PImportValues (Option.value ~default:[] $2) }

import_stmt:
  | IMPORT lseparated_nonempty_list(comma, import_shape) comma? FROM file_path { Imp.mk ~loc:(to_loc $loc) $2 $5 }

data_declaration_stmt:
  | EXPORT data_declaration { (Exported, $2) }
  | data_declaration { (Nonexported, $1) }

data_declaration_stmts:
  | separated_nonempty_list(comma, data_declaration_stmt) { $1 }

export_exception:
  | EXCEPT lseparated_nonempty_list(comma, export_id_str) {$2}

export_stmt:
  | attributes EXPORT LET REC value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Exported Recursive Immutable $5 }
  | attributes EXPORT LET value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Exported Nonrecursive Immutable $4 }
  | attributes EXPORT LET REC MUT value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Exported Recursive Mutable $6 }
  | attributes EXPORT LET MUT value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Exported Nonrecursive Mutable $5 }
  | attributes EXPORT foreign_stmt { Top.foreign ~loc:(to_loc $sloc) ~attributes:$1 Exported $3 }
  | attributes EXPORT primitive_stmt { Top.primitive ~loc:(to_loc $sloc) ~attributes:$1 Exported $3 }
  | attributes EXPORT exception_stmt { Top.grain_exception ~loc:(to_loc $sloc) ~attributes:$1 Exported $3 }
  | attributes EXPORT separated_nonempty_list(comma, aliasable(any_id_str)) { Top.export ~loc:(to_loc $sloc) ~attributes:$1 (Ex.mk ~loc:(to_loc $loc($3)) $3) }
  | attributes EXPORT STAR export_exception? { Top.export_all ~loc:(to_loc $sloc) ~attributes:$1 (Option.value ~default:[] $4) }

data_constructor:
  | TYPEID { CDecl.singleton ~loc:(to_loc $loc) (mkstr $loc $1) }
  | TYPEID lparen typs? rparen { CDecl.tuple ~loc:(to_loc $loc) (mkstr $loc $1) (Option.value ~default:[] $3) }
  /* Special support for lists */
  | lbrack rbrack { CDecl.singleton ~loc:(to_loc $loc) (mkstr $loc "[]") }
  | lbrack ELLIPSIS rbrack lparen typs? rparen { CDecl.tuple ~loc:(to_loc $loc) (mkstr $loc "[...]") (Option.value ~default:[] $5) }

data_constructors:
  | lbrace lseparated_nonempty_list(comma, data_constructor) comma? rbrace { $2 }

data_label:
  | simple_id colon typ { LDecl.mk ~loc:(to_loc $loc) $1 $3 Immutable }
  | MUT simple_id colon typ { LDecl.mk ~loc:(to_loc $loc) $2 $4 Mutable }

data_labels:
  | lbrace lseparated_nonempty_list(comma, data_label) comma? rbrace { $2 }

id_typ:
  | ID { Typ.var ~loc:(to_loc $loc) $1 }

id_vec:
  | lcaret lseparated_nonempty_list(comma, id_typ) comma? rcaret {$2}

data_declaration:
  | TYPE TYPEID id_vec? equal typ { Dat.abstract ~loc:(to_loc $loc) (mkstr $loc($2) $2) (Option.value ~default:[] $3) (Some $5) }
  | ENUM TYPEID id_vec? data_constructors { Dat.variant ~loc:(to_loc $loc) (mkstr $loc($2) $2) (Option.value ~default:[] $3) $4 }
  | RECORD TYPEID id_vec? data_labels { Dat.record ~loc:(to_loc $loc) (mkstr $loc($2) $2) (Option.value ~default:[] $3) $4 }

prim1_expr:
  | NOT non_assign_expr { Exp.apply ~loc:(to_loc $loc) (mkid_expr $loc($1) ["!"]) [$2] }

paren_expr:
  | lparen expr rparen { $2 }

app_expr:
  | left_accessor_expr lparen lseparated_list(comma, expr) comma? rparen { Exp.apply ~loc:(to_loc $loc) $1 $3 }

// These are all inlined to carry over their precedence.
%inline plus_op:
  | PLUS { "+" }
%inline plusplus_op:
  | PLUSPLUS { "++" }
%inline dash_op:
  | DASH { "-" }
%inline star_op:
  | STAR { "*" }
%inline slash_op:
  | SLASH { "/" }
%inline percent_op:
  | PERCENT { "%" }
%inline is_op:
  | IS { "is" }
%inline isnt_op:
  | ISNT { "isnt" }
%inline eqeq_op:
  | EQEQ { "==" }
%inline noteq_op:
  | NOTEQ { "!=" }
%inline caret_op:
  | CARET { "^" }
%inline lcaret_op:
  | LCARET { "<" }
%inline llcaret_op:
  | LCARETLCARET { "<<" }
%inline rcaret_op:
  | RCARET { ">" }
%inline rrcaret_op:
  | RCARET RCARET { ">>" }
%inline rrrcaret_op:
  | RCARET RCARET RCARET { ">>>" }
%inline lesseq_op:
  | LESSEQ { "<=" }
%inline greatereq_op:
  | GREATEREQ { ">=" }
%inline amp_op:
  | AMP { "&" }
%inline ampamp_op:
  | AMPAMP { "&&" }
%inline pipe_op:
  | PIPE { "|" }
%inline pipepipe_op:
  | PIPEPIPE { "||" }
%inline pluseq_op:
  | PLUSEQ { "+" }
%inline dasheq_op:
  | DASHEQ { "-" }
%inline stareq_op:
  | STAREQ { "*" }
%inline slasheq_op:
  | SLASHEQ { "/" }
%inline percenteq_op:
  | PERCENTEQ { "%" }

%inline infix_op:
  | plus_op
  | dash_op
  | star_op
  | slash_op
  | percent_op
  | is_op
  | isnt_op
  | eqeq_op
  | plusplus_op
  | noteq_op
  | caret_op
  | lcaret_op
  | llcaret_op
  | rcaret_op
  | rrcaret_op
  | rrrcaret_op
  | lesseq_op
  | greatereq_op
  | amp_op
  | ampamp_op
  | pipe_op
  | pipepipe_op {$1}

prefix_op:
  | NOT { "!" }

primitive_:
  | ASSERT { "assert" }
  | THROW { "throw" }
  | FAIL { "fail" }

special_op:
  | infix_op | prefix_op {$1}

%inline special_id:
  | lparen special_op rparen { $2 }

%inline modid:
  | lseparated_nonempty_list(dot, TYPEID) { $1 }

non_modid:
  | ID
  | special_id { [$1] }

id:
  | modid dot non_modid { mkid (List.append $1 $3) (to_loc $loc) }
  | modid %prec DOT { (mkid $1) (to_loc $loc) }
  | non_modid { (mkid $1) (to_loc $loc) }

simple_id:
  | ID { (mkid [$1]) (to_loc $loc) }

type_id:
  | lseparated_nonempty_list(dot, TYPEID) { (mkid $1) (to_loc $loc) }

id_expr:
  // Force any following colon to cause a shift
  | id %prec COLON { Exp.ident ~loc:(to_loc $loc) $1 }

simple_expr:
  | const { Exp.constant ~loc:(to_loc (snd $1)) (fst $1) }
  | lparen tuple_exprs rparen { Exp.tuple ~loc:(to_loc $loc) $2 }
  | id_expr { $1 }

braced_expr:
  | lbrace block_body rbrace { Exp.block ~loc:(to_loc $loc) $2 }
  | lbrace record_exprs rbrace { Exp.record ~loc:(to_loc $loc) $2 }

block:
  | lbrace block_body rbrace { Exp.block ~loc:(to_loc $loc) $2 }

lam_expr:
  | FUN lparen patterns? rparen thickarrow expr { Exp.lambda ~loc:(to_loc $loc) (Option.value ~default:[] $3) $6 }
  | FUN ID thickarrow expr { Exp.lambda ~loc:(to_loc $loc) [Pat.var ~loc:(to_loc $loc($2)) (mkstr $loc($2) $2)] $4 }

attribute_argument:
  | STRING { mkstr $loc $1 }

attribute_arguments:
  | lparen lseparated_list(comma, attribute_argument) rparen { $2 }

attribute:
  | AT id_str loption(attribute_arguments) opt_eols { $2, $3 }

attributes:
  | attribute* { $1 }

let_expr:
  | attributes LET REC value_binds { Exp.let_ ~loc:(to_loc $sloc) ~attributes:$1 Recursive Immutable $4 }
  | attributes LET value_binds { Exp.let_ ~loc:(to_loc $sloc) ~attributes:$1 Nonrecursive Immutable $3 }
  | attributes LET REC MUT value_binds { Exp.let_ ~loc:(to_loc $sloc) ~attributes:$1 Recursive Mutable $5 }
  | attributes LET MUT value_binds { Exp.let_ ~loc:(to_loc $sloc) ~attributes:$1 Nonrecursive Mutable $4 }

%inline else_expr:
  | ELSE opt_eols expr { $3 }

if_expr:
  | IF lparen expr rparen opt_eols expr ioption(else_expr) %prec _if { Exp.if_ ~loc:(to_loc $loc) $3 $6 (Option.value ~default:(Exp.block ~loc:(to_loc $loc($7)) []) $7) }

while_expr:
  | WHILE lparen expr rparen block { Exp.while_ ~loc:(to_loc $loc) $3 $5 }

for_inner_expr:
  | %prec EOL { None }
  | expr { Some $1 }

for_expr:
  | FOR lparen block_body_expr? opt_eols SEMI opt_eols for_inner_expr opt_eols SEMI opt_eols for_inner_expr rparen block { Exp.for_ ~loc:(to_loc $loc) $3 $7 $11 $13 }

when_guard:
  | opt_eols WHEN expr { $3 }

match_branch:
  | pattern ioption(when_guard) thickarrow expr { Mb.mk ~loc:(to_loc $loc) $1 $4 $2 }

match_branches:
  | lseparated_nonempty_list(comma, match_branch) comma? { $1 }

match_expr:
  | MATCH lparen expr rparen lbrace match_branches rbrace { Exp.match_ ~loc:(to_loc $loc) $3 $6 }

list_item:
  | ELLIPSIS expr { ListSpread ($2, to_loc $loc) }
  | expr { ListItem $1 }

list_expr:
  | lbrack rbrack { Exp.list ~loc:(to_loc $loc) [] }
  | lbrack lseparated_nonempty_list(comma, list_item) comma? rbrack { Exp.list ~loc:(to_loc $loc) $2 }

array_expr:
  | lbrackrcaret rbrack { Exp.array ~loc:(to_loc $loc) [] }
  | lbrackrcaret opt_eols lseparated_nonempty_list(comma, expr) comma? rbrack { Exp.array ~loc:(to_loc $loc) $3 }

stmt_expr:
  | THROW expr { Exp.apply ~loc:(to_loc $loc) (mkid_expr $loc($1) ["throw"]) [$2] }
  | ASSERT expr { Exp.apply ~loc:(to_loc $loc) (mkid_expr $loc($1) ["assert"]) [$2] }
  | FAIL expr { Exp.apply ~loc:(to_loc $loc) (mkid_expr $loc($1) ["fail"]) [$2] }
  | CONTINUE { Exp.continue ~loc:(to_loc $loc) () }
  | BREAK { Exp.break ~loc:(to_loc $loc) () }

assign_binop_op:
  | pluseq_op
  | dasheq_op
  | stareq_op
  | slasheq_op
  | percenteq_op {$1}

assign_expr:
  | left_accessor_expr GETS opt_eols expr { Exp.box_assign ~loc:(to_loc $loc) $1 $4 }
  | id_expr equal expr { Exp.assign ~loc:(to_loc $loc) $1 $3 }
  | id_expr assign_binop_op opt_eols expr { Exp.assign ~loc:(to_loc $loc) $1 (Exp.apply ~loc:(to_loc $loc) (mkid_expr $loc($2) [$2]) [$1; $4]) }
  | record_set { $1 }
  | array_set { $1 }

non_assign_expr:
  | app_expr    { $1 }
  | prim1_expr  { $1 }
  | simple_expr { $1 }
  | record_get  { $1 }
  | paren_expr  { $1 }
  | braced_expr { $1 }
  | if_expr     { $1 }
  | while_expr  { $1 }
  | for_expr    { $1 }
  | match_expr  { $1 }
  | list_expr   { $1 }
  | array_get   { $1 }
  | array_expr  { $1 }

%inline left_accessor_expr:
  | app_expr    { $1 }
  | simple_expr { $1 }
  | array_get   { $1 }
  | record_get  { $1 }
  | paren_expr  { $1 }
  | braced_expr { $1 }

block_body_expr:
  | let_expr    { $1 }
  | expr  { $1 }

%inline tuple_expr_ending:
  | ioption(eols) lseparated_nonempty_list(comma, expr) comma? { $2 }

tuple_exprs:
  | expr COMMA ioption(tuple_expr_ending) { $1::(Option.value ~default:[] $3) }

array_get:
  | left_accessor_expr lbrack expr rbrack { Exp.array_get ~loc:(to_loc $loc) $1 $3 }

array_set:
  | left_accessor_expr lbrack expr rbrack equal expr { Exp.array_set ~loc:(to_loc $loc) $1 $3 $6 }

record_get:
  | left_accessor_expr dot simple_id { Exp.record_get ~loc:(to_loc $loc) $1 $3 }

record_set:
  | left_accessor_expr dot simple_id equal expr { Exp.record_set ~loc:(to_loc $loc) $1 $3 $5 }

%inline record_field_value:
  | colon expr {$2}

punned_record_field:
  | id { $1, (Exp.ident ~loc:(to_loc $loc) $1) }

non_punned_record_field:
  | id record_field_value { $1, $2 }

%inline record_field:
  | punned_record_field { $1 }
  | non_punned_record_field { $1 }

record_exprs:
  // Don't ever parse {x} as a record
  | non_punned_record_field comma? { [$1] }
  | punned_record_field comma { [$1] }
  | record_field comma lseparated_nonempty_list(comma, record_field) comma? { $1::$3 }

block_body:
  | lseparated_nonempty_list(eos, block_body_expr) ioption(eos) %prec SEMI { $1 }

file_path:
  | STRING { Location.mkloc $1 (to_loc $loc) }

id_str:
  | ID { Location.mkloc $1 (to_loc $loc) }
  | special_id { Location.mkloc $1 (to_loc $loc) }

type_id_str:
  | TYPEID { Location.mkloc $1 (to_loc $loc) }

any_id_str:
  | id_str { $1 }
  | type_id_str { $1 }

export_id_str:
  | id_str { ExportExceptValue $1 }
  | type_id_str { ExportExceptData $1 }

foreign_stmt:
  | FOREIGN WASM id_str colon typ as_prefix(id_str)? FROM file_path { Val.mk ~loc:(to_loc $loc) ~mod_:$8 ~name:$3 ~alias:$6 ~typ:$5 ~prim:[] () }

prim:
  | primitive_ { Location.mkloc $1 (to_loc $loc) }

primitive_stmt:
  | PRIMITIVE id_str colon typ equal STRING { Val.mk ~loc:(to_loc $loc) ~mod_:{$2 with txt="primitive"} ~name:$2 ~alias:None ~typ:$4 ~prim:[$6] () }
  | PRIMITIVE prim colon typ equal STRING { Val.mk ~loc:(to_loc $loc) ~mod_:{$2 with txt="primitive"} ~name:$2 ~alias:None ~typ:$4 ~prim:[$6] () }

exception_stmt:
  | EXCEPTION type_id_str { Except.singleton ~loc:(to_loc $loc) $2 }
  | EXCEPTION type_id_str lparen typs? rparen { Except.tuple ~loc:(to_loc $loc) $2 (Option.value ~default:[] $4) }

toplevel_stmt:
  | attributes LET REC value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Nonexported Recursive Immutable $4 }
  | attributes LET value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Nonexported Nonrecursive Immutable $3 }
  | attributes LET REC MUT value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Nonexported Recursive Mutable $5 }
  | attributes LET MUT value_binds { Top.let_ ~loc:(to_loc $sloc) ~attributes:$1 Nonexported Nonrecursive Mutable $4 }
  | attributes data_declaration_stmts { Top.data ~loc:(to_loc $sloc) ~attributes:$1 $2 }
  | attributes IMPORT foreign_stmt { Top.foreign ~loc:(to_loc $loc) ~attributes:$1 Nonexported $3 }
  | attributes import_stmt { Top.import ~loc:(to_loc $loc) ~attributes:$1 $2 }
  | expr { Top.expr ~loc:(to_loc $loc) $1 }
  | export_stmt { $1 }
  | primitive_stmt { Top.primitive ~loc:(to_loc $loc) Nonexported $1 }
  | exception_stmt { Top.grain_exception ~loc:(to_loc $loc) Nonexported $1 }

toplevel_stmts:
  | lseparated_nonempty_list(eos, toplevel_stmt) eos? { $1 }

program:
  | opt_eols toplevel_stmts EOF { make_program $2 }
