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


%token <string> RATIONAL
%token <string> NUMBER_INT NUMBER_FLOAT
%token <string> INT8 INT16 INT32 INT64 UINT8 UINT16 UINT32 UINT64 FLOAT32 FLOAT64 BIGINT
%token <string> WASMI32 WASMI64 WASMF32 WASMF64
%token <string> LIDENT UIDENT
%token <string> STRING BYTES CHAR
%token LBRACK LBRACKRCARET RBRACK LPAREN RPAREN LBRACE RBRACE LCARET RCARET
%token COMMA SEMI AS
%token THICKARROW ARROW
%token EQUAL GETS
%token UNDERSCORE
%token COLON QUESTION DOT ELLIPSIS

%token ASSERT FAIL EXCEPTION THROW

%token TRUE FALSE VOID

%token LET MUT REC IF WHEN ELSE MATCH WHILE FOR CONTINUE BREAK RETURN
%token AT

%token <string> INFIX_10 INFIX_30 INFIX_40 INFIX_50 INFIX_60 INFIX_70
%token <string> INFIX_80 INFIX_90 INFIX_100 INFIX_110 INFIX_120
%token <string> PREFIX_150
%token <string> INFIX_ASSIGNMENT_10

%token ENUM RECORD TYPE MODULE INCLUDE USE PROVIDE ABSTRACT FOREIGN WASM PRIMITIVE
%token AND
%token EXCEPT FROM STAR
%token SLASH DASH PIPE
%token EOL EOF

// reserved tokens
%token TRY CATCH COLONCOLON MACRO YIELD

// Not a real token, this is injected by the lexer
%token FUN

/* Operator precedence may be found in /docs/contributor/operator_precedence.md */

%nonassoc _below_infix

%left AS
%left INFIX_30
%left INFIX_40
%left INFIX_50 PIPE
%left INFIX_60
%left INFIX_70
%left INFIX_80
%left INFIX_90 LCARET RCARET
%left INFIX_100
%left INFIX_110 DASH
%left INFIX_120 STAR SLASH

%right SEMI EOL COMMA DOT COLON LPAREN EQUAL

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
  qualified_uid
  qualified_lid
  value_binds
  construct_expr
  app_arg
  arg_default

%%

// List helpers. These will parse a left-associated list.
// Solves the shift/reduce conflicts you get from Menhir's
// built-in lists.

lnonempty_list_inner(X):
  | lnonempty_list_inner(X) X { $2::$1 }
  | X { [$1] }

%inline lnonempty_list(X):
  | lnonempty_list_inner(X) { List.rev $1 }

%inline llist(X):
  | { [] }
  | lnonempty_list(X) { $1 }

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

either_arrow:
  | arrow {}
  | thickarrow {}

equal:
  | EQUAL opt_eols {}

const:
  // Rational literals are a special case of the division binop_expr.
  | NUMBER_INT { Constant.number (PConstNumberInt (mkstr $loc $1)) }
  | NUMBER_FLOAT { Constant.number (PConstNumberFloat (mkstr $loc $1)) }
  | INT8 { Constant.int8 (mkstr $loc $1) }
  | INT16 { Constant.int16 (mkstr $loc $1) }
  | INT32 { Constant.int32 (mkstr $loc $1) }
  | INT64 { Constant.int64 (mkstr $loc $1) }
  | UINT8 { Constant.uint8 (mkstr $loc $1) }
  | UINT16 { Constant.uint16 (mkstr $loc $1) }
  | UINT32 { Constant.uint32 (mkstr $loc $1) }
  | UINT64 { Constant.uint64 (mkstr $loc $1) }
  | FLOAT32 { Constant.float32 (mkstr $loc $1) }
  | FLOAT64 { Constant.float64 (mkstr $loc $1) }
  | WASMI32 { Constant.wasmi32 (mkstr $loc $1) }
  | WASMI64 { Constant.wasmi64 (mkstr $loc $1) }
  | WASMF32 { Constant.wasmf32 (mkstr $loc $1) }
  | WASMF64 { Constant.wasmf64 (mkstr $loc $1) }
  | BIGINT { Constant.bigint (mkstr $loc $1) }
  | RATIONAL { Constant.rational (mkstr $loc $1) }
  // The minus sign is not an optional non-terminal or inlined to allow propagation
  // of correct locations, as $sloc only applies to the current rule.
  | DASH NUMBER_INT { Constant.number (PConstNumberInt (mkstr $loc ("-" ^ $2))) }
  | DASH NUMBER_FLOAT { Constant.number (PConstNumberFloat (mkstr $loc ("-" ^ $2))) }
  | DASH INT8 { Constant.int8 (mkstr $loc ("-" ^ $2)) }
  | DASH INT16 { Constant.int16 (mkstr $loc ("-" ^ $2)) }
  | DASH INT32 { Constant.int32 (mkstr $loc ("-" ^ $2)) }
  | DASH INT64 { Constant.int64 (mkstr $loc ("-" ^ $2)) }
  | DASH UINT8 { Constant.uint8 (mkstr $loc ("-" ^ $2)) }
  | DASH UINT16 { Constant.uint16 (mkstr $loc ("-" ^ $2)) }
  | DASH UINT32 { Constant.uint32 (mkstr $loc ("-" ^ $2)) }
  | DASH UINT64 { Constant.uint64 (mkstr $loc ("-" ^ $2)) }
  | DASH FLOAT32 { Constant.float32 (mkstr $loc ("-" ^ $2)) }
  | DASH FLOAT64 { Constant.float64 (mkstr $loc ("-" ^ $2)) }
  | DASH WASMI32 { Constant.wasmi32 (mkstr $loc ("-" ^ $2)) }
  | DASH WASMI64 { Constant.wasmi64 (mkstr $loc ("-" ^ $2)) }
  | DASH WASMF32 { Constant.wasmf32 (mkstr $loc ("-" ^ $2)) }
  | DASH WASMF64 { Constant.wasmf64 (mkstr $loc ("-" ^ $2)) }
  | DASH BIGINT { Constant.bigint (mkstr $loc ("-" ^ $2)) }
  | DASH RATIONAL { Constant.rational (mkstr $loc ("-" ^ $2)) }
  | TRUE { Constant.bool true }
  | FALSE { Constant.bool false }
  | VOID { Constant.void }
  | STRING { Constant.string (mkstr $loc $1) }
  | BYTES { Constant.bytes (mkstr $loc $1) }
  | CHAR { Constant.char (mkstr $loc $1) }

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
  | non_binop_expr colon typ { Expression.constraint_ ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }

binop_expr:
  | non_stmt_expr infix_op opt_eols non_stmt_expr { Expression.binop ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($2) [mkstr $loc($2) $2]) $1 $4 }
  | non_stmt_expr rcaret_rcaret_op opt_eols non_stmt_expr %prec INFIX_100 { Expression.binop ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($2) [mkstr $loc($2) $2]) $1 $4 }

ellipsis_prefix(X):
  | ELLIPSIS X {$2}

pattern:
  | pattern colon typ { Pattern.constraint_ ~loc:(to_loc $loc) $1 $3 }
  | UNDERSCORE { Pattern.any ~loc:(to_loc $loc) () }
  | const { Pattern.constant ~loc:(to_loc $loc) $1 }
  // Allow rational numbers in patterns
  | NUMBER_INT SLASH DASH? NUMBER_INT { Pattern.constant ~loc:(to_loc $sloc) @@ Constant.number (Number.rational (mkstr $loc($1) $1) (to_loc($loc($2))) (if Option.is_some $3 then (mkstr (fst $loc($3), snd $loc($4)) ("-" ^ $4)) else mkstr $loc($4) $4)) }
  | DASH NUMBER_INT SLASH DASH? NUMBER_INT { Pattern.constant ~loc:(to_loc $sloc) @@ Constant.number (Number.rational (mkstr (fst $loc($1), snd $loc($2)) ("-" ^ $2)) (to_loc($loc($3))) (if Option.is_some $4 then (mkstr (fst $loc($4), snd $loc($5)) ("-" ^ $5)) else mkstr $loc($5) $5)) }
  | LIDENT { Pattern.var ~loc:(to_loc $loc) (mkstr $loc $1) }
  | special_id { Pattern.var ~loc:(to_loc $loc) $1 }
  | primitive_ { Pattern.var ~loc:(to_loc $loc) (mkstr $loc $1) }
  | lparen tuple_patterns rparen { Pattern.tuple ~loc:(to_loc $loc) $2 }
  | lbrackrcaret patterns rbrack { Pattern.array ~loc:(to_loc $loc) $2 }
  | lbrackrcaret rbrack { Pattern.array ~loc:(to_loc $loc) [] }
  | lparen pattern rparen { $2 }
  | lbrace record_patterns rbrace { Pattern.record ~loc:(to_loc $loc) $2 }
  | qualified_uid lparen patterns rparen { Pattern.tuple_construct ~loc:(to_loc $loc) $1 $3 }
  | qualified_uid lbrace record_patterns rbrace { Pattern.record_construct ~loc:(to_loc $loc) $1 $3 }
  | qualified_uid { Pattern.singleton_construct ~loc:(to_loc $loc) $1 }
  | lbrack rbrack { Pattern.list ~loc:(to_loc $loc) [] }
  | lbrack lseparated_nonempty_list(comma, list_item_pat) comma? rbrack { Pattern.list ~loc:(to_loc $loc) $2 }
  | pattern PIPE opt_eols pattern %prec PIPE { Pattern.or_ ~loc:(to_loc $loc) $1 $4 }
  | pattern AS opt_eols id_str { Pattern.alias ~loc:(to_loc $loc) $1 $4 }

list_item_pat:
  | ELLIPSIS pattern { ListSpread ($2, to_loc $loc) }
  | pattern { ListItem $1 }

patterns:
  | lseparated_nonempty_list(comma, pattern) comma? { $1 }

%inline tuple_pattern_ending:
  | ioption(eols) lseparated_nonempty_list(comma, pattern) ioption(comma) { $2 }

tuple_patterns:
  | pattern COMMA tuple_pattern_ending { $1::$3 }

record_patterns:
  | lseparated_nonempty_list(comma, record_pattern) comma? { $1 }

record_pattern:
  | UNDERSCORE { None, Open }
  | qualified_lid colon pattern { Some($1, $3), Closed }
  | qualified_lid { Some($1, Pattern.var ~loc:(to_loc $loc) (mkstr $loc (Identifier.last $1.txt))), Closed }

data_typ:
  | qualified_uid lcaret typs rcaret { Type.constr ~loc:(to_loc $loc) $1 $3 }
  // Resolve Foo < n > abiguity in favor of the type vector
  | qualified_uid %prec _below_infix { Type.constr ~loc:(to_loc $loc) $1 [] }

typ:
  | FUN data_typ either_arrow typ { Type.arrow ~loc:(to_loc $loc) [TypeArgument.mk ~loc:(to_loc $loc($2)) Unlabeled $2] $4 }
  | FUN LIDENT either_arrow typ { Type.arrow ~loc:(to_loc $loc) [TypeArgument.mk ~loc:(to_loc $loc($2)) Unlabeled (Type.var ~loc:(to_loc $loc($2)) $2)] $4 }
  | FUN lparen arg_typs? rparen either_arrow typ { Type.arrow ~loc:(to_loc $loc) (Option.value ~default:[] $3) $6 }
  | lparen tuple_typs rparen { Type.tuple ~loc:(to_loc $loc) $2 }
  | lparen typ rparen { $2 }
  | LIDENT { Type.var ~loc:(to_loc $loc) $1 }
  | data_typ { $1 }

arg_typ:
  | LIDENT colon typ { TypeArgument.mk ~loc:(to_loc $loc) (Labeled (mkstr $loc($1) $1)) $3 }
  | QUESTION LIDENT colon typ { TypeArgument.mk ~loc:(to_loc $loc) (Default (mkstr $loc($2) $2)) $4 }
  | typ { TypeArgument.mk ~loc:(to_loc $loc) Unlabeled $1 }

typs:
  | lseparated_nonempty_list(comma, typ) comma? { $1 }

arg_typs:
  | lseparated_nonempty_list(comma, arg_typ) comma? { $1 }

%inline tuple_typ_ending:
  | ioption(eols) lseparated_nonempty_list(comma, typ) ioption(comma) { $2 }

tuple_typs:
  | typ COMMA tuple_typ_ending { $1::$3 }

value_bind:
  | pattern equal expr { ValueBinding.mk ~loc:(to_loc $loc) $1 $3 }

value_binds:
  | lseparated_nonempty_list(AND, value_bind) { $1 }

as_prefix(X):
  | AS opt_eols X {$3}

aliasable(X):
  | X as_prefix(X)? {($1, $2)}

use_item:
  | TYPE aliasable(uid) { PUseType { name=fst $2; alias = snd $2; loc=to_loc $loc} }
  | MODULE aliasable(uid) { PUseModule { name=fst $2; alias = snd $2; loc=to_loc $loc} }
  | EXCEPTION aliasable(uid) { PUseException { name=fst $2; alias = snd $2; loc=to_loc $loc} }
  | aliasable(lid) { PUseValue { name=fst $1; alias = snd $1; loc=to_loc $loc} }

use_items:
  | lseparated_nonempty_list(comma, use_item) comma? {$1}

use_shape:
  | STAR { PUseAll }
  | lbrace use_items? rbrace { PUseItems (Option.value ~default:[] $2) }

use_stmt:
  | USE qualified_uid_inline dot use_shape { Expression.use ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $2 $4 }

include_alias:
  | AS opt_eols qualified_uid { make_include_alias $3 }

include_stmt:
  | FROM file_path INCLUDE qualified_uid include_alias? { IncludeDeclaration.mk ~loc:(to_loc $loc) $2 (make_include_ident $4) $5 }

data_declaration_stmt:
  | ABSTRACT data_declaration { (Abstract, $2, to_loc($loc)) }
  | PROVIDE data_declaration { (Provided, $2, to_loc($loc)) }
  | data_declaration { (NotProvided, $1, to_loc($loc)) }

data_declaration_stmts:
  | separated_nonempty_list(AND, data_declaration_stmt) { $1 }

provide_item:
  | TYPE aliasable(uid) { PProvideType { name=fst $2; alias = snd $2; loc=to_loc $loc} }
  | MODULE aliasable(uid) { PProvideModule { name=fst $2; alias = snd $2; loc=to_loc $loc} }
  | EXCEPTION aliasable(uid) { PProvideException { name=fst $2; alias = snd $2; loc=to_loc $loc} }
  | aliasable(lid) { PProvideValue { name=fst $1; alias = snd $1; loc=to_loc $loc} }

provide_items:
  | lseparated_nonempty_list(comma, provide_item) comma? {$1}

provide_shape:
  | lbrace provide_items? rbrace { Option.value ~default:[] $2 }

provide_stmt:
  | attributes PROVIDE LET REC value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided Recursive Immutable $5 }
  | attributes PROVIDE LET value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided Nonrecursive Immutable $4 }
  | attributes PROVIDE LET REC MUT value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided Recursive Mutable $6 }
  | attributes PROVIDE LET MUT value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided Nonrecursive Mutable $5 }
  | attributes PROVIDE foreign_stmt { Toplevel.foreign ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided $3 }
  | attributes PROVIDE primitive_stmt { Toplevel.primitive ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided $3 }
  | attributes PROVIDE exception_stmt { Toplevel.grain_exception ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided $3 }
  | attributes PROVIDE provide_shape { Toplevel.provide ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 $3 }
  | attributes PROVIDE module_stmt { Toplevel.module_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Provided $3 }

data_constructor:
  | UIDENT { ConstructorDeclaration.singleton ~loc:(to_loc $loc) (mkstr $loc($1) $1) }
  | UIDENT data_tuple_body { ConstructorDeclaration.tuple ~loc:(to_loc $loc) (mkstr $loc($1) $1) (Location.mkloc $2 (to_loc $loc($2))) }
  | UIDENT data_record_body { ConstructorDeclaration.record ~loc:(to_loc $loc) (mkstr $loc($1) $1) (Location.mkloc $2 (to_loc $loc($2))) }

data_constructors:
  | lbrace lseparated_nonempty_list(comma, data_constructor) comma? rbrace { $2 }

data_label:
  | lid colon typ { LabelDeclaration.mk ~loc:(to_loc $loc) $1 $3 Immutable }
  | MUT lid colon typ { LabelDeclaration.mk ~loc:(to_loc $loc) $2 $4 Mutable }

data_tuple_body:
  | lparen typs rparen { $2 }

data_record_body:
  | lbrace lseparated_nonempty_list(comma, data_label) comma? rbrace { $2 }

id_typ:
  | LIDENT { Type.var ~loc:(to_loc $loc) $1 }

id_vec:
  | lcaret lseparated_nonempty_list(comma, id_typ) comma? rcaret {$2}

rec_flag:
  | REC { Recursive }

data_declaration:
  | TYPE rec_flag? UIDENT id_vec? equal typ { DataDeclaration.abstract ~loc:(to_loc $loc) ?rec_flag:$2 (mkstr $loc($3) $3) (Option.value ~default:[] $4) (Some $6) }
  | ENUM rec_flag? UIDENT id_vec? data_constructors { DataDeclaration.variant ~loc:(to_loc $loc) ?rec_flag:$2 (mkstr $loc($3) $3) (Option.value ~default:[] $4) $5 }
  | RECORD rec_flag? UIDENT id_vec? data_record_body { DataDeclaration.record ~loc:(to_loc $loc) ?rec_flag:$2 (mkstr $loc($3) $3) (Option.value ~default:[] $4) $5 }

unop_expr:
  | prefix_op non_assign_expr { Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($1) [mkstr $loc($1) $1]) [{paa_label=Unlabeled; paa_expr=$2; paa_loc=(to_loc $loc($2))}] }

paren_expr:
  | lparen expr rparen { $2 }

app_arg:
  | expr { {paa_label=Unlabeled; paa_expr=$1; paa_loc=to_loc $loc} }
  | id_str EQUAL expr { {paa_label=(Labeled $1); paa_expr=$3; paa_loc=to_loc $loc} }

app_expr:
  | left_accessor_expr lparen lseparated_list(comma, app_arg) comma? rparen { Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }

rcaret_rcaret_op:
  | lnonempty_list(RCARET) RCARET { (String.init (1 + List.length $1) (fun _ -> '>')) }

construct_expr:
  | qualified_uid lparen lseparated_list(comma, expr) comma? rparen { Expression.tuple_construct ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }
  | qualified_uid lbrace lseparated_nonempty_list(comma, record_field) comma? rbrace { Expression.record_construct ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }
  | qualified_uid %prec LPAREN { Expression.singleton_construct ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 }

// These are all inlined to carry over their precedence.
%inline infix_op:
  | INFIX_30
  | INFIX_40
  | INFIX_50
  | INFIX_60
  | INFIX_70
  | INFIX_80
  | INFIX_90
  | INFIX_100
  | INFIX_110
  | INFIX_120 {$1}
  | STAR { "*" }
  | SLASH { "/" }
  | DASH { "-" }
  | PIPE { "|" }
  | LCARET { "<" }
  | RCARET { ">" }

%inline prefix_op:
  | PREFIX_150 {$1}

primitive_:
  | ASSERT { "assert" }
  | THROW { "throw" }
  | FAIL { "fail" }

special_op:
  | infix_op | rcaret_rcaret_op | prefix_op {$1}

%inline special_id:
  | lparen special_op rparen { mkstr $loc($2) $2 }

%inline modid:
  | lseparated_nonempty_list(dot, type_id_str) { $1 }

qualified_lid:
  | modid dot id_str { mkid (List.append $1 [$3]) (to_loc $loc) }
  | id_str %prec EQUAL { (mkid [$1]) (to_loc $loc) }

%inline qualified_uid_inline:
  | lseparated_nonempty_list(dot, type_id_str) { (mkid $1) (to_loc $loc) }

qualified_uid:
  | qualified_uid_inline %prec DOT { $1 }

lid:
  | id_str { (mkid [$1]) (to_loc $loc) }

uid:
  | UIDENT { (mkid [mkstr $loc $1]) (to_loc $loc) }

id_expr:
  // Force any following colon to cause a shift
  | qualified_lid %prec COLON { Expression.ident ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 }

simple_expr:
  | const { Expression.constant ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 }
  | lparen tuple_exprs rparen { Expression.tuple ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $2 }
  | id_expr { $1 }

braced_expr:
  | lbrace block_body rbrace { Expression.block ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $2 }
  | lbrace record_exprs rbrace { Expression.record_fields ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $2 }

arg_default:
  | EQUAL non_stmt_expr { $2 }

lam_arg:
  | pattern arg_default? { LambdaArgument.mk ~loc:(to_loc $loc) $1 $2 }

lam_args:
  | lseparated_nonempty_list(comma, lam_arg) comma? { $1 }

lam_expr:
  | FUN lparen lam_args? rparen thickarrow expr { Expression.lambda ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (Option.value ~default:[] $3) $6 }
  | FUN LIDENT thickarrow expr { Expression.lambda ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) [LambdaArgument.mk ~loc:(to_loc $loc($2)) (Pattern.var ~loc:(to_loc $loc($2)) (mkstr $loc($2) $2)) None] $4 }

attribute_argument:
  | STRING { mkstr $loc $1 }

attribute_arguments:
  | lparen lseparated_list(comma, attribute_argument) rparen { $2 }

attribute:
  | AT id_str loption(attribute_arguments) { Attribute.mk ~loc:(to_loc $loc) $2 $3 }

attributes:
  | terminated(attribute, opt_eols)* { $1 }

let_expr:
  | attributes LET REC value_binds { Expression.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Recursive Immutable $4 }
  | attributes LET value_binds { Expression.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Nonrecursive Immutable $3 }
  | attributes LET REC MUT value_binds { Expression.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Recursive Mutable $5 }
  | attributes LET MUT value_binds { Expression.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 Nonrecursive Mutable $4 }

%inline else_expr:
  | ELSE opt_eols expr { $3 }

if_expr:
  | IF lparen expr rparen opt_eols expr ioption(else_expr) %prec _if { Expression.if_ ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $3 $6 $7 }

while_expr:
  | WHILE lparen expr rparen opt_eols expr { Expression.while_ ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $3 $6 }

for_inner_expr:
  | %prec EOL { None }
  | expr { Some $1 }

for_expr:
  | FOR lparen block_body_expr? opt_eols SEMI opt_eols for_inner_expr opt_eols SEMI opt_eols for_inner_expr rparen opt_eols expr { Expression.for_ ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $3 $7 $11 $14 }

when_guard:
  | opt_eols WHEN expr { $3 }

match_branch:
  | pattern ioption(when_guard) thickarrow expr { MatchBranch.mk ~loc:(to_loc $loc) $1 $4 $2 }

match_branches:
  | lseparated_nonempty_list(comma, match_branch) comma? { $1 }

match_expr:
  | MATCH lparen expr rparen lbrace match_branches rbrace { Expression.match_ ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $3 (mkloc $6 (to_loc (fst $loc($5), snd $loc($7)))) }

list_item:
  | ELLIPSIS expr { ListSpread ($2, to_loc $loc) }
  | expr { ListItem $1 }

list_expr:
  | lbrack rbrack { Expression.list ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) [] }
  | lbrack lseparated_nonempty_list(comma, list_item) comma? rbrack { Expression.list ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $2 }

array_expr:
  | lbrackrcaret rbrack { Expression.array ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) [] }
  | lbrackrcaret opt_eols lseparated_nonempty_list(comma, expr) comma? rbrack { Expression.array ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $3 }

stmt_expr:
  | THROW expr { Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($1) [mkstr $loc($1) "throw"]) [{paa_label=Unlabeled; paa_expr=$2; paa_loc=(to_loc $loc($2))}] }
  | ASSERT expr { Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($1) [mkstr $loc($1) "assert"]) [{paa_label=Unlabeled; paa_expr=$2; paa_loc=(to_loc $loc($2))}] }
  | FAIL expr { Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($1) [mkstr $loc($1) "fail"]) [{paa_label=Unlabeled; paa_expr=$2; paa_loc=(to_loc $loc($2))}] }
  // allow DASH to cause a shift instead of the usual reduction of the left side for subtraction
  | RETURN ioption(expr) %prec _below_infix { Expression.return ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $2 }
  | CONTINUE { Expression.continue ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) () }
  | BREAK { Expression.break ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) () }
  | use_stmt { $1 }

assign_binop_op:
  | INFIX_ASSIGNMENT_10 { mkstr $loc $1 }

assign_expr:
  | left_accessor_expr GETS opt_eols expr { Expression.box_assign ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $4 }
  | id_expr equal expr { Expression.assign ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }
  | id_expr assign_binop_op opt_eols expr { Expression.assign ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 (Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($2) [$2]) [{paa_label=Unlabeled; paa_expr=$1; paa_loc=(to_loc $loc($1))}; {paa_label=Unlabeled; paa_expr=$4; paa_loc=(to_loc $loc($4))}]) }
  | record_set { $1 }
  | array_set { $1 }

non_assign_expr:
  | left_accessor_expr { $1 }
  | unop_expr          { $1 }
  | if_expr            { $1 }
  | while_expr         { $1 }
  | for_expr           { $1 }
  | match_expr         { $1 }

left_accessor_expr:
  | app_expr       { $1 }
  | construct_expr { $1 }
  | simple_expr    { $1 }
  | array_get      { $1 }
  | record_get     { $1 }
  | paren_expr     { $1 }
  | braced_expr    { $1 }
  | list_expr      { $1 }
  | array_expr     { $1 }

block_body_expr:
  | let_expr    { $1 }
  | expr  { $1 }

%inline tuple_expr_ending:
  | ioption(eols) lseparated_nonempty_list(comma, expr) comma? { $2 }

tuple_exprs:
  | expr COMMA tuple_expr_ending { $1::$3 }

array_get:
  | left_accessor_expr lbrack expr rbrack { Expression.array_get ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }

array_set:
  | left_accessor_expr lbrack expr rbrack equal expr { Expression.array_set ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) ~lhs_loc:(to_loc (fst $loc($1), snd $loc($4))) $1 $3 $6 }
  | left_accessor_expr lbrack expr rbrack assign_binop_op expr { Expression.array_set ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) ~infix_op:(mkid_expr $loc($5) [$5]) ~lhs_loc:(to_loc (fst $loc($1), snd $loc($4))) $1 $3 $6 }

record_get:
  | left_accessor_expr dot lid { Expression.record_get ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 }

record_set:
  | left_accessor_expr dot lid equal expr { Expression.record_set ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 $5 }
  | left_accessor_expr dot lid assign_binop_op opt_eols expr { Expression.record_set ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3 (Expression.apply ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) (mkid_expr $loc($4) [$4]) [{paa_label=Unlabeled; paa_expr=Expression.record_get ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 $3; paa_loc=(to_loc $loc($6))}; {paa_label=Unlabeled; paa_expr=$6; paa_loc=(to_loc $loc($6))}]) }

%inline record_field_value:
  | colon expr {$2}

punned_record_field:
  | qualified_lid { RecordItem ($1, (Expression.ident ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1)) }

non_punned_record_field:
  | qualified_lid record_field_value { RecordItem ($1, $2) }

spread_record_field:
  | ELLIPSIS expr { RecordSpread ($2, to_loc $loc) }

%inline record_field:
  | punned_record_field { $1 }
  | non_punned_record_field { $1 }
  | spread_record_field { $1 }

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
  | LIDENT { Location.mkloc $1 (to_loc $loc) }
  | special_id { $1 }

type_id_str:
  | UIDENT { Location.mkloc $1 (to_loc $loc) }

foreign_stmt:
  | FOREIGN WASM id_str colon typ as_prefix(id_str)? FROM file_path { ValueDescription.mk ~loc:(to_loc $loc) ~mod_:$8 ~name:$3 ~alias:$6 ~typ:$5 () }

prim:
  | primitive_ { Location.mkloc $1 (to_loc $loc) }

primitive_stmt:
  | PRIMITIVE id_str equal STRING { PrimitiveDescription.mk ~loc:(to_loc $loc) ~ident:$2 ~name:(mkstr $loc($4) $4) () }
  | PRIMITIVE prim equal STRING { PrimitiveDescription.mk ~loc:(to_loc $loc) ~ident:$2 ~name:(mkstr $loc($4) $4) () }

exception_stmt:
  | EXCEPTION type_id_str { Exception.singleton ~loc:(to_loc $loc) $2 }
  | EXCEPTION type_id_str lparen typs? rparen { Exception.tuple ~loc:(to_loc $loc) $2 (Location.mkloc (Option.value ~default:[] $4) (to_loc $loc($4))) }
  | EXCEPTION type_id_str data_record_body { Exception.record ~loc:(to_loc $loc) $2 (Location.mkloc $3 (to_loc $loc($3))) }

module_stmt:
  | MODULE UIDENT lbrace toplevel_stmts RBRACE { ModuleDeclaration.mk ~loc:(to_loc $loc) (mkstr $loc($2) $2) $4 }

toplevel_stmt:
  | attributes LET REC value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided Recursive Immutable $4 }
  | attributes LET value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided Nonrecursive Immutable $3 }
  | attributes LET REC MUT value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided Recursive Mutable $5 }
  | attributes LET MUT value_binds { Toplevel.let_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided Nonrecursive Mutable $4 }
  | attributes data_declaration_stmts { Toplevel.data ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 $2 }
  | attributes foreign_stmt { Toplevel.foreign ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided $2 }
  | attributes include_stmt { Toplevel.include_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 $2 }
  | attributes module_stmt { Toplevel.module_ ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided $2 }
  | attributes primitive_stmt { Toplevel.primitive ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($2), snd $loc)) ~attributes:$1 NotProvided $2 }
  | expr { Toplevel.expr ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) $1 }
  | provide_stmt { $1 }
  | exception_stmt { Toplevel.grain_exception ~loc:(to_loc $loc) ~core_loc:(to_loc $loc) NotProvided $1 }

toplevel_stmts:
  | lseparated_nonempty_list(eos, toplevel_stmt) eos? { $1 }

module_header:
  | MODULE UIDENT { mkstr $loc($2) $2 }

program:
  | opt_eols attributes module_header eos toplevel_stmts EOF { make_program ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($3), snd $loc)) ~attributes:$2 $3 $5 }
  | opt_eols attributes module_header eos? EOF { make_program ~loc:(to_loc $sloc) ~core_loc:(to_loc (fst $loc($3), snd $loc)) ~attributes:$2 $3 [] }
