
let _ =
  if "20191116" <> Dyp.version
  then (Printf.fprintf stderr
    "version mismatch, dypgen version 20191116 and dyplib version %s\n" Dyp.version;
  exit 2)

type token =
  | EOF
  | EOL
  | FROM
  | EXCEPT
  | PRIMITIVE
  | WASM
  | FOREIGN
  | EXPORT
  | IMPORT
  | DATA
  | NOT
  | PIPEPIPE
  | AMPAMP
  | WHILE
  | MATCH
  | ELSE
  | IF
  | REC
  | LET
  | FALSE
  | TRUE
  | PERCENT
  | SLASH
  | STAR
  | DASH
  | PLUS
  | FAIL
  | ASSERT
  | ELLIPSIS
  | DOT
  | COLONCOLON
  | COLON
  | UNDERSCORE
  | GETS
  | EQUAL
  | GREATEREQ
  | LESSEQ
  | EQEQ
  | PIPE
  | ARROW
  | THICKARROW
  | AS
  | SEMI
  | COMMA
  | CARET
  | RCARET
  | LCARET
  | RBRACE
  | LBRACE
  | RPAREN
  | LPARENNOSPACE
  | LPAREN
  | RBRACK
  | LBRACK
  | STRING of (string)
  | TYPEID of (string)
  | ID of (string)
  | NUM of (int)

module Dyp_symbols =
struct
  let get_token_name t = match t with
    | AMPAMP -> 0
    | ARROW -> 1
    | AS -> 2
    | ASSERT -> 3
    | CARET -> 4
    | COLON -> 5
    | COLONCOLON -> 6
    | COMMA -> 7
    | DASH -> 8
    | DATA -> 9
    | DOT -> 10
    | ELLIPSIS -> 11
    | ELSE -> 12
    | EOF -> 13
    | EOL -> 14
    | EQEQ -> 15
    | EQUAL -> 16
    | EXCEPT -> 17
    | EXPORT -> 18
    | FAIL -> 19
    | FALSE -> 20
    | FOREIGN -> 21
    | FROM -> 22
    | GETS -> 23
    | GREATEREQ -> 24
    | ID _ -> 25
    | IF -> 26
    | IMPORT -> 27
    | LBRACE -> 28
    | LBRACK -> 29
    | LCARET -> 30
    | LESSEQ -> 31
    | LET -> 32
    | LPAREN -> 33
    | LPARENNOSPACE -> 34
    | MATCH -> 35
    | NOT -> 36
    | NUM _ -> 37
    | PERCENT -> 38
    | PIPE -> 39
    | PIPEPIPE -> 40
    | PLUS -> 41
    | PRIMITIVE -> 42
    | RBRACE -> 43
    | RBRACK -> 44
    | RCARET -> 45
    | REC -> 46
    | RPAREN -> 47
    | SEMI -> 48
    | SLASH -> 49
    | STAR -> 50
    | STRING _ -> 51
    | THICKARROW -> 52
    | TRUE -> 53
    | TYPEID _ -> 54
    | UNDERSCORE -> 55
    | WASM -> 56
    | WHILE -> 57
  let str_token t = match t with
    | AMPAMP -> "AMPAMP"
    | ARROW -> "ARROW"
    | AS -> "AS"
    | ASSERT -> "ASSERT"
    | CARET -> "CARET"
    | COLON -> "COLON"
    | COLONCOLON -> "COLONCOLON"
    | COMMA -> "COMMA"
    | DASH -> "DASH"
    | DATA -> "DATA"
    | DOT -> "DOT"
    | ELLIPSIS -> "ELLIPSIS"
    | ELSE -> "ELSE"
    | EOF -> "EOF"
    | EOL -> "EOL"
    | EQEQ -> "EQEQ"
    | EQUAL -> "EQUAL"
    | EXCEPT -> "EXCEPT"
    | EXPORT -> "EXPORT"
    | FAIL -> "FAIL"
    | FALSE -> "FALSE"
    | FOREIGN -> "FOREIGN"
    | FROM -> "FROM"
    | GETS -> "GETS"
    | GREATEREQ -> "GREATEREQ"
    | ID _ -> "ID"
    | IF -> "IF"
    | IMPORT -> "IMPORT"
    | LBRACE -> "LBRACE"
    | LBRACK -> "LBRACK"
    | LCARET -> "LCARET"
    | LESSEQ -> "LESSEQ"
    | LET -> "LET"
    | LPAREN -> "LPAREN"
    | LPARENNOSPACE -> "LPARENNOSPACE"
    | MATCH -> "MATCH"
    | NOT -> "NOT"
    | NUM _ -> "NUM"
    | PERCENT -> "PERCENT"
    | PIPE -> "PIPE"
    | PIPEPIPE -> "PIPEPIPE"
    | PLUS -> "PLUS"
    | PRIMITIVE -> "PRIMITIVE"
    | RBRACE -> "RBRACE"
    | RBRACK -> "RBRACK"
    | RCARET -> "RCARET"
    | REC -> "REC"
    | RPAREN -> "RPAREN"
    | SEMI -> "SEMI"
    | SLASH -> "SLASH"
    | STAR -> "STAR"
    | STRING _ -> "STRING"
    | THICKARROW -> "THICKARROW"
    | TRUE -> "TRUE"
    | TYPEID _ -> "TYPEID"
    | UNDERSCORE -> "UNDERSCORE"
    | WASM -> "WASM"
    | WHILE -> "WHILE"
  let ter_string_list = [
      ("AMPAMP",0);
      ("ARROW",1);
      ("AS",2);
      ("ASSERT",3);
      ("CARET",4);
      ("COLON",5);
      ("COLONCOLON",6);
      ("COMMA",7);
      ("DASH",8);
      ("DATA",9);
      ("DOT",10);
      ("ELLIPSIS",11);
      ("ELSE",12);
      ("EOF",13);
      ("EOL",14);
      ("EQEQ",15);
      ("EQUAL",16);
      ("EXCEPT",17);
      ("EXPORT",18);
      ("FAIL",19);
      ("FALSE",20);
      ("FOREIGN",21);
      ("FROM",22);
      ("GETS",23);
      ("GREATEREQ",24);
      ("ID",25);
      ("IF",26);
      ("IMPORT",27);
      ("LBRACE",28);
      ("LBRACK",29);
      ("LCARET",30);
      ("LESSEQ",31);
      ("LET",32);
      ("LPAREN",33);
      ("LPARENNOSPACE",34);
      ("MATCH",35);
      ("NOT",36);
      ("NUM",37);
      ("PERCENT",38);
      ("PIPE",39);
      ("PIPEPIPE",40);
      ("PLUS",41);
      ("PRIMITIVE",42);
      ("RBRACE",43);
      ("RBRACK",44);
      ("RCARET",45);
      ("REC",46);
      ("RPAREN",47);
      ("SEMI",48);
      ("SLASH",49);
      ("STAR",50);
      ("STRING",51);
      ("THICKARROW",52);
      ("TRUE",53);
      ("TYPEID",54);
      ("UNDERSCORE",55);
      ("WASM",56);
      ("WHILE",57);]
end

type ('dypgen__Obj_ampamp_op, 'dypgen__Obj_any_id_str, 'dypgen__Obj_any_or_var_pat, 'dypgen__Obj_app_arg_exprs, 'dypgen__Obj_app_expr, 'dypgen__Obj_array_expr, 'dypgen__Obj_array_get, 'dypgen__Obj_array_set, 'dypgen__Obj_arrow, 'dypgen__Obj_assign_expr, 'dypgen__Obj_binop_expr, 'dypgen__Obj_block, 'dypgen__Obj_block_body, 'dypgen__Obj_block_body_expr, 'dypgen__Obj_block_body_stmt, 'dypgen__Obj_block_expr, 'dypgen__Obj_block_or_expr, 'dypgen__Obj_colon, 'dypgen__Obj_comma, 'dypgen__Obj_const, 'dypgen__Obj_dash_op, 'dypgen__Obj_dasheq_op, 'dypgen__Obj_data_constructor, 'dypgen__Obj_data_constructors, 'dypgen__Obj_data_declaration, 'dypgen__Obj_data_label, 'dypgen__Obj_data_labels, 'dypgen__Obj_data_typ, 'dypgen__Obj_dot, 'dypgen__Obj_dypgen__nested_nt_0, 'dypgen__Obj_dypgen__nested_nt_1, 'dypgen__Obj_dypgen__nested_nt_10, 'dypgen__Obj_dypgen__nested_nt_100, 'dypgen__Obj_dypgen__nested_nt_101, 'dypgen__Obj_dypgen__nested_nt_102, 'dypgen__Obj_dypgen__nested_nt_103, 'dypgen__Obj_dypgen__nested_nt_104, 'dypgen__Obj_dypgen__nested_nt_105, 'dypgen__Obj_dypgen__nested_nt_106, 'dypgen__Obj_dypgen__nested_nt_107, 'dypgen__Obj_dypgen__nested_nt_108, 'dypgen__Obj_dypgen__nested_nt_109, 'dypgen__Obj_dypgen__nested_nt_11, 'dypgen__Obj_dypgen__nested_nt_110, 'dypgen__Obj_dypgen__nested_nt_111, 'dypgen__Obj_dypgen__nested_nt_12, 'dypgen__Obj_dypgen__nested_nt_13, 'dypgen__Obj_dypgen__nested_nt_14, 'dypgen__Obj_dypgen__nested_nt_15, 'dypgen__Obj_dypgen__nested_nt_16, 'dypgen__Obj_dypgen__nested_nt_17, 'dypgen__Obj_dypgen__nested_nt_18, 'dypgen__Obj_dypgen__nested_nt_19, 'dypgen__Obj_dypgen__nested_nt_2, 'dypgen__Obj_dypgen__nested_nt_20, 'dypgen__Obj_dypgen__nested_nt_21, 'dypgen__Obj_dypgen__nested_nt_22, 'dypgen__Obj_dypgen__nested_nt_23, 'dypgen__Obj_dypgen__nested_nt_24, 'dypgen__Obj_dypgen__nested_nt_25, 'dypgen__Obj_dypgen__nested_nt_26, 'dypgen__Obj_dypgen__nested_nt_27, 'dypgen__Obj_dypgen__nested_nt_28, 'dypgen__Obj_dypgen__nested_nt_29, 'dypgen__Obj_dypgen__nested_nt_3, 'dypgen__Obj_dypgen__nested_nt_30, 'dypgen__Obj_dypgen__nested_nt_31, 'dypgen__Obj_dypgen__nested_nt_32, 'dypgen__Obj_dypgen__nested_nt_33, 'dypgen__Obj_dypgen__nested_nt_34, 'dypgen__Obj_dypgen__nested_nt_35, 'dypgen__Obj_dypgen__nested_nt_36, 'dypgen__Obj_dypgen__nested_nt_37, 'dypgen__Obj_dypgen__nested_nt_38, 'dypgen__Obj_dypgen__nested_nt_39, 'dypgen__Obj_dypgen__nested_nt_4, 'dypgen__Obj_dypgen__nested_nt_40, 'dypgen__Obj_dypgen__nested_nt_41, 'dypgen__Obj_dypgen__nested_nt_42, 'dypgen__Obj_dypgen__nested_nt_43, 'dypgen__Obj_dypgen__nested_nt_44, 'dypgen__Obj_dypgen__nested_nt_45, 'dypgen__Obj_dypgen__nested_nt_46, 'dypgen__Obj_dypgen__nested_nt_47, 'dypgen__Obj_dypgen__nested_nt_48, 'dypgen__Obj_dypgen__nested_nt_49, 'dypgen__Obj_dypgen__nested_nt_5, 'dypgen__Obj_dypgen__nested_nt_50, 'dypgen__Obj_dypgen__nested_nt_51, 'dypgen__Obj_dypgen__nested_nt_52, 'dypgen__Obj_dypgen__nested_nt_53, 'dypgen__Obj_dypgen__nested_nt_54, 'dypgen__Obj_dypgen__nested_nt_55, 'dypgen__Obj_dypgen__nested_nt_56, 'dypgen__Obj_dypgen__nested_nt_57, 'dypgen__Obj_dypgen__nested_nt_58, 'dypgen__Obj_dypgen__nested_nt_59, 'dypgen__Obj_dypgen__nested_nt_6, 'dypgen__Obj_dypgen__nested_nt_60, 'dypgen__Obj_dypgen__nested_nt_61, 'dypgen__Obj_dypgen__nested_nt_62, 'dypgen__Obj_dypgen__nested_nt_63, 'dypgen__Obj_dypgen__nested_nt_64, 'dypgen__Obj_dypgen__nested_nt_65, 'dypgen__Obj_dypgen__nested_nt_66, 'dypgen__Obj_dypgen__nested_nt_67, 'dypgen__Obj_dypgen__nested_nt_68, 'dypgen__Obj_dypgen__nested_nt_69, 'dypgen__Obj_dypgen__nested_nt_7, 'dypgen__Obj_dypgen__nested_nt_70, 'dypgen__Obj_dypgen__nested_nt_71, 'dypgen__Obj_dypgen__nested_nt_72, 'dypgen__Obj_dypgen__nested_nt_73, 'dypgen__Obj_dypgen__nested_nt_74, 'dypgen__Obj_dypgen__nested_nt_75, 'dypgen__Obj_dypgen__nested_nt_76, 'dypgen__Obj_dypgen__nested_nt_77, 'dypgen__Obj_dypgen__nested_nt_78, 'dypgen__Obj_dypgen__nested_nt_79, 'dypgen__Obj_dypgen__nested_nt_8, 'dypgen__Obj_dypgen__nested_nt_80, 'dypgen__Obj_dypgen__nested_nt_81, 'dypgen__Obj_dypgen__nested_nt_82, 'dypgen__Obj_dypgen__nested_nt_83, 'dypgen__Obj_dypgen__nested_nt_84, 'dypgen__Obj_dypgen__nested_nt_85, 'dypgen__Obj_dypgen__nested_nt_86, 'dypgen__Obj_dypgen__nested_nt_87, 'dypgen__Obj_dypgen__nested_nt_88, 'dypgen__Obj_dypgen__nested_nt_89, 'dypgen__Obj_dypgen__nested_nt_9, 'dypgen__Obj_dypgen__nested_nt_90, 'dypgen__Obj_dypgen__nested_nt_91, 'dypgen__Obj_dypgen__nested_nt_92, 'dypgen__Obj_dypgen__nested_nt_93, 'dypgen__Obj_dypgen__nested_nt_94, 'dypgen__Obj_dypgen__nested_nt_95, 'dypgen__Obj_dypgen__nested_nt_96, 'dypgen__Obj_dypgen__nested_nt_97, 'dypgen__Obj_dypgen__nested_nt_98, 'dypgen__Obj_dypgen__nested_nt_99, 'dypgen__Obj_dypgen__option_EOL, 'dypgen__Obj_eos, 'dypgen__Obj_eqeq_op, 'dypgen__Obj_equal, 'dypgen__Obj_export_id_str, 'dypgen__Obj_export_stmt, 'dypgen__Obj_expr, 'dypgen__Obj_ext_constructor, 'dypgen__Obj_file_path, 'dypgen__Obj_foreign_stmt, 'dypgen__Obj_greatereq_op, 'dypgen__Obj_id, 'dypgen__Obj_id_expr, 'dypgen__Obj_id_str, 'dypgen__Obj_if_expr, 'dypgen__Obj_import_shape, 'dypgen__Obj_import_stmt, 'dypgen__Obj_infix, 'dypgen__Obj_infix_op, 'dypgen__Obj_lam_args, 'dypgen__Obj_lam_expr, 'dypgen__Obj_lbrace, 'dypgen__Obj_lbrack, 'dypgen__Obj_lcaret, 'dypgen__Obj_lcaret_op, 'dypgen__Obj_lesseq_op, 'dypgen__Obj_let_expr, 'dypgen__Obj_list_expr, 'dypgen__Obj_lparen, 'dypgen__Obj_match_branch, 'dypgen__Obj_match_branches, 'dypgen__Obj_match_expr, 'dypgen__Obj_non_assign_expr, 'dypgen__Obj_non_binop_expr, 'dypgen__Obj_paren_expr, 'dypgen__Obj_pattern, 'dypgen__Obj_patterns, 'dypgen__Obj_percent_op, 'dypgen__Obj_pipe, 'dypgen__Obj_pipepipe_op, 'dypgen__Obj_plus_op, 'dypgen__Obj_pluseq_op, 'dypgen__Obj_prefix, 'dypgen__Obj_prim1_expr, 'dypgen__Obj_primitive, 'dypgen__Obj_primitive_stmt, 'dypgen__Obj_rbrace, 'dypgen__Obj_rbrack, 'dypgen__Obj_rcaret, 'dypgen__Obj_rcaret_op, 'dypgen__Obj_record_exprs, 'dypgen__Obj_record_field, 'dypgen__Obj_record_get, 'dypgen__Obj_record_pattern, 'dypgen__Obj_record_patterns, 'dypgen__Obj_record_pun, 'dypgen__Obj_rparen, 'dypgen__Obj_simple_expr, 'dypgen__Obj_simple_id, 'dypgen__Obj_slash_op, 'dypgen__Obj_slasheq_op, 'dypgen__Obj_star_op, 'dypgen__Obj_stareq_op, 'dypgen__Obj_stmt_expr, 'dypgen__Obj_thickarrow, 'dypgen__Obj_toplevel_stmt, 'dypgen__Obj_toplevel_stmts, 'dypgen__Obj_tuple_exprs, 'dypgen__Obj_tuple_patterns, 'dypgen__Obj_tuple_typs, 'dypgen__Obj_typ, 'dypgen__Obj_type_id, 'dypgen__Obj_type_id_str, 'dypgen__Obj_typs, 'dypgen__Obj_value_bind, 'dypgen__Obj_value_binds, 'dypgen__Obj_while_expr) obj =
  | Lexeme_matched of string
  | Obj_AMPAMP
  | Obj_ARROW
  | Obj_AS
  | Obj_ASSERT
  | Obj_CARET
  | Obj_COLON
  | Obj_COLONCOLON
  | Obj_COMMA
  | Obj_DASH
  | Obj_DATA
  | Obj_DOT
  | Obj_ELLIPSIS
  | Obj_ELSE
  | Obj_EOF
  | Obj_EOL
  | Obj_EQEQ
  | Obj_EQUAL
  | Obj_EXCEPT
  | Obj_EXPORT
  | Obj_FAIL
  | Obj_FALSE
  | Obj_FOREIGN
  | Obj_FROM
  | Obj_GETS
  | Obj_GREATEREQ
  | Obj_ID of (string)
  | Obj_IF
  | Obj_IMPORT
  | Obj_LBRACE
  | Obj_LBRACK
  | Obj_LCARET
  | Obj_LESSEQ
  | Obj_LET
  | Obj_LPAREN
  | Obj_LPARENNOSPACE
  | Obj_MATCH
  | Obj_NOT
  | Obj_NUM of (int)
  | Obj_PERCENT
  | Obj_PIPE
  | Obj_PIPEPIPE
  | Obj_PLUS
  | Obj_PRIMITIVE
  | Obj_RBRACE
  | Obj_RBRACK
  | Obj_RCARET
  | Obj_REC
  | Obj_RPAREN
  | Obj_SEMI
  | Obj_SLASH
  | Obj_STAR
  | Obj_STRING of (string)
  | Obj_THICKARROW
  | Obj_TRUE
  | Obj_TYPEID of (string)
  | Obj_UNDERSCORE
  | Obj_WASM
  | Obj_WHILE
  | Obj_ampamp_op of 'dypgen__Obj_ampamp_op
  | Obj_any_id_str of 'dypgen__Obj_any_id_str
  | Obj_any_or_var_pat of 'dypgen__Obj_any_or_var_pat
  | Obj_app_arg_exprs of 'dypgen__Obj_app_arg_exprs
  | Obj_app_expr of 'dypgen__Obj_app_expr
  | Obj_array_expr of 'dypgen__Obj_array_expr
  | Obj_array_get of 'dypgen__Obj_array_get
  | Obj_array_set of 'dypgen__Obj_array_set
  | Obj_arrow of 'dypgen__Obj_arrow
  | Obj_assign_expr of 'dypgen__Obj_assign_expr
  | Obj_binop_expr of 'dypgen__Obj_binop_expr
  | Obj_block of 'dypgen__Obj_block
  | Obj_block_body of 'dypgen__Obj_block_body
  | Obj_block_body_expr of 'dypgen__Obj_block_body_expr
  | Obj_block_body_stmt of 'dypgen__Obj_block_body_stmt
  | Obj_block_expr of 'dypgen__Obj_block_expr
  | Obj_block_or_expr of 'dypgen__Obj_block_or_expr
  | Obj_colon of 'dypgen__Obj_colon
  | Obj_comma of 'dypgen__Obj_comma
  | Obj_const of 'dypgen__Obj_const
  | Obj_dash_op of 'dypgen__Obj_dash_op
  | Obj_dasheq_op of 'dypgen__Obj_dasheq_op
  | Obj_data_constructor of 'dypgen__Obj_data_constructor
  | Obj_data_constructors of 'dypgen__Obj_data_constructors
  | Obj_data_declaration of 'dypgen__Obj_data_declaration
  | Obj_data_label of 'dypgen__Obj_data_label
  | Obj_data_labels of 'dypgen__Obj_data_labels
  | Obj_data_typ of 'dypgen__Obj_data_typ
  | Obj_dot of 'dypgen__Obj_dot
  | Obj_dypgen__nested_nt_0 of 'dypgen__Obj_dypgen__nested_nt_0
  | Obj_dypgen__nested_nt_1 of 'dypgen__Obj_dypgen__nested_nt_1
  | Obj_dypgen__nested_nt_10 of 'dypgen__Obj_dypgen__nested_nt_10
  | Obj_dypgen__nested_nt_100 of 'dypgen__Obj_dypgen__nested_nt_100
  | Obj_dypgen__nested_nt_101 of 'dypgen__Obj_dypgen__nested_nt_101
  | Obj_dypgen__nested_nt_102 of 'dypgen__Obj_dypgen__nested_nt_102
  | Obj_dypgen__nested_nt_103 of 'dypgen__Obj_dypgen__nested_nt_103
  | Obj_dypgen__nested_nt_104 of 'dypgen__Obj_dypgen__nested_nt_104
  | Obj_dypgen__nested_nt_105 of 'dypgen__Obj_dypgen__nested_nt_105
  | Obj_dypgen__nested_nt_106 of 'dypgen__Obj_dypgen__nested_nt_106
  | Obj_dypgen__nested_nt_107 of 'dypgen__Obj_dypgen__nested_nt_107
  | Obj_dypgen__nested_nt_108 of 'dypgen__Obj_dypgen__nested_nt_108
  | Obj_dypgen__nested_nt_109 of 'dypgen__Obj_dypgen__nested_nt_109
  | Obj_dypgen__nested_nt_11 of 'dypgen__Obj_dypgen__nested_nt_11
  | Obj_dypgen__nested_nt_110 of 'dypgen__Obj_dypgen__nested_nt_110
  | Obj_dypgen__nested_nt_111 of 'dypgen__Obj_dypgen__nested_nt_111
  | Obj_dypgen__nested_nt_12 of 'dypgen__Obj_dypgen__nested_nt_12
  | Obj_dypgen__nested_nt_13 of 'dypgen__Obj_dypgen__nested_nt_13
  | Obj_dypgen__nested_nt_14 of 'dypgen__Obj_dypgen__nested_nt_14
  | Obj_dypgen__nested_nt_15 of 'dypgen__Obj_dypgen__nested_nt_15
  | Obj_dypgen__nested_nt_16 of 'dypgen__Obj_dypgen__nested_nt_16
  | Obj_dypgen__nested_nt_17 of 'dypgen__Obj_dypgen__nested_nt_17
  | Obj_dypgen__nested_nt_18 of 'dypgen__Obj_dypgen__nested_nt_18
  | Obj_dypgen__nested_nt_19 of 'dypgen__Obj_dypgen__nested_nt_19
  | Obj_dypgen__nested_nt_2 of 'dypgen__Obj_dypgen__nested_nt_2
  | Obj_dypgen__nested_nt_20 of 'dypgen__Obj_dypgen__nested_nt_20
  | Obj_dypgen__nested_nt_21 of 'dypgen__Obj_dypgen__nested_nt_21
  | Obj_dypgen__nested_nt_22 of 'dypgen__Obj_dypgen__nested_nt_22
  | Obj_dypgen__nested_nt_23 of 'dypgen__Obj_dypgen__nested_nt_23
  | Obj_dypgen__nested_nt_24 of 'dypgen__Obj_dypgen__nested_nt_24
  | Obj_dypgen__nested_nt_25 of 'dypgen__Obj_dypgen__nested_nt_25
  | Obj_dypgen__nested_nt_26 of 'dypgen__Obj_dypgen__nested_nt_26
  | Obj_dypgen__nested_nt_27 of 'dypgen__Obj_dypgen__nested_nt_27
  | Obj_dypgen__nested_nt_28 of 'dypgen__Obj_dypgen__nested_nt_28
  | Obj_dypgen__nested_nt_29 of 'dypgen__Obj_dypgen__nested_nt_29
  | Obj_dypgen__nested_nt_3 of 'dypgen__Obj_dypgen__nested_nt_3
  | Obj_dypgen__nested_nt_30 of 'dypgen__Obj_dypgen__nested_nt_30
  | Obj_dypgen__nested_nt_31 of 'dypgen__Obj_dypgen__nested_nt_31
  | Obj_dypgen__nested_nt_32 of 'dypgen__Obj_dypgen__nested_nt_32
  | Obj_dypgen__nested_nt_33 of 'dypgen__Obj_dypgen__nested_nt_33
  | Obj_dypgen__nested_nt_34 of 'dypgen__Obj_dypgen__nested_nt_34
  | Obj_dypgen__nested_nt_35 of 'dypgen__Obj_dypgen__nested_nt_35
  | Obj_dypgen__nested_nt_36 of 'dypgen__Obj_dypgen__nested_nt_36
  | Obj_dypgen__nested_nt_37 of 'dypgen__Obj_dypgen__nested_nt_37
  | Obj_dypgen__nested_nt_38 of 'dypgen__Obj_dypgen__nested_nt_38
  | Obj_dypgen__nested_nt_39 of 'dypgen__Obj_dypgen__nested_nt_39
  | Obj_dypgen__nested_nt_4 of 'dypgen__Obj_dypgen__nested_nt_4
  | Obj_dypgen__nested_nt_40 of 'dypgen__Obj_dypgen__nested_nt_40
  | Obj_dypgen__nested_nt_41 of 'dypgen__Obj_dypgen__nested_nt_41
  | Obj_dypgen__nested_nt_42 of 'dypgen__Obj_dypgen__nested_nt_42
  | Obj_dypgen__nested_nt_43 of 'dypgen__Obj_dypgen__nested_nt_43
  | Obj_dypgen__nested_nt_44 of 'dypgen__Obj_dypgen__nested_nt_44
  | Obj_dypgen__nested_nt_45 of 'dypgen__Obj_dypgen__nested_nt_45
  | Obj_dypgen__nested_nt_46 of 'dypgen__Obj_dypgen__nested_nt_46
  | Obj_dypgen__nested_nt_47 of 'dypgen__Obj_dypgen__nested_nt_47
  | Obj_dypgen__nested_nt_48 of 'dypgen__Obj_dypgen__nested_nt_48
  | Obj_dypgen__nested_nt_49 of 'dypgen__Obj_dypgen__nested_nt_49
  | Obj_dypgen__nested_nt_5 of 'dypgen__Obj_dypgen__nested_nt_5
  | Obj_dypgen__nested_nt_50 of 'dypgen__Obj_dypgen__nested_nt_50
  | Obj_dypgen__nested_nt_51 of 'dypgen__Obj_dypgen__nested_nt_51
  | Obj_dypgen__nested_nt_52 of 'dypgen__Obj_dypgen__nested_nt_52
  | Obj_dypgen__nested_nt_53 of 'dypgen__Obj_dypgen__nested_nt_53
  | Obj_dypgen__nested_nt_54 of 'dypgen__Obj_dypgen__nested_nt_54
  | Obj_dypgen__nested_nt_55 of 'dypgen__Obj_dypgen__nested_nt_55
  | Obj_dypgen__nested_nt_56 of 'dypgen__Obj_dypgen__nested_nt_56
  | Obj_dypgen__nested_nt_57 of 'dypgen__Obj_dypgen__nested_nt_57
  | Obj_dypgen__nested_nt_58 of 'dypgen__Obj_dypgen__nested_nt_58
  | Obj_dypgen__nested_nt_59 of 'dypgen__Obj_dypgen__nested_nt_59
  | Obj_dypgen__nested_nt_6 of 'dypgen__Obj_dypgen__nested_nt_6
  | Obj_dypgen__nested_nt_60 of 'dypgen__Obj_dypgen__nested_nt_60
  | Obj_dypgen__nested_nt_61 of 'dypgen__Obj_dypgen__nested_nt_61
  | Obj_dypgen__nested_nt_62 of 'dypgen__Obj_dypgen__nested_nt_62
  | Obj_dypgen__nested_nt_63 of 'dypgen__Obj_dypgen__nested_nt_63
  | Obj_dypgen__nested_nt_64 of 'dypgen__Obj_dypgen__nested_nt_64
  | Obj_dypgen__nested_nt_65 of 'dypgen__Obj_dypgen__nested_nt_65
  | Obj_dypgen__nested_nt_66 of 'dypgen__Obj_dypgen__nested_nt_66
  | Obj_dypgen__nested_nt_67 of 'dypgen__Obj_dypgen__nested_nt_67
  | Obj_dypgen__nested_nt_68 of 'dypgen__Obj_dypgen__nested_nt_68
  | Obj_dypgen__nested_nt_69 of 'dypgen__Obj_dypgen__nested_nt_69
  | Obj_dypgen__nested_nt_7 of 'dypgen__Obj_dypgen__nested_nt_7
  | Obj_dypgen__nested_nt_70 of 'dypgen__Obj_dypgen__nested_nt_70
  | Obj_dypgen__nested_nt_71 of 'dypgen__Obj_dypgen__nested_nt_71
  | Obj_dypgen__nested_nt_72 of 'dypgen__Obj_dypgen__nested_nt_72
  | Obj_dypgen__nested_nt_73 of 'dypgen__Obj_dypgen__nested_nt_73
  | Obj_dypgen__nested_nt_74 of 'dypgen__Obj_dypgen__nested_nt_74
  | Obj_dypgen__nested_nt_75 of 'dypgen__Obj_dypgen__nested_nt_75
  | Obj_dypgen__nested_nt_76 of 'dypgen__Obj_dypgen__nested_nt_76
  | Obj_dypgen__nested_nt_77 of 'dypgen__Obj_dypgen__nested_nt_77
  | Obj_dypgen__nested_nt_78 of 'dypgen__Obj_dypgen__nested_nt_78
  | Obj_dypgen__nested_nt_79 of 'dypgen__Obj_dypgen__nested_nt_79
  | Obj_dypgen__nested_nt_8 of 'dypgen__Obj_dypgen__nested_nt_8
  | Obj_dypgen__nested_nt_80 of 'dypgen__Obj_dypgen__nested_nt_80
  | Obj_dypgen__nested_nt_81 of 'dypgen__Obj_dypgen__nested_nt_81
  | Obj_dypgen__nested_nt_82 of 'dypgen__Obj_dypgen__nested_nt_82
  | Obj_dypgen__nested_nt_83 of 'dypgen__Obj_dypgen__nested_nt_83
  | Obj_dypgen__nested_nt_84 of 'dypgen__Obj_dypgen__nested_nt_84
  | Obj_dypgen__nested_nt_85 of 'dypgen__Obj_dypgen__nested_nt_85
  | Obj_dypgen__nested_nt_86 of 'dypgen__Obj_dypgen__nested_nt_86
  | Obj_dypgen__nested_nt_87 of 'dypgen__Obj_dypgen__nested_nt_87
  | Obj_dypgen__nested_nt_88 of 'dypgen__Obj_dypgen__nested_nt_88
  | Obj_dypgen__nested_nt_89 of 'dypgen__Obj_dypgen__nested_nt_89
  | Obj_dypgen__nested_nt_9 of 'dypgen__Obj_dypgen__nested_nt_9
  | Obj_dypgen__nested_nt_90 of 'dypgen__Obj_dypgen__nested_nt_90
  | Obj_dypgen__nested_nt_91 of 'dypgen__Obj_dypgen__nested_nt_91
  | Obj_dypgen__nested_nt_92 of 'dypgen__Obj_dypgen__nested_nt_92
  | Obj_dypgen__nested_nt_93 of 'dypgen__Obj_dypgen__nested_nt_93
  | Obj_dypgen__nested_nt_94 of 'dypgen__Obj_dypgen__nested_nt_94
  | Obj_dypgen__nested_nt_95 of 'dypgen__Obj_dypgen__nested_nt_95
  | Obj_dypgen__nested_nt_96 of 'dypgen__Obj_dypgen__nested_nt_96
  | Obj_dypgen__nested_nt_97 of 'dypgen__Obj_dypgen__nested_nt_97
  | Obj_dypgen__nested_nt_98 of 'dypgen__Obj_dypgen__nested_nt_98
  | Obj_dypgen__nested_nt_99 of 'dypgen__Obj_dypgen__nested_nt_99
  | Obj_dypgen__option_EOL of 'dypgen__Obj_dypgen__option_EOL
  | Obj_dypgen__option_eos of 'dypgen__Obj_eos option
  | Obj_dypgen__option_patterns of 'dypgen__Obj_patterns option
  | Obj_dypgen__option_pipe of 'dypgen__Obj_pipe option
  | Obj_dypgen__star___block_body_stmt of 'dypgen__Obj_block_body_stmt list
  | Obj_eos of 'dypgen__Obj_eos
  | Obj_eqeq_op of 'dypgen__Obj_eqeq_op
  | Obj_equal of 'dypgen__Obj_equal
  | Obj_export_id_str of 'dypgen__Obj_export_id_str
  | Obj_export_stmt of 'dypgen__Obj_export_stmt
  | Obj_expr of 'dypgen__Obj_expr
  | Obj_ext_constructor of 'dypgen__Obj_ext_constructor
  | Obj_file_path of 'dypgen__Obj_file_path
  | Obj_foreign_stmt of 'dypgen__Obj_foreign_stmt
  | Obj_greatereq_op of 'dypgen__Obj_greatereq_op
  | Obj_id of 'dypgen__Obj_id
  | Obj_id_expr of 'dypgen__Obj_id_expr
  | Obj_id_str of 'dypgen__Obj_id_str
  | Obj_if_expr of 'dypgen__Obj_if_expr
  | Obj_import_shape of 'dypgen__Obj_import_shape
  | Obj_import_stmt of 'dypgen__Obj_import_stmt
  | Obj_infix of 'dypgen__Obj_infix
  | Obj_infix_op of 'dypgen__Obj_infix_op
  | Obj_lam_args of 'dypgen__Obj_lam_args
  | Obj_lam_expr of 'dypgen__Obj_lam_expr
  | Obj_lbrace of 'dypgen__Obj_lbrace
  | Obj_lbrack of 'dypgen__Obj_lbrack
  | Obj_lcaret of 'dypgen__Obj_lcaret
  | Obj_lcaret_op of 'dypgen__Obj_lcaret_op
  | Obj_lesseq_op of 'dypgen__Obj_lesseq_op
  | Obj_let_expr of 'dypgen__Obj_let_expr
  | Obj_list_expr of 'dypgen__Obj_list_expr
  | Obj_lparen of 'dypgen__Obj_lparen
  | Obj_match_branch of 'dypgen__Obj_match_branch
  | Obj_match_branches of 'dypgen__Obj_match_branches
  | Obj_match_expr of 'dypgen__Obj_match_expr
  | Obj_non_assign_expr of 'dypgen__Obj_non_assign_expr
  | Obj_non_binop_expr of 'dypgen__Obj_non_binop_expr
  | Obj_paren_expr of 'dypgen__Obj_paren_expr
  | Obj_pattern of 'dypgen__Obj_pattern
  | Obj_patterns of 'dypgen__Obj_patterns
  | Obj_percent_op of 'dypgen__Obj_percent_op
  | Obj_pipe of 'dypgen__Obj_pipe
  | Obj_pipepipe_op of 'dypgen__Obj_pipepipe_op
  | Obj_plus_op of 'dypgen__Obj_plus_op
  | Obj_pluseq_op of 'dypgen__Obj_pluseq_op
  | Obj_prefix of 'dypgen__Obj_prefix
  | Obj_prim1_expr of 'dypgen__Obj_prim1_expr
  | Obj_primitive of 'dypgen__Obj_primitive
  | Obj_primitive_stmt of 'dypgen__Obj_primitive_stmt
  | Obj_program of (Parsetree.parsed_program)
  | Obj_rbrace of 'dypgen__Obj_rbrace
  | Obj_rbrack of 'dypgen__Obj_rbrack
  | Obj_rcaret of 'dypgen__Obj_rcaret
  | Obj_rcaret_op of 'dypgen__Obj_rcaret_op
  | Obj_record_exprs of 'dypgen__Obj_record_exprs
  | Obj_record_field of 'dypgen__Obj_record_field
  | Obj_record_get of 'dypgen__Obj_record_get
  | Obj_record_pattern of 'dypgen__Obj_record_pattern
  | Obj_record_patterns of 'dypgen__Obj_record_patterns
  | Obj_record_pun of 'dypgen__Obj_record_pun
  | Obj_rparen of 'dypgen__Obj_rparen
  | Obj_simple_expr of 'dypgen__Obj_simple_expr
  | Obj_simple_id of 'dypgen__Obj_simple_id
  | Obj_slash_op of 'dypgen__Obj_slash_op
  | Obj_slasheq_op of 'dypgen__Obj_slasheq_op
  | Obj_star_op of 'dypgen__Obj_star_op
  | Obj_stareq_op of 'dypgen__Obj_stareq_op
  | Obj_stmt_expr of 'dypgen__Obj_stmt_expr
  | Obj_thickarrow of 'dypgen__Obj_thickarrow
  | Obj_toplevel_stmt of 'dypgen__Obj_toplevel_stmt
  | Obj_toplevel_stmts of 'dypgen__Obj_toplevel_stmts
  | Obj_tuple_exprs of 'dypgen__Obj_tuple_exprs
  | Obj_tuple_patterns of 'dypgen__Obj_tuple_patterns
  | Obj_tuple_typs of 'dypgen__Obj_tuple_typs
  | Obj_typ of 'dypgen__Obj_typ
  | Obj_type_id of 'dypgen__Obj_type_id
  | Obj_type_id_str of 'dypgen__Obj_type_id_str
  | Obj_typs of 'dypgen__Obj_typs
  | Obj_value_bind of 'dypgen__Obj_value_bind
  | Obj_value_binds of 'dypgen__Obj_value_binds
  | Obj_while_expr of 'dypgen__Obj_while_expr

module Dyp_symbols_array =
struct
  let token_name_array =
  [|"AMPAMP";
    "ARROW";
    "AS";
    "ASSERT";
    "CARET";
    "COLON";
    "COLONCOLON";
    "COMMA";
    "DASH";
    "DATA";
    "DOT";
    "ELLIPSIS";
    "ELSE";
    "EOF";
    "EOL";
    "EQEQ";
    "EQUAL";
    "EXCEPT";
    "EXPORT";
    "FAIL";
    "FALSE";
    "FOREIGN";
    "FROM";
    "GETS";
    "GREATEREQ";
    "ID";
    "IF";
    "IMPORT";
    "LBRACE";
    "LBRACK";
    "LCARET";
    "LESSEQ";
    "LET";
    "LPAREN";
    "LPARENNOSPACE";
    "MATCH";
    "NOT";
    "NUM";
    "PERCENT";
    "PIPE";
    "PIPEPIPE";
    "PLUS";
    "PRIMITIVE";
    "RBRACE";
    "RBRACK";
    "RCARET";
    "REC";
    "RPAREN";
    "SEMI";
    "SLASH";
    "STAR";
    "STRING";
    "THICKARROW";
    "TRUE";
    "TYPEID";
    "UNDERSCORE";
    "WASM";
    "WHILE"|]
  let nt_cons_list =
  [
    ("ampamp_op",5);
    ("any_id_str",6);
    ("any_or_var_pat",7);
    ("app_arg_exprs",8);
    ("app_expr",9);
    ("array_expr",10);
    ("array_get",11);
    ("array_set",12);
    ("arrow",13);
    ("assign_expr",14);
    ("binop_expr",15);
    ("block",16);
    ("block_body",17);
    ("block_body_expr",18);
    ("block_body_stmt",19);
    ("block_expr",20);
    ("block_or_expr",21);
    ("colon",22);
    ("comma",23);
    ("const",24);
    ("dash_op",25);
    ("dasheq_op",26);
    ("data_constructor",27);
    ("data_constructors",28);
    ("data_declaration",29);
    ("data_label",30);
    ("data_labels",31);
    ("data_typ",32);
    ("dot",33);
    ("dypgen__nested_nt_0",34);
    ("dypgen__nested_nt_1",35);
    ("dypgen__nested_nt_10",36);
    ("dypgen__nested_nt_100",37);
    ("dypgen__nested_nt_101",38);
    ("dypgen__nested_nt_102",39);
    ("dypgen__nested_nt_103",40);
    ("dypgen__nested_nt_104",41);
    ("dypgen__nested_nt_105",42);
    ("dypgen__nested_nt_106",43);
    ("dypgen__nested_nt_107",44);
    ("dypgen__nested_nt_108",45);
    ("dypgen__nested_nt_109",46);
    ("dypgen__nested_nt_11",47);
    ("dypgen__nested_nt_110",48);
    ("dypgen__nested_nt_111",49);
    ("dypgen__nested_nt_12",50);
    ("dypgen__nested_nt_13",51);
    ("dypgen__nested_nt_14",52);
    ("dypgen__nested_nt_15",53);
    ("dypgen__nested_nt_16",54);
    ("dypgen__nested_nt_17",55);
    ("dypgen__nested_nt_18",56);
    ("dypgen__nested_nt_19",57);
    ("dypgen__nested_nt_2",58);
    ("dypgen__nested_nt_20",59);
    ("dypgen__nested_nt_21",60);
    ("dypgen__nested_nt_22",61);
    ("dypgen__nested_nt_23",62);
    ("dypgen__nested_nt_24",63);
    ("dypgen__nested_nt_25",64);
    ("dypgen__nested_nt_26",65);
    ("dypgen__nested_nt_27",66);
    ("dypgen__nested_nt_28",67);
    ("dypgen__nested_nt_29",68);
    ("dypgen__nested_nt_3",69);
    ("dypgen__nested_nt_30",70);
    ("dypgen__nested_nt_31",71);
    ("dypgen__nested_nt_32",72);
    ("dypgen__nested_nt_33",73);
    ("dypgen__nested_nt_34",74);
    ("dypgen__nested_nt_35",75);
    ("dypgen__nested_nt_36",76);
    ("dypgen__nested_nt_37",77);
    ("dypgen__nested_nt_38",78);
    ("dypgen__nested_nt_39",79);
    ("dypgen__nested_nt_4",80);
    ("dypgen__nested_nt_40",81);
    ("dypgen__nested_nt_41",82);
    ("dypgen__nested_nt_42",83);
    ("dypgen__nested_nt_43",84);
    ("dypgen__nested_nt_44",85);
    ("dypgen__nested_nt_45",86);
    ("dypgen__nested_nt_46",87);
    ("dypgen__nested_nt_47",88);
    ("dypgen__nested_nt_48",89);
    ("dypgen__nested_nt_49",90);
    ("dypgen__nested_nt_5",91);
    ("dypgen__nested_nt_50",92);
    ("dypgen__nested_nt_51",93);
    ("dypgen__nested_nt_52",94);
    ("dypgen__nested_nt_53",95);
    ("dypgen__nested_nt_54",96);
    ("dypgen__nested_nt_55",97);
    ("dypgen__nested_nt_56",98);
    ("dypgen__nested_nt_57",99);
    ("dypgen__nested_nt_58",100);
    ("dypgen__nested_nt_59",101);
    ("dypgen__nested_nt_6",102);
    ("dypgen__nested_nt_60",103);
    ("dypgen__nested_nt_61",104);
    ("dypgen__nested_nt_62",105);
    ("dypgen__nested_nt_63",106);
    ("dypgen__nested_nt_64",107);
    ("dypgen__nested_nt_65",108);
    ("dypgen__nested_nt_66",109);
    ("dypgen__nested_nt_67",110);
    ("dypgen__nested_nt_68",111);
    ("dypgen__nested_nt_69",112);
    ("dypgen__nested_nt_7",113);
    ("dypgen__nested_nt_70",114);
    ("dypgen__nested_nt_71",115);
    ("dypgen__nested_nt_72",116);
    ("dypgen__nested_nt_73",117);
    ("dypgen__nested_nt_74",118);
    ("dypgen__nested_nt_75",119);
    ("dypgen__nested_nt_76",120);
    ("dypgen__nested_nt_77",121);
    ("dypgen__nested_nt_78",122);
    ("dypgen__nested_nt_79",123);
    ("dypgen__nested_nt_8",124);
    ("dypgen__nested_nt_80",125);
    ("dypgen__nested_nt_81",126);
    ("dypgen__nested_nt_82",127);
    ("dypgen__nested_nt_83",128);
    ("dypgen__nested_nt_84",129);
    ("dypgen__nested_nt_85",130);
    ("dypgen__nested_nt_86",131);
    ("dypgen__nested_nt_87",132);
    ("dypgen__nested_nt_88",133);
    ("dypgen__nested_nt_89",134);
    ("dypgen__nested_nt_9",135);
    ("dypgen__nested_nt_90",136);
    ("dypgen__nested_nt_91",137);
    ("dypgen__nested_nt_92",138);
    ("dypgen__nested_nt_93",139);
    ("dypgen__nested_nt_94",140);
    ("dypgen__nested_nt_95",141);
    ("dypgen__nested_nt_96",142);
    ("dypgen__nested_nt_97",143);
    ("dypgen__nested_nt_98",144);
    ("dypgen__nested_nt_99",145);
    ("dypgen__option_EOL",146);
    ("dypgen__option_eos",147);
    ("dypgen__option_patterns",148);
    ("dypgen__option_pipe",149);
    ("dypgen__star___block_body_stmt",150);
    ("eos",151);
    ("eqeq_op",152);
    ("equal",153);
    ("export_id_str",154);
    ("export_stmt",155);
    ("expr",156);
    ("ext_constructor",157);
    ("file_path",158);
    ("foreign_stmt",159);
    ("greatereq_op",160);
    ("id",161);
    ("id_expr",162);
    ("id_str",163);
    ("if_expr",164);
    ("import_shape",165);
    ("import_stmt",166);
    ("infix",167);
    ("infix_op",168);
    ("lam_args",169);
    ("lam_expr",170);
    ("lbrace",171);
    ("lbrack",172);
    ("lcaret",173);
    ("lcaret_op",174);
    ("lesseq_op",175);
    ("let_expr",176);
    ("list_expr",177);
    ("lparen",178);
    ("match_branch",179);
    ("match_branches",180);
    ("match_expr",181);
    ("non_assign_expr",182);
    ("non_binop_expr",183);
    ("paren_expr",184);
    ("pattern",185);
    ("patterns",186);
    ("percent_op",187);
    ("pipe",188);
    ("pipepipe_op",189);
    ("plus_op",190);
    ("pluseq_op",191);
    ("prefix",192);
    ("prim1_expr",193);
    ("primitive",194);
    ("primitive_stmt",195);
    ("program",196);
    ("rbrace",197);
    ("rbrack",198);
    ("rcaret",199);
    ("rcaret_op",200);
    ("record_exprs",201);
    ("record_field",202);
    ("record_get",203);
    ("record_pattern",204);
    ("record_patterns",205);
    ("record_pun",206);
    ("rparen",207);
    ("simple_expr",208);
    ("simple_id",209);
    ("slash_op",210);
    ("slasheq_op",211);
    ("star_op",212);
    ("stareq_op",213);
    ("stmt_expr",214);
    ("thickarrow",215);
    ("toplevel_stmt",216);
    ("toplevel_stmts",217);
    ("tuple_exprs",218);
    ("tuple_patterns",219);
    ("tuple_typs",220);
    ("typ",221);
    ("type_id",222);
    ("type_id_str",223);
    ("typs",224);
    ("value_bind",225);
    ("value_binds",226);
    ("while_expr",227)]
  let str_cons o = match o with
    | Lexeme_matched _ -> "Lexeme_matched"
    | Obj_ID _ -> "Obj_ID"
    | Obj_NUM _ -> "Obj_NUM"
    | Obj_STRING _ -> "Obj_STRING"
    | Obj_TYPEID _ -> "Obj_TYPEID"
    | Obj_ampamp_op _ -> "Obj_ampamp_op"
    | Obj_any_id_str _ -> "Obj_any_id_str"
    | Obj_any_or_var_pat _ -> "Obj_any_or_var_pat"
    | Obj_app_arg_exprs _ -> "Obj_app_arg_exprs"
    | Obj_app_expr _ -> "Obj_app_expr"
    | Obj_array_expr _ -> "Obj_array_expr"
    | Obj_array_get _ -> "Obj_array_get"
    | Obj_array_set _ -> "Obj_array_set"
    | Obj_arrow _ -> "Obj_arrow"
    | Obj_assign_expr _ -> "Obj_assign_expr"
    | Obj_binop_expr _ -> "Obj_binop_expr"
    | Obj_block _ -> "Obj_block"
    | Obj_block_body _ -> "Obj_block_body"
    | Obj_block_body_expr _ -> "Obj_block_body_expr"
    | Obj_block_body_stmt _ -> "Obj_block_body_stmt"
    | Obj_block_expr _ -> "Obj_block_expr"
    | Obj_block_or_expr _ -> "Obj_block_or_expr"
    | Obj_colon _ -> "Obj_colon"
    | Obj_comma _ -> "Obj_comma"
    | Obj_const _ -> "Obj_const"
    | Obj_dash_op _ -> "Obj_dash_op"
    | Obj_dasheq_op _ -> "Obj_dasheq_op"
    | Obj_data_constructor _ -> "Obj_data_constructor"
    | Obj_data_constructors _ -> "Obj_data_constructors"
    | Obj_data_declaration _ -> "Obj_data_declaration"
    | Obj_data_label _ -> "Obj_data_label"
    | Obj_data_labels _ -> "Obj_data_labels"
    | Obj_data_typ _ -> "Obj_data_typ"
    | Obj_dot _ -> "Obj_dot"
    | Obj_dypgen__nested_nt_0 _ -> "Obj_dypgen__nested_nt_0"
    | Obj_dypgen__nested_nt_1 _ -> "Obj_dypgen__nested_nt_1"
    | Obj_dypgen__nested_nt_10 _ -> "Obj_dypgen__nested_nt_10"
    | Obj_dypgen__nested_nt_100 _ -> "Obj_dypgen__nested_nt_100"
    | Obj_dypgen__nested_nt_101 _ -> "Obj_dypgen__nested_nt_101"
    | Obj_dypgen__nested_nt_102 _ -> "Obj_dypgen__nested_nt_102"
    | Obj_dypgen__nested_nt_103 _ -> "Obj_dypgen__nested_nt_103"
    | Obj_dypgen__nested_nt_104 _ -> "Obj_dypgen__nested_nt_104"
    | Obj_dypgen__nested_nt_105 _ -> "Obj_dypgen__nested_nt_105"
    | Obj_dypgen__nested_nt_106 _ -> "Obj_dypgen__nested_nt_106"
    | Obj_dypgen__nested_nt_107 _ -> "Obj_dypgen__nested_nt_107"
    | Obj_dypgen__nested_nt_108 _ -> "Obj_dypgen__nested_nt_108"
    | Obj_dypgen__nested_nt_109 _ -> "Obj_dypgen__nested_nt_109"
    | Obj_dypgen__nested_nt_11 _ -> "Obj_dypgen__nested_nt_11"
    | Obj_dypgen__nested_nt_110 _ -> "Obj_dypgen__nested_nt_110"
    | Obj_dypgen__nested_nt_111 _ -> "Obj_dypgen__nested_nt_111"
    | Obj_dypgen__nested_nt_12 _ -> "Obj_dypgen__nested_nt_12"
    | Obj_dypgen__nested_nt_13 _ -> "Obj_dypgen__nested_nt_13"
    | Obj_dypgen__nested_nt_14 _ -> "Obj_dypgen__nested_nt_14"
    | Obj_dypgen__nested_nt_15 _ -> "Obj_dypgen__nested_nt_15"
    | Obj_dypgen__nested_nt_16 _ -> "Obj_dypgen__nested_nt_16"
    | Obj_dypgen__nested_nt_17 _ -> "Obj_dypgen__nested_nt_17"
    | Obj_dypgen__nested_nt_18 _ -> "Obj_dypgen__nested_nt_18"
    | Obj_dypgen__nested_nt_19 _ -> "Obj_dypgen__nested_nt_19"
    | Obj_dypgen__nested_nt_2 _ -> "Obj_dypgen__nested_nt_2"
    | Obj_dypgen__nested_nt_20 _ -> "Obj_dypgen__nested_nt_20"
    | Obj_dypgen__nested_nt_21 _ -> "Obj_dypgen__nested_nt_21"
    | Obj_dypgen__nested_nt_22 _ -> "Obj_dypgen__nested_nt_22"
    | Obj_dypgen__nested_nt_23 _ -> "Obj_dypgen__nested_nt_23"
    | Obj_dypgen__nested_nt_24 _ -> "Obj_dypgen__nested_nt_24"
    | Obj_dypgen__nested_nt_25 _ -> "Obj_dypgen__nested_nt_25"
    | Obj_dypgen__nested_nt_26 _ -> "Obj_dypgen__nested_nt_26"
    | Obj_dypgen__nested_nt_27 _ -> "Obj_dypgen__nested_nt_27"
    | Obj_dypgen__nested_nt_28 _ -> "Obj_dypgen__nested_nt_28"
    | Obj_dypgen__nested_nt_29 _ -> "Obj_dypgen__nested_nt_29"
    | Obj_dypgen__nested_nt_3 _ -> "Obj_dypgen__nested_nt_3"
    | Obj_dypgen__nested_nt_30 _ -> "Obj_dypgen__nested_nt_30"
    | Obj_dypgen__nested_nt_31 _ -> "Obj_dypgen__nested_nt_31"
    | Obj_dypgen__nested_nt_32 _ -> "Obj_dypgen__nested_nt_32"
    | Obj_dypgen__nested_nt_33 _ -> "Obj_dypgen__nested_nt_33"
    | Obj_dypgen__nested_nt_34 _ -> "Obj_dypgen__nested_nt_34"
    | Obj_dypgen__nested_nt_35 _ -> "Obj_dypgen__nested_nt_35"
    | Obj_dypgen__nested_nt_36 _ -> "Obj_dypgen__nested_nt_36"
    | Obj_dypgen__nested_nt_37 _ -> "Obj_dypgen__nested_nt_37"
    | Obj_dypgen__nested_nt_38 _ -> "Obj_dypgen__nested_nt_38"
    | Obj_dypgen__nested_nt_39 _ -> "Obj_dypgen__nested_nt_39"
    | Obj_dypgen__nested_nt_4 _ -> "Obj_dypgen__nested_nt_4"
    | Obj_dypgen__nested_nt_40 _ -> "Obj_dypgen__nested_nt_40"
    | Obj_dypgen__nested_nt_41 _ -> "Obj_dypgen__nested_nt_41"
    | Obj_dypgen__nested_nt_42 _ -> "Obj_dypgen__nested_nt_42"
    | Obj_dypgen__nested_nt_43 _ -> "Obj_dypgen__nested_nt_43"
    | Obj_dypgen__nested_nt_44 _ -> "Obj_dypgen__nested_nt_44"
    | Obj_dypgen__nested_nt_45 _ -> "Obj_dypgen__nested_nt_45"
    | Obj_dypgen__nested_nt_46 _ -> "Obj_dypgen__nested_nt_46"
    | Obj_dypgen__nested_nt_47 _ -> "Obj_dypgen__nested_nt_47"
    | Obj_dypgen__nested_nt_48 _ -> "Obj_dypgen__nested_nt_48"
    | Obj_dypgen__nested_nt_49 _ -> "Obj_dypgen__nested_nt_49"
    | Obj_dypgen__nested_nt_5 _ -> "Obj_dypgen__nested_nt_5"
    | Obj_dypgen__nested_nt_50 _ -> "Obj_dypgen__nested_nt_50"
    | Obj_dypgen__nested_nt_51 _ -> "Obj_dypgen__nested_nt_51"
    | Obj_dypgen__nested_nt_52 _ -> "Obj_dypgen__nested_nt_52"
    | Obj_dypgen__nested_nt_53 _ -> "Obj_dypgen__nested_nt_53"
    | Obj_dypgen__nested_nt_54 _ -> "Obj_dypgen__nested_nt_54"
    | Obj_dypgen__nested_nt_55 _ -> "Obj_dypgen__nested_nt_55"
    | Obj_dypgen__nested_nt_56 _ -> "Obj_dypgen__nested_nt_56"
    | Obj_dypgen__nested_nt_57 _ -> "Obj_dypgen__nested_nt_57"
    | Obj_dypgen__nested_nt_58 _ -> "Obj_dypgen__nested_nt_58"
    | Obj_dypgen__nested_nt_59 _ -> "Obj_dypgen__nested_nt_59"
    | Obj_dypgen__nested_nt_6 _ -> "Obj_dypgen__nested_nt_6"
    | Obj_dypgen__nested_nt_60 _ -> "Obj_dypgen__nested_nt_60"
    | Obj_dypgen__nested_nt_61 _ -> "Obj_dypgen__nested_nt_61"
    | Obj_dypgen__nested_nt_62 _ -> "Obj_dypgen__nested_nt_62"
    | Obj_dypgen__nested_nt_63 _ -> "Obj_dypgen__nested_nt_63"
    | Obj_dypgen__nested_nt_64 _ -> "Obj_dypgen__nested_nt_64"
    | Obj_dypgen__nested_nt_65 _ -> "Obj_dypgen__nested_nt_65"
    | Obj_dypgen__nested_nt_66 _ -> "Obj_dypgen__nested_nt_66"
    | Obj_dypgen__nested_nt_67 _ -> "Obj_dypgen__nested_nt_67"
    | Obj_dypgen__nested_nt_68 _ -> "Obj_dypgen__nested_nt_68"
    | Obj_dypgen__nested_nt_69 _ -> "Obj_dypgen__nested_nt_69"
    | Obj_dypgen__nested_nt_7 _ -> "Obj_dypgen__nested_nt_7"
    | Obj_dypgen__nested_nt_70 _ -> "Obj_dypgen__nested_nt_70"
    | Obj_dypgen__nested_nt_71 _ -> "Obj_dypgen__nested_nt_71"
    | Obj_dypgen__nested_nt_72 _ -> "Obj_dypgen__nested_nt_72"
    | Obj_dypgen__nested_nt_73 _ -> "Obj_dypgen__nested_nt_73"
    | Obj_dypgen__nested_nt_74 _ -> "Obj_dypgen__nested_nt_74"
    | Obj_dypgen__nested_nt_75 _ -> "Obj_dypgen__nested_nt_75"
    | Obj_dypgen__nested_nt_76 _ -> "Obj_dypgen__nested_nt_76"
    | Obj_dypgen__nested_nt_77 _ -> "Obj_dypgen__nested_nt_77"
    | Obj_dypgen__nested_nt_78 _ -> "Obj_dypgen__nested_nt_78"
    | Obj_dypgen__nested_nt_79 _ -> "Obj_dypgen__nested_nt_79"
    | Obj_dypgen__nested_nt_8 _ -> "Obj_dypgen__nested_nt_8"
    | Obj_dypgen__nested_nt_80 _ -> "Obj_dypgen__nested_nt_80"
    | Obj_dypgen__nested_nt_81 _ -> "Obj_dypgen__nested_nt_81"
    | Obj_dypgen__nested_nt_82 _ -> "Obj_dypgen__nested_nt_82"
    | Obj_dypgen__nested_nt_83 _ -> "Obj_dypgen__nested_nt_83"
    | Obj_dypgen__nested_nt_84 _ -> "Obj_dypgen__nested_nt_84"
    | Obj_dypgen__nested_nt_85 _ -> "Obj_dypgen__nested_nt_85"
    | Obj_dypgen__nested_nt_86 _ -> "Obj_dypgen__nested_nt_86"
    | Obj_dypgen__nested_nt_87 _ -> "Obj_dypgen__nested_nt_87"
    | Obj_dypgen__nested_nt_88 _ -> "Obj_dypgen__nested_nt_88"
    | Obj_dypgen__nested_nt_89 _ -> "Obj_dypgen__nested_nt_89"
    | Obj_dypgen__nested_nt_9 _ -> "Obj_dypgen__nested_nt_9"
    | Obj_dypgen__nested_nt_90 _ -> "Obj_dypgen__nested_nt_90"
    | Obj_dypgen__nested_nt_91 _ -> "Obj_dypgen__nested_nt_91"
    | Obj_dypgen__nested_nt_92 _ -> "Obj_dypgen__nested_nt_92"
    | Obj_dypgen__nested_nt_93 _ -> "Obj_dypgen__nested_nt_93"
    | Obj_dypgen__nested_nt_94 _ -> "Obj_dypgen__nested_nt_94"
    | Obj_dypgen__nested_nt_95 _ -> "Obj_dypgen__nested_nt_95"
    | Obj_dypgen__nested_nt_96 _ -> "Obj_dypgen__nested_nt_96"
    | Obj_dypgen__nested_nt_97 _ -> "Obj_dypgen__nested_nt_97"
    | Obj_dypgen__nested_nt_98 _ -> "Obj_dypgen__nested_nt_98"
    | Obj_dypgen__nested_nt_99 _ -> "Obj_dypgen__nested_nt_99"
    | Obj_dypgen__option_EOL _ -> "Obj_dypgen__option_EOL"
    | Obj_dypgen__option_eos _ -> "Obj_dypgen__option_eos"
    | Obj_dypgen__option_patterns _ -> "Obj_dypgen__option_patterns"
    | Obj_dypgen__option_pipe _ -> "Obj_dypgen__option_pipe"
    | Obj_dypgen__star___block_body_stmt _ -> "Obj_dypgen__star___block_body_stmt"
    | Obj_eos _ -> "Obj_eos"
    | Obj_eqeq_op _ -> "Obj_eqeq_op"
    | Obj_equal _ -> "Obj_equal"
    | Obj_export_id_str _ -> "Obj_export_id_str"
    | Obj_export_stmt _ -> "Obj_export_stmt"
    | Obj_expr _ -> "Obj_expr"
    | Obj_ext_constructor _ -> "Obj_ext_constructor"
    | Obj_file_path _ -> "Obj_file_path"
    | Obj_foreign_stmt _ -> "Obj_foreign_stmt"
    | Obj_greatereq_op _ -> "Obj_greatereq_op"
    | Obj_id _ -> "Obj_id"
    | Obj_id_expr _ -> "Obj_id_expr"
    | Obj_id_str _ -> "Obj_id_str"
    | Obj_if_expr _ -> "Obj_if_expr"
    | Obj_import_shape _ -> "Obj_import_shape"
    | Obj_import_stmt _ -> "Obj_import_stmt"
    | Obj_infix _ -> "Obj_infix"
    | Obj_infix_op _ -> "Obj_infix_op"
    | Obj_lam_args _ -> "Obj_lam_args"
    | Obj_lam_expr _ -> "Obj_lam_expr"
    | Obj_lbrace _ -> "Obj_lbrace"
    | Obj_lbrack _ -> "Obj_lbrack"
    | Obj_lcaret _ -> "Obj_lcaret"
    | Obj_lcaret_op _ -> "Obj_lcaret_op"
    | Obj_lesseq_op _ -> "Obj_lesseq_op"
    | Obj_let_expr _ -> "Obj_let_expr"
    | Obj_list_expr _ -> "Obj_list_expr"
    | Obj_lparen _ -> "Obj_lparen"
    | Obj_match_branch _ -> "Obj_match_branch"
    | Obj_match_branches _ -> "Obj_match_branches"
    | Obj_match_expr _ -> "Obj_match_expr"
    | Obj_non_assign_expr _ -> "Obj_non_assign_expr"
    | Obj_non_binop_expr _ -> "Obj_non_binop_expr"
    | Obj_paren_expr _ -> "Obj_paren_expr"
    | Obj_pattern _ -> "Obj_pattern"
    | Obj_patterns _ -> "Obj_patterns"
    | Obj_percent_op _ -> "Obj_percent_op"
    | Obj_pipe _ -> "Obj_pipe"
    | Obj_pipepipe_op _ -> "Obj_pipepipe_op"
    | Obj_plus_op _ -> "Obj_plus_op"
    | Obj_pluseq_op _ -> "Obj_pluseq_op"
    | Obj_prefix _ -> "Obj_prefix"
    | Obj_prim1_expr _ -> "Obj_prim1_expr"
    | Obj_primitive _ -> "Obj_primitive"
    | Obj_primitive_stmt _ -> "Obj_primitive_stmt"
    | Obj_program _ -> "Obj_program"
    | Obj_rbrace _ -> "Obj_rbrace"
    | Obj_rbrack _ -> "Obj_rbrack"
    | Obj_rcaret _ -> "Obj_rcaret"
    | Obj_rcaret_op _ -> "Obj_rcaret_op"
    | Obj_record_exprs _ -> "Obj_record_exprs"
    | Obj_record_field _ -> "Obj_record_field"
    | Obj_record_get _ -> "Obj_record_get"
    | Obj_record_pattern _ -> "Obj_record_pattern"
    | Obj_record_patterns _ -> "Obj_record_patterns"
    | Obj_record_pun _ -> "Obj_record_pun"
    | Obj_rparen _ -> "Obj_rparen"
    | Obj_simple_expr _ -> "Obj_simple_expr"
    | Obj_simple_id _ -> "Obj_simple_id"
    | Obj_slash_op _ -> "Obj_slash_op"
    | Obj_slasheq_op _ -> "Obj_slasheq_op"
    | Obj_star_op _ -> "Obj_star_op"
    | Obj_stareq_op _ -> "Obj_stareq_op"
    | Obj_stmt_expr _ -> "Obj_stmt_expr"
    | Obj_thickarrow _ -> "Obj_thickarrow"
    | Obj_toplevel_stmt _ -> "Obj_toplevel_stmt"
    | Obj_toplevel_stmts _ -> "Obj_toplevel_stmts"
    | Obj_tuple_exprs _ -> "Obj_tuple_exprs"
    | Obj_tuple_patterns _ -> "Obj_tuple_patterns"
    | Obj_tuple_typs _ -> "Obj_tuple_typs"
    | Obj_typ _ -> "Obj_typ"
    | Obj_type_id _ -> "Obj_type_id"
    | Obj_type_id_str _ -> "Obj_type_id_str"
    | Obj_typs _ -> "Obj_typs"
    | Obj_value_bind _ -> "Obj_value_bind"
    | Obj_value_binds _ -> "Obj_value_binds"
    | Obj_while_expr _ -> "Obj_while_expr"
    | _ -> failwith "str_cons, unexpected constructor"
  let cons_array = [|
    "Lexeme_matched";
    "Obj_ID";
    "Obj_NUM";
    "Obj_STRING";
    "Obj_TYPEID";
    "Obj_ampamp_op";
    "Obj_any_id_str";
    "Obj_any_or_var_pat";
    "Obj_app_arg_exprs";
    "Obj_app_expr";
    "Obj_array_expr";
    "Obj_array_get";
    "Obj_array_set";
    "Obj_arrow";
    "Obj_assign_expr";
    "Obj_binop_expr";
    "Obj_block";
    "Obj_block_body";
    "Obj_block_body_expr";
    "Obj_block_body_stmt";
    "Obj_block_expr";
    "Obj_block_or_expr";
    "Obj_colon";
    "Obj_comma";
    "Obj_const";
    "Obj_dash_op";
    "Obj_dasheq_op";
    "Obj_data_constructor";
    "Obj_data_constructors";
    "Obj_data_declaration";
    "Obj_data_label";
    "Obj_data_labels";
    "Obj_data_typ";
    "Obj_dot";
    "Obj_dypgen__nested_nt_0";
    "Obj_dypgen__nested_nt_1";
    "Obj_dypgen__nested_nt_10";
    "Obj_dypgen__nested_nt_100";
    "Obj_dypgen__nested_nt_101";
    "Obj_dypgen__nested_nt_102";
    "Obj_dypgen__nested_nt_103";
    "Obj_dypgen__nested_nt_104";
    "Obj_dypgen__nested_nt_105";
    "Obj_dypgen__nested_nt_106";
    "Obj_dypgen__nested_nt_107";
    "Obj_dypgen__nested_nt_108";
    "Obj_dypgen__nested_nt_109";
    "Obj_dypgen__nested_nt_11";
    "Obj_dypgen__nested_nt_110";
    "Obj_dypgen__nested_nt_111";
    "Obj_dypgen__nested_nt_12";
    "Obj_dypgen__nested_nt_13";
    "Obj_dypgen__nested_nt_14";
    "Obj_dypgen__nested_nt_15";
    "Obj_dypgen__nested_nt_16";
    "Obj_dypgen__nested_nt_17";
    "Obj_dypgen__nested_nt_18";
    "Obj_dypgen__nested_nt_19";
    "Obj_dypgen__nested_nt_2";
    "Obj_dypgen__nested_nt_20";
    "Obj_dypgen__nested_nt_21";
    "Obj_dypgen__nested_nt_22";
    "Obj_dypgen__nested_nt_23";
    "Obj_dypgen__nested_nt_24";
    "Obj_dypgen__nested_nt_25";
    "Obj_dypgen__nested_nt_26";
    "Obj_dypgen__nested_nt_27";
    "Obj_dypgen__nested_nt_28";
    "Obj_dypgen__nested_nt_29";
    "Obj_dypgen__nested_nt_3";
    "Obj_dypgen__nested_nt_30";
    "Obj_dypgen__nested_nt_31";
    "Obj_dypgen__nested_nt_32";
    "Obj_dypgen__nested_nt_33";
    "Obj_dypgen__nested_nt_34";
    "Obj_dypgen__nested_nt_35";
    "Obj_dypgen__nested_nt_36";
    "Obj_dypgen__nested_nt_37";
    "Obj_dypgen__nested_nt_38";
    "Obj_dypgen__nested_nt_39";
    "Obj_dypgen__nested_nt_4";
    "Obj_dypgen__nested_nt_40";
    "Obj_dypgen__nested_nt_41";
    "Obj_dypgen__nested_nt_42";
    "Obj_dypgen__nested_nt_43";
    "Obj_dypgen__nested_nt_44";
    "Obj_dypgen__nested_nt_45";
    "Obj_dypgen__nested_nt_46";
    "Obj_dypgen__nested_nt_47";
    "Obj_dypgen__nested_nt_48";
    "Obj_dypgen__nested_nt_49";
    "Obj_dypgen__nested_nt_5";
    "Obj_dypgen__nested_nt_50";
    "Obj_dypgen__nested_nt_51";
    "Obj_dypgen__nested_nt_52";
    "Obj_dypgen__nested_nt_53";
    "Obj_dypgen__nested_nt_54";
    "Obj_dypgen__nested_nt_55";
    "Obj_dypgen__nested_nt_56";
    "Obj_dypgen__nested_nt_57";
    "Obj_dypgen__nested_nt_58";
    "Obj_dypgen__nested_nt_59";
    "Obj_dypgen__nested_nt_6";
    "Obj_dypgen__nested_nt_60";
    "Obj_dypgen__nested_nt_61";
    "Obj_dypgen__nested_nt_62";
    "Obj_dypgen__nested_nt_63";
    "Obj_dypgen__nested_nt_64";
    "Obj_dypgen__nested_nt_65";
    "Obj_dypgen__nested_nt_66";
    "Obj_dypgen__nested_nt_67";
    "Obj_dypgen__nested_nt_68";
    "Obj_dypgen__nested_nt_69";
    "Obj_dypgen__nested_nt_7";
    "Obj_dypgen__nested_nt_70";
    "Obj_dypgen__nested_nt_71";
    "Obj_dypgen__nested_nt_72";
    "Obj_dypgen__nested_nt_73";
    "Obj_dypgen__nested_nt_74";
    "Obj_dypgen__nested_nt_75";
    "Obj_dypgen__nested_nt_76";
    "Obj_dypgen__nested_nt_77";
    "Obj_dypgen__nested_nt_78";
    "Obj_dypgen__nested_nt_79";
    "Obj_dypgen__nested_nt_8";
    "Obj_dypgen__nested_nt_80";
    "Obj_dypgen__nested_nt_81";
    "Obj_dypgen__nested_nt_82";
    "Obj_dypgen__nested_nt_83";
    "Obj_dypgen__nested_nt_84";
    "Obj_dypgen__nested_nt_85";
    "Obj_dypgen__nested_nt_86";
    "Obj_dypgen__nested_nt_87";
    "Obj_dypgen__nested_nt_88";
    "Obj_dypgen__nested_nt_89";
    "Obj_dypgen__nested_nt_9";
    "Obj_dypgen__nested_nt_90";
    "Obj_dypgen__nested_nt_91";
    "Obj_dypgen__nested_nt_92";
    "Obj_dypgen__nested_nt_93";
    "Obj_dypgen__nested_nt_94";
    "Obj_dypgen__nested_nt_95";
    "Obj_dypgen__nested_nt_96";
    "Obj_dypgen__nested_nt_97";
    "Obj_dypgen__nested_nt_98";
    "Obj_dypgen__nested_nt_99";
    "Obj_dypgen__option_EOL";
    "Obj_dypgen__option_eos";
    "Obj_dypgen__option_patterns";
    "Obj_dypgen__option_pipe";
    "Obj_dypgen__star___block_body_stmt";
    "Obj_eos";
    "Obj_eqeq_op";
    "Obj_equal";
    "Obj_export_id_str";
    "Obj_export_stmt";
    "Obj_expr";
    "Obj_ext_constructor";
    "Obj_file_path";
    "Obj_foreign_stmt";
    "Obj_greatereq_op";
    "Obj_id";
    "Obj_id_expr";
    "Obj_id_str";
    "Obj_if_expr";
    "Obj_import_shape";
    "Obj_import_stmt";
    "Obj_infix";
    "Obj_infix_op";
    "Obj_lam_args";
    "Obj_lam_expr";
    "Obj_lbrace";
    "Obj_lbrack";
    "Obj_lcaret";
    "Obj_lcaret_op";
    "Obj_lesseq_op";
    "Obj_let_expr";
    "Obj_list_expr";
    "Obj_lparen";
    "Obj_match_branch";
    "Obj_match_branches";
    "Obj_match_expr";
    "Obj_non_assign_expr";
    "Obj_non_binop_expr";
    "Obj_paren_expr";
    "Obj_pattern";
    "Obj_patterns";
    "Obj_percent_op";
    "Obj_pipe";
    "Obj_pipepipe_op";
    "Obj_plus_op";
    "Obj_pluseq_op";
    "Obj_prefix";
    "Obj_prim1_expr";
    "Obj_primitive";
    "Obj_primitive_stmt";
    "Obj_program";
    "Obj_rbrace";
    "Obj_rbrack";
    "Obj_rcaret";
    "Obj_rcaret_op";
    "Obj_record_exprs";
    "Obj_record_field";
    "Obj_record_get";
    "Obj_record_pattern";
    "Obj_record_patterns";
    "Obj_record_pun";
    "Obj_rparen";
    "Obj_simple_expr";
    "Obj_simple_id";
    "Obj_slash_op";
    "Obj_slasheq_op";
    "Obj_star_op";
    "Obj_stareq_op";
    "Obj_stmt_expr";
    "Obj_thickarrow";
    "Obj_toplevel_stmt";
    "Obj_toplevel_stmts";
    "Obj_tuple_exprs";
    "Obj_tuple_patterns";
    "Obj_tuple_typs";
    "Obj_typ";
    "Obj_type_id";
    "Obj_type_id_str";
    "Obj_typs";
    "Obj_value_bind";
    "Obj_value_binds";
    "Obj_while_expr";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
  |]
  let entry_points = [
    "program";]
end

let dypgen_lexbuf_position lexbuf =
  (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)

module Dyp_aux_functions =
struct
  let get_token_value t = match t with
    | AMPAMP -> Obj_AMPAMP
    | ARROW -> Obj_ARROW
    | AS -> Obj_AS
    | ASSERT -> Obj_ASSERT
    | CARET -> Obj_CARET
    | COLON -> Obj_COLON
    | COLONCOLON -> Obj_COLONCOLON
    | COMMA -> Obj_COMMA
    | DASH -> Obj_DASH
    | DATA -> Obj_DATA
    | DOT -> Obj_DOT
    | ELLIPSIS -> Obj_ELLIPSIS
    | ELSE -> Obj_ELSE
    | EOF -> Obj_EOF
    | EOL -> Obj_EOL
    | EQEQ -> Obj_EQEQ
    | EQUAL -> Obj_EQUAL
    | EXCEPT -> Obj_EXCEPT
    | EXPORT -> Obj_EXPORT
    | FAIL -> Obj_FAIL
    | FALSE -> Obj_FALSE
    | FOREIGN -> Obj_FOREIGN
    | FROM -> Obj_FROM
    | GETS -> Obj_GETS
    | GREATEREQ -> Obj_GREATEREQ
    | ID x -> Obj_ID x
    | IF -> Obj_IF
    | IMPORT -> Obj_IMPORT
    | LBRACE -> Obj_LBRACE
    | LBRACK -> Obj_LBRACK
    | LCARET -> Obj_LCARET
    | LESSEQ -> Obj_LESSEQ
    | LET -> Obj_LET
    | LPAREN -> Obj_LPAREN
    | LPARENNOSPACE -> Obj_LPARENNOSPACE
    | MATCH -> Obj_MATCH
    | NOT -> Obj_NOT
    | NUM x -> Obj_NUM x
    | PERCENT -> Obj_PERCENT
    | PIPE -> Obj_PIPE
    | PIPEPIPE -> Obj_PIPEPIPE
    | PLUS -> Obj_PLUS
    | PRIMITIVE -> Obj_PRIMITIVE
    | RBRACE -> Obj_RBRACE
    | RBRACK -> Obj_RBRACK
    | RCARET -> Obj_RCARET
    | REC -> Obj_REC
    | RPAREN -> Obj_RPAREN
    | SEMI -> Obj_SEMI
    | SLASH -> Obj_SLASH
    | STAR -> Obj_STAR
    | STRING x -> Obj_STRING x
    | THICKARROW -> Obj_THICKARROW
    | TRUE -> Obj_TRUE
    | TYPEID x -> Obj_TYPEID x
    | UNDERSCORE -> Obj_UNDERSCORE
    | WASM -> Obj_WASM
    | WHILE -> Obj_WHILE
  let cons_table = Dyp.Tools.hashtbl_of_array Dyp_symbols_array.cons_array
end

module Dyp_priority_data =
struct
  let relations = [
    ["pe";"pt";"pp";"pb";"pc";"pa";"pl";];
  ]
end

let global_data = ()
let local_data = ()
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_Lexeme_matched = Dyp.Tools.keep_zero
let dyp_merge_Obj_ID = Dyp.Tools.keep_zero
let dyp_merge_Obj_NUM = Dyp.Tools.keep_zero
let dyp_merge_Obj_STRING = Dyp.Tools.keep_zero
let dyp_merge_Obj_TYPEID = Dyp.Tools.keep_zero
let dyp_merge_Obj_ampamp_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_any_id_str = Dyp.Tools.keep_zero
let dyp_merge_Obj_any_or_var_pat = Dyp.Tools.keep_zero
let dyp_merge_Obj_app_arg_exprs = Dyp.Tools.keep_zero
let dyp_merge_Obj_app_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_array_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_array_get = Dyp.Tools.keep_zero
let dyp_merge_Obj_array_set = Dyp.Tools.keep_zero
let dyp_merge_Obj_arrow = Dyp.Tools.keep_zero
let dyp_merge_Obj_assign_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_binop_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_block = Dyp.Tools.keep_zero
let dyp_merge_Obj_block_body = Dyp.Tools.keep_zero
let dyp_merge_Obj_block_body_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_block_body_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_block_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_block_or_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_colon = Dyp.Tools.keep_zero
let dyp_merge_Obj_comma = Dyp.Tools.keep_zero
let dyp_merge_Obj_const = Dyp.Tools.keep_zero
let dyp_merge_Obj_dash_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_dasheq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_data_constructor = Dyp.Tools.keep_zero
let dyp_merge_Obj_data_constructors = Dyp.Tools.keep_zero
let dyp_merge_Obj_data_declaration = Dyp.Tools.keep_zero
let dyp_merge_Obj_data_label = Dyp.Tools.keep_zero
let dyp_merge_Obj_data_labels = Dyp.Tools.keep_zero
let dyp_merge_Obj_data_typ = Dyp.Tools.keep_zero
let dyp_merge_Obj_dot = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_0 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_1 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_10 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_100 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_101 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_102 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_103 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_104 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_105 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_106 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_107 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_108 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_109 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_11 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_110 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_111 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_12 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_13 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_14 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_15 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_16 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_17 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_18 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_19 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_2 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_20 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_21 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_22 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_23 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_24 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_25 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_26 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_27 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_28 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_29 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_3 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_30 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_31 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_32 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_33 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_34 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_35 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_36 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_37 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_38 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_39 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_4 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_40 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_41 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_42 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_43 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_44 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_45 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_46 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_47 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_48 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_49 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_5 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_50 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_51 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_52 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_53 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_54 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_55 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_56 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_57 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_58 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_59 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_6 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_60 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_61 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_62 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_63 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_64 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_65 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_66 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_67 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_68 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_69 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_7 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_70 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_71 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_72 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_73 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_74 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_75 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_76 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_77 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_78 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_79 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_8 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_80 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_81 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_82 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_83 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_84 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_85 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_86 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_87 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_88 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_89 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_9 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_90 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_91 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_92 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_93 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_94 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_95 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_96 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_97 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_98 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_99 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_EOL = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_eos = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_patterns = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_pipe = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__star___block_body_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_eos = Dyp.Tools.keep_zero
let dyp_merge_Obj_eqeq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_equal = Dyp.Tools.keep_zero
let dyp_merge_Obj_export_id_str = Dyp.Tools.keep_zero
let dyp_merge_Obj_export_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_ext_constructor = Dyp.Tools.keep_zero
let dyp_merge_Obj_file_path = Dyp.Tools.keep_zero
let dyp_merge_Obj_foreign_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_greatereq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_id = Dyp.Tools.keep_zero
let dyp_merge_Obj_id_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_id_str = Dyp.Tools.keep_zero
let dyp_merge_Obj_if_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_import_shape = Dyp.Tools.keep_zero
let dyp_merge_Obj_import_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_infix = Dyp.Tools.keep_zero
let dyp_merge_Obj_infix_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_lam_args = Dyp.Tools.keep_zero
let dyp_merge_Obj_lam_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_lbrace = Dyp.Tools.keep_zero
let dyp_merge_Obj_lbrack = Dyp.Tools.keep_zero
let dyp_merge_Obj_lcaret = Dyp.Tools.keep_zero
let dyp_merge_Obj_lcaret_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_lesseq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_let_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_list_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_lparen = Dyp.Tools.keep_zero
let dyp_merge_Obj_match_branch = Dyp.Tools.keep_zero
let dyp_merge_Obj_match_branches = Dyp.Tools.keep_zero
let dyp_merge_Obj_match_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_non_assign_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_non_binop_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_paren_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_pattern = Dyp.Tools.keep_zero
let dyp_merge_Obj_patterns = Dyp.Tools.keep_zero
let dyp_merge_Obj_percent_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_pipe = Dyp.Tools.keep_zero
let dyp_merge_Obj_pipepipe_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_plus_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_pluseq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_prefix = Dyp.Tools.keep_zero
let dyp_merge_Obj_prim1_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_primitive = Dyp.Tools.keep_zero
let dyp_merge_Obj_primitive_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_program = Dyp.Tools.keep_zero
let dyp_merge_Obj_rbrace = Dyp.Tools.keep_zero
let dyp_merge_Obj_rbrack = Dyp.Tools.keep_zero
let dyp_merge_Obj_rcaret = Dyp.Tools.keep_zero
let dyp_merge_Obj_rcaret_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_record_exprs = Dyp.Tools.keep_zero
let dyp_merge_Obj_record_field = Dyp.Tools.keep_zero
let dyp_merge_Obj_record_get = Dyp.Tools.keep_zero
let dyp_merge_Obj_record_pattern = Dyp.Tools.keep_zero
let dyp_merge_Obj_record_patterns = Dyp.Tools.keep_zero
let dyp_merge_Obj_record_pun = Dyp.Tools.keep_zero
let dyp_merge_Obj_rparen = Dyp.Tools.keep_zero
let dyp_merge_Obj_simple_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_simple_id = Dyp.Tools.keep_zero
let dyp_merge_Obj_slash_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_slasheq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_star_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_stareq_op = Dyp.Tools.keep_zero
let dyp_merge_Obj_stmt_expr = Dyp.Tools.keep_zero
let dyp_merge_Obj_thickarrow = Dyp.Tools.keep_zero
let dyp_merge_Obj_toplevel_stmt = Dyp.Tools.keep_zero
let dyp_merge_Obj_toplevel_stmts = Dyp.Tools.keep_zero
let dyp_merge_Obj_tuple_exprs = Dyp.Tools.keep_zero
let dyp_merge_Obj_tuple_patterns = Dyp.Tools.keep_zero
let dyp_merge_Obj_tuple_typs = Dyp.Tools.keep_zero
let dyp_merge_Obj_typ = Dyp.Tools.keep_zero
let dyp_merge_Obj_type_id = Dyp.Tools.keep_zero
let dyp_merge_Obj_type_id_str = Dyp.Tools.keep_zero
let dyp_merge_Obj_typs = Dyp.Tools.keep_zero
let dyp_merge_Obj_value_bind = Dyp.Tools.keep_zero
let dyp_merge_Obj_value_binds = Dyp.Tools.keep_zero
let dyp_merge_Obj_while_expr = Dyp.Tools.keep_zero
let dyp_merge = Dyp.keep_one
let dypgen_match_length = `shortest
let dypgen_choose_token = `first
let dypgen_keep_data = `both
let dypgen_use_rule_order = false
let dypgen_use_all_actions = false

# 1 "parser.dyp"

open Location
open Identifier
open Dyp
open Parsetree
open Ast_helper

(* Used for error messages and as a default, in case anything slips through
   without an explicit loc. *)
let first_loc = ref Location.dummy_loc
let last_loc = ref Location.dummy_loc
let dyp_merge = keep_all

let last_state_printer = ref (fun () -> ())

let when_debug ?n thunk =
  match n with
  | Some(n) ->
      if !Grain_utils.Config.parser_debug_level >= n then
        thunk()
  | None -> ()

let prerr_string s = when_debug ~n:1 (fun () -> Stdlib.prerr_string s)

let debug_print_state () =
  when_debug !last_state_printer

let symbol_rloc dyp =
  let ret = {
    loc_start = dyp.symbol_start_pos ();
    loc_end = dyp.symbol_end_pos ();
    loc_ghost = false;
  } in
  last_state_printer := (fun () -> dyp.print_state stderr);
  when_debug ~n:1 !last_state_printer;
  last_loc := ret;
  ret

let symbol_gloc dyp =
  let ret = {
    loc_start = dyp.symbol_start_pos ();
    loc_end = dyp.symbol_end_pos ();
    loc_ghost = true;
  } in
  last_state_printer := (fun () -> dyp.print_state stderr);
  when_debug ~n:1 !last_state_printer;
  last_loc := ret;
  ret

let rhs_loc dyp n =
  let ret = {
    loc_start = dyp.rhs_start_pos n;
    loc_end = dyp.rhs_end_pos n;
    loc_ghost = false;
  } in
  last_state_printer := (fun () -> dyp.print_state stderr);
  when_debug ~n:1 !last_state_printer;
  last_loc := ret;
  ret

let fix_block_mapper super =
  let open Ast_mapper in
  let expr mapper ({pexp_desc; pexp_loc} as e) =
    match pexp_desc with
    | PExpBlock([]) -> super.expr mapper e
    | PExpBlock(elts) ->
      let elts = List.map (mapper.expr mapper) elts in
      (* Properly nest let bindings *)
      let elts = List.fold_right (fun ({pexp_desc} as stmt) elts ->
        match pexp_desc with
        | PExpLet(r, binds, _) -> [{stmt with pexp_desc=PExpLet(r, binds, {stmt with pexp_desc=PExpBlock(elts)})}]
        | _ -> stmt::elts
      ) elts [] in
      {e with pexp_desc=PExpBlock(elts)}
    | _ -> super.expr mapper e in
  {super with expr}

let fix_tyvar_mapper super =
  let open Ast_mapper in
  let open Ast_helper in
  let typ mapper ({ptyp_desc; ptyp_loc} as t) =
    match ptyp_desc with
    | PTyVar v when (v <> "") && (match v.[0] with 'A'..'Z' -> true | _ -> false) ->
      let id = mkloc (IdentName v) ptyp_loc in
      {t with ptyp_desc=PTyConstr(id, [])}
    | _ -> super.typ mapper t in
  {super with typ}

let fix_blocks ({statements} as prog) =
  let open Ast_mapper in
  let mapper = default_mapper
    |> fix_block_mapper
    |> fix_tyvar_mapper in
  {prog with
   statements=List.map (mapper.toplevel mapper) statements}

let no_record_block exprs =
  match exprs with
  | [{pexp_desc=PExpId {txt=IdentName _}}] -> raise Dyp.Giveup
  | _ -> ()

let no_brace_expr expr =
  match expr.pexp_desc with
  | PExpBlock _
  | PExpRecord _ -> raise Dyp.Giveup
  | _ -> ()

let no_uppercase_ident expr =
  match expr.pexp_desc with
  | PExpId {txt=id} ->
    let first_char = String.get (Identifier.last id) 0 in
    if first_char = BatChar.uppercase first_char then raise Dyp.Giveup
  | _ -> ()

let no_array_access expr =
  match expr.pexp_desc with
  | PExpArrayGet _ -> raise Dyp.Giveup
  | _ -> ()


let mkid ns =
  let help ns =
    let rec help ns (acc_ident, acc_str) =
      let ident = Option.map_default (fun i -> IdentExternal(i, acc_str)) (IdentName acc_str) acc_ident in
      match ns with
        | [] -> ident
        | n::tl -> help tl (Some ident, n) in
    match ns with
      | [] -> failwith "Should be impossible"
      | n::tl -> help tl (None, n) in
  mkloc @@ help ns

let mkid_expr dyp ns = Exp.ident ~loc:(symbol_rloc dyp) (mkid ns (symbol_rloc dyp))

let mkstr dyp s = mkloc s (symbol_rloc dyp)

let make_program statements =
  let prog_loc = {
    loc_start=(!first_loc).loc_end;
    loc_end=(!last_loc).loc_end;
    loc_ghost=false;
  } in
  fix_blocks {statements; prog_loc}


let _ = () (* dummy line to improve OCaml error location *)
# 1799               "parser.ml"
let __dypgen_ra_list, __dypgen_main_lexer, __dypgen_aux_lexer =
[
(("eos",[Dyp.Ter "EOL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_eos 
# 178 "parser.dyp"
(
        ( None ):'dypgen__Obj_eos)
# 1808               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("eos",[Dyp.Ter "SEMI";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1816               "parser.ml"
 as _2))] -> Obj_eos 
# 179 "parser.dyp"
(
              ( None ):'dypgen__Obj_eos)
# 1821               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_EOL",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_EOL 
(
(None):'dypgen__Obj_dypgen__option_EOL)
# 1830               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_EOL",[Dyp.Ter "EOL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_dypgen__option_EOL 
(
(None):'dypgen__Obj_dypgen__option_EOL)
# 1839               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lbrack",[Dyp.Ter "LBRACK";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1847               "parser.ml"
 as _2))] -> Obj_lbrack 
# 182 "parser.dyp"
(
                ( () ):'dypgen__Obj_lbrack)
# 1852               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rbrack",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "RBRACK"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1860               "parser.ml"
 as _1)); _2] -> Obj_rbrack 
# 185 "parser.dyp"
(
                ( () ):'dypgen__Obj_rbrack)
# 1865               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lparen",[Dyp.Ter "LPAREN";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1873               "parser.ml"
 as _2))] -> Obj_lparen 
# 188 "parser.dyp"
(
                ( () ):'dypgen__Obj_lparen)
# 1878               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rparen",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1886               "parser.ml"
 as _1)); _2] -> Obj_rparen 
# 191 "parser.dyp"
(
                ( () ):'dypgen__Obj_rparen)
# 1891               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lbrace",[Dyp.Ter "LBRACE";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1899               "parser.ml"
 as _2))] -> Obj_lbrace 
# 194 "parser.dyp"
(
                ( () ):'dypgen__Obj_lbrace)
# 1904               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rbrace",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "RBRACE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1912               "parser.ml"
 as _1)); _2] -> Obj_rbrace 
# 197 "parser.dyp"
(
                ( () ):'dypgen__Obj_rbrace)
# 1917               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lcaret",[Dyp.Ter "LCARET";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1925               "parser.ml"
 as _2))] -> Obj_lcaret 
# 200 "parser.dyp"
(
                ( () ):'dypgen__Obj_lcaret)
# 1930               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rcaret",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "RCARET"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1938               "parser.ml"
 as _1)); _2] -> Obj_rcaret 
# 203 "parser.dyp"
(
                ( () ):'dypgen__Obj_rcaret)
# 1943               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("comma",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "COMMA";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1951               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1954               "parser.ml"
 as _3))] -> Obj_comma 
# 206 "parser.dyp"
(
                    ( () ):'dypgen__Obj_comma)
# 1959               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("colon",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "COLON";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1967               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1970               "parser.ml"
 as _3))] -> Obj_colon 
# 209 "parser.dyp"
(
                    ( () ):'dypgen__Obj_colon)
# 1975               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dot",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "DOT";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1983               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1986               "parser.ml"
 as _3))] -> Obj_dot 
# 212 "parser.dyp"
(
                  ( () ):'dypgen__Obj_dot)
# 1991               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("arrow",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "ARROW";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 1999               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2002               "parser.ml"
 as _3))] -> Obj_arrow 
# 215 "parser.dyp"
(
                    ( () ):'dypgen__Obj_arrow)
# 2007               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("thickarrow",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "THICKARROW";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2015               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2018               "parser.ml"
 as _3))] -> Obj_thickarrow 
# 218 "parser.dyp"
(
                         ( () ):'dypgen__Obj_thickarrow)
# 2023               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pipe",[Dyp.Ter "PIPE";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2031               "parser.ml"
 as _2))] -> Obj_pipe 
# 221 "parser.dyp"
(
              ( () ):'dypgen__Obj_pipe)
# 2036               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("equal",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "EQUAL";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2044               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2047               "parser.ml"
 as _3))] -> Obj_equal 
# 224 "parser.dyp"
(
                    ( () ):'dypgen__Obj_equal)
# 2052               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("const",[Dyp.Ter "NUM"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_NUM  (
(_:(int))
# 2060               "parser.ml"
 as _1)] -> Obj_const 
# 227 "parser.dyp"
(
        ( Const.int _1 ):'dypgen__Obj_const)
# 2065               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("const",[Dyp.Ter "TRUE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_const 
# 228 "parser.dyp"
(
         ( Const.bool true ):'dypgen__Obj_const)
# 2075               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("const",[Dyp.Ter "FALSE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_const 
# 229 "parser.dyp"
(
          ( Const.bool false ):'dypgen__Obj_const)
# 2085               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("const",[Dyp.Ter "STRING"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_STRING  (
(_:(string))
# 2093               "parser.ml"
 as _1)] -> Obj_const 
# 230 "parser.dyp"
(
           ( Const.string _1 ):'dypgen__Obj_const)
# 2098               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("expr",[Dyp.Non_ter ("stmt_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_stmt_expr ( (
(_:'dypgen__Obj_stmt_expr)
# 2106               "parser.ml"
 as _1))] -> Obj_expr 
(
(_1):'dypgen__Obj_expr)
# 2110               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("expr",[Dyp.Non_ter ("binop_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2118               "parser.ml"
 as _1))] -> Obj_expr 
(
(_1):'dypgen__Obj_expr)
# 2122               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pa");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("pluseq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pa")],"pa",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2130               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2133               "parser.ml"
 as _2));Obj_pluseq_op ( (
(_:'dypgen__Obj_pluseq_op)
# 2136               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2139               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2142               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 237 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2147               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pa");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("dasheq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pa")],"pa",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2155               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2158               "parser.ml"
 as _2));Obj_dasheq_op ( (
(_:'dypgen__Obj_dasheq_op)
# 2161               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2164               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2167               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 238 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2172               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pa");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("stareq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pa")],"pa",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2180               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2183               "parser.ml"
 as _2));Obj_stareq_op ( (
(_:'dypgen__Obj_stareq_op)
# 2186               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2189               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2192               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 239 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2197               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pa");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("slasheq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pa")],"pa",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2205               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2208               "parser.ml"
 as _2));Obj_slasheq_op ( (
(_:'dypgen__Obj_slasheq_op)
# 2211               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2214               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2217               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 240 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2222               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pp");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("plus_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pp")],"pp",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2230               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2233               "parser.ml"
 as _2));Obj_plus_op ( (
(_:'dypgen__Obj_plus_op)
# 2236               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2239               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2242               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 241 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2247               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pp");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("dash_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pp")],"pp",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2255               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2258               "parser.ml"
 as _2));Obj_dash_op ( (
(_:'dypgen__Obj_dash_op)
# 2261               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2264               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2267               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 242 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2272               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pt");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("star_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pt")],"pt",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2280               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2283               "parser.ml"
 as _2));Obj_star_op ( (
(_:'dypgen__Obj_star_op)
# 2286               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2289               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2292               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 243 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2297               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pt");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("slash_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pt")],"pt",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2305               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2308               "parser.ml"
 as _2));Obj_slash_op ( (
(_:'dypgen__Obj_slash_op)
# 2311               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2314               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2317               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 244 "parser.dyp"
(
                                                            ( Exp.prim2 ~loc:(symbol_rloc dyp) Divide _1 _5 ):'dypgen__Obj_binop_expr)
# 2322               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pb");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("percent_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pb")],"pb",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2330               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2333               "parser.ml"
 as _2));Obj_percent_op ( (
(_:'dypgen__Obj_percent_op)
# 2336               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2339               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2342               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 245 "parser.dyp"
(
                                                            ( Exp.prim2 ~loc:(symbol_rloc dyp) Mod _1 _5 ):'dypgen__Obj_binop_expr)
# 2347               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pc");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("eqeq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pc")],"pc",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2355               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2358               "parser.ml"
 as _2));Obj_eqeq_op ( (
(_:'dypgen__Obj_eqeq_op)
# 2361               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2364               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2367               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 246 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2372               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pc");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("lcaret_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pc")],"pc",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2380               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2383               "parser.ml"
 as _2));Obj_lcaret_op ( (
(_:'dypgen__Obj_lcaret_op)
# 2386               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2389               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2392               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 247 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2397               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pc");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("rcaret_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pc")],"pc",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2405               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2408               "parser.ml"
 as _2));Obj_rcaret_op ( (
(_:'dypgen__Obj_rcaret_op)
# 2411               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2414               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2417               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 248 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2422               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pc");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("lesseq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pc")],"pc",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2430               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2433               "parser.ml"
 as _2));Obj_lesseq_op ( (
(_:'dypgen__Obj_lesseq_op)
# 2436               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2439               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2442               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 249 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2447               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pc");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("greatereq_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pc")],"pc",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2455               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2458               "parser.ml"
 as _2));Obj_greatereq_op ( (
(_:'dypgen__Obj_greatereq_op)
# 2461               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2464               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2467               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 250 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2472               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pb");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("ampamp_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pb")],"pb",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2480               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2483               "parser.ml"
 as _2));Obj_ampamp_op ( (
(_:'dypgen__Obj_ampamp_op)
# 2486               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2489               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2492               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 251 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2497               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.Lesseq_priority "pb");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("pipepipe_op",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pb")],"pb",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2505               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2508               "parser.ml"
 as _2));Obj_pipepipe_op ( (
(_:'dypgen__Obj_pipepipe_op)
# 2511               "parser.ml"
 as _3));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 2514               "parser.ml"
 as _4));Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2517               "parser.ml"
 as _5))] -> Obj_binop_expr 
# 252 "parser.dyp"
(
                                                            ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp [_3]) [_1; _5] ):'dypgen__Obj_binop_expr)
# 2522               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("binop_expr",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"pe",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 2530               "parser.ml"
 as _1));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 2533               "parser.ml"
 as _2));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 2536               "parser.ml"
 as _3))] -> Obj_binop_expr 
# 253 "parser.dyp"
(
                         ( Exp.constraint_ ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_binop_expr)
# 2541               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("lam_expr",Dyp.No_priority )],"pl",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lam_expr ( (
(_:'dypgen__Obj_lam_expr)
# 2549               "parser.ml"
 as _1))] -> Obj_binop_expr 
# 254 "parser.dyp"
(
             ( _1 ):'dypgen__Obj_binop_expr)
# 2554               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("non_assign_expr",Dyp.No_priority )],"pe",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_non_assign_expr ( (
(_:'dypgen__Obj_non_assign_expr)
# 2562               "parser.ml"
 as _1))] -> Obj_binop_expr 
# 255 "parser.dyp"
(
                    ( _1 ):'dypgen__Obj_binop_expr)
# 2567               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("binop_expr",[Dyp.Non_ter ("assign_expr",Dyp.No_priority )],"pa",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_assign_expr ( (
(_:'dypgen__Obj_assign_expr)
# 2575               "parser.ml"
 as _1))] -> Obj_binop_expr 
# 256 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_binop_expr)
# 2580               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_binop_expr",[Dyp.Non_ter ("non_assign_expr",Dyp.No_priority )],"pe",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_non_assign_expr ( (
(_:'dypgen__Obj_non_assign_expr)
# 2588               "parser.ml"
 as _1))] -> Obj_non_binop_expr 
# 259 "parser.dyp"
(
                    ( _1 ):'dypgen__Obj_non_binop_expr)
# 2593               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_binop_expr",[Dyp.Non_ter ("assign_expr",Dyp.No_priority )],"pa",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_assign_expr ( (
(_:'dypgen__Obj_assign_expr)
# 2601               "parser.ml"
 as _1))] -> Obj_non_binop_expr 
# 260 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_binop_expr)
# 2606               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 2614               "parser.ml"
 as _1));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 2617               "parser.ml"
 as _2));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 2620               "parser.ml"
 as _3))] -> Obj_pattern 
# 263 "parser.dyp"
(
                      ( Pat.constraint_ ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_pattern)
# 2625               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Ter "UNDERSCORE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_pattern 
# 264 "parser.dyp"
(
               ( Pat.any ~loc:(symbol_rloc dyp) () ):'dypgen__Obj_pattern)
# 2635               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("ext_constructor",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ext_constructor ( (
(_:'dypgen__Obj_ext_constructor)
# 2643               "parser.ml"
 as _1))] -> Obj_pattern 
# 266 "parser.dyp"
(
                    ( Pat.construct ~loc:(symbol_rloc dyp) _1 [] ):'dypgen__Obj_pattern)
# 2648               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("dypgen__nested_nt_0",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_0 ( (
(_:'dypgen__Obj_dypgen__nested_nt_0)
# 2656               "parser.ml"
 as _1))] -> Obj_pattern 
# 267 "parser.dyp"
(
                 ( Pat.var ~loc:(symbol_rloc dyp) (mkstr dyp _1) ):'dypgen__Obj_pattern)
# 2661               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("tuple_patterns",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 2669               "parser.ml"
 as _1));Obj_tuple_patterns ( (
(_:'dypgen__Obj_tuple_patterns)
# 2672               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 2675               "parser.ml"
 as _3))] -> Obj_pattern 
# 268 "parser.dyp"
(
                                 ( Pat.tuple ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_pattern)
# 2680               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 2688               "parser.ml"
 as _1));Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 2691               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 2694               "parser.ml"
 as _3))] -> Obj_pattern 
# 269 "parser.dyp"
(
                          ( _2 ):'dypgen__Obj_pattern)
# 2699               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("record_patterns",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 2707               "parser.ml"
 as _1));Obj_record_patterns ( (
(_:'dypgen__Obj_record_patterns)
# 2710               "parser.ml"
 as _2));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 2713               "parser.ml"
 as _3))] -> Obj_pattern 
# 270 "parser.dyp"
(
                                  ( Pat.record ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_pattern)
# 2718               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("type_id",Dyp.No_priority );Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("patterns",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_type_id ( (
(_:'dypgen__Obj_type_id)
# 2726               "parser.ml"
 as _1));Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 2729               "parser.ml"
 as _2));Obj_patterns ( (
(_:'dypgen__Obj_patterns)
# 2732               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 2735               "parser.ml"
 as _4))] -> Obj_pattern 
# 271 "parser.dyp"
(
                                   ( Pat.construct ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_pattern)
# 2740               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("type_id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_type_id ( (
(_:'dypgen__Obj_type_id)
# 2748               "parser.ml"
 as _1))] -> Obj_pattern 
# 272 "parser.dyp"
(
            ( Pat.construct ~loc:(symbol_rloc dyp) _1 [] ):'dypgen__Obj_pattern)
# 2753               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("patterns",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_2",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 2761               "parser.ml"
 as _1));Obj_patterns ( (
(_:'dypgen__Obj_patterns)
# 2764               "parser.ml"
 as _2));Obj_dypgen__nested_nt_2 ( (
(_:'dypgen__Obj_dypgen__nested_nt_2)
# 2767               "parser.ml"
 as _3));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 2770               "parser.ml"
 as _4))] -> Obj_pattern 
# 273 "parser.dyp"
(
                                                                 ( Pat.list ~loc:(symbol_rloc dyp) _2 _3 ):'dypgen__Obj_pattern)
# 2775               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pattern",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_4",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 2783               "parser.ml"
 as _1));Obj_dypgen__nested_nt_4 ( (
(_:'dypgen__Obj_dypgen__nested_nt_4)
# 2786               "parser.ml"
 as _2));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 2789               "parser.ml"
 as _3))] -> Obj_pattern 
# 274 "parser.dyp"
(
                                                  ( Pat.list ~loc:(symbol_rloc dyp) [] _2 ):'dypgen__Obj_pattern)
# 2794               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_0",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 2802               "parser.ml"
 as _1)] -> Obj_dypgen__nested_nt_0 
(
(_1):'dypgen__Obj_dypgen__nested_nt_0)
# 2806               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_0",[Dyp.Non_ter ("infix",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_infix ( (
(_:'dypgen__Obj_infix)
# 2814               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_0 
(
(_1):'dypgen__Obj_dypgen__nested_nt_0)
# 2818               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_1",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Ter "ELLIPSIS";Dyp.Non_ter ("any_or_var_pat",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 2826               "parser.ml"
 as _1)); _2;Obj_any_or_var_pat ( (
(_:'dypgen__Obj_any_or_var_pat)
# 2829               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_1 
# 273 "parser.dyp"
(
                                                   (_3):'dypgen__Obj_dypgen__nested_nt_1)
# 2834               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_2",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_2 
(
(None):'dypgen__Obj_dypgen__nested_nt_2)
# 2843               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_2",[Dyp.Non_ter ("dypgen__nested_nt_1",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_1 ( (
(_:'dypgen__Obj_dypgen__nested_nt_1)
# 2851               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_2 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_2)
# 2855               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_3",[Dyp.Ter "ELLIPSIS";Dyp.Non_ter ("any_or_var_pat",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_any_or_var_pat ( (
(_:'dypgen__Obj_any_or_var_pat)
# 2863               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_3 
# 274 "parser.dyp"
(
                                    (_2):'dypgen__Obj_dypgen__nested_nt_3)
# 2868               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_4",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_4 
(
(None):'dypgen__Obj_dypgen__nested_nt_4)
# 2877               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_4",[Dyp.Non_ter ("dypgen__nested_nt_3",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_3 ( (
(_:'dypgen__Obj_dypgen__nested_nt_3)
# 2885               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_4 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_4)
# 2889               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("any_or_var_pat",[Dyp.Ter "UNDERSCORE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_any_or_var_pat 
# 277 "parser.dyp"
(
               ( Pat.any ~loc:(symbol_rloc dyp) () ):'dypgen__Obj_any_or_var_pat)
# 2899               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("any_or_var_pat",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 2907               "parser.ml"
 as _1)] -> Obj_any_or_var_pat 
# 278 "parser.dyp"
(
       ( Pat.var ~loc:(symbol_rloc dyp) (mkstr dyp _1) ):'dypgen__Obj_any_or_var_pat)
# 2912               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("patterns",[Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_6",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 2920               "parser.ml"
 as _1));Obj_dypgen__nested_nt_6 ( (
(_:'dypgen__Obj_dypgen__nested_nt_6)
# 2923               "parser.ml"
 as _2))] -> Obj_patterns 
# 281 "parser.dyp"
(
                                  ( _1::_2 ):'dypgen__Obj_patterns)
# 2928               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_5",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 2936               "parser.ml"
 as _1));Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 2939               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_5 
# 281 "parser.dyp"
(
                           (_2):'dypgen__Obj_dypgen__nested_nt_5)
# 2944               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_6",[Dyp.Non_ter ("dypgen__nested_nt_7",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_7 ( (
(_:'dypgen__Obj_dypgen__nested_nt_7)
# 2952               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_6 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_6)
# 2956               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_7",[Dyp.Non_ter ("dypgen__nested_nt_7",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_5",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_7 ( (
(_:'dypgen__Obj_dypgen__nested_nt_7)
# 2964               "parser.ml"
 as _1));Obj_dypgen__nested_nt_5 ( (
(_:'dypgen__Obj_dypgen__nested_nt_5)
# 2967               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_7 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_7)
# 2971               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_7",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_7 
(
([]):'dypgen__Obj_dypgen__nested_nt_7)
# 2980               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("tuple_patterns",[Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("comma",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 2988               "parser.ml"
 as _1));Obj_comma ( (
(_:'dypgen__Obj_comma)
# 2991               "parser.ml"
 as _2))] -> Obj_tuple_patterns 
# 284 "parser.dyp"
(
                  ( [_1] ):'dypgen__Obj_tuple_patterns)
# 2996               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("tuple_patterns",[Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_9",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 3004               "parser.ml"
 as _1));Obj_dypgen__nested_nt_9 ( (
(_:'dypgen__Obj_dypgen__nested_nt_9)
# 3007               "parser.ml"
 as _2))] -> Obj_tuple_patterns 
# 285 "parser.dyp"
(
                                  ( _1::_2 ):'dypgen__Obj_tuple_patterns)
# 3012               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_8",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3020               "parser.ml"
 as _1));Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 3023               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_8 
# 285 "parser.dyp"
(
                           (_2):'dypgen__Obj_dypgen__nested_nt_8)
# 3028               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_9",[Dyp.Non_ter ("dypgen__nested_nt_10",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_10 ( (
(_:'dypgen__Obj_dypgen__nested_nt_10)
# 3036               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_9 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_9)
# 3040               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_10",[Dyp.Non_ter ("dypgen__nested_nt_10",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_8",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_10 ( (
(_:'dypgen__Obj_dypgen__nested_nt_10)
# 3048               "parser.ml"
 as _1));Obj_dypgen__nested_nt_8 ( (
(_:'dypgen__Obj_dypgen__nested_nt_8)
# 3051               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_10 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_10)
# 3055               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_10",[Dyp.Non_ter ("dypgen__nested_nt_8",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_8 ( (
(_:'dypgen__Obj_dypgen__nested_nt_8)
# 3063               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_10 
(
([_1]):'dypgen__Obj_dypgen__nested_nt_10)
# 3067               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_patterns",[Dyp.Non_ter ("record_pattern",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_12",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_record_pattern ( (
(_:'dypgen__Obj_record_pattern)
# 3075               "parser.ml"
 as _1));Obj_dypgen__nested_nt_12 ( (
(_:'dypgen__Obj_dypgen__nested_nt_12)
# 3078               "parser.ml"
 as _2))] -> Obj_record_patterns 
# 288 "parser.dyp"
(
                                                ( _1::_2 ):'dypgen__Obj_record_patterns)
# 3083               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_11",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("record_pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3091               "parser.ml"
 as _1));Obj_record_pattern ( (
(_:'dypgen__Obj_record_pattern)
# 3094               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_11 
# 288 "parser.dyp"
(
                                         (_2):'dypgen__Obj_dypgen__nested_nt_11)
# 3099               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_12",[Dyp.Non_ter ("dypgen__nested_nt_13",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_13 ( (
(_:'dypgen__Obj_dypgen__nested_nt_13)
# 3107               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_12 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_12)
# 3111               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_13",[Dyp.Non_ter ("dypgen__nested_nt_13",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_11",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_13 ( (
(_:'dypgen__Obj_dypgen__nested_nt_13)
# 3119               "parser.ml"
 as _1));Obj_dypgen__nested_nt_11 ( (
(_:'dypgen__Obj_dypgen__nested_nt_11)
# 3122               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_13 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_13)
# 3126               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_13",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_13 
(
([]):'dypgen__Obj_dypgen__nested_nt_13)
# 3135               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_pattern",[Dyp.Ter "UNDERSCORE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_record_pattern 
# 291 "parser.dyp"
(
               ( None, Open ):'dypgen__Obj_record_pattern)
# 3145               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_pattern",[Dyp.Non_ter ("id",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id ( (
(_:'dypgen__Obj_id)
# 3153               "parser.ml"
 as _1));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 3156               "parser.ml"
 as _2));Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 3159               "parser.ml"
 as _3))] -> Obj_record_pattern 
# 292 "parser.dyp"
(
                     ( Some(_1, _3), Closed ):'dypgen__Obj_record_pattern)
# 3164               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_pattern",[Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id ( (
(_:'dypgen__Obj_id)
# 3172               "parser.ml"
 as _1))] -> Obj_record_pattern 
# 293 "parser.dyp"
(
       ( Some(_1, Pat.var ~loc:(symbol_rloc dyp) (mkstr dyp (Identifier.last _1.txt))), Closed ):'dypgen__Obj_record_pattern)
# 3177               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_typ",[Dyp.Non_ter ("type_id",Dyp.No_priority );Dyp.Non_ter ("lcaret",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_15",Dyp.No_priority );Dyp.Non_ter ("rcaret",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_type_id ( (
(_:'dypgen__Obj_type_id)
# 3185               "parser.ml"
 as _1));Obj_lcaret ( (
(_:'dypgen__Obj_lcaret)
# 3188               "parser.ml"
 as _2));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3191               "parser.ml"
 as _3));Obj_dypgen__nested_nt_15 ( (
(_:'dypgen__Obj_dypgen__nested_nt_15)
# 3194               "parser.ml"
 as _4));Obj_rcaret ( (
(_:'dypgen__Obj_rcaret)
# 3197               "parser.ml"
 as _5))] -> Obj_data_typ 
# 296 "parser.dyp"
(
                                                ( Typ.constr _1 (_3::_4) ):'dypgen__Obj_data_typ)
# 3202               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_typ",[Dyp.Non_ter ("type_id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_type_id ( (
(_:'dypgen__Obj_type_id)
# 3210               "parser.ml"
 as _1))] -> Obj_data_typ 
# 297 "parser.dyp"
(
            ( Typ.constr _1 [] ):'dypgen__Obj_data_typ)
# 3215               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_14",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3223               "parser.ml"
 as _1));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3226               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_14 
# 296 "parser.dyp"
(
                                  (_2):'dypgen__Obj_dypgen__nested_nt_14)
# 3231               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_15",[Dyp.Non_ter ("dypgen__nested_nt_16",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_16 ( (
(_:'dypgen__Obj_dypgen__nested_nt_16)
# 3239               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_15 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_15)
# 3243               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_16",[Dyp.Non_ter ("dypgen__nested_nt_16",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_14",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_16 ( (
(_:'dypgen__Obj_dypgen__nested_nt_16)
# 3251               "parser.ml"
 as _1));Obj_dypgen__nested_nt_14 ( (
(_:'dypgen__Obj_dypgen__nested_nt_14)
# 3254               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_16 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_16)
# 3258               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_16",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_16 
(
([]):'dypgen__Obj_dypgen__nested_nt_16)
# 3267               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Non_ter ("data_typ",Dyp.No_priority );Dyp.Non_ter ("arrow",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_data_typ ( (
(_:'dypgen__Obj_data_typ)
# 3275               "parser.ml"
 as _1));Obj_arrow ( (
(_:'dypgen__Obj_arrow)
# 3278               "parser.ml"
 as _2));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3281               "parser.ml"
 as _3))] -> Obj_typ 
# 301 "parser.dyp"
(
                       ( Typ.arrow ~loc:(symbol_rloc dyp) [_1] _3 ):'dypgen__Obj_typ)
# 3286               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Ter "ID";Dyp.Non_ter ("arrow",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 3294               "parser.ml"
 as _1);Obj_arrow ( (
(_:'dypgen__Obj_arrow)
# 3297               "parser.ml"
 as _2));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3300               "parser.ml"
 as _3))] -> Obj_typ 
# 302 "parser.dyp"
(
                 ( Typ.arrow ~loc:(symbol_rloc dyp) [(Typ.var _1)] _3 ):'dypgen__Obj_typ)
# 3305               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("typs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority );Dyp.Non_ter ("arrow",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 3313               "parser.ml"
 as _1));Obj_typs ( (
(_:'dypgen__Obj_typs)
# 3316               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 3319               "parser.ml"
 as _3));Obj_arrow ( (
(_:'dypgen__Obj_arrow)
# 3322               "parser.ml"
 as _4));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3325               "parser.ml"
 as _5))] -> Obj_typ 
# 303 "parser.dyp"
(
                                 ( Typ.arrow ~loc:(symbol_rloc dyp) _2 _5 ):'dypgen__Obj_typ)
# 3330               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("tuple_typs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 3338               "parser.ml"
 as _1));Obj_tuple_typs ( (
(_:'dypgen__Obj_tuple_typs)
# 3341               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 3344               "parser.ml"
 as _3))] -> Obj_typ 
# 304 "parser.dyp"
(
                             ( Typ.tuple ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_typ)
# 3349               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 3357               "parser.ml"
 as _1));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3360               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 3363               "parser.ml"
 as _3))] -> Obj_typ 
# 305 "parser.dyp"
(
                      ( _2 ):'dypgen__Obj_typ)
# 3368               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 3376               "parser.ml"
 as _1)] -> Obj_typ 
# 306 "parser.dyp"
(
       ( Typ.var _1 ):'dypgen__Obj_typ)
# 3381               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typ",[Dyp.Non_ter ("data_typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_data_typ ( (
(_:'dypgen__Obj_data_typ)
# 3389               "parser.ml"
 as _1))] -> Obj_typ 
(
(_1):'dypgen__Obj_typ)
# 3393               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("typs",[Dyp.Non_ter ("dypgen__nested_nt_21",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_21 ( (
(_:'dypgen__Obj_dypgen__nested_nt_21)
# 3401               "parser.ml"
 as _1))] -> Obj_typs 
# 310 "parser.dyp"
(
                                      ( Option.default [] _1 ):'dypgen__Obj_typs)
# 3406               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_20",[Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_18",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3414               "parser.ml"
 as _1));Obj_dypgen__nested_nt_18 ( (
(_:'dypgen__Obj_dypgen__nested_nt_18)
# 3417               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_20 
# 310 "parser.dyp"
(
                           (_1::_2):'dypgen__Obj_dypgen__nested_nt_20)
# 3422               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_17",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3430               "parser.ml"
 as _1));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3433               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_17 
# 310 "parser.dyp"
(
                    (_2):'dypgen__Obj_dypgen__nested_nt_17)
# 3438               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_18",[Dyp.Non_ter ("dypgen__nested_nt_19",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_19 ( (
(_:'dypgen__Obj_dypgen__nested_nt_19)
# 3446               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_18 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_18)
# 3450               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_19",[Dyp.Non_ter ("dypgen__nested_nt_19",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_17",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_19 ( (
(_:'dypgen__Obj_dypgen__nested_nt_19)
# 3458               "parser.ml"
 as _1));Obj_dypgen__nested_nt_17 ( (
(_:'dypgen__Obj_dypgen__nested_nt_17)
# 3461               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_19 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_19)
# 3465               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_19",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_19 
(
([]):'dypgen__Obj_dypgen__nested_nt_19)
# 3474               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_21",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_21 
(
(None):'dypgen__Obj_dypgen__nested_nt_21)
# 3483               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_21",[Dyp.Non_ter ("dypgen__nested_nt_20",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_20 ( (
(_:'dypgen__Obj_dypgen__nested_nt_20)
# 3491               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_21 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_21)
# 3495               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("tuple_typs",[Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("comma",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3503               "parser.ml"
 as _1));Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3506               "parser.ml"
 as _2))] -> Obj_tuple_typs 
# 313 "parser.dyp"
(
              ( [_1] ):'dypgen__Obj_tuple_typs)
# 3511               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("tuple_typs",[Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_23",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3519               "parser.ml"
 as _1));Obj_dypgen__nested_nt_23 ( (
(_:'dypgen__Obj_dypgen__nested_nt_23)
# 3522               "parser.ml"
 as _2))] -> Obj_tuple_typs 
# 314 "parser.dyp"
(
                          ( _1::_2 ):'dypgen__Obj_tuple_typs)
# 3527               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_22",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3535               "parser.ml"
 as _1));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 3538               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_22 
# 314 "parser.dyp"
(
                   (_2):'dypgen__Obj_dypgen__nested_nt_22)
# 3543               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_23",[Dyp.Non_ter ("dypgen__nested_nt_24",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_24 ( (
(_:'dypgen__Obj_dypgen__nested_nt_24)
# 3551               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_23 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_23)
# 3555               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_24",[Dyp.Non_ter ("dypgen__nested_nt_24",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_22",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_24 ( (
(_:'dypgen__Obj_dypgen__nested_nt_24)
# 3563               "parser.ml"
 as _1));Obj_dypgen__nested_nt_22 ( (
(_:'dypgen__Obj_dypgen__nested_nt_22)
# 3566               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_24 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_24)
# 3570               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_24",[Dyp.Non_ter ("dypgen__nested_nt_22",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_22 ( (
(_:'dypgen__Obj_dypgen__nested_nt_22)
# 3578               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_24 
(
([_1]):'dypgen__Obj_dypgen__nested_nt_24)
# 3582               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("value_bind",[Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("equal",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 3590               "parser.ml"
 as _1));Obj_equal ( (
(_:'dypgen__Obj_equal)
# 3593               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 3596               "parser.ml"
 as _3))] -> Obj_value_bind 
# 317 "parser.dyp"
(
                       ( Vb.mk ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_value_bind)
# 3601               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("value_binds",[Dyp.Non_ter ("value_bind",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_26",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_value_bind ( (
(_:'dypgen__Obj_value_bind)
# 3609               "parser.ml"
 as _1));Obj_dypgen__nested_nt_26 ( (
(_:'dypgen__Obj_dypgen__nested_nt_26)
# 3612               "parser.ml"
 as _2))] -> Obj_value_binds 
# 320 "parser.dyp"
(
                                        ( _1::_2 ):'dypgen__Obj_value_binds)
# 3617               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_25",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("value_bind",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3625               "parser.ml"
 as _1));Obj_value_bind ( (
(_:'dypgen__Obj_value_bind)
# 3628               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_25 
# 320 "parser.dyp"
(
                                 (_2):'dypgen__Obj_dypgen__nested_nt_25)
# 3633               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_26",[Dyp.Non_ter ("dypgen__nested_nt_27",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_27 ( (
(_:'dypgen__Obj_dypgen__nested_nt_27)
# 3641               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_26 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_26)
# 3645               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_27",[Dyp.Non_ter ("dypgen__nested_nt_27",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_25",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_27 ( (
(_:'dypgen__Obj_dypgen__nested_nt_27)
# 3653               "parser.ml"
 as _1));Obj_dypgen__nested_nt_25 ( (
(_:'dypgen__Obj_dypgen__nested_nt_25)
# 3656               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_27 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_27)
# 3660               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_27",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_27 
(
([]):'dypgen__Obj_dypgen__nested_nt_27)
# 3669               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("import_shape",[Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id ( (
(_:'dypgen__Obj_id)
# 3677               "parser.ml"
 as _1))] -> Obj_import_shape 
# 323 "parser.dyp"
(
       ( PImportModule, Some _1 ):'dypgen__Obj_import_shape)
# 3682               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("import_shape",[Dyp.Ter "STAR";Dyp.Non_ter ("dypgen__nested_nt_32",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_34",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__nested_nt_32 ( (
(_:'dypgen__Obj_dypgen__nested_nt_32)
# 3690               "parser.ml"
 as _2));Obj_dypgen__nested_nt_34 ( (
(_:'dypgen__Obj_dypgen__nested_nt_34)
# 3693               "parser.ml"
 as _3))] -> Obj_import_shape 
# 324 "parser.dyp"
(
                                                                            ( PImportAllExcept (Option.default [] _2), _3 ):'dypgen__Obj_import_shape)
# 3698               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("import_shape",[Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_43",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_45",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 3706               "parser.ml"
 as _1));Obj_dypgen__nested_nt_43 ( (
(_:'dypgen__Obj_dypgen__nested_nt_43)
# 3709               "parser.ml"
 as _2));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 3712               "parser.ml"
 as _3));Obj_dypgen__nested_nt_45 ( (
(_:'dypgen__Obj_dypgen__nested_nt_45)
# 3715               "parser.ml"
 as _4))] -> Obj_import_shape 
# 325 "parser.dyp"
(
                                                                                                        ( PImportValues (Option.default [] _2), _4 ):'dypgen__Obj_import_shape)
# 3720               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_31",[Dyp.Ter "EXCEPT";Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("id",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_29",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 3728               "parser.ml"
 as _2));Obj_id ( (
(_:'dypgen__Obj_id)
# 3731               "parser.ml"
 as _3));Obj_dypgen__nested_nt_29 ( (
(_:'dypgen__Obj_dypgen__nested_nt_29)
# 3734               "parser.ml"
 as _4));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 3737               "parser.ml"
 as _5))] -> Obj_dypgen__nested_nt_31 
# 324 "parser.dyp"
(
                                                   (_3::_4):'dypgen__Obj_dypgen__nested_nt_31)
# 3742               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_28",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3750               "parser.ml"
 as _1));Obj_id ( (
(_:'dypgen__Obj_id)
# 3753               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_28 
# 324 "parser.dyp"
(
                                     (_2):'dypgen__Obj_dypgen__nested_nt_28)
# 3758               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_29",[Dyp.Non_ter ("dypgen__nested_nt_30",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_30 ( (
(_:'dypgen__Obj_dypgen__nested_nt_30)
# 3766               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_29 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_29)
# 3770               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_30",[Dyp.Non_ter ("dypgen__nested_nt_30",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_28",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_30 ( (
(_:'dypgen__Obj_dypgen__nested_nt_30)
# 3778               "parser.ml"
 as _1));Obj_dypgen__nested_nt_28 ( (
(_:'dypgen__Obj_dypgen__nested_nt_28)
# 3781               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_30 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_30)
# 3785               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_30",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_30 
(
([]):'dypgen__Obj_dypgen__nested_nt_30)
# 3794               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_32",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_32 
(
(None):'dypgen__Obj_dypgen__nested_nt_32)
# 3803               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_32",[Dyp.Non_ter ("dypgen__nested_nt_31",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_31 ( (
(_:'dypgen__Obj_dypgen__nested_nt_31)
# 3811               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_32 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_32)
# 3815               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_33",[Dyp.Ter "AS";Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_id ( (
(_:'dypgen__Obj_id)
# 3823               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_33 
# 324 "parser.dyp"
(
                                                                     (_2):'dypgen__Obj_dypgen__nested_nt_33)
# 3828               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_34",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_34 
(
(None):'dypgen__Obj_dypgen__nested_nt_34)
# 3837               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_34",[Dyp.Non_ter ("dypgen__nested_nt_33",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_33 ( (
(_:'dypgen__Obj_dypgen__nested_nt_33)
# 3845               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_34 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_34)
# 3849               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_42",[Dyp.Non_ter ("id",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_36",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_40",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id ( (
(_:'dypgen__Obj_id)
# 3857               "parser.ml"
 as _1));Obj_dypgen__nested_nt_36 ( (
(_:'dypgen__Obj_dypgen__nested_nt_36)
# 3860               "parser.ml"
 as _2));Obj_dypgen__nested_nt_40 ( (
(_:'dypgen__Obj_dypgen__nested_nt_40)
# 3863               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_42 
# 325 "parser.dyp"
(
                                                                  ((_1, _2)::_3):'dypgen__Obj_dypgen__nested_nt_42)
# 3868               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_35",[Dyp.Ter "AS";Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_id ( (
(_:'dypgen__Obj_id)
# 3876               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_35 
# 325 "parser.dyp"
(
                      (_2):'dypgen__Obj_dypgen__nested_nt_35)
# 3881               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_36",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_36 
(
(None):'dypgen__Obj_dypgen__nested_nt_36)
# 3890               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_36",[Dyp.Non_ter ("dypgen__nested_nt_35",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_35 ( (
(_:'dypgen__Obj_dypgen__nested_nt_35)
# 3898               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_36 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_36)
# 3902               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_39",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("id",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_38",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 3910               "parser.ml"
 as _1));Obj_id ( (
(_:'dypgen__Obj_id)
# 3913               "parser.ml"
 as _2));Obj_dypgen__nested_nt_38 ( (
(_:'dypgen__Obj_dypgen__nested_nt_38)
# 3916               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_39 
# 325 "parser.dyp"
(
                                                     ((_2, _3)):'dypgen__Obj_dypgen__nested_nt_39)
# 3921               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_37",[Dyp.Ter "AS";Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_id ( (
(_:'dypgen__Obj_id)
# 3929               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_37 
# 325 "parser.dyp"
(
                                              (_2):'dypgen__Obj_dypgen__nested_nt_37)
# 3934               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_38",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_38 
(
(None):'dypgen__Obj_dypgen__nested_nt_38)
# 3943               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_38",[Dyp.Non_ter ("dypgen__nested_nt_37",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_37 ( (
(_:'dypgen__Obj_dypgen__nested_nt_37)
# 3951               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_38 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_38)
# 3955               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_40",[Dyp.Non_ter ("dypgen__nested_nt_41",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_41 ( (
(_:'dypgen__Obj_dypgen__nested_nt_41)
# 3963               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_40 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_40)
# 3967               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_41",[Dyp.Non_ter ("dypgen__nested_nt_41",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_39",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_41 ( (
(_:'dypgen__Obj_dypgen__nested_nt_41)
# 3975               "parser.ml"
 as _1));Obj_dypgen__nested_nt_39 ( (
(_:'dypgen__Obj_dypgen__nested_nt_39)
# 3978               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_41 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_41)
# 3982               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_41",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_41 
(
([]):'dypgen__Obj_dypgen__nested_nt_41)
# 3991               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_43",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_43 
(
(None):'dypgen__Obj_dypgen__nested_nt_43)
# 4000               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_43",[Dyp.Non_ter ("dypgen__nested_nt_42",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_42 ( (
(_:'dypgen__Obj_dypgen__nested_nt_42)
# 4008               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_43 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_43)
# 4012               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_44",[Dyp.Ter "AS";Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_id ( (
(_:'dypgen__Obj_id)
# 4020               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_44 
# 325 "parser.dyp"
(
                                                                                                 (_2):'dypgen__Obj_dypgen__nested_nt_44)
# 4025               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_45",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_45 
(
(None):'dypgen__Obj_dypgen__nested_nt_45)
# 4034               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_45",[Dyp.Non_ter ("dypgen__nested_nt_44",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_44 ( (
(_:'dypgen__Obj_dypgen__nested_nt_44)
# 4042               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_45 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_45)
# 4046               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("import_stmt",[Dyp.Ter "IMPORT";Dyp.Non_ter ("import_shape",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_47",Dyp.No_priority );Dyp.Ter "FROM";Dyp.Non_ter ("file_path",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_import_shape ( (
(_:'dypgen__Obj_import_shape)
# 4054               "parser.ml"
 as _2));Obj_dypgen__nested_nt_47 ( (
(_:'dypgen__Obj_dypgen__nested_nt_47)
# 4057               "parser.ml"
 as _3)); _4;Obj_file_path ( (
(_:'dypgen__Obj_file_path)
# 4060               "parser.ml"
 as _5))] -> Obj_import_stmt 
# 328 "parser.dyp"
(
                                                                  ( Imp.mk (_2::_3) _5 ):'dypgen__Obj_import_stmt)
# 4065               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_46",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("import_shape",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 4073               "parser.ml"
 as _1));Obj_import_shape ( (
(_:'dypgen__Obj_import_shape)
# 4076               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_46 
# 328 "parser.dyp"
(
                                            (_2):'dypgen__Obj_dypgen__nested_nt_46)
# 4081               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_47",[Dyp.Non_ter ("dypgen__nested_nt_48",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_48 ( (
(_:'dypgen__Obj_dypgen__nested_nt_48)
# 4089               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_47 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_47)
# 4093               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_48",[Dyp.Non_ter ("dypgen__nested_nt_48",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_46",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_48 ( (
(_:'dypgen__Obj_dypgen__nested_nt_48)
# 4101               "parser.ml"
 as _1));Obj_dypgen__nested_nt_46 ( (
(_:'dypgen__Obj_dypgen__nested_nt_46)
# 4104               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_48 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_48)
# 4108               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_48",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_48 
(
([]):'dypgen__Obj_dypgen__nested_nt_48)
# 4117               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Ter "LET";Dyp.Ter "REC";Dyp.Non_ter ("value_binds",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2; _3;Obj_value_binds ( (
(_:'dypgen__Obj_value_binds)
# 4125               "parser.ml"
 as _4))] -> Obj_export_stmt 
# 331 "parser.dyp"
(
                               ( Top.let_ Exported Recursive _4 ):'dypgen__Obj_export_stmt)
# 4130               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Ter "LET";Dyp.Non_ter ("value_binds",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_value_binds ( (
(_:'dypgen__Obj_value_binds)
# 4138               "parser.ml"
 as _3))] -> Obj_export_stmt 
# 332 "parser.dyp"
(
                           ( Top.let_ Exported Nonrecursive _3 ):'dypgen__Obj_export_stmt)
# 4143               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Non_ter ("foreign_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_foreign_stmt ( (
(_:'dypgen__Obj_foreign_stmt)
# 4151               "parser.ml"
 as _2))] -> Obj_export_stmt 
# 333 "parser.dyp"
(
                        ( Top.foreign ~loc:(symbol_rloc dyp) Exported _2 ):'dypgen__Obj_export_stmt)
# 4156               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Non_ter ("primitive_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_primitive_stmt ( (
(_:'dypgen__Obj_primitive_stmt)
# 4164               "parser.ml"
 as _2))] -> Obj_export_stmt 
# 334 "parser.dyp"
(
                          ( Top.primitive ~loc:(symbol_rloc dyp) Exported _2 ):'dypgen__Obj_export_stmt)
# 4169               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Non_ter ("data_declaration",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_data_declaration ( (
(_:'dypgen__Obj_data_declaration)
# 4177               "parser.ml"
 as _2))] -> Obj_export_stmt 
# 335 "parser.dyp"
(
                            ( Top.data Exported _2 ):'dypgen__Obj_export_stmt)
# 4182               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Non_ter ("any_id_str",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_50",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_54",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_any_id_str ( (
(_:'dypgen__Obj_any_id_str)
# 4190               "parser.ml"
 as _2));Obj_dypgen__nested_nt_50 ( (
(_:'dypgen__Obj_dypgen__nested_nt_50)
# 4193               "parser.ml"
 as _3));Obj_dypgen__nested_nt_54 ( (
(_:'dypgen__Obj_dypgen__nested_nt_54)
# 4196               "parser.ml"
 as _4))] -> Obj_export_stmt 
# 336 "parser.dyp"
(
                                                                                               ( Top.export ~loc:(symbol_rloc dyp) (Ex.mk ~loc:(symbol_rloc dyp) ((_2, _3)::_4)) ):'dypgen__Obj_export_stmt)
# 4201               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_stmt",[Dyp.Ter "EXPORT";Dyp.Ter "STAR";Dyp.Non_ter ("dypgen__nested_nt_60",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_dypgen__nested_nt_60 ( (
(_:'dypgen__Obj_dypgen__nested_nt_60)
# 4209               "parser.ml"
 as _3))] -> Obj_export_stmt 
# 337 "parser.dyp"
(
                                                                             ( Top.export_all ~loc:(symbol_rloc dyp) (Option.default [] _3) ):'dypgen__Obj_export_stmt)
# 4214               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_49",[Dyp.Ter "AS";Dyp.Non_ter ("any_id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_any_id_str ( (
(_:'dypgen__Obj_any_id_str)
# 4222               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_49 
# 336 "parser.dyp"
(
                                     (_2):'dypgen__Obj_dypgen__nested_nt_49)
# 4227               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_50",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_50 
(
(None):'dypgen__Obj_dypgen__nested_nt_50)
# 4236               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_50",[Dyp.Non_ter ("dypgen__nested_nt_49",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_49 ( (
(_:'dypgen__Obj_dypgen__nested_nt_49)
# 4244               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_50 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_50)
# 4248               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_53",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("any_id_str",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_52",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 4256               "parser.ml"
 as _1));Obj_any_id_str ( (
(_:'dypgen__Obj_any_id_str)
# 4259               "parser.ml"
 as _2));Obj_dypgen__nested_nt_52 ( (
(_:'dypgen__Obj_dypgen__nested_nt_52)
# 4262               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_53 
# 336 "parser.dyp"
(
                                                                                    (_2, _3):'dypgen__Obj_dypgen__nested_nt_53)
# 4267               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_51",[Dyp.Ter "AS";Dyp.Non_ter ("any_id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_any_id_str ( (
(_:'dypgen__Obj_any_id_str)
# 4275               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_51 
# 336 "parser.dyp"
(
                                                                             (_2):'dypgen__Obj_dypgen__nested_nt_51)
# 4280               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_52",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_52 
(
(None):'dypgen__Obj_dypgen__nested_nt_52)
# 4289               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_52",[Dyp.Non_ter ("dypgen__nested_nt_51",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_51 ( (
(_:'dypgen__Obj_dypgen__nested_nt_51)
# 4297               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_52 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_52)
# 4301               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_54",[Dyp.Non_ter ("dypgen__nested_nt_55",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_55 ( (
(_:'dypgen__Obj_dypgen__nested_nt_55)
# 4309               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_54 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_54)
# 4313               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_55",[Dyp.Non_ter ("dypgen__nested_nt_55",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_53",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_55 ( (
(_:'dypgen__Obj_dypgen__nested_nt_55)
# 4321               "parser.ml"
 as _1));Obj_dypgen__nested_nt_53 ( (
(_:'dypgen__Obj_dypgen__nested_nt_53)
# 4324               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_55 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_55)
# 4328               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_55",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_55 
(
([]):'dypgen__Obj_dypgen__nested_nt_55)
# 4337               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_59",[Dyp.Ter "EXCEPT";Dyp.Non_ter ("export_id_str",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_57",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_export_id_str ( (
(_:'dypgen__Obj_export_id_str)
# 4345               "parser.ml"
 as _2));Obj_dypgen__nested_nt_57 ( (
(_:'dypgen__Obj_dypgen__nested_nt_57)
# 4348               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_59 
# 337 "parser.dyp"
(
                                                                  (_2::_3):'dypgen__Obj_dypgen__nested_nt_59)
# 4353               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_56",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("export_id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 4361               "parser.ml"
 as _1));Obj_export_id_str ( (
(_:'dypgen__Obj_export_id_str)
# 4364               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_56 
# 337 "parser.dyp"
(
                                                           (_2):'dypgen__Obj_dypgen__nested_nt_56)
# 4369               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_57",[Dyp.Non_ter ("dypgen__nested_nt_58",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_58 ( (
(_:'dypgen__Obj_dypgen__nested_nt_58)
# 4377               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_57 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_57)
# 4381               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_58",[Dyp.Non_ter ("dypgen__nested_nt_58",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_56",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_58 ( (
(_:'dypgen__Obj_dypgen__nested_nt_58)
# 4389               "parser.ml"
 as _1));Obj_dypgen__nested_nt_56 ( (
(_:'dypgen__Obj_dypgen__nested_nt_56)
# 4392               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_58 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_58)
# 4396               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_58",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_58 
(
([]):'dypgen__Obj_dypgen__nested_nt_58)
# 4405               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_60",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_60 
(
(None):'dypgen__Obj_dypgen__nested_nt_60)
# 4414               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_60",[Dyp.Non_ter ("dypgen__nested_nt_59",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_59 ( (
(_:'dypgen__Obj_dypgen__nested_nt_59)
# 4422               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_60 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_60)
# 4426               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_constructor",[Dyp.Ter "TYPEID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 4434               "parser.ml"
 as _1)] -> Obj_data_constructor 
# 340 "parser.dyp"
(
           ( CDecl.singleton ~loc:(symbol_rloc dyp) (mkstr dyp _1) ):'dypgen__Obj_data_constructor)
# 4439               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_constructor",[Dyp.Ter "TYPEID";Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("typs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 4447               "parser.ml"
 as _1);Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 4450               "parser.ml"
 as _2));Obj_typs ( (
(_:'dypgen__Obj_typs)
# 4453               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 4456               "parser.ml"
 as _4))] -> Obj_data_constructor 
# 341 "parser.dyp"
(
                              ( CDecl.tuple ~loc:(symbol_rloc dyp) (mkstr dyp _1) _3 ):'dypgen__Obj_data_constructor)
# 4461               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_constructor",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 4469               "parser.ml"
 as _1));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 4472               "parser.ml"
 as _2))] -> Obj_data_constructor 
# 343 "parser.dyp"
(
                  ( CDecl.singleton ~loc:(symbol_rloc dyp) (mkstr dyp "[]") ):'dypgen__Obj_data_constructor)
# 4477               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_constructor",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Ter "ELLIPSIS";Dyp.Non_ter ("rbrack",Dyp.No_priority );Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("typs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 4485               "parser.ml"
 as _1)); _2;Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 4488               "parser.ml"
 as _3));Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 4491               "parser.ml"
 as _4));Obj_typs ( (
(_:'dypgen__Obj_typs)
# 4494               "parser.ml"
 as _5));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 4497               "parser.ml"
 as _6))] -> Obj_data_constructor 
# 344 "parser.dyp"
(
                                              ( CDecl.tuple ~loc:(symbol_rloc dyp) (mkstr dyp "[...]") _5 ):'dypgen__Obj_data_constructor)
# 4502               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_constructors",[Dyp.Non_ter ("dypgen__option_pipe",Dyp.No_priority );Dyp.Non_ter ("data_constructor",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_62",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_pipe ( (
(_:'dypgen__Obj_pipe option)
# 4510               "parser.ml"
 as _1));Obj_data_constructor ( (
(_:'dypgen__Obj_data_constructor)
# 4513               "parser.ml"
 as _2));Obj_dypgen__nested_nt_62 ( (
(_:'dypgen__Obj_dypgen__nested_nt_62)
# 4516               "parser.ml"
 as _3))] -> Obj_data_constructors 
# 347 "parser.dyp"
(
                                                              ( _2::_3 ):'dypgen__Obj_data_constructors)
# 4521               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_pipe",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_pipe 
(
(None):'dypgen__Obj_pipe option)
# 4530               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_pipe",[Dyp.Non_ter ("pipe",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pipe ( (
(_:'dypgen__Obj_pipe)
# 4538               "parser.ml"
 as _1))] -> Obj_dypgen__option_pipe 
(
(Some _1):'dypgen__Obj_pipe option)
# 4542               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_61",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("pipe",Dyp.No_priority );Dyp.Non_ter ("data_constructor",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 4550               "parser.ml"
 as _1));Obj_pipe ( (
(_:'dypgen__Obj_pipe)
# 4553               "parser.ml"
 as _2));Obj_data_constructor ( (
(_:'dypgen__Obj_data_constructor)
# 4556               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_61 
# 347 "parser.dyp"
(
                                                       (_3):'dypgen__Obj_dypgen__nested_nt_61)
# 4561               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_62",[Dyp.Non_ter ("dypgen__nested_nt_63",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_63 ( (
(_:'dypgen__Obj_dypgen__nested_nt_63)
# 4569               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_62 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_62)
# 4573               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_63",[Dyp.Non_ter ("dypgen__nested_nt_63",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_61",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_63 ( (
(_:'dypgen__Obj_dypgen__nested_nt_63)
# 4581               "parser.ml"
 as _1));Obj_dypgen__nested_nt_61 ( (
(_:'dypgen__Obj_dypgen__nested_nt_61)
# 4584               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_63 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_63)
# 4588               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_63",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_63 
(
([]):'dypgen__Obj_dypgen__nested_nt_63)
# 4597               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_label",[Dyp.Non_ter ("simple_id",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_simple_id ( (
(_:'dypgen__Obj_simple_id)
# 4605               "parser.ml"
 as _1));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 4608               "parser.ml"
 as _2));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 4611               "parser.ml"
 as _3))] -> Obj_data_label 
# 350 "parser.dyp"
(
                        ( LDecl.mk ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_data_label)
# 4616               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_labels",[Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("data_label",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_65",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 4624               "parser.ml"
 as _1));Obj_data_label ( (
(_:'dypgen__Obj_data_label)
# 4627               "parser.ml"
 as _2));Obj_dypgen__nested_nt_65 ( (
(_:'dypgen__Obj_dypgen__nested_nt_65)
# 4630               "parser.ml"
 as _3));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 4633               "parser.ml"
 as _4))] -> Obj_data_labels 
# 353 "parser.dyp"
(
                                                      ( _2::_3 ):'dypgen__Obj_data_labels)
# 4638               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_64",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("data_label",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 4646               "parser.ml"
 as _1));Obj_data_label ( (
(_:'dypgen__Obj_data_label)
# 4649               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_64 
# 353 "parser.dyp"
(
                                        (_2):'dypgen__Obj_dypgen__nested_nt_64)
# 4654               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_65",[Dyp.Non_ter ("dypgen__nested_nt_66",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_66 ( (
(_:'dypgen__Obj_dypgen__nested_nt_66)
# 4662               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_65 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_65)
# 4666               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_66",[Dyp.Non_ter ("dypgen__nested_nt_66",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_64",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_66 ( (
(_:'dypgen__Obj_dypgen__nested_nt_66)
# 4674               "parser.ml"
 as _1));Obj_dypgen__nested_nt_64 ( (
(_:'dypgen__Obj_dypgen__nested_nt_64)
# 4677               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_66 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_66)
# 4681               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_66",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_66 
(
([]):'dypgen__Obj_dypgen__nested_nt_66)
# 4690               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_declaration",[Dyp.Ter "DATA";Dyp.Ter "TYPEID";Dyp.Non_ter ("dypgen__nested_nt_71",Dyp.No_priority );Dyp.Non_ter ("equal",Dyp.No_priority );Dyp.Non_ter ("data_constructors",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_TYPEID  (
(_:(string))
# 4698               "parser.ml"
 as _2);Obj_dypgen__nested_nt_71 ( (
(_:'dypgen__Obj_dypgen__nested_nt_71)
# 4701               "parser.ml"
 as _3));Obj_equal ( (
(_:'dypgen__Obj_equal)
# 4704               "parser.ml"
 as _4));Obj_data_constructors ( (
(_:'dypgen__Obj_data_constructors)
# 4707               "parser.ml"
 as _5))] -> Obj_data_declaration 
# 356 "parser.dyp"
(
                                                                                      ( Dat.variant ~loc:(symbol_rloc dyp) (mkstr dyp _2) (List.map Typ.var (Option.default [] _3)) _5 ):'dypgen__Obj_data_declaration)
# 4712               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("data_declaration",[Dyp.Ter "DATA";Dyp.Ter "TYPEID";Dyp.Non_ter ("dypgen__nested_nt_76",Dyp.No_priority );Dyp.Non_ter ("equal",Dyp.No_priority );Dyp.Non_ter ("data_labels",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_TYPEID  (
(_:(string))
# 4720               "parser.ml"
 as _2);Obj_dypgen__nested_nt_76 ( (
(_:'dypgen__Obj_dypgen__nested_nt_76)
# 4723               "parser.ml"
 as _3));Obj_equal ( (
(_:'dypgen__Obj_equal)
# 4726               "parser.ml"
 as _4));Obj_data_labels ( (
(_:'dypgen__Obj_data_labels)
# 4729               "parser.ml"
 as _5))] -> Obj_data_declaration 
# 357 "parser.dyp"
(
                                                                                ( Dat.record ~loc:(symbol_rloc dyp) (mkstr dyp _2) (List.map Typ.var (Option.default [] _3)) _5 ):'dypgen__Obj_data_declaration)
# 4734               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_70",[Dyp.Non_ter ("lcaret",Dyp.No_priority );Dyp.Ter "ID";Dyp.Non_ter ("dypgen__nested_nt_68",Dyp.No_priority );Dyp.Non_ter ("rcaret",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lcaret ( (
(_:'dypgen__Obj_lcaret)
# 4742               "parser.ml"
 as _1));Obj_ID  (
(_:(string))
# 4745               "parser.ml"
 as _2);Obj_dypgen__nested_nt_68 ( (
(_:'dypgen__Obj_dypgen__nested_nt_68)
# 4748               "parser.ml"
 as _3));Obj_rcaret ( (
(_:'dypgen__Obj_rcaret)
# 4751               "parser.ml"
 as _4))] -> Obj_dypgen__nested_nt_70 
# 356 "parser.dyp"
(
                                                   (_2::_3):'dypgen__Obj_dypgen__nested_nt_70)
# 4756               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_67",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 4764               "parser.ml"
 as _1));Obj_ID  (
(_:(string))
# 4767               "parser.ml"
 as _2)] -> Obj_dypgen__nested_nt_67 
# 356 "parser.dyp"
(
                                     (_2):'dypgen__Obj_dypgen__nested_nt_67)
# 4772               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_68",[Dyp.Non_ter ("dypgen__nested_nt_69",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_69 ( (
(_:'dypgen__Obj_dypgen__nested_nt_69)
# 4780               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_68 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_68)
# 4784               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_69",[Dyp.Non_ter ("dypgen__nested_nt_69",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_67",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_69 ( (
(_:'dypgen__Obj_dypgen__nested_nt_69)
# 4792               "parser.ml"
 as _1));Obj_dypgen__nested_nt_67 ( (
(_:'dypgen__Obj_dypgen__nested_nt_67)
# 4795               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_69 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_69)
# 4799               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_69",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_69 
(
([]):'dypgen__Obj_dypgen__nested_nt_69)
# 4808               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_71",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_71 
(
(None):'dypgen__Obj_dypgen__nested_nt_71)
# 4817               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_71",[Dyp.Non_ter ("dypgen__nested_nt_70",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_70 ( (
(_:'dypgen__Obj_dypgen__nested_nt_70)
# 4825               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_71 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_71)
# 4829               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_75",[Dyp.Non_ter ("lcaret",Dyp.No_priority );Dyp.Ter "ID";Dyp.Non_ter ("dypgen__nested_nt_73",Dyp.No_priority );Dyp.Non_ter ("rcaret",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lcaret ( (
(_:'dypgen__Obj_lcaret)
# 4837               "parser.ml"
 as _1));Obj_ID  (
(_:(string))
# 4840               "parser.ml"
 as _2);Obj_dypgen__nested_nt_73 ( (
(_:'dypgen__Obj_dypgen__nested_nt_73)
# 4843               "parser.ml"
 as _3));Obj_rcaret ( (
(_:'dypgen__Obj_rcaret)
# 4846               "parser.ml"
 as _4))] -> Obj_dypgen__nested_nt_75 
# 357 "parser.dyp"
(
                                                   (_2::_3):'dypgen__Obj_dypgen__nested_nt_75)
# 4851               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_72",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 4859               "parser.ml"
 as _1));Obj_ID  (
(_:(string))
# 4862               "parser.ml"
 as _2)] -> Obj_dypgen__nested_nt_72 
# 357 "parser.dyp"
(
                                     (_2):'dypgen__Obj_dypgen__nested_nt_72)
# 4867               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_73",[Dyp.Non_ter ("dypgen__nested_nt_74",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_74 ( (
(_:'dypgen__Obj_dypgen__nested_nt_74)
# 4875               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_73 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_73)
# 4879               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_74",[Dyp.Non_ter ("dypgen__nested_nt_74",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_72",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_74 ( (
(_:'dypgen__Obj_dypgen__nested_nt_74)
# 4887               "parser.ml"
 as _1));Obj_dypgen__nested_nt_72 ( (
(_:'dypgen__Obj_dypgen__nested_nt_72)
# 4890               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_74 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_74)
# 4894               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_74",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_74 
(
([]):'dypgen__Obj_dypgen__nested_nt_74)
# 4903               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_76",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_76 
(
(None):'dypgen__Obj_dypgen__nested_nt_76)
# 4912               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_76",[Dyp.Non_ter ("dypgen__nested_nt_75",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_75 ( (
(_:'dypgen__Obj_dypgen__nested_nt_75)
# 4920               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_76 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_76)
# 4924               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("prim1_expr",[Dyp.Ter "NOT";Dyp.Non_ter ("non_assign_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_non_assign_expr ( (
(_:'dypgen__Obj_non_assign_expr)
# 4932               "parser.ml"
 as _2))] -> Obj_prim1_expr 
# 360 "parser.dyp"
(
                        ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp ["!"]) [_2] ):'dypgen__Obj_prim1_expr)
# 4937               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("prim1_expr",[Dyp.Ter "CARET";Dyp.Non_ter ("non_assign_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_non_assign_expr ( (
(_:'dypgen__Obj_non_assign_expr)
# 4945               "parser.ml"
 as _2))] -> Obj_prim1_expr 
# 361 "parser.dyp"
(
                          ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp ["^"]) [_2] ):'dypgen__Obj_prim1_expr)
# 4950               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("paren_expr",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 4958               "parser.ml"
 as _1));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 4961               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 4964               "parser.ml"
 as _3))] -> Obj_paren_expr 
# 364 "parser.dyp"
(
                       ( _2 ):'dypgen__Obj_paren_expr)
# 4969               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("app_arg_exprs",[Dyp.Non_ter ("dypgen__nested_nt_81",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_81 ( (
(_:'dypgen__Obj_dypgen__nested_nt_81)
# 4977               "parser.ml"
 as _1))] -> Obj_app_arg_exprs 
# 367 "parser.dyp"
(
                                          ( Option.default [] _1 ):'dypgen__Obj_app_arg_exprs)
# 4982               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_80",[Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_78",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 4990               "parser.ml"
 as _1));Obj_dypgen__nested_nt_78 ( (
(_:'dypgen__Obj_dypgen__nested_nt_78)
# 4993               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_80 
# 367 "parser.dyp"
(
                             ( _1::_2 ):'dypgen__Obj_dypgen__nested_nt_80)
# 4998               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_77",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 5006               "parser.ml"
 as _1));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 5009               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_77 
# 367 "parser.dyp"
(
                      (_2):'dypgen__Obj_dypgen__nested_nt_77)
# 5014               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_78",[Dyp.Non_ter ("dypgen__nested_nt_79",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_79 ( (
(_:'dypgen__Obj_dypgen__nested_nt_79)
# 5022               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_78 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_78)
# 5026               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_79",[Dyp.Non_ter ("dypgen__nested_nt_79",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_77",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_79 ( (
(_:'dypgen__Obj_dypgen__nested_nt_79)
# 5034               "parser.ml"
 as _1));Obj_dypgen__nested_nt_77 ( (
(_:'dypgen__Obj_dypgen__nested_nt_77)
# 5037               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_79 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_79)
# 5041               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_79",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_79 
(
([]):'dypgen__Obj_dypgen__nested_nt_79)
# 5050               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_81",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_81 
(
(None):'dypgen__Obj_dypgen__nested_nt_81)
# 5059               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_81",[Dyp.Non_ter ("dypgen__nested_nt_80",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_80 ( (
(_:'dypgen__Obj_dypgen__nested_nt_80)
# 5067               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_81 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_81)
# 5071               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("app_expr",[Dyp.Non_ter ("id_expr",Dyp.No_priority );Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("app_arg_exprs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id_expr ( (
(_:'dypgen__Obj_id_expr)
# 5079               "parser.ml"
 as _1));Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5082               "parser.ml"
 as _2));Obj_app_arg_exprs ( (
(_:'dypgen__Obj_app_arg_exprs)
# 5085               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5088               "parser.ml"
 as _4))] -> Obj_app_expr 
# 370 "parser.dyp"
(
                                        ( prerr_string "\napp_expr\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); Exp.apply ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_app_expr)
# 5093               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("app_expr",[Dyp.Non_ter ("paren_expr",Dyp.No_priority );Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("app_arg_exprs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_paren_expr ( (
(_:'dypgen__Obj_paren_expr)
# 5101               "parser.ml"
 as _1));Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5104               "parser.ml"
 as _2));Obj_app_arg_exprs ( (
(_:'dypgen__Obj_app_arg_exprs)
# 5107               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5110               "parser.ml"
 as _4))] -> Obj_app_expr 
# 371 "parser.dyp"
(
                                           ( prerr_string "\napp_expr\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); Exp.apply ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_app_expr)
# 5115               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("app_expr",[Dyp.Non_ter ("app_expr",Dyp.No_priority );Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("app_arg_exprs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_app_expr ( (
(_:'dypgen__Obj_app_expr)
# 5123               "parser.ml"
 as _1));Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5126               "parser.ml"
 as _2));Obj_app_arg_exprs ( (
(_:'dypgen__Obj_app_arg_exprs)
# 5129               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5132               "parser.ml"
 as _4))] -> Obj_app_expr 
# 372 "parser.dyp"
(
                                         ( prerr_string "\napp_expr\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); Exp.apply ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_app_expr)
# 5137               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("ext_constructor",[Dyp.Ter "TYPEID";Dyp.Non_ter ("dypgen__nested_nt_83",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 5145               "parser.ml"
 as _1);Obj_dypgen__nested_nt_83 ( (
(_:'dypgen__Obj_dypgen__nested_nt_83)
# 5148               "parser.ml"
 as _2))] -> Obj_ext_constructor 
# 375 "parser.dyp"
(
                              ( prerr_string "\nid\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); (mkid (_1::_2)) (symbol_rloc dyp) ):'dypgen__Obj_ext_constructor)
# 5153               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_82",[Dyp.Non_ter ("dot",Dyp.No_priority );Dyp.Ter "TYPEID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dot ( (
(_:'dypgen__Obj_dot)
# 5161               "parser.ml"
 as _1));Obj_TYPEID  (
(_:(string))
# 5164               "parser.ml"
 as _2)] -> Obj_dypgen__nested_nt_82 
# 375 "parser.dyp"
(
                       (_2):'dypgen__Obj_dypgen__nested_nt_82)
# 5169               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_83",[Dyp.Non_ter ("dypgen__nested_nt_84",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_84 ( (
(_:'dypgen__Obj_dypgen__nested_nt_84)
# 5177               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_83 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_83)
# 5181               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_84",[Dyp.Non_ter ("dypgen__nested_nt_84",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_82",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_84 ( (
(_:'dypgen__Obj_dypgen__nested_nt_84)
# 5189               "parser.ml"
 as _1));Obj_dypgen__nested_nt_82 ( (
(_:'dypgen__Obj_dypgen__nested_nt_82)
# 5192               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_84 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_84)
# 5196               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_84",[Dyp.Non_ter ("dypgen__nested_nt_82",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_82 ( (
(_:'dypgen__Obj_dypgen__nested_nt_82)
# 5204               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_84 
(
([_1]):'dypgen__Obj_dypgen__nested_nt_84)
# 5208               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("plus_op",[Dyp.Ter "PLUS"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_plus_op 
# 378 "parser.dyp"
(
         ( "+" ):'dypgen__Obj_plus_op)
# 5218               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dash_op",[Dyp.Ter "DASH"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_dash_op 
# 380 "parser.dyp"
(
         ( "-" ):'dypgen__Obj_dash_op)
# 5228               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("star_op",[Dyp.Ter "STAR"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_star_op 
# 382 "parser.dyp"
(
         ( "*" ):'dypgen__Obj_star_op)
# 5238               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("slash_op",[Dyp.Ter "SLASH"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_slash_op 
# 384 "parser.dyp"
(
          ( "/" ):'dypgen__Obj_slash_op)
# 5248               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("percent_op",[Dyp.Ter "PERCENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_percent_op 
# 386 "parser.dyp"
(
            ( "%" ):'dypgen__Obj_percent_op)
# 5258               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("eqeq_op",[Dyp.Ter "EQEQ"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_eqeq_op 
# 388 "parser.dyp"
(
         ( "==" ):'dypgen__Obj_eqeq_op)
# 5268               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lcaret_op",[Dyp.Ter "LCARET"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_lcaret_op 
# 390 "parser.dyp"
(
           ( "<" ):'dypgen__Obj_lcaret_op)
# 5278               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rcaret_op",[Dyp.Ter "RCARET"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_rcaret_op 
# 392 "parser.dyp"
(
           ( ">" ):'dypgen__Obj_rcaret_op)
# 5288               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lesseq_op",[Dyp.Ter "LESSEQ"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_lesseq_op 
# 394 "parser.dyp"
(
           ( "<=" ):'dypgen__Obj_lesseq_op)
# 5298               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("greatereq_op",[Dyp.Ter "GREATEREQ"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_greatereq_op 
# 396 "parser.dyp"
(
              ( ">=" ):'dypgen__Obj_greatereq_op)
# 5308               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("ampamp_op",[Dyp.Ter "AMPAMP"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_ampamp_op 
# 398 "parser.dyp"
(
           ( "&&" ):'dypgen__Obj_ampamp_op)
# 5318               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pipepipe_op",[Dyp.Ter "PIPEPIPE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_pipepipe_op 
# 400 "parser.dyp"
(
             ( "||" ):'dypgen__Obj_pipepipe_op)
# 5328               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("pluseq_op",[Dyp.Ter "PLUS";Dyp.Ter "EQUAL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2] -> Obj_pluseq_op 
# 402 "parser.dyp"
(
               ( "+=" ):'dypgen__Obj_pluseq_op)
# 5338               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dasheq_op",[Dyp.Ter "DASH";Dyp.Ter "EQUAL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2] -> Obj_dasheq_op 
# 404 "parser.dyp"
(
               ( "-=" ):'dypgen__Obj_dasheq_op)
# 5348               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("stareq_op",[Dyp.Ter "STAR";Dyp.Ter "EQUAL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2] -> Obj_stareq_op 
# 406 "parser.dyp"
(
               ( "*=" ):'dypgen__Obj_stareq_op)
# 5358               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("slasheq_op",[Dyp.Ter "SLASH";Dyp.Ter "EQUAL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2] -> Obj_slasheq_op 
# 408 "parser.dyp"
(
                ( "/=" ):'dypgen__Obj_slasheq_op)
# 5368               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("plus_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_plus_op ( (
(_:'dypgen__Obj_plus_op)
# 5376               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5380               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("dash_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dash_op ( (
(_:'dypgen__Obj_dash_op)
# 5388               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5392               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("star_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_star_op ( (
(_:'dypgen__Obj_star_op)
# 5400               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5404               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("slash_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_slash_op ( (
(_:'dypgen__Obj_slash_op)
# 5412               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5416               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("percent_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_percent_op ( (
(_:'dypgen__Obj_percent_op)
# 5424               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5428               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("eqeq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_eqeq_op ( (
(_:'dypgen__Obj_eqeq_op)
# 5436               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5440               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("lcaret_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lcaret_op ( (
(_:'dypgen__Obj_lcaret_op)
# 5448               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5452               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("rcaret_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_rcaret_op ( (
(_:'dypgen__Obj_rcaret_op)
# 5460               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5464               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("lesseq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lesseq_op ( (
(_:'dypgen__Obj_lesseq_op)
# 5472               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5476               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("greatereq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_greatereq_op ( (
(_:'dypgen__Obj_greatereq_op)
# 5484               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5488               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("ampamp_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ampamp_op ( (
(_:'dypgen__Obj_ampamp_op)
# 5496               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5500               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("pipepipe_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pipepipe_op ( (
(_:'dypgen__Obj_pipepipe_op)
# 5508               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5512               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("pluseq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pluseq_op ( (
(_:'dypgen__Obj_pluseq_op)
# 5520               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5524               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("dasheq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dasheq_op ( (
(_:'dypgen__Obj_dasheq_op)
# 5532               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5536               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("stareq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_stareq_op ( (
(_:'dypgen__Obj_stareq_op)
# 5544               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5548               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix_op",[Dyp.Non_ter ("slasheq_op",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_slasheq_op ( (
(_:'dypgen__Obj_slasheq_op)
# 5556               "parser.ml"
 as _1))] -> Obj_infix_op 
(
(_1):'dypgen__Obj_infix_op)
# 5560               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("infix",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("infix_op",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5568               "parser.ml"
 as _1));Obj_infix_op ( (
(_:'dypgen__Obj_infix_op)
# 5571               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5574               "parser.ml"
 as _3))] -> Obj_infix 
# 429 "parser.dyp"
(
                           ( _2 ):'dypgen__Obj_infix)
# 5579               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("prefix",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Ter "NOT";Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5587               "parser.ml"
 as _1)); _2;Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5590               "parser.ml"
 as _3))] -> Obj_prefix 
# 432 "parser.dyp"
(
                      ( "!" ):'dypgen__Obj_prefix)
# 5595               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("prefix",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Ter "CARET";Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5603               "parser.ml"
 as _1)); _2;Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5606               "parser.ml"
 as _3))] -> Obj_prefix 
# 433 "parser.dyp"
(
                        ( "^" ):'dypgen__Obj_prefix)
# 5611               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("id",[Dyp.Non_ter ("dypgen__nested_nt_86",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_88",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_86 ( (
(_:'dypgen__Obj_dypgen__nested_nt_86)
# 5619               "parser.ml"
 as _1));Obj_dypgen__nested_nt_88 ( (
(_:'dypgen__Obj_dypgen__nested_nt_88)
# 5622               "parser.ml"
 as _2))] -> Obj_id 
# 436 "parser.dyp"
(
                                                      ( prerr_string "\nid\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); (mkid (List.append _1 [_2])) (symbol_rloc dyp) ):'dypgen__Obj_id)
# 5627               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_85",[Dyp.Ter "TYPEID";Dyp.Non_ter ("dot",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 5635               "parser.ml"
 as _1);Obj_dot ( (
(_:'dypgen__Obj_dot)
# 5638               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_85 
# 436 "parser.dyp"
(
                (_1):'dypgen__Obj_dypgen__nested_nt_85)
# 5643               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_86",[Dyp.Non_ter ("dypgen__nested_nt_87",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_87 ( (
(_:'dypgen__Obj_dypgen__nested_nt_87)
# 5651               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_86 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_86)
# 5655               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_87",[Dyp.Non_ter ("dypgen__nested_nt_87",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_85",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_87 ( (
(_:'dypgen__Obj_dypgen__nested_nt_87)
# 5663               "parser.ml"
 as _1));Obj_dypgen__nested_nt_85 ( (
(_:'dypgen__Obj_dypgen__nested_nt_85)
# 5666               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_87 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_87)
# 5670               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_87",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_87 
(
([]):'dypgen__Obj_dypgen__nested_nt_87)
# 5679               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_88",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 5687               "parser.ml"
 as _1)] -> Obj_dypgen__nested_nt_88 
(
(_1):'dypgen__Obj_dypgen__nested_nt_88)
# 5691               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_88",[Dyp.Ter "TYPEID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 5699               "parser.ml"
 as _1)] -> Obj_dypgen__nested_nt_88 
(
(_1):'dypgen__Obj_dypgen__nested_nt_88)
# 5703               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_88",[Dyp.Non_ter ("infix",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_infix ( (
(_:'dypgen__Obj_infix)
# 5711               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_88 
(
(_1):'dypgen__Obj_dypgen__nested_nt_88)
# 5715               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_88",[Dyp.Non_ter ("prefix",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_prefix ( (
(_:'dypgen__Obj_prefix)
# 5723               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_88 
(
(_1):'dypgen__Obj_dypgen__nested_nt_88)
# 5727               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("simple_id",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 5735               "parser.ml"
 as _1)] -> Obj_simple_id 
# 439 "parser.dyp"
(
       ( (mkid [_1]) (symbol_rloc dyp) ):'dypgen__Obj_simple_id)
# 5740               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("type_id",[Dyp.Ter "TYPEID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 5748               "parser.ml"
 as _1)] -> Obj_type_id 
# 442 "parser.dyp"
(
           ( prerr_string "\nid\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); (mkid [_1]) (symbol_rloc dyp) ):'dypgen__Obj_type_id)
# 5753               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("id_expr",[Dyp.Non_ter ("id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id ( (
(_:'dypgen__Obj_id)
# 5761               "parser.ml"
 as _1))] -> Obj_id_expr 
# 445 "parser.dyp"
(
       ( prerr_string "\nsimple_expr\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); Exp.ident ~loc:(symbol_rloc dyp) _1 ):'dypgen__Obj_id_expr)
# 5766               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("simple_expr",[Dyp.Non_ter ("const",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_const ( (
(_:'dypgen__Obj_const)
# 5774               "parser.ml"
 as _1))] -> Obj_simple_expr 
# 448 "parser.dyp"
(
          ( Exp.constant ~loc:(symbol_rloc dyp) _1 ):'dypgen__Obj_simple_expr)
# 5779               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("simple_expr",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("tuple_exprs",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5787               "parser.ml"
 as _1));Obj_tuple_exprs ( (
(_:'dypgen__Obj_tuple_exprs)
# 5790               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5793               "parser.ml"
 as _3))] -> Obj_simple_expr 
# 449 "parser.dyp"
(
                              ( Exp.tuple ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_simple_expr)
# 5798               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("simple_expr",[Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("record_exprs",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 5806               "parser.ml"
 as _1));Obj_record_exprs ( (
(_:'dypgen__Obj_record_exprs)
# 5809               "parser.ml"
 as _2));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 5812               "parser.ml"
 as _3))] -> Obj_simple_expr 
# 450 "parser.dyp"
(
                               ( Exp.record ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_simple_expr)
# 5817               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("simple_expr",[Dyp.Non_ter ("id_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id_expr ( (
(_:'dypgen__Obj_id_expr)
# 5825               "parser.ml"
 as _1))] -> Obj_simple_expr 
# 451 "parser.dyp"
(
            ( _1 ):'dypgen__Obj_simple_expr)
# 5830               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_expr",[Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("block_body",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 5838               "parser.ml"
 as _1));Obj_block_body ( (
(_:'dypgen__Obj_block_body)
# 5841               "parser.ml"
 as _2));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 5844               "parser.ml"
 as _3))] -> Obj_block_expr 
# 454 "parser.dyp"
(
                             ( no_record_block _2; Exp.block ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_block_expr)
# 5849               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block",[Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("block_body",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 5857               "parser.ml"
 as _1));Obj_block_body ( (
(_:'dypgen__Obj_block_body)
# 5860               "parser.ml"
 as _2));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 5863               "parser.ml"
 as _3))] -> Obj_block 
# 457 "parser.dyp"
(
                             ( Exp.block ~loc:(symbol_rloc dyp) _2 ):'dypgen__Obj_block)
# 5868               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_or_expr",[Dyp.Non_ter ("block",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_block ( (
(_:'dypgen__Obj_block)
# 5876               "parser.ml"
 as _1))] -> Obj_block_or_expr 
# 460 "parser.dyp"
(
          ( _1 ):'dypgen__Obj_block_or_expr)
# 5881               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_or_expr",[Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 5889               "parser.ml"
 as _1))] -> Obj_block_or_expr 
# 461 "parser.dyp"
(
         ( no_brace_expr _1; _1 ):'dypgen__Obj_block_or_expr)
# 5894               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lam_args",[Dyp.Non_ter ("dypgen__option_patterns",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_patterns ( (
(_:'dypgen__Obj_patterns option)
# 5902               "parser.ml"
 as _1))] -> Obj_lam_args 
# 464 "parser.dyp"
(
              ( Option.default [] _1 ):'dypgen__Obj_lam_args)
# 5907               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_patterns",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_patterns 
(
(None):'dypgen__Obj_patterns option)
# 5916               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_patterns",[Dyp.Non_ter ("patterns",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_patterns ( (
(_:'dypgen__Obj_patterns)
# 5924               "parser.ml"
 as _1))] -> Obj_dypgen__option_patterns 
(
(Some _1):'dypgen__Obj_patterns option)
# 5928               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lam_expr",[Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("lam_args",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority );Dyp.Non_ter ("thickarrow",Dyp.No_priority );Dyp.Non_ter ("block_or_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 5936               "parser.ml"
 as _1));Obj_lam_args ( (
(_:'dypgen__Obj_lam_args)
# 5939               "parser.ml"
 as _2));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 5942               "parser.ml"
 as _3));Obj_thickarrow ( (
(_:'dypgen__Obj_thickarrow)
# 5945               "parser.ml"
 as _4));Obj_block_or_expr ( (
(_:'dypgen__Obj_block_or_expr)
# 5948               "parser.ml"
 as _5))] -> Obj_lam_expr 
# 467 "parser.dyp"
(
                                                    ( Exp.lambda ~loc:(symbol_rloc dyp) _2 _5 ):'dypgen__Obj_lam_expr)
# 5953               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lam_expr",[Dyp.Ter "ID";Dyp.Non_ter ("thickarrow",Dyp.No_priority );Dyp.Non_ter ("block_or_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 5961               "parser.ml"
 as _1);Obj_thickarrow ( (
(_:'dypgen__Obj_thickarrow)
# 5964               "parser.ml"
 as _2));Obj_block_or_expr ( (
(_:'dypgen__Obj_block_or_expr)
# 5967               "parser.ml"
 as _3))] -> Obj_lam_expr 
# 468 "parser.dyp"
(
                                ( Exp.lambda ~loc:(symbol_rloc dyp) [Pat.var ~loc:(rhs_loc dyp 1) (mkstr dyp _1)] _3 ):'dypgen__Obj_lam_expr)
# 5972               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("let_expr",[Dyp.Ter "LET";Dyp.Ter "REC";Dyp.Non_ter ("value_binds",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_value_binds ( (
(_:'dypgen__Obj_value_binds)
# 5980               "parser.ml"
 as _3))] -> Obj_let_expr 
# 471 "parser.dyp"
(
                        ( Exp.let_ ~loc:(symbol_rloc dyp) Recursive _3 (Exp.block []) ):'dypgen__Obj_let_expr)
# 5985               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("let_expr",[Dyp.Ter "LET";Dyp.Non_ter ("value_binds",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_value_binds ( (
(_:'dypgen__Obj_value_binds)
# 5993               "parser.ml"
 as _2))] -> Obj_let_expr 
# 472 "parser.dyp"
(
                    ( Exp.let_ ~loc:(symbol_rloc dyp) Nonrecursive _2 (Exp.block []) ):'dypgen__Obj_let_expr)
# 5998               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("if_expr",[Dyp.Ter "IF";Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("block_or_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 6006               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6009               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 6012               "parser.ml"
 as _4));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6015               "parser.ml"
 as _5));Obj_block_or_expr ( (
(_:'dypgen__Obj_block_or_expr)
# 6018               "parser.ml"
 as _6))] -> Obj_if_expr 
# 475 "parser.dyp"
(
                                             ( Exp.if_ ~loc:(symbol_rloc dyp) _3 _6 (Exp.block []) ):'dypgen__Obj_if_expr)
# 6023               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("if_expr",[Dyp.Ter "IF";Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("block_or_expr",Dyp.No_priority );Dyp.Ter "ELSE";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("block_or_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 6031               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6034               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 6037               "parser.ml"
 as _4));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6040               "parser.ml"
 as _5));Obj_block_or_expr ( (
(_:'dypgen__Obj_block_or_expr)
# 6043               "parser.ml"
 as _6)); _7;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6046               "parser.ml"
 as _8));Obj_block_or_expr ( (
(_:'dypgen__Obj_block_or_expr)
# 6049               "parser.ml"
 as _9))] -> Obj_if_expr 
# 476 "parser.dyp"
(
                                                                     ( Exp.if_ ~loc:(symbol_rloc dyp) _3 _6 _9 ):'dypgen__Obj_if_expr)
# 6054               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("while_expr",[Dyp.Ter "WHILE";Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority );Dyp.Non_ter ("block",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 6062               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6065               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 6068               "parser.ml"
 as _4));Obj_block ( (
(_:'dypgen__Obj_block)
# 6071               "parser.ml"
 as _5))] -> Obj_while_expr 
# 479 "parser.dyp"
(
                                   ( Exp.while_ ~loc:(symbol_rloc dyp) _3 _5 ):'dypgen__Obj_while_expr)
# 6076               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("match_branch",[Dyp.Non_ter ("pattern",Dyp.No_priority );Dyp.Non_ter ("thickarrow",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_pattern ( (
(_:'dypgen__Obj_pattern)
# 6084               "parser.ml"
 as _1));Obj_thickarrow ( (
(_:'dypgen__Obj_thickarrow)
# 6087               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6090               "parser.ml"
 as _3))] -> Obj_match_branch 
# 482 "parser.dyp"
(
                            ( Mb.mk ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_match_branch)
# 6095               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("match_branches",[Dyp.Non_ter ("dypgen__option_pipe",Dyp.No_priority );Dyp.Non_ter ("match_branch",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_90",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_pipe ( (
(_:'dypgen__Obj_pipe option)
# 6103               "parser.ml"
 as _1));Obj_match_branch ( (
(_:'dypgen__Obj_match_branch)
# 6106               "parser.ml"
 as _2));Obj_dypgen__nested_nt_90 ( (
(_:'dypgen__Obj_dypgen__nested_nt_90)
# 6109               "parser.ml"
 as _3))] -> Obj_match_branches 
# 485 "parser.dyp"
(
                                                      ( _2::_3 ):'dypgen__Obj_match_branches)
# 6114               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_89",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("pipe",Dyp.No_priority );Dyp.Non_ter ("match_branch",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6122               "parser.ml"
 as _1));Obj_pipe ( (
(_:'dypgen__Obj_pipe)
# 6125               "parser.ml"
 as _2));Obj_match_branch ( (
(_:'dypgen__Obj_match_branch)
# 6128               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_89 
# 485 "parser.dyp"
(
                                               (_3):'dypgen__Obj_dypgen__nested_nt_89)
# 6133               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_90",[Dyp.Non_ter ("dypgen__nested_nt_91",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_91 ( (
(_:'dypgen__Obj_dypgen__nested_nt_91)
# 6141               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_90 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_90)
# 6145               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_91",[Dyp.Non_ter ("dypgen__nested_nt_91",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_89",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_91 ( (
(_:'dypgen__Obj_dypgen__nested_nt_91)
# 6153               "parser.ml"
 as _1));Obj_dypgen__nested_nt_89 ( (
(_:'dypgen__Obj_dypgen__nested_nt_89)
# 6156               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_91 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_91)
# 6160               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_91",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_91 
(
([]):'dypgen__Obj_dypgen__nested_nt_91)
# 6169               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("match_expr",[Dyp.Ter "MATCH";Dyp.Non_ter ("lparen",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rparen",Dyp.No_priority );Dyp.Non_ter ("lbrace",Dyp.No_priority );Dyp.Non_ter ("match_branches",Dyp.No_priority );Dyp.Non_ter ("rbrace",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_lparen ( (
(_:'dypgen__Obj_lparen)
# 6177               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6180               "parser.ml"
 as _3));Obj_rparen ( (
(_:'dypgen__Obj_rparen)
# 6183               "parser.ml"
 as _4));Obj_lbrace ( (
(_:'dypgen__Obj_lbrace)
# 6186               "parser.ml"
 as _5));Obj_match_branches ( (
(_:'dypgen__Obj_match_branches)
# 6189               "parser.ml"
 as _6));Obj_rbrace ( (
(_:'dypgen__Obj_rbrace)
# 6192               "parser.ml"
 as _7))] -> Obj_match_expr 
# 488 "parser.dyp"
(
                                                          ( Exp.match_ ~loc:(symbol_rloc dyp) _3 _6 ):'dypgen__Obj_match_expr)
# 6197               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("list_expr",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 6205               "parser.ml"
 as _1));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 6208               "parser.ml"
 as _2))] -> Obj_list_expr 
# 491 "parser.dyp"
(
                  ( Exp.list ~loc:(symbol_rloc dyp) [] None ):'dypgen__Obj_list_expr)
# 6213               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("list_expr",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_93",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_96",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 6221               "parser.ml"
 as _1));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6224               "parser.ml"
 as _2));Obj_dypgen__nested_nt_93 ( (
(_:'dypgen__Obj_dypgen__nested_nt_93)
# 6227               "parser.ml"
 as _3));Obj_dypgen__nested_nt_96 ( (
(_:'dypgen__Obj_dypgen__nested_nt_96)
# 6230               "parser.ml"
 as _4));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 6233               "parser.ml"
 as _5))] -> Obj_list_expr 
# 492 "parser.dyp"
(
                                                                      ( Exp.list ~loc:(symbol_rloc dyp) (_2::_3) _4 ):'dypgen__Obj_list_expr)
# 6238               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_92",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 6246               "parser.ml"
 as _1));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6249               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_92 
# 492 "parser.dyp"
(
                            (_2):'dypgen__Obj_dypgen__nested_nt_92)
# 6254               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_93",[Dyp.Non_ter ("dypgen__nested_nt_94",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_94 ( (
(_:'dypgen__Obj_dypgen__nested_nt_94)
# 6262               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_93 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_93)
# 6266               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_94",[Dyp.Non_ter ("dypgen__nested_nt_94",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_92",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_94 ( (
(_:'dypgen__Obj_dypgen__nested_nt_94)
# 6274               "parser.ml"
 as _1));Obj_dypgen__nested_nt_92 ( (
(_:'dypgen__Obj_dypgen__nested_nt_92)
# 6277               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_94 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_94)
# 6281               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_94",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_94 
(
([]):'dypgen__Obj_dypgen__nested_nt_94)
# 6290               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_95",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Ter "ELLIPSIS";Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 6298               "parser.ml"
 as _1)); _2;Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6301               "parser.ml"
 as _3))] -> Obj_dypgen__nested_nt_95 
# 492 "parser.dyp"
(
                                                        (_3):'dypgen__Obj_dypgen__nested_nt_95)
# 6306               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_96",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_96 
(
(None):'dypgen__Obj_dypgen__nested_nt_96)
# 6315               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_96",[Dyp.Non_ter ("dypgen__nested_nt_95",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_95 ( (
(_:'dypgen__Obj_dypgen__nested_nt_95)
# 6323               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_96 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_96)
# 6327               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("array_expr",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("rcaret",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 6335               "parser.ml"
 as _1));Obj_rcaret ( (
(_:'dypgen__Obj_rcaret)
# 6338               "parser.ml"
 as _2));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 6341               "parser.ml"
 as _3))] -> Obj_array_expr 
# 495 "parser.dyp"
(
                         ( Exp.array ~loc:(symbol_rloc dyp) [] ):'dypgen__Obj_array_expr)
# 6346               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("array_expr",[Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("rcaret",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_98",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 6354               "parser.ml"
 as _1));Obj_rcaret ( (
(_:'dypgen__Obj_rcaret)
# 6357               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6360               "parser.ml"
 as _3));Obj_dypgen__nested_nt_98 ( (
(_:'dypgen__Obj_dypgen__nested_nt_98)
# 6363               "parser.ml"
 as _4));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 6366               "parser.ml"
 as _5))] -> Obj_array_expr 
# 496 "parser.dyp"
(
                                                 ( Exp.array ~loc:(symbol_rloc dyp) (_3::_4) ):'dypgen__Obj_array_expr)
# 6371               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_97",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 6379               "parser.ml"
 as _1));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6382               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_97 
# 496 "parser.dyp"
(
                                   (_2):'dypgen__Obj_dypgen__nested_nt_97)
# 6387               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_98",[Dyp.Non_ter ("dypgen__nested_nt_99",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_99 ( (
(_:'dypgen__Obj_dypgen__nested_nt_99)
# 6395               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_98 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_98)
# 6399               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_99",[Dyp.Non_ter ("dypgen__nested_nt_99",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_97",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_99 ( (
(_:'dypgen__Obj_dypgen__nested_nt_99)
# 6407               "parser.ml"
 as _1));Obj_dypgen__nested_nt_97 ( (
(_:'dypgen__Obj_dypgen__nested_nt_97)
# 6410               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_99 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_99)
# 6414               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_99",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_99 
(
([]):'dypgen__Obj_dypgen__nested_nt_99)
# 6423               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("stmt_expr",[Dyp.Ter "ASSERT";Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6431               "parser.ml"
 as _2))] -> Obj_stmt_expr 
# 499 "parser.dyp"
(
                ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp ["assert"]) [_2] ):'dypgen__Obj_stmt_expr)
# 6436               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("stmt_expr",[Dyp.Ter "FAIL";Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6444               "parser.ml"
 as _2))] -> Obj_stmt_expr 
# 500 "parser.dyp"
(
              ( Exp.apply ~loc:(symbol_rloc dyp) (mkid_expr dyp ["fail"]) [_2] ):'dypgen__Obj_stmt_expr)
# 6449               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("assign_expr",[Dyp.Non_ter ("binop_expr",Dyp.Less_priority "pl");Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Ter "GETS";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_binop_expr ( (
(_:'dypgen__Obj_binop_expr)
# 6457               "parser.ml"
 as _1));Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6460               "parser.ml"
 as _2)); _3;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6463               "parser.ml"
 as _4));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6466               "parser.ml"
 as _5))] -> Obj_assign_expr 
# 503 "parser.dyp"
(
                                        ( no_array_access _1; Exp.assign ~loc:(symbol_rloc dyp) _1 _5 ):'dypgen__Obj_assign_expr)
# 6471               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("assign_expr",[Dyp.Non_ter ("array_set",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_array_set ( (
(_:'dypgen__Obj_array_set)
# 6479               "parser.ml"
 as _1))] -> Obj_assign_expr 
# 504 "parser.dyp"
(
              ( _1 ):'dypgen__Obj_assign_expr)
# 6484               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("app_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_app_expr ( (
(_:'dypgen__Obj_app_expr)
# 6492               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 507 "parser.dyp"
(
                ( prerr_string "\nexpr_app_expr\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); _1 ):'dypgen__Obj_non_assign_expr)
# 6497               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("prim1_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_prim1_expr ( (
(_:'dypgen__Obj_prim1_expr)
# 6505               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 508 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6510               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("simple_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_simple_expr ( (
(_:'dypgen__Obj_simple_expr)
# 6518               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 509 "parser.dyp"
(
                ( prerr_string "\nexpr_simple_expr\n"; when_debug ~n:1 (fun () -> dyp.print_state stderr); _1 ):'dypgen__Obj_non_assign_expr)
# 6523               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("record_get",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_record_get ( (
(_:'dypgen__Obj_record_get)
# 6531               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 510 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6536               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("paren_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_paren_expr ( (
(_:'dypgen__Obj_paren_expr)
# 6544               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 511 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6549               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("block_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_block_expr ( (
(_:'dypgen__Obj_block_expr)
# 6557               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 512 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6562               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("if_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_if_expr ( (
(_:'dypgen__Obj_if_expr)
# 6570               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 513 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6575               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("while_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_while_expr ( (
(_:'dypgen__Obj_while_expr)
# 6583               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 514 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6588               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("match_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_match_expr ( (
(_:'dypgen__Obj_match_expr)
# 6596               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 515 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6601               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("list_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_list_expr ( (
(_:'dypgen__Obj_list_expr)
# 6609               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 516 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6614               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("array_get",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_array_get ( (
(_:'dypgen__Obj_array_get)
# 6622               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 517 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6627               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("non_assign_expr",[Dyp.Non_ter ("array_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_array_expr ( (
(_:'dypgen__Obj_array_expr)
# 6635               "parser.ml"
 as _1))] -> Obj_non_assign_expr 
# 518 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_non_assign_expr)
# 6640               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_body_expr",[Dyp.Non_ter ("let_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_let_expr ( (
(_:'dypgen__Obj_let_expr)
# 6648               "parser.ml"
 as _1))] -> Obj_block_body_expr 
# 521 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_block_body_expr)
# 6653               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_body_expr",[Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6661               "parser.ml"
 as _1))] -> Obj_block_body_expr 
# 522 "parser.dyp"
(
          ( _1 ):'dypgen__Obj_block_body_expr)
# 6666               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_body_stmt",[Dyp.Non_ter ("block_body_expr",Dyp.No_priority );Dyp.Ter "SEMI";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_block_body_expr ( (
(_:'dypgen__Obj_block_body_expr)
# 6674               "parser.ml"
 as _1)); _2;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6677               "parser.ml"
 as _3))] -> Obj_block_body_stmt 
# 525 "parser.dyp"
(
                              ( Exp.ignore _1 ):'dypgen__Obj_block_body_stmt)
# 6682               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_body_stmt",[Dyp.Non_ter ("block_body_expr",Dyp.No_priority );Dyp.Ter "EOL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_block_body_expr ( (
(_:'dypgen__Obj_block_body_expr)
# 6690               "parser.ml"
 as _1)); _2] -> Obj_block_body_stmt 
# 526 "parser.dyp"
(
                        ( _1 ):'dypgen__Obj_block_body_stmt)
# 6695               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("tuple_exprs",[Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("comma",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6703               "parser.ml"
 as _1));Obj_comma ( (
(_:'dypgen__Obj_comma)
# 6706               "parser.ml"
 as _2))] -> Obj_tuple_exprs 
# 529 "parser.dyp"
(
               ( [_1] ):'dypgen__Obj_tuple_exprs)
# 6711               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("tuple_exprs",[Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_101",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6719               "parser.ml"
 as _1));Obj_dypgen__nested_nt_101 ( (
(_:'dypgen__Obj_dypgen__nested_nt_101)
# 6722               "parser.ml"
 as _2))] -> Obj_tuple_exprs 
# 530 "parser.dyp"
(
                            ( _1::_2 ):'dypgen__Obj_tuple_exprs)
# 6727               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_100",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 6735               "parser.ml"
 as _1));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6738               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_100 
# 530 "parser.dyp"
(
                     (_2):'dypgen__Obj_dypgen__nested_nt_100)
# 6743               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_101",[Dyp.Non_ter ("dypgen__nested_nt_102",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_102 ( (
(_:'dypgen__Obj_dypgen__nested_nt_102)
# 6751               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_101 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_101)
# 6755               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_102",[Dyp.Non_ter ("dypgen__nested_nt_102",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_100",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_102 ( (
(_:'dypgen__Obj_dypgen__nested_nt_102)
# 6763               "parser.ml"
 as _1));Obj_dypgen__nested_nt_100 ( (
(_:'dypgen__Obj_dypgen__nested_nt_100)
# 6766               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_102 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_102)
# 6770               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_102",[Dyp.Non_ter ("dypgen__nested_nt_100",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_100 ( (
(_:'dypgen__Obj_dypgen__nested_nt_100)
# 6778               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_102 
(
([_1]):'dypgen__Obj_dypgen__nested_nt_102)
# 6782               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("array_get",[Dyp.Non_ter ("non_binop_expr",Dyp.No_priority );Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_non_binop_expr ( (
(_:'dypgen__Obj_non_binop_expr)
# 6790               "parser.ml"
 as _1));Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 6793               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6796               "parser.ml"
 as _3));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 6799               "parser.ml"
 as _4))] -> Obj_array_get 
# 533 "parser.dyp"
(
                                      ( Exp.array_get ~loc:(symbol_rloc dyp) _1 _3 ):'dypgen__Obj_array_get)
# 6804               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("array_set",[Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("lbrack",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority );Dyp.Non_ter ("rbrack",Dyp.No_priority );Dyp.Ter "GETS";Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6812               "parser.ml"
 as _1));Obj_lbrack ( (
(_:'dypgen__Obj_lbrack)
# 6815               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6818               "parser.ml"
 as _3));Obj_rbrack ( (
(_:'dypgen__Obj_rbrack)
# 6821               "parser.ml"
 as _4)); _5;Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 6824               "parser.ml"
 as _6));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6827               "parser.ml"
 as _7))] -> Obj_array_set 
# 536 "parser.dyp"
(
                                           ( Exp.array_set ~loc:(symbol_rloc dyp) _1 _3 _7 ):'dypgen__Obj_array_set)
# 6832               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_get",[Dyp.Non_ter ("non_binop_expr",Dyp.No_priority );Dyp.Non_ter ("dot",Dyp.No_priority );Dyp.Non_ter ("simple_id",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_non_binop_expr ( (
(_:'dypgen__Obj_non_binop_expr)
# 6840               "parser.ml"
 as _1));Obj_dot ( (
(_:'dypgen__Obj_dot)
# 6843               "parser.ml"
 as _2));Obj_simple_id ( (
(_:'dypgen__Obj_simple_id)
# 6846               "parser.ml"
 as _3))] -> Obj_record_get 
# 539 "parser.dyp"
(
                                 ( no_uppercase_ident _1; Exp.record_get _1 _3 ):'dypgen__Obj_record_get)
# 6851               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_field",[Dyp.Non_ter ("id",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id ( (
(_:'dypgen__Obj_id)
# 6859               "parser.ml"
 as _1));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 6862               "parser.ml"
 as _2));Obj_expr ( (
(_:'dypgen__Obj_expr)
# 6865               "parser.ml"
 as _3))] -> Obj_record_field 
# 542 "parser.dyp"
(
                  ( _1, _3 ):'dypgen__Obj_record_field)
# 6870               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_pun",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 6878               "parser.ml"
 as _1)] -> Obj_record_pun 
# 545 "parser.dyp"
(
       ( mkid [_1] (symbol_rloc dyp), Exp.ident ~loc:(symbol_rloc dyp) (mkid [_1] (symbol_rloc dyp)) ):'dypgen__Obj_record_pun)
# 6883               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("record_exprs",[Dyp.Non_ter ("dypgen__nested_nt_103",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_106",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_103 ( (
(_:'dypgen__Obj_dypgen__nested_nt_103)
# 6891               "parser.ml"
 as _1));Obj_dypgen__nested_nt_106 ( (
(_:'dypgen__Obj_dypgen__nested_nt_106)
# 6894               "parser.ml"
 as _2))] -> Obj_record_exprs 
# 548 "parser.dyp"
(
                                                                          (_1::_2):'dypgen__Obj_record_exprs)
# 6899               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_103",[Dyp.Non_ter ("record_field",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_record_field ( (
(_:'dypgen__Obj_record_field)
# 6907               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_103 
(
(_1):'dypgen__Obj_dypgen__nested_nt_103)
# 6911               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_103",[Dyp.Non_ter ("record_pun",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_record_pun ( (
(_:'dypgen__Obj_record_pun)
# 6919               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_103 
(
(_1):'dypgen__Obj_dypgen__nested_nt_103)
# 6923               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_105",[Dyp.Non_ter ("comma",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_104",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_comma ( (
(_:'dypgen__Obj_comma)
# 6931               "parser.ml"
 as _1));Obj_dypgen__nested_nt_104 ( (
(_:'dypgen__Obj_dypgen__nested_nt_104)
# 6934               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_105 
# 548 "parser.dyp"
(
                                                                   (_2):'dypgen__Obj_dypgen__nested_nt_105)
# 6939               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_104",[Dyp.Non_ter ("record_field",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_record_field ( (
(_:'dypgen__Obj_record_field)
# 6947               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_104 
(
(_1):'dypgen__Obj_dypgen__nested_nt_104)
# 6951               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_104",[Dyp.Non_ter ("record_pun",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_record_pun ( (
(_:'dypgen__Obj_record_pun)
# 6959               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_104 
(
(_1):'dypgen__Obj_dypgen__nested_nt_104)
# 6963               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_106",[Dyp.Non_ter ("dypgen__nested_nt_107",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_107 ( (
(_:'dypgen__Obj_dypgen__nested_nt_107)
# 6971               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_106 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_106)
# 6975               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_107",[Dyp.Non_ter ("dypgen__nested_nt_107",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_105",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_107 ( (
(_:'dypgen__Obj_dypgen__nested_nt_107)
# 6983               "parser.ml"
 as _1));Obj_dypgen__nested_nt_105 ( (
(_:'dypgen__Obj_dypgen__nested_nt_105)
# 6986               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_107 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_107)
# 6990               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_107",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_107 
(
([]):'dypgen__Obj_dypgen__nested_nt_107)
# 6999               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_body",[Dyp.Non_ter ("dypgen__star___block_body_stmt",Dyp.No_priority );Dyp.Non_ter ("block_body_expr",Dyp.No_priority );Dyp.Ter "SEMI"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__star___block_body_stmt ( (
(_:'dypgen__Obj_block_body_stmt list)
# 7007               "parser.ml"
 as _1));Obj_block_body_expr ( (
(_:'dypgen__Obj_block_body_expr)
# 7010               "parser.ml"
 as _2)); _3] -> Obj_block_body 
# 551 "parser.dyp"
(
                                          ( _1 @ [Exp.ignore _2] ):'dypgen__Obj_block_body)
# 7015               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("block_body",[Dyp.Non_ter ("dypgen__star___block_body_stmt",Dyp.No_priority );Dyp.Non_ter ("block_body_expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__star___block_body_stmt ( (
(_:'dypgen__Obj_block_body_stmt list)
# 7023               "parser.ml"
 as _1));Obj_block_body_expr ( (
(_:'dypgen__Obj_block_body_expr)
# 7026               "parser.ml"
 as _2))] -> Obj_block_body 
# 552 "parser.dyp"
(
                                     ( _1 @ [_2] ):'dypgen__Obj_block_body)
# 7031               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__star___block_body_stmt",[Dyp.Non_ter ("dypgen__nested_nt_108",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_108 ( (
(_:'dypgen__Obj_dypgen__nested_nt_108)
# 7039               "parser.ml"
 as _1))] -> Obj_dypgen__star___block_body_stmt 
(
(List.rev _1):'dypgen__Obj_block_body_stmt list)
# 7043               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_108",[Dyp.Non_ter ("dypgen__nested_nt_108",Dyp.No_priority );Dyp.Non_ter ("block_body_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_108 ( (
(_:'dypgen__Obj_dypgen__nested_nt_108)
# 7051               "parser.ml"
 as _1));Obj_block_body_stmt ( (
(_:'dypgen__Obj_block_body_stmt)
# 7054               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_108 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_108)
# 7058               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_108",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_108 
(
([]):'dypgen__Obj_dypgen__nested_nt_108)
# 7067               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("file_path",[Dyp.Ter "STRING"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_STRING  (
(_:(string))
# 7075               "parser.ml"
 as _1)] -> Obj_file_path 
# 555 "parser.dyp"
(
           ( Location.mkloc _1 (symbol_rloc dyp) ):'dypgen__Obj_file_path)
# 7080               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("id_str",[Dyp.Ter "ID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ID  (
(_:(string))
# 7088               "parser.ml"
 as _1)] -> Obj_id_str 
# 558 "parser.dyp"
(
       ( Location.mkloc _1 (symbol_rloc dyp) ):'dypgen__Obj_id_str)
# 7093               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("id_str",[Dyp.Non_ter ("infix",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_infix ( (
(_:'dypgen__Obj_infix)
# 7101               "parser.ml"
 as _1))] -> Obj_id_str 
# 559 "parser.dyp"
(
          ( Location.mkloc _1 (symbol_rloc dyp) ):'dypgen__Obj_id_str)
# 7106               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("id_str",[Dyp.Non_ter ("prefix",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_prefix ( (
(_:'dypgen__Obj_prefix)
# 7114               "parser.ml"
 as _1))] -> Obj_id_str 
# 560 "parser.dyp"
(
           ( Location.mkloc _1 (symbol_rloc dyp) ):'dypgen__Obj_id_str)
# 7119               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("type_id_str",[Dyp.Ter "TYPEID"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_TYPEID  (
(_:(string))
# 7127               "parser.ml"
 as _1)] -> Obj_type_id_str 
# 563 "parser.dyp"
(
           ( Location.mkloc _1 (symbol_rloc dyp) ):'dypgen__Obj_type_id_str)
# 7132               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("any_id_str",[Dyp.Non_ter ("id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id_str ( (
(_:'dypgen__Obj_id_str)
# 7140               "parser.ml"
 as _1))] -> Obj_any_id_str 
# 566 "parser.dyp"
(
           ( _1 ):'dypgen__Obj_any_id_str)
# 7145               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("any_id_str",[Dyp.Non_ter ("type_id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_type_id_str ( (
(_:'dypgen__Obj_type_id_str)
# 7153               "parser.ml"
 as _1))] -> Obj_any_id_str 
# 567 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_any_id_str)
# 7158               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_id_str",[Dyp.Non_ter ("id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_id_str ( (
(_:'dypgen__Obj_id_str)
# 7166               "parser.ml"
 as _1))] -> Obj_export_id_str 
# 570 "parser.dyp"
(
           ( ExportExceptValue _1 ):'dypgen__Obj_export_id_str)
# 7171               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("export_id_str",[Dyp.Non_ter ("type_id_str",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_type_id_str ( (
(_:'dypgen__Obj_type_id_str)
# 7179               "parser.ml"
 as _1))] -> Obj_export_id_str 
# 571 "parser.dyp"
(
                ( ExportExceptData _1 ):'dypgen__Obj_export_id_str)
# 7184               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("foreign_stmt",[Dyp.Ter "FOREIGN";Dyp.Ter "WASM";Dyp.Non_ter ("id_str",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Ter "FROM";Dyp.Non_ter ("file_path",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_id_str ( (
(_:'dypgen__Obj_id_str)
# 7192               "parser.ml"
 as _3));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 7195               "parser.ml"
 as _4));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 7198               "parser.ml"
 as _5)); _6;Obj_file_path ( (
(_:'dypgen__Obj_file_path)
# 7201               "parser.ml"
 as _7))] -> Obj_foreign_stmt 
# 574 "parser.dyp"
(
                                                 ( Val.mk ~loc:(symbol_rloc dyp) ~mod_:_7 ~name:_3 ~typ:_5 ~prim:[] ):'dypgen__Obj_foreign_stmt)
# 7206               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("primitive",[Dyp.Ter "ASSERT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_primitive 
# 577 "parser.dyp"
(
           ( Location.mkloc "assert" (symbol_rloc dyp) ):'dypgen__Obj_primitive)
# 7216               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("primitive",[Dyp.Ter "FAIL"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_primitive 
# 578 "parser.dyp"
(
         ( Location.mkloc "fail" (symbol_rloc dyp) ):'dypgen__Obj_primitive)
# 7226               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("primitive_stmt",[Dyp.Ter "PRIMITIVE";Dyp.Non_ter ("id_str",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("equal",Dyp.No_priority );Dyp.Ter "STRING"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_id_str ( (
(_:'dypgen__Obj_id_str)
# 7234               "parser.ml"
 as _2));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 7237               "parser.ml"
 as _3));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 7240               "parser.ml"
 as _4));Obj_equal ( (
(_:'dypgen__Obj_equal)
# 7243               "parser.ml"
 as _5));Obj_STRING  (
(_:(string))
# 7246               "parser.ml"
 as _6)] -> Obj_primitive_stmt 
# 581 "parser.dyp"
(
                                            ( Val.mk ~loc:(symbol_rloc dyp) ~mod_:{_2 with txt="primitive"} ~name:_2 ~typ:_4 ~prim:[_6] ):'dypgen__Obj_primitive_stmt)
# 7251               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("primitive_stmt",[Dyp.Ter "PRIMITIVE";Dyp.Non_ter ("primitive",Dyp.No_priority );Dyp.Non_ter ("colon",Dyp.No_priority );Dyp.Non_ter ("typ",Dyp.No_priority );Dyp.Non_ter ("equal",Dyp.No_priority );Dyp.Ter "STRING"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_primitive ( (
(_:'dypgen__Obj_primitive)
# 7259               "parser.ml"
 as _2));Obj_colon ( (
(_:'dypgen__Obj_colon)
# 7262               "parser.ml"
 as _3));Obj_typ ( (
(_:'dypgen__Obj_typ)
# 7265               "parser.ml"
 as _4));Obj_equal ( (
(_:'dypgen__Obj_equal)
# 7268               "parser.ml"
 as _5));Obj_STRING  (
(_:(string))
# 7271               "parser.ml"
 as _6)] -> Obj_primitive_stmt 
# 582 "parser.dyp"
(
                                               ( Val.mk ~loc:(symbol_rloc dyp) ~mod_:{_2 with txt="primitive"} ~name:_2 ~typ:_4 ~prim:[_6] ):'dypgen__Obj_primitive_stmt)
# 7276               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Ter "LET";Dyp.Ter "REC";Dyp.Non_ter ("value_binds",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_value_binds ( (
(_:'dypgen__Obj_value_binds)
# 7284               "parser.ml"
 as _3))] -> Obj_toplevel_stmt 
# 585 "parser.dyp"
(
                        ( Top.let_ Nonexported Recursive _3 ):'dypgen__Obj_toplevel_stmt)
# 7289               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Ter "LET";Dyp.Non_ter ("value_binds",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_value_binds ( (
(_:'dypgen__Obj_value_binds)
# 7297               "parser.ml"
 as _2))] -> Obj_toplevel_stmt 
# 586 "parser.dyp"
(
                    ( Top.let_ Nonexported Nonrecursive _2 ):'dypgen__Obj_toplevel_stmt)
# 7302               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Non_ter ("expr",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_expr ( (
(_:'dypgen__Obj_expr)
# 7310               "parser.ml"
 as _1))] -> Obj_toplevel_stmt 
# 587 "parser.dyp"
(
         ( Top.expr _1 ):'dypgen__Obj_toplevel_stmt)
# 7315               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Non_ter ("import_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_import_stmt ( (
(_:'dypgen__Obj_import_stmt)
# 7323               "parser.ml"
 as _1))] -> Obj_toplevel_stmt 
# 588 "parser.dyp"
(
                ( Top.import _1 ):'dypgen__Obj_toplevel_stmt)
# 7328               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Ter "IMPORT";Dyp.Non_ter ("foreign_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_foreign_stmt ( (
(_:'dypgen__Obj_foreign_stmt)
# 7336               "parser.ml"
 as _2))] -> Obj_toplevel_stmt 
# 589 "parser.dyp"
(
                        ( Top.foreign ~loc:(symbol_rloc dyp) Nonexported _2 ):'dypgen__Obj_toplevel_stmt)
# 7341               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Non_ter ("export_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_export_stmt ( (
(_:'dypgen__Obj_export_stmt)
# 7349               "parser.ml"
 as _1))] -> Obj_toplevel_stmt 
# 590 "parser.dyp"
(
                ( _1 ):'dypgen__Obj_toplevel_stmt)
# 7354               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Non_ter ("primitive_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_primitive_stmt ( (
(_:'dypgen__Obj_primitive_stmt)
# 7362               "parser.ml"
 as _1))] -> Obj_toplevel_stmt 
# 591 "parser.dyp"
(
                   ( Top.primitive ~loc:(symbol_rloc dyp) Nonexported _1 ):'dypgen__Obj_toplevel_stmt)
# 7367               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmt",[Dyp.Non_ter ("data_declaration",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_data_declaration ( (
(_:'dypgen__Obj_data_declaration)
# 7375               "parser.ml"
 as _1))] -> Obj_toplevel_stmt 
# 592 "parser.dyp"
(
                     ( Top.data Nonexported _1 ):'dypgen__Obj_toplevel_stmt)
# 7380               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("toplevel_stmts",[Dyp.Non_ter ("toplevel_stmt",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_110",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_toplevel_stmt ( (
(_:'dypgen__Obj_toplevel_stmt)
# 7388               "parser.ml"
 as _1));Obj_dypgen__nested_nt_110 ( (
(_:'dypgen__Obj_dypgen__nested_nt_110)
# 7391               "parser.ml"
 as _2))] -> Obj_toplevel_stmts 
# 595 "parser.dyp"
(
                                            ( _1::_2 ):'dypgen__Obj_toplevel_stmts)
# 7396               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_109",[Dyp.Non_ter ("eos",Dyp.No_priority );Dyp.Non_ter ("toplevel_stmt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_eos ( (
(_:'dypgen__Obj_eos)
# 7404               "parser.ml"
 as _1));Obj_toplevel_stmt ( (
(_:'dypgen__Obj_toplevel_stmt)
# 7407               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_109 
# 595 "parser.dyp"
(
                                     (_2):'dypgen__Obj_dypgen__nested_nt_109)
# 7412               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_110",[Dyp.Non_ter ("dypgen__nested_nt_111",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_111 ( (
(_:'dypgen__Obj_dypgen__nested_nt_111)
# 7420               "parser.ml"
 as _1))] -> Obj_dypgen__nested_nt_110 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_110)
# 7424               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_111",[Dyp.Non_ter ("dypgen__nested_nt_111",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_109",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_111 ( (
(_:'dypgen__Obj_dypgen__nested_nt_111)
# 7432               "parser.ml"
 as _1));Obj_dypgen__nested_nt_109 ( (
(_:'dypgen__Obj_dypgen__nested_nt_109)
# 7435               "parser.ml"
 as _2))] -> Obj_dypgen__nested_nt_111 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_111)
# 7439               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_111",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_111 
(
([]):'dypgen__Obj_dypgen__nested_nt_111)
# 7448               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("program",[Dyp.Non_ter ("dypgen__option_EOL",Dyp.No_priority );Dyp.Non_ter ("toplevel_stmts",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_eos",Dyp.No_priority );Dyp.Ter "EOF"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_EOL ( (
(_:'dypgen__Obj_dypgen__option_EOL)
# 7456               "parser.ml"
 as _1));Obj_toplevel_stmts ( (
(_:'dypgen__Obj_toplevel_stmts)
# 7459               "parser.ml"
 as _2));Obj_dypgen__option_eos ( (
(_:'dypgen__Obj_eos option)
# 7462               "parser.ml"
 as _3)); _4] -> Obj_program 
# 598 "parser.dyp"
(
                                 ( make_program _2 ):(Parsetree.parsed_program))
# 7467               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_eos",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_eos 
(
(None):'dypgen__Obj_eos option)
# 7476               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_eos",[Dyp.Non_ter ("eos",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_eos ( (
(_:'dypgen__Obj_eos)
# 7484               "parser.ml"
 as _1))] -> Obj_dypgen__option_eos 
(
(Some _1):'dypgen__Obj_eos option)
# 7488               "parser.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])],

(["dummy_entry",Dyp.RE_Eof_char],
[0,(fun _ -> Lexeme_matched "")]),

[]

let __dypgen_regexp_decl = []

let dyp_merge_Lexeme_matched l =
  match dyp_merge_Lexeme_matched l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_ID l =
  match dyp_merge_Obj_ID l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_NUM l =
  match dyp_merge_Obj_NUM l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_STRING l =
  match dyp_merge_Obj_STRING l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_TYPEID l =
  match dyp_merge_Obj_TYPEID l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_ampamp_op l =
  match dyp_merge_Obj_ampamp_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_any_id_str l =
  match dyp_merge_Obj_any_id_str l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_any_or_var_pat l =
  match dyp_merge_Obj_any_or_var_pat l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_app_arg_exprs l =
  match dyp_merge_Obj_app_arg_exprs l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_app_expr l =
  match dyp_merge_Obj_app_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_array_expr l =
  match dyp_merge_Obj_array_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_array_get l =
  match dyp_merge_Obj_array_get l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_array_set l =
  match dyp_merge_Obj_array_set l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_arrow l =
  match dyp_merge_Obj_arrow l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_assign_expr l =
  match dyp_merge_Obj_assign_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_binop_expr l =
  match dyp_merge_Obj_binop_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_block l =
  match dyp_merge_Obj_block l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_block_body l =
  match dyp_merge_Obj_block_body l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_block_body_expr l =
  match dyp_merge_Obj_block_body_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_block_body_stmt l =
  match dyp_merge_Obj_block_body_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_block_expr l =
  match dyp_merge_Obj_block_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_block_or_expr l =
  match dyp_merge_Obj_block_or_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_colon l =
  match dyp_merge_Obj_colon l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_comma l =
  match dyp_merge_Obj_comma l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_const l =
  match dyp_merge_Obj_const l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dash_op l =
  match dyp_merge_Obj_dash_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dasheq_op l =
  match dyp_merge_Obj_dasheq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_data_constructor l =
  match dyp_merge_Obj_data_constructor l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_data_constructors l =
  match dyp_merge_Obj_data_constructors l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_data_declaration l =
  match dyp_merge_Obj_data_declaration l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_data_label l =
  match dyp_merge_Obj_data_label l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_data_labels l =
  match dyp_merge_Obj_data_labels l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_data_typ l =
  match dyp_merge_Obj_data_typ l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dot l =
  match dyp_merge_Obj_dot l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_0 l =
  match dyp_merge_Obj_dypgen__nested_nt_0 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_1 l =
  match dyp_merge_Obj_dypgen__nested_nt_1 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_10 l =
  match dyp_merge_Obj_dypgen__nested_nt_10 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_100 l =
  match dyp_merge_Obj_dypgen__nested_nt_100 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_101 l =
  match dyp_merge_Obj_dypgen__nested_nt_101 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_102 l =
  match dyp_merge_Obj_dypgen__nested_nt_102 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_103 l =
  match dyp_merge_Obj_dypgen__nested_nt_103 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_104 l =
  match dyp_merge_Obj_dypgen__nested_nt_104 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_105 l =
  match dyp_merge_Obj_dypgen__nested_nt_105 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_106 l =
  match dyp_merge_Obj_dypgen__nested_nt_106 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_107 l =
  match dyp_merge_Obj_dypgen__nested_nt_107 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_108 l =
  match dyp_merge_Obj_dypgen__nested_nt_108 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_109 l =
  match dyp_merge_Obj_dypgen__nested_nt_109 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_11 l =
  match dyp_merge_Obj_dypgen__nested_nt_11 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_110 l =
  match dyp_merge_Obj_dypgen__nested_nt_110 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_111 l =
  match dyp_merge_Obj_dypgen__nested_nt_111 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_12 l =
  match dyp_merge_Obj_dypgen__nested_nt_12 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_13 l =
  match dyp_merge_Obj_dypgen__nested_nt_13 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_14 l =
  match dyp_merge_Obj_dypgen__nested_nt_14 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_15 l =
  match dyp_merge_Obj_dypgen__nested_nt_15 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_16 l =
  match dyp_merge_Obj_dypgen__nested_nt_16 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_17 l =
  match dyp_merge_Obj_dypgen__nested_nt_17 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_18 l =
  match dyp_merge_Obj_dypgen__nested_nt_18 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_19 l =
  match dyp_merge_Obj_dypgen__nested_nt_19 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_2 l =
  match dyp_merge_Obj_dypgen__nested_nt_2 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_20 l =
  match dyp_merge_Obj_dypgen__nested_nt_20 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_21 l =
  match dyp_merge_Obj_dypgen__nested_nt_21 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_22 l =
  match dyp_merge_Obj_dypgen__nested_nt_22 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_23 l =
  match dyp_merge_Obj_dypgen__nested_nt_23 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_24 l =
  match dyp_merge_Obj_dypgen__nested_nt_24 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_25 l =
  match dyp_merge_Obj_dypgen__nested_nt_25 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_26 l =
  match dyp_merge_Obj_dypgen__nested_nt_26 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_27 l =
  match dyp_merge_Obj_dypgen__nested_nt_27 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_28 l =
  match dyp_merge_Obj_dypgen__nested_nt_28 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_29 l =
  match dyp_merge_Obj_dypgen__nested_nt_29 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_3 l =
  match dyp_merge_Obj_dypgen__nested_nt_3 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_30 l =
  match dyp_merge_Obj_dypgen__nested_nt_30 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_31 l =
  match dyp_merge_Obj_dypgen__nested_nt_31 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_32 l =
  match dyp_merge_Obj_dypgen__nested_nt_32 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_33 l =
  match dyp_merge_Obj_dypgen__nested_nt_33 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_34 l =
  match dyp_merge_Obj_dypgen__nested_nt_34 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_35 l =
  match dyp_merge_Obj_dypgen__nested_nt_35 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_36 l =
  match dyp_merge_Obj_dypgen__nested_nt_36 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_37 l =
  match dyp_merge_Obj_dypgen__nested_nt_37 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_38 l =
  match dyp_merge_Obj_dypgen__nested_nt_38 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_39 l =
  match dyp_merge_Obj_dypgen__nested_nt_39 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_4 l =
  match dyp_merge_Obj_dypgen__nested_nt_4 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_40 l =
  match dyp_merge_Obj_dypgen__nested_nt_40 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_41 l =
  match dyp_merge_Obj_dypgen__nested_nt_41 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_42 l =
  match dyp_merge_Obj_dypgen__nested_nt_42 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_43 l =
  match dyp_merge_Obj_dypgen__nested_nt_43 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_44 l =
  match dyp_merge_Obj_dypgen__nested_nt_44 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_45 l =
  match dyp_merge_Obj_dypgen__nested_nt_45 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_46 l =
  match dyp_merge_Obj_dypgen__nested_nt_46 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_47 l =
  match dyp_merge_Obj_dypgen__nested_nt_47 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_48 l =
  match dyp_merge_Obj_dypgen__nested_nt_48 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_49 l =
  match dyp_merge_Obj_dypgen__nested_nt_49 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_5 l =
  match dyp_merge_Obj_dypgen__nested_nt_5 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_50 l =
  match dyp_merge_Obj_dypgen__nested_nt_50 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_51 l =
  match dyp_merge_Obj_dypgen__nested_nt_51 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_52 l =
  match dyp_merge_Obj_dypgen__nested_nt_52 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_53 l =
  match dyp_merge_Obj_dypgen__nested_nt_53 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_54 l =
  match dyp_merge_Obj_dypgen__nested_nt_54 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_55 l =
  match dyp_merge_Obj_dypgen__nested_nt_55 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_56 l =
  match dyp_merge_Obj_dypgen__nested_nt_56 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_57 l =
  match dyp_merge_Obj_dypgen__nested_nt_57 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_58 l =
  match dyp_merge_Obj_dypgen__nested_nt_58 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_59 l =
  match dyp_merge_Obj_dypgen__nested_nt_59 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_6 l =
  match dyp_merge_Obj_dypgen__nested_nt_6 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_60 l =
  match dyp_merge_Obj_dypgen__nested_nt_60 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_61 l =
  match dyp_merge_Obj_dypgen__nested_nt_61 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_62 l =
  match dyp_merge_Obj_dypgen__nested_nt_62 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_63 l =
  match dyp_merge_Obj_dypgen__nested_nt_63 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_64 l =
  match dyp_merge_Obj_dypgen__nested_nt_64 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_65 l =
  match dyp_merge_Obj_dypgen__nested_nt_65 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_66 l =
  match dyp_merge_Obj_dypgen__nested_nt_66 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_67 l =
  match dyp_merge_Obj_dypgen__nested_nt_67 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_68 l =
  match dyp_merge_Obj_dypgen__nested_nt_68 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_69 l =
  match dyp_merge_Obj_dypgen__nested_nt_69 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_7 l =
  match dyp_merge_Obj_dypgen__nested_nt_7 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_70 l =
  match dyp_merge_Obj_dypgen__nested_nt_70 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_71 l =
  match dyp_merge_Obj_dypgen__nested_nt_71 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_72 l =
  match dyp_merge_Obj_dypgen__nested_nt_72 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_73 l =
  match dyp_merge_Obj_dypgen__nested_nt_73 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_74 l =
  match dyp_merge_Obj_dypgen__nested_nt_74 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_75 l =
  match dyp_merge_Obj_dypgen__nested_nt_75 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_76 l =
  match dyp_merge_Obj_dypgen__nested_nt_76 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_77 l =
  match dyp_merge_Obj_dypgen__nested_nt_77 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_78 l =
  match dyp_merge_Obj_dypgen__nested_nt_78 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_79 l =
  match dyp_merge_Obj_dypgen__nested_nt_79 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_8 l =
  match dyp_merge_Obj_dypgen__nested_nt_8 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_80 l =
  match dyp_merge_Obj_dypgen__nested_nt_80 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_81 l =
  match dyp_merge_Obj_dypgen__nested_nt_81 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_82 l =
  match dyp_merge_Obj_dypgen__nested_nt_82 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_83 l =
  match dyp_merge_Obj_dypgen__nested_nt_83 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_84 l =
  match dyp_merge_Obj_dypgen__nested_nt_84 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_85 l =
  match dyp_merge_Obj_dypgen__nested_nt_85 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_86 l =
  match dyp_merge_Obj_dypgen__nested_nt_86 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_87 l =
  match dyp_merge_Obj_dypgen__nested_nt_87 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_88 l =
  match dyp_merge_Obj_dypgen__nested_nt_88 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_89 l =
  match dyp_merge_Obj_dypgen__nested_nt_89 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_9 l =
  match dyp_merge_Obj_dypgen__nested_nt_9 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_90 l =
  match dyp_merge_Obj_dypgen__nested_nt_90 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_91 l =
  match dyp_merge_Obj_dypgen__nested_nt_91 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_92 l =
  match dyp_merge_Obj_dypgen__nested_nt_92 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_93 l =
  match dyp_merge_Obj_dypgen__nested_nt_93 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_94 l =
  match dyp_merge_Obj_dypgen__nested_nt_94 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_95 l =
  match dyp_merge_Obj_dypgen__nested_nt_95 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_96 l =
  match dyp_merge_Obj_dypgen__nested_nt_96 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_97 l =
  match dyp_merge_Obj_dypgen__nested_nt_97 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_98 l =
  match dyp_merge_Obj_dypgen__nested_nt_98 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_99 l =
  match dyp_merge_Obj_dypgen__nested_nt_99 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_EOL l =
  match dyp_merge_Obj_dypgen__option_EOL l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_eos l =
  match dyp_merge_Obj_dypgen__option_eos l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_patterns l =
  match dyp_merge_Obj_dypgen__option_patterns l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_pipe l =
  match dyp_merge_Obj_dypgen__option_pipe l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__star___block_body_stmt l =
  match dyp_merge_Obj_dypgen__star___block_body_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_eos l =
  match dyp_merge_Obj_eos l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_eqeq_op l =
  match dyp_merge_Obj_eqeq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_equal l =
  match dyp_merge_Obj_equal l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_export_id_str l =
  match dyp_merge_Obj_export_id_str l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_export_stmt l =
  match dyp_merge_Obj_export_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_expr l =
  match dyp_merge_Obj_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_ext_constructor l =
  match dyp_merge_Obj_ext_constructor l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_file_path l =
  match dyp_merge_Obj_file_path l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_foreign_stmt l =
  match dyp_merge_Obj_foreign_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_greatereq_op l =
  match dyp_merge_Obj_greatereq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_id l =
  match dyp_merge_Obj_id l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_id_expr l =
  match dyp_merge_Obj_id_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_id_str l =
  match dyp_merge_Obj_id_str l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_if_expr l =
  match dyp_merge_Obj_if_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_import_shape l =
  match dyp_merge_Obj_import_shape l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_import_stmt l =
  match dyp_merge_Obj_import_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_infix l =
  match dyp_merge_Obj_infix l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_infix_op l =
  match dyp_merge_Obj_infix_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lam_args l =
  match dyp_merge_Obj_lam_args l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lam_expr l =
  match dyp_merge_Obj_lam_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lbrace l =
  match dyp_merge_Obj_lbrace l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lbrack l =
  match dyp_merge_Obj_lbrack l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lcaret l =
  match dyp_merge_Obj_lcaret l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lcaret_op l =
  match dyp_merge_Obj_lcaret_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lesseq_op l =
  match dyp_merge_Obj_lesseq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_let_expr l =
  match dyp_merge_Obj_let_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_list_expr l =
  match dyp_merge_Obj_list_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lparen l =
  match dyp_merge_Obj_lparen l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_match_branch l =
  match dyp_merge_Obj_match_branch l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_match_branches l =
  match dyp_merge_Obj_match_branches l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_match_expr l =
  match dyp_merge_Obj_match_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_non_assign_expr l =
  match dyp_merge_Obj_non_assign_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_non_binop_expr l =
  match dyp_merge_Obj_non_binop_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_paren_expr l =
  match dyp_merge_Obj_paren_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_pattern l =
  match dyp_merge_Obj_pattern l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_patterns l =
  match dyp_merge_Obj_patterns l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_percent_op l =
  match dyp_merge_Obj_percent_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_pipe l =
  match dyp_merge_Obj_pipe l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_pipepipe_op l =
  match dyp_merge_Obj_pipepipe_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_plus_op l =
  match dyp_merge_Obj_plus_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_pluseq_op l =
  match dyp_merge_Obj_pluseq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_prefix l =
  match dyp_merge_Obj_prefix l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_prim1_expr l =
  match dyp_merge_Obj_prim1_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_primitive l =
  match dyp_merge_Obj_primitive l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_primitive_stmt l =
  match dyp_merge_Obj_primitive_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_program l =
  match dyp_merge_Obj_program l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rbrace l =
  match dyp_merge_Obj_rbrace l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rbrack l =
  match dyp_merge_Obj_rbrack l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rcaret l =
  match dyp_merge_Obj_rcaret l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rcaret_op l =
  match dyp_merge_Obj_rcaret_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_record_exprs l =
  match dyp_merge_Obj_record_exprs l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_record_field l =
  match dyp_merge_Obj_record_field l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_record_get l =
  match dyp_merge_Obj_record_get l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_record_pattern l =
  match dyp_merge_Obj_record_pattern l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_record_patterns l =
  match dyp_merge_Obj_record_patterns l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_record_pun l =
  match dyp_merge_Obj_record_pun l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rparen l =
  match dyp_merge_Obj_rparen l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_simple_expr l =
  match dyp_merge_Obj_simple_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_simple_id l =
  match dyp_merge_Obj_simple_id l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_slash_op l =
  match dyp_merge_Obj_slash_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_slasheq_op l =
  match dyp_merge_Obj_slasheq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_star_op l =
  match dyp_merge_Obj_star_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_stareq_op l =
  match dyp_merge_Obj_stareq_op l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_stmt_expr l =
  match dyp_merge_Obj_stmt_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_thickarrow l =
  match dyp_merge_Obj_thickarrow l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_toplevel_stmt l =
  match dyp_merge_Obj_toplevel_stmt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_toplevel_stmts l =
  match dyp_merge_Obj_toplevel_stmts l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_tuple_exprs l =
  match dyp_merge_Obj_tuple_exprs l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_tuple_patterns l =
  match dyp_merge_Obj_tuple_patterns l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_tuple_typs l =
  match dyp_merge_Obj_tuple_typs l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_typ l =
  match dyp_merge_Obj_typ l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_type_id l =
  match dyp_merge_Obj_type_id l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_type_id_str l =
  match dyp_merge_Obj_type_id_str l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_typs l =
  match dyp_merge_Obj_typs l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_value_bind l =
  match dyp_merge_Obj_value_bind l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_value_binds l =
  match dyp_merge_Obj_value_binds l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_while_expr l =
  match dyp_merge_Obj_while_expr l with
    | ([],_,_) -> dyp_merge l
    | res -> res

let __dypgen_merge_list = [(fun l -> (
  let f1 (o,gd,ld) = match o with Lexeme_matched ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Lexeme_matched"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Lexeme_matched l in
  let f2 o = Lexeme_matched o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ID ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ID"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ID l in
  let f2 o = Obj_ID o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_NUM ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_NUM"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_NUM l in
  let f2 o = Obj_NUM o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_STRING ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_STRING"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_STRING l in
  let f2 o = Obj_STRING o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_TYPEID ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_TYPEID"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_TYPEID l in
  let f2 o = Obj_TYPEID o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ampamp_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ampamp_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ampamp_op l in
  let f2 o = Obj_ampamp_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_any_id_str ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_any_id_str"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_any_id_str l in
  let f2 o = Obj_any_id_str o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_any_or_var_pat ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_any_or_var_pat"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_any_or_var_pat l in
  let f2 o = Obj_any_or_var_pat o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_app_arg_exprs ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_app_arg_exprs"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_app_arg_exprs l in
  let f2 o = Obj_app_arg_exprs o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_app_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_app_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_app_expr l in
  let f2 o = Obj_app_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_array_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_array_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_array_expr l in
  let f2 o = Obj_array_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_array_get ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_array_get"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_array_get l in
  let f2 o = Obj_array_get o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_array_set ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_array_set"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_array_set l in
  let f2 o = Obj_array_set o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_arrow ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_arrow"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_arrow l in
  let f2 o = Obj_arrow o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_assign_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_assign_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_assign_expr l in
  let f2 o = Obj_assign_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_binop_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_binop_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_binop_expr l in
  let f2 o = Obj_binop_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_block ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_block"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_block l in
  let f2 o = Obj_block o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_block_body ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_block_body"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_block_body l in
  let f2 o = Obj_block_body o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_block_body_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_block_body_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_block_body_expr l in
  let f2 o = Obj_block_body_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_block_body_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_block_body_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_block_body_stmt l in
  let f2 o = Obj_block_body_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_block_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_block_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_block_expr l in
  let f2 o = Obj_block_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_block_or_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_block_or_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_block_or_expr l in
  let f2 o = Obj_block_or_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_colon ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_colon"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_colon l in
  let f2 o = Obj_colon o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_comma ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_comma"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_comma l in
  let f2 o = Obj_comma o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_const ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_const"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_const l in
  let f2 o = Obj_const o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dash_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dash_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dash_op l in
  let f2 o = Obj_dash_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dasheq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dasheq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dasheq_op l in
  let f2 o = Obj_dasheq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_data_constructor ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_data_constructor"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_data_constructor l in
  let f2 o = Obj_data_constructor o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_data_constructors ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_data_constructors"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_data_constructors l in
  let f2 o = Obj_data_constructors o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_data_declaration ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_data_declaration"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_data_declaration l in
  let f2 o = Obj_data_declaration o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_data_label ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_data_label"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_data_label l in
  let f2 o = Obj_data_label o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_data_labels ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_data_labels"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_data_labels l in
  let f2 o = Obj_data_labels o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_data_typ ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_data_typ"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_data_typ l in
  let f2 o = Obj_data_typ o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dot ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dot"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dot l in
  let f2 o = Obj_dot o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_0 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_0"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_0 l in
  let f2 o = Obj_dypgen__nested_nt_0 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_1 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_1"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_1 l in
  let f2 o = Obj_dypgen__nested_nt_1 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_10 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_10"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_10 l in
  let f2 o = Obj_dypgen__nested_nt_10 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_100 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_100"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_100 l in
  let f2 o = Obj_dypgen__nested_nt_100 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_101 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_101"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_101 l in
  let f2 o = Obj_dypgen__nested_nt_101 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_102 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_102"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_102 l in
  let f2 o = Obj_dypgen__nested_nt_102 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_103 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_103"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_103 l in
  let f2 o = Obj_dypgen__nested_nt_103 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_104 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_104"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_104 l in
  let f2 o = Obj_dypgen__nested_nt_104 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_105 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_105"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_105 l in
  let f2 o = Obj_dypgen__nested_nt_105 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_106 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_106"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_106 l in
  let f2 o = Obj_dypgen__nested_nt_106 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_107 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_107"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_107 l in
  let f2 o = Obj_dypgen__nested_nt_107 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_108 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_108"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_108 l in
  let f2 o = Obj_dypgen__nested_nt_108 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_109 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_109"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_109 l in
  let f2 o = Obj_dypgen__nested_nt_109 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_11 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_11"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_11 l in
  let f2 o = Obj_dypgen__nested_nt_11 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_110 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_110"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_110 l in
  let f2 o = Obj_dypgen__nested_nt_110 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_111 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_111"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_111 l in
  let f2 o = Obj_dypgen__nested_nt_111 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_12 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_12"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_12 l in
  let f2 o = Obj_dypgen__nested_nt_12 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_13 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_13"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_13 l in
  let f2 o = Obj_dypgen__nested_nt_13 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_14 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_14"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_14 l in
  let f2 o = Obj_dypgen__nested_nt_14 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_15 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_15"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_15 l in
  let f2 o = Obj_dypgen__nested_nt_15 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_16 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_16"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_16 l in
  let f2 o = Obj_dypgen__nested_nt_16 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_17 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_17"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_17 l in
  let f2 o = Obj_dypgen__nested_nt_17 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_18 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_18"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_18 l in
  let f2 o = Obj_dypgen__nested_nt_18 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_19 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_19"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_19 l in
  let f2 o = Obj_dypgen__nested_nt_19 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_2 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_2"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_2 l in
  let f2 o = Obj_dypgen__nested_nt_2 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_20 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_20"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_20 l in
  let f2 o = Obj_dypgen__nested_nt_20 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_21 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_21"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_21 l in
  let f2 o = Obj_dypgen__nested_nt_21 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_22 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_22"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_22 l in
  let f2 o = Obj_dypgen__nested_nt_22 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_23 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_23"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_23 l in
  let f2 o = Obj_dypgen__nested_nt_23 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_24 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_24"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_24 l in
  let f2 o = Obj_dypgen__nested_nt_24 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_25 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_25"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_25 l in
  let f2 o = Obj_dypgen__nested_nt_25 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_26 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_26"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_26 l in
  let f2 o = Obj_dypgen__nested_nt_26 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_27 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_27"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_27 l in
  let f2 o = Obj_dypgen__nested_nt_27 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_28 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_28"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_28 l in
  let f2 o = Obj_dypgen__nested_nt_28 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_29 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_29"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_29 l in
  let f2 o = Obj_dypgen__nested_nt_29 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_3 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_3"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_3 l in
  let f2 o = Obj_dypgen__nested_nt_3 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_30 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_30"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_30 l in
  let f2 o = Obj_dypgen__nested_nt_30 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_31 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_31"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_31 l in
  let f2 o = Obj_dypgen__nested_nt_31 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_32 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_32"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_32 l in
  let f2 o = Obj_dypgen__nested_nt_32 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_33 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_33"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_33 l in
  let f2 o = Obj_dypgen__nested_nt_33 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_34 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_34"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_34 l in
  let f2 o = Obj_dypgen__nested_nt_34 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_35 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_35"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_35 l in
  let f2 o = Obj_dypgen__nested_nt_35 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_36 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_36"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_36 l in
  let f2 o = Obj_dypgen__nested_nt_36 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_37 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_37"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_37 l in
  let f2 o = Obj_dypgen__nested_nt_37 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_38 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_38"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_38 l in
  let f2 o = Obj_dypgen__nested_nt_38 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_39 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_39"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_39 l in
  let f2 o = Obj_dypgen__nested_nt_39 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_4 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_4"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_4 l in
  let f2 o = Obj_dypgen__nested_nt_4 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_40 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_40"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_40 l in
  let f2 o = Obj_dypgen__nested_nt_40 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_41 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_41"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_41 l in
  let f2 o = Obj_dypgen__nested_nt_41 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_42 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_42"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_42 l in
  let f2 o = Obj_dypgen__nested_nt_42 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_43 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_43"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_43 l in
  let f2 o = Obj_dypgen__nested_nt_43 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_44 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_44"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_44 l in
  let f2 o = Obj_dypgen__nested_nt_44 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_45 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_45"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_45 l in
  let f2 o = Obj_dypgen__nested_nt_45 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_46 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_46"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_46 l in
  let f2 o = Obj_dypgen__nested_nt_46 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_47 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_47"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_47 l in
  let f2 o = Obj_dypgen__nested_nt_47 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_48 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_48"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_48 l in
  let f2 o = Obj_dypgen__nested_nt_48 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_49 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_49"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_49 l in
  let f2 o = Obj_dypgen__nested_nt_49 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_5 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_5"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_5 l in
  let f2 o = Obj_dypgen__nested_nt_5 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_50 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_50"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_50 l in
  let f2 o = Obj_dypgen__nested_nt_50 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_51 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_51"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_51 l in
  let f2 o = Obj_dypgen__nested_nt_51 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_52 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_52"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_52 l in
  let f2 o = Obj_dypgen__nested_nt_52 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_53 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_53"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_53 l in
  let f2 o = Obj_dypgen__nested_nt_53 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_54 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_54"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_54 l in
  let f2 o = Obj_dypgen__nested_nt_54 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_55 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_55"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_55 l in
  let f2 o = Obj_dypgen__nested_nt_55 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_56 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_56"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_56 l in
  let f2 o = Obj_dypgen__nested_nt_56 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_57 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_57"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_57 l in
  let f2 o = Obj_dypgen__nested_nt_57 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_58 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_58"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_58 l in
  let f2 o = Obj_dypgen__nested_nt_58 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_59 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_59"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_59 l in
  let f2 o = Obj_dypgen__nested_nt_59 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_6 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_6"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_6 l in
  let f2 o = Obj_dypgen__nested_nt_6 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_60 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_60"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_60 l in
  let f2 o = Obj_dypgen__nested_nt_60 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_61 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_61"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_61 l in
  let f2 o = Obj_dypgen__nested_nt_61 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_62 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_62"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_62 l in
  let f2 o = Obj_dypgen__nested_nt_62 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_63 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_63"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_63 l in
  let f2 o = Obj_dypgen__nested_nt_63 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_64 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_64"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_64 l in
  let f2 o = Obj_dypgen__nested_nt_64 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_65 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_65"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_65 l in
  let f2 o = Obj_dypgen__nested_nt_65 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_66 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_66"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_66 l in
  let f2 o = Obj_dypgen__nested_nt_66 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_67 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_67"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_67 l in
  let f2 o = Obj_dypgen__nested_nt_67 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_68 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_68"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_68 l in
  let f2 o = Obj_dypgen__nested_nt_68 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_69 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_69"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_69 l in
  let f2 o = Obj_dypgen__nested_nt_69 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_7 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_7"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_7 l in
  let f2 o = Obj_dypgen__nested_nt_7 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_70 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_70"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_70 l in
  let f2 o = Obj_dypgen__nested_nt_70 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_71 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_71"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_71 l in
  let f2 o = Obj_dypgen__nested_nt_71 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_72 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_72"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_72 l in
  let f2 o = Obj_dypgen__nested_nt_72 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_73 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_73"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_73 l in
  let f2 o = Obj_dypgen__nested_nt_73 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_74 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_74"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_74 l in
  let f2 o = Obj_dypgen__nested_nt_74 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_75 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_75"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_75 l in
  let f2 o = Obj_dypgen__nested_nt_75 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_76 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_76"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_76 l in
  let f2 o = Obj_dypgen__nested_nt_76 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_77 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_77"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_77 l in
  let f2 o = Obj_dypgen__nested_nt_77 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_78 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_78"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_78 l in
  let f2 o = Obj_dypgen__nested_nt_78 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_79 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_79"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_79 l in
  let f2 o = Obj_dypgen__nested_nt_79 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_8 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_8"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_8 l in
  let f2 o = Obj_dypgen__nested_nt_8 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_80 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_80"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_80 l in
  let f2 o = Obj_dypgen__nested_nt_80 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_81 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_81"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_81 l in
  let f2 o = Obj_dypgen__nested_nt_81 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_82 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_82"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_82 l in
  let f2 o = Obj_dypgen__nested_nt_82 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_83 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_83"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_83 l in
  let f2 o = Obj_dypgen__nested_nt_83 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_84 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_84"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_84 l in
  let f2 o = Obj_dypgen__nested_nt_84 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_85 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_85"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_85 l in
  let f2 o = Obj_dypgen__nested_nt_85 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_86 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_86"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_86 l in
  let f2 o = Obj_dypgen__nested_nt_86 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_87 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_87"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_87 l in
  let f2 o = Obj_dypgen__nested_nt_87 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_88 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_88"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_88 l in
  let f2 o = Obj_dypgen__nested_nt_88 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_89 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_89"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_89 l in
  let f2 o = Obj_dypgen__nested_nt_89 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_9 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_9"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_9 l in
  let f2 o = Obj_dypgen__nested_nt_9 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_90 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_90"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_90 l in
  let f2 o = Obj_dypgen__nested_nt_90 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_91 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_91"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_91 l in
  let f2 o = Obj_dypgen__nested_nt_91 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_92 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_92"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_92 l in
  let f2 o = Obj_dypgen__nested_nt_92 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_93 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_93"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_93 l in
  let f2 o = Obj_dypgen__nested_nt_93 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_94 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_94"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_94 l in
  let f2 o = Obj_dypgen__nested_nt_94 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_95 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_95"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_95 l in
  let f2 o = Obj_dypgen__nested_nt_95 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_96 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_96"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_96 l in
  let f2 o = Obj_dypgen__nested_nt_96 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_97 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_97"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_97 l in
  let f2 o = Obj_dypgen__nested_nt_97 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_98 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_98"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_98 l in
  let f2 o = Obj_dypgen__nested_nt_98 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_99 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_99"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_99 l in
  let f2 o = Obj_dypgen__nested_nt_99 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_EOL ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_EOL"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_EOL l in
  let f2 o = Obj_dypgen__option_EOL o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_eos ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_eos"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_eos l in
  let f2 o = Obj_dypgen__option_eos o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_patterns ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_patterns"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_patterns l in
  let f2 o = Obj_dypgen__option_patterns o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_pipe ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_pipe"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_pipe l in
  let f2 o = Obj_dypgen__option_pipe o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__star___block_body_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__star___block_body_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__star___block_body_stmt l in
  let f2 o = Obj_dypgen__star___block_body_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_eos ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_eos"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_eos l in
  let f2 o = Obj_eos o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_eqeq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_eqeq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_eqeq_op l in
  let f2 o = Obj_eqeq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_equal ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_equal"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_equal l in
  let f2 o = Obj_equal o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_export_id_str ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_export_id_str"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_export_id_str l in
  let f2 o = Obj_export_id_str o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_export_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_export_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_export_stmt l in
  let f2 o = Obj_export_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_expr l in
  let f2 o = Obj_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ext_constructor ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ext_constructor"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ext_constructor l in
  let f2 o = Obj_ext_constructor o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_file_path ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_file_path"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_file_path l in
  let f2 o = Obj_file_path o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_foreign_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_foreign_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_foreign_stmt l in
  let f2 o = Obj_foreign_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_greatereq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_greatereq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_greatereq_op l in
  let f2 o = Obj_greatereq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_id ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_id"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_id l in
  let f2 o = Obj_id o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_id_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_id_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_id_expr l in
  let f2 o = Obj_id_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_id_str ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_id_str"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_id_str l in
  let f2 o = Obj_id_str o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_if_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_if_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_if_expr l in
  let f2 o = Obj_if_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_import_shape ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_import_shape"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_import_shape l in
  let f2 o = Obj_import_shape o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_import_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_import_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_import_stmt l in
  let f2 o = Obj_import_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_infix ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_infix"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_infix l in
  let f2 o = Obj_infix o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_infix_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_infix_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_infix_op l in
  let f2 o = Obj_infix_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lam_args ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lam_args"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lam_args l in
  let f2 o = Obj_lam_args o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lam_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lam_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lam_expr l in
  let f2 o = Obj_lam_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lbrace ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lbrace"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lbrace l in
  let f2 o = Obj_lbrace o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lbrack ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lbrack"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lbrack l in
  let f2 o = Obj_lbrack o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lcaret ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lcaret"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lcaret l in
  let f2 o = Obj_lcaret o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lcaret_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lcaret_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lcaret_op l in
  let f2 o = Obj_lcaret_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lesseq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lesseq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lesseq_op l in
  let f2 o = Obj_lesseq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_let_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_let_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_let_expr l in
  let f2 o = Obj_let_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_list_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_list_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_list_expr l in
  let f2 o = Obj_list_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lparen ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lparen"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lparen l in
  let f2 o = Obj_lparen o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_match_branch ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_match_branch"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_match_branch l in
  let f2 o = Obj_match_branch o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_match_branches ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_match_branches"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_match_branches l in
  let f2 o = Obj_match_branches o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_match_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_match_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_match_expr l in
  let f2 o = Obj_match_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_non_assign_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_non_assign_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_non_assign_expr l in
  let f2 o = Obj_non_assign_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_non_binop_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_non_binop_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_non_binop_expr l in
  let f2 o = Obj_non_binop_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_paren_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_paren_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_paren_expr l in
  let f2 o = Obj_paren_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_pattern ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_pattern"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_pattern l in
  let f2 o = Obj_pattern o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_patterns ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_patterns"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_patterns l in
  let f2 o = Obj_patterns o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_percent_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_percent_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_percent_op l in
  let f2 o = Obj_percent_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_pipe ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_pipe"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_pipe l in
  let f2 o = Obj_pipe o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_pipepipe_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_pipepipe_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_pipepipe_op l in
  let f2 o = Obj_pipepipe_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_plus_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_plus_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_plus_op l in
  let f2 o = Obj_plus_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_pluseq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_pluseq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_pluseq_op l in
  let f2 o = Obj_pluseq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_prefix ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_prefix"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_prefix l in
  let f2 o = Obj_prefix o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_prim1_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_prim1_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_prim1_expr l in
  let f2 o = Obj_prim1_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_primitive ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_primitive"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_primitive l in
  let f2 o = Obj_primitive o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_primitive_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_primitive_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_primitive_stmt l in
  let f2 o = Obj_primitive_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_program ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_program"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_program l in
  let f2 o = Obj_program o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rbrace ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rbrace"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rbrace l in
  let f2 o = Obj_rbrace o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rbrack ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rbrack"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rbrack l in
  let f2 o = Obj_rbrack o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rcaret ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rcaret"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rcaret l in
  let f2 o = Obj_rcaret o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rcaret_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rcaret_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rcaret_op l in
  let f2 o = Obj_rcaret_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_record_exprs ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_record_exprs"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_record_exprs l in
  let f2 o = Obj_record_exprs o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_record_field ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_record_field"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_record_field l in
  let f2 o = Obj_record_field o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_record_get ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_record_get"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_record_get l in
  let f2 o = Obj_record_get o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_record_pattern ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_record_pattern"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_record_pattern l in
  let f2 o = Obj_record_pattern o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_record_patterns ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_record_patterns"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_record_patterns l in
  let f2 o = Obj_record_patterns o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_record_pun ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_record_pun"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_record_pun l in
  let f2 o = Obj_record_pun o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rparen ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rparen"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rparen l in
  let f2 o = Obj_rparen o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_simple_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_simple_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_simple_expr l in
  let f2 o = Obj_simple_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_simple_id ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_simple_id"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_simple_id l in
  let f2 o = Obj_simple_id o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_slash_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_slash_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_slash_op l in
  let f2 o = Obj_slash_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_slasheq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_slasheq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_slasheq_op l in
  let f2 o = Obj_slasheq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_star_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_star_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_star_op l in
  let f2 o = Obj_star_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_stareq_op ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_stareq_op"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_stareq_op l in
  let f2 o = Obj_stareq_op o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_stmt_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_stmt_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_stmt_expr l in
  let f2 o = Obj_stmt_expr o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_thickarrow ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_thickarrow"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_thickarrow l in
  let f2 o = Obj_thickarrow o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_toplevel_stmt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_toplevel_stmt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_toplevel_stmt l in
  let f2 o = Obj_toplevel_stmt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_toplevel_stmts ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_toplevel_stmts"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_toplevel_stmts l in
  let f2 o = Obj_toplevel_stmts o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_tuple_exprs ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_tuple_exprs"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_tuple_exprs l in
  let f2 o = Obj_tuple_exprs o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_tuple_patterns ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_tuple_patterns"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_tuple_patterns l in
  let f2 o = Obj_tuple_patterns o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_tuple_typs ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_tuple_typs"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_tuple_typs l in
  let f2 o = Obj_tuple_typs o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_typ ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_typ"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_typ l in
  let f2 o = Obj_typ o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_type_id ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_type_id"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_type_id l in
  let f2 o = Obj_type_id o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_type_id_str ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_type_id_str"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_type_id_str l in
  let f2 o = Obj_type_id_str o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_typs ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_typs"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_typs l in
  let f2 o = Obj_typs o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_value_bind ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_value_bind"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_value_bind l in
  let f2 o = Obj_value_bind o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_value_binds ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_value_binds"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_value_binds l in
  let f2 o = Obj_value_binds o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_while_expr ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_while_expr"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_while_expr l in
  let f2 o = Obj_while_expr o in
  (List.map f2 ol, gd, ld)))]



let __dypgen_test_cons () =  [|
  (fun x -> match x with Lexeme_matched _ -> true | _ -> false);
  (fun x -> match x with Obj_ID _ -> true | _ -> false);
  (fun x -> match x with Obj_NUM _ -> true | _ -> false);
  (fun x -> match x with Obj_STRING _ -> true | _ -> false);
  (fun x -> match x with Obj_TYPEID _ -> true | _ -> false);
  (fun x -> match x with Obj_ampamp_op _ -> true | _ -> false);
  (fun x -> match x with Obj_any_id_str _ -> true | _ -> false);
  (fun x -> match x with Obj_any_or_var_pat _ -> true | _ -> false);
  (fun x -> match x with Obj_app_arg_exprs _ -> true | _ -> false);
  (fun x -> match x with Obj_app_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_array_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_array_get _ -> true | _ -> false);
  (fun x -> match x with Obj_array_set _ -> true | _ -> false);
  (fun x -> match x with Obj_arrow _ -> true | _ -> false);
  (fun x -> match x with Obj_assign_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_binop_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_block _ -> true | _ -> false);
  (fun x -> match x with Obj_block_body _ -> true | _ -> false);
  (fun x -> match x with Obj_block_body_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_block_body_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_block_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_block_or_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_colon _ -> true | _ -> false);
  (fun x -> match x with Obj_comma _ -> true | _ -> false);
  (fun x -> match x with Obj_const _ -> true | _ -> false);
  (fun x -> match x with Obj_dash_op _ -> true | _ -> false);
  (fun x -> match x with Obj_dasheq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_data_constructor _ -> true | _ -> false);
  (fun x -> match x with Obj_data_constructors _ -> true | _ -> false);
  (fun x -> match x with Obj_data_declaration _ -> true | _ -> false);
  (fun x -> match x with Obj_data_label _ -> true | _ -> false);
  (fun x -> match x with Obj_data_labels _ -> true | _ -> false);
  (fun x -> match x with Obj_data_typ _ -> true | _ -> false);
  (fun x -> match x with Obj_dot _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_0 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_1 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_10 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_100 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_101 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_102 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_103 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_104 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_105 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_106 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_107 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_108 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_109 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_11 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_110 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_111 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_12 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_13 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_14 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_15 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_16 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_17 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_18 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_19 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_2 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_20 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_21 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_22 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_23 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_24 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_25 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_26 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_27 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_28 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_29 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_3 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_30 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_31 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_32 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_33 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_34 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_35 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_36 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_37 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_38 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_39 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_4 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_40 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_41 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_42 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_43 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_44 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_45 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_46 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_47 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_48 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_49 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_5 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_50 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_51 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_52 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_53 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_54 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_55 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_56 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_57 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_58 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_59 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_6 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_60 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_61 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_62 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_63 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_64 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_65 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_66 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_67 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_68 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_69 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_7 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_70 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_71 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_72 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_73 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_74 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_75 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_76 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_77 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_78 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_79 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_8 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_80 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_81 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_82 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_83 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_84 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_85 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_86 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_87 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_88 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_89 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_9 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_90 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_91 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_92 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_93 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_94 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_95 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_96 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_97 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_98 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_99 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_EOL _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_eos _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_patterns _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_pipe _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__star___block_body_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_eos _ -> true | _ -> false);
  (fun x -> match x with Obj_eqeq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_equal _ -> true | _ -> false);
  (fun x -> match x with Obj_export_id_str _ -> true | _ -> false);
  (fun x -> match x with Obj_export_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_ext_constructor _ -> true | _ -> false);
  (fun x -> match x with Obj_file_path _ -> true | _ -> false);
  (fun x -> match x with Obj_foreign_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_greatereq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_id _ -> true | _ -> false);
  (fun x -> match x with Obj_id_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_id_str _ -> true | _ -> false);
  (fun x -> match x with Obj_if_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_import_shape _ -> true | _ -> false);
  (fun x -> match x with Obj_import_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_infix _ -> true | _ -> false);
  (fun x -> match x with Obj_infix_op _ -> true | _ -> false);
  (fun x -> match x with Obj_lam_args _ -> true | _ -> false);
  (fun x -> match x with Obj_lam_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_lbrace _ -> true | _ -> false);
  (fun x -> match x with Obj_lbrack _ -> true | _ -> false);
  (fun x -> match x with Obj_lcaret _ -> true | _ -> false);
  (fun x -> match x with Obj_lcaret_op _ -> true | _ -> false);
  (fun x -> match x with Obj_lesseq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_let_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_list_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_lparen _ -> true | _ -> false);
  (fun x -> match x with Obj_match_branch _ -> true | _ -> false);
  (fun x -> match x with Obj_match_branches _ -> true | _ -> false);
  (fun x -> match x with Obj_match_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_non_assign_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_non_binop_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_paren_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_pattern _ -> true | _ -> false);
  (fun x -> match x with Obj_patterns _ -> true | _ -> false);
  (fun x -> match x with Obj_percent_op _ -> true | _ -> false);
  (fun x -> match x with Obj_pipe _ -> true | _ -> false);
  (fun x -> match x with Obj_pipepipe_op _ -> true | _ -> false);
  (fun x -> match x with Obj_plus_op _ -> true | _ -> false);
  (fun x -> match x with Obj_pluseq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_prefix _ -> true | _ -> false);
  (fun x -> match x with Obj_prim1_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_primitive _ -> true | _ -> false);
  (fun x -> match x with Obj_primitive_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_program _ -> true | _ -> false);
  (fun x -> match x with Obj_rbrace _ -> true | _ -> false);
  (fun x -> match x with Obj_rbrack _ -> true | _ -> false);
  (fun x -> match x with Obj_rcaret _ -> true | _ -> false);
  (fun x -> match x with Obj_rcaret_op _ -> true | _ -> false);
  (fun x -> match x with Obj_record_exprs _ -> true | _ -> false);
  (fun x -> match x with Obj_record_field _ -> true | _ -> false);
  (fun x -> match x with Obj_record_get _ -> true | _ -> false);
  (fun x -> match x with Obj_record_pattern _ -> true | _ -> false);
  (fun x -> match x with Obj_record_patterns _ -> true | _ -> false);
  (fun x -> match x with Obj_record_pun _ -> true | _ -> false);
  (fun x -> match x with Obj_rparen _ -> true | _ -> false);
  (fun x -> match x with Obj_simple_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_simple_id _ -> true | _ -> false);
  (fun x -> match x with Obj_slash_op _ -> true | _ -> false);
  (fun x -> match x with Obj_slasheq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_star_op _ -> true | _ -> false);
  (fun x -> match x with Obj_stareq_op _ -> true | _ -> false);
  (fun x -> match x with Obj_stmt_expr _ -> true | _ -> false);
  (fun x -> match x with Obj_thickarrow _ -> true | _ -> false);
  (fun x -> match x with Obj_toplevel_stmt _ -> true | _ -> false);
  (fun x -> match x with Obj_toplevel_stmts _ -> true | _ -> false);
  (fun x -> match x with Obj_tuple_exprs _ -> true | _ -> false);
  (fun x -> match x with Obj_tuple_patterns _ -> true | _ -> false);
  (fun x -> match x with Obj_tuple_typs _ -> true | _ -> false);
  (fun x -> match x with Obj_typ _ -> true | _ -> false);
  (fun x -> match x with Obj_type_id _ -> true | _ -> false);
  (fun x -> match x with Obj_type_id_str _ -> true | _ -> false);
  (fun x -> match x with Obj_typs _ -> true | _ -> false);
  (fun x -> match x with Obj_value_bind _ -> true | _ -> false);
  (fun x -> match x with Obj_value_binds _ -> true | _ -> false);
  (fun x -> match x with Obj_while_expr _ -> true | _ -> false)|]

let __dypgen_dummy_marker_2 = ()
let pp () = Dyp.make_parser
  __dypgen_ra_list Dyp_priority_data.relations global_data local_data
  (Dyp.Tools.make_nt_cons_map Dyp_symbols_array.nt_cons_list)
  Dyp_symbols_array.entry_points
  
  false 58 true
  
  Dyp_aux_functions.get_token_value
  Dyp_symbols.get_token_name Dyp_symbols.str_token
  global_data_equal local_data_equal (__dypgen_test_cons ())
  Dyp_symbols_array.str_cons
  Dyp_symbols_array.cons_array Dyp_aux_functions.cons_table
  (Dyp.Tools.array_of_list __dypgen_merge_list)
  dypgen_lexbuf_position __dypgen_regexp_decl __dypgen_main_lexer
  __dypgen_aux_lexer Dyp_symbols.ter_string_list
  (fun lexbuf -> Lexeme_matched (Dyp.lexeme lexbuf))
  false


let __dypgen_dummy_marker_5 = ()

let __dypgen_dummy_marker_3 = ()

let program ?(global_data=global_data) ?(local_data=local_data) f lexbuf =
  let pf = Dyp.parse (pp ()) "program" ~global_data:global_data
    ~local_data:local_data ~match_len:dypgen_match_length
    ~keep_data:dypgen_keep_data
    ~use_rule_order:dypgen_use_rule_order
    ~use_all_actions:dypgen_use_all_actions
    ~lexpos:dypgen_lexbuf_position f lexbuf in
  let aux1 (o,p) = match o with
    | Obj_program r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf


let __dypgen_dummy_marker_4 = ()


# 603 "parser.dyp"

let parse_program t lexbuf =
  Dyp.dypgen_verbose := !Grain_utils.Config.parser_debug_level;
  first_loc := Location.curr lexbuf;
  with_default_loc_src (fun() -> !last_loc) (fun() -> program t lexbuf)

let print_syntax_error =
  let open Printf in
  let open Location in
  function
  | Syntax_error -> begin
      debug_print_state();
      Some(errorf ~loc:(!last_loc) "Syntax error")
    end
  | _ -> None

let () =
  Dyp.dypgen_verbose := !Grain_utils.Config.parser_debug_level;
  Location.register_error_of_exn print_syntax_error

let _ = () (* dummy line to improve OCaml error location *)
# 10531              "parser.ml"
