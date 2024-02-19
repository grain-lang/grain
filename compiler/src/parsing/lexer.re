open Lexing;
open Parser;
open Parser_header;
open Printf;

let lexbuf_loc = lexbuf => {
  let (loc_start, loc_end) = Sedlexing.lexing_positions(lexbuf);
  Location.{loc_start, loc_end, loc_ghost: false};
};

type error =
  | UnrecognizedToken
  | UnclosedString(int)
  | UnclosedBytes(int)
  | UnclosedChar(int)
  | UnclosedBlockComment(int)
  | UnclosedDocComment(int)
  | FloatWithoutLeadingZero(string);

exception Error(Location.t, error);

let report_error = (ppf, err) =>
  switch (err) {
  | UnrecognizedToken =>
    Format.fprintf(ppf, "The Grain lexer doesn't recognize this token.")
  | UnclosedString(line) =>
    Format.fprintf(ppf, "Unclosed string literal, opened on line %d", line)
  | UnclosedBytes(line) =>
    Format.fprintf(ppf, "Unclosed byte literal, opened on line %d", line)
  | UnclosedChar(line) =>
    Format.fprintf(ppf, "Unclosed character literal, opened on line %d", line)
  | UnclosedBlockComment(line) =>
    Format.fprintf(ppf, "Unclosed block comment, opened on line %d", line)
  | UnclosedDocComment(line) =>
    Format.fprintf(ppf, "Unclosed doc comment, opened on line %d", line)
  | FloatWithoutLeadingZero(f) =>
    Format.fprintf(
      ppf,
      "Floats must contain a leading zero. Use 0%s instead.",
      f,
    )
  };

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );

let comments = ref([]);

let reset = () => {
  comments := [];
};

let consume_comments = () => {
  let out_comments = comments^;
  List.rev(out_comments);
};

let collect_comment = (comment_type, source, loc, lexbuf) => {
  comments := [comment_type(source, loc), ...comments^];
};

// Grain follows the Unicode properties for programming languages outlined in
// https://unicode.org/reports/tr31/#Pattern_Syntax

let dec_digit = [%sedlex.regexp? '0' .. '9'];
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'];
let oct_digit = [%sedlex.regexp? '0' .. '7'];
let bin_digit = [%sedlex.regexp? '0' .. '1'];

let dec_int = [%sedlex.regexp? (dec_digit, Star(dec_digit | '_'))];
let hex_int = [%sedlex.regexp?
  ('0', 'x' | 'X', hex_digit, Star(hex_digit | '_'))
];
let oct_int = [%sedlex.regexp?
  ('0', 'o' | 'O', oct_digit, Star(oct_digit | '_'))
];
let bin_int = [%sedlex.regexp?
  ('0', 'b' | 'B', bin_digit, Star(bin_digit | '_'))
];

let unsigned_int = [%sedlex.regexp? dec_int | hex_int | oct_int | bin_int];

let dec_float_exp = [%sedlex.regexp?
  ('e' | 'E', Opt('+' | '-'), dec_digit, Star(dec_digit | '_'))
];
let hex_float_exp = [%sedlex.regexp?
  ('p' | 'P', Opt('+' | '-'), dec_digit, Star(dec_digit | '_'))
];

let hex_float_decimal = [%sedlex.regexp? ('.', hex_digit, Star(hex_digit))];

let hex_float_integral = [%sedlex.regexp? hex_int];

let dec_float_decimal = [%sedlex.regexp?
  ('.', dec_digit, Star(dec_digit | '_'))
];
let dec_float_integral = [%sedlex.regexp?
  (dec_digit, Star(dec_digit | '_'))
];
let dec_float_alphabetic = [%sedlex.regexp? "Infinity" | "NaN"];

let dec_float = [%sedlex.regexp?
  (hex_float_integral, hex_float_decimal, hex_float_exp) |
  (hex_int, hex_float_exp) |
  (dec_float_integral, dec_float_decimal, Opt(dec_float_exp)) |
  (dec_float_integral, dec_float_exp) |
  dec_float_alphabetic
];

let unsigned_float = [%sedlex.regexp? dec_float];
let invalid_float = [%sedlex.regexp?
  (dec_float_decimal, Opt(dec_float_exp))
];

let uident = [%sedlex.regexp?
  (Intersect(xid_start, lu), Star(xid_continue))
];
let lident = [%sedlex.regexp?
  (Sub(xid_start, lu) | '_', Star(xid_continue))
];

let operator_char = [%sedlex.regexp?
  '$' | '&' | '*' | '/' | '+' | '-' | '=' | '>' | '<' | '^' | '|' | '!' | '?' |
  '%' |
  ':' |
  '.'
];
let operator_chars = [%sedlex.regexp? Star(operator_char)];

// We make sure that `>` operators contain at least one non-`>` char to not
// confuse them for `>>>` chains at the end of types, e.g. `List<Option<Box<a>>>`
let rcaret_operator_char = [%sedlex.regexp? Sub(operator_char, '>')];
let rcaret_operator_chars = [%sedlex.regexp?
  (operator_chars, rcaret_operator_char, operator_chars)
];

// Similarly, we do this for `<` even though it's a simpler case
let lcaret_operator_char = [%sedlex.regexp? Sub(operator_char, '<')];
let lcaret_operator_chars = [%sedlex.regexp?
  (operator_chars, lcaret_operator_char, operator_chars)
];

// Infix operators are not allowed to start with `//` or `/*` as they
// indicate comments
let slash_operator_chars = [%sedlex.regexp?
  (Sub(operator_char, '/' | '*'), operator_chars)
];

let unicode_esc = [%sedlex.regexp? ("\\u{", Rep(hex_digit, 1 .. 6), "}")];
let unicode4_esc = [%sedlex.regexp? ("\\u", Rep(hex_digit, 4))];
let hex_esc = [%sedlex.regexp? ("\\x", Rep(hex_digit, 1 .. 2))];
let oct_esc = [%sedlex.regexp? ("\\", Rep(oct_digit, 1 .. 3))];
let num_esc = [%sedlex.regexp? unicode_esc | unicode4_esc | hex_esc | oct_esc];

// Whitespace follows Pattern_White_Space, though we separate spaces from newlines
// https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=[:Pattern_White_Space=Yes:]

// HORIZONTAL TABULATION
// VERTICAL TABULATION
// SPACE
// LEFT-TO-RIGHT MARK
// RIGHT-TO-LEFT MARK
let blank = [%sedlex.regexp? Plus(0x09 | 0x0B | 0x20 | 0x200E | 0x200F)];

// LINE FEED
// FORM FEED
// CARRIAGE RETURN
// NEXT LINE
// LINE SEPARATOR
// PARAGRAPH SEPARATOR
let newline_char = [%sedlex.regexp?
  0x0A | 0x0C | 0x0D | 0x85 | 0x2028 | 0x2029
];
let newlines = [%sedlex.regexp? (Star(newline_char | blank), newline_char)];

let line_comment = [%sedlex.regexp? ("//", Star(Compl(newline_char)))];
let shebang_comment = [%sedlex.regexp? ("#!", Star(Compl(newline_char)))];

let with_position = (lexbuf, token) => {
  let (start_p, end_p) = Sedlexing.lexing_positions(lexbuf);
  (token, start_p, end_p);
};

let rec token = lexbuf => {
  let positioned = with_position(lexbuf);
  switch%sedlex (lexbuf) {
  | line_comment =>
    let source = Sedlexing.Utf8.lexeme(lexbuf);
    let loc = Location.curr(lexbuf);
    collect_comment(make_line_comment, source, loc, lexbuf);
    positioned(EOL);
  | shebang_comment =>
    let source = Sedlexing.Utf8.lexeme(lexbuf);
    let loc = Location.curr(lexbuf);
    collect_comment(make_shebang_comment, source, loc, lexbuf);
    positioned(EOL);
  | "/*" =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    let buf = Buffer.create(128);
    Buffer.add_string(buf, "/*");
    read_block_comment(start_p, buf, lexbuf);
  | "/**" =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    let buf = Buffer.create(128);
    Buffer.add_string(buf, "/**");
    read_doc_comment(start_p, buf, lexbuf);
  | blank => token(lexbuf)
  | newlines => positioned(EOL)
  | (unsigned_float, 'f') =>
    positioned(FLOAT32(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_float, 'd') =>
    positioned(FLOAT64(Sedlexing.Utf8.lexeme(lexbuf)))
  | unsigned_float =>
    positioned(NUMBER_FLOAT(Sedlexing.Utf8.lexeme(lexbuf)))
  | (invalid_float, Opt('f' | 'd' | 'w' | 'W')) =>
    raise(
      Error(
        lexbuf_loc(lexbuf),
        FloatWithoutLeadingZero(Sedlexing.Utf8.lexeme(lexbuf)),
      ),
    )
  | (unsigned_int, 's') => positioned(INT8(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 'S') => positioned(INT16(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 'l') => positioned(INT32(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 'L') => positioned(INT64(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, "us") =>
    positioned(UINT8(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, "uS") =>
    positioned(UINT16(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, "ul") =>
    positioned(UINT32(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, "uL") =>
    positioned(UINT64(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 'n') =>
    positioned(WASMI32(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 'N') =>
    positioned(WASMI64(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_float, 'w') =>
    positioned(WASMF32(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_float, 'W') =>
    positioned(WASMF64(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 't') =>
    positioned(BIGINT(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, '/', Opt('-'), unsigned_int, 'r') =>
    positioned(RATIONAL(Sedlexing.Utf8.lexeme(lexbuf)))
  | unsigned_int => positioned(NUMBER_INT(Sedlexing.Utf8.lexeme(lexbuf)))
  | "primitive" => positioned(PRIMITIVE)
  | "foreign" => positioned(FOREIGN)
  | "wasm" => positioned(WASM)
  | "while" => positioned(WHILE)
  | "for" => positioned(FOR)
  | "continue" => positioned(CONTINUE)
  | "break" => positioned(BREAK)
  | "return" => positioned(RETURN)
  | "if" => positioned(IF)
  | "when" => positioned(WHEN)
  | "else" => positioned(ELSE)
  | "true" => positioned(TRUE)
  | "false" => positioned(FALSE)
  | "void" => positioned(VOID)
  | "include" => positioned(INCLUDE)
  | "use" => positioned(USE)
  | "provide" => positioned(PROVIDE)
  | "abstract" => positioned(ABSTRACT)
  | "except" => positioned(EXCEPT)
  | "from" => positioned(FROM)
  | "*" => positioned(STAR)
  | "/" => positioned(SLASH)
  | "|" => positioned(PIPE)
  | "-" => positioned(DASH)
  | "->" => positioned(ARROW)
  | "=>" => positioned(THICKARROW)
  | "type" => positioned(TYPE)
  | "enum" => positioned(ENUM)
  | "record" => positioned(RECORD)
  | "module" => positioned(MODULE)
  | "let" => positioned(LET)
  | "mut" => positioned(MUT)
  | "rec" => positioned(REC)
  | "match" => positioned(MATCH)
  | "assert" => positioned(ASSERT)
  | "fail" => positioned(FAIL)
  | "exception" => positioned(EXCEPTION)
  | "try" => positioned(TRY)
  | "throw" => positioned(THROW)
  | "catch" => positioned(CATCH)
  | "macro" => positioned(MACRO)
  | "yield" => positioned(YIELD)
  | "..." => positioned(ELLIPSIS)
  | "." => positioned(DOT)
  | "::" => positioned(COLONCOLON)
  | ":=" => positioned(GETS)
  | ":" => positioned(COLON)
  | "?" => positioned(QUESTION)
  | "=" => positioned(EQUAL)
  | "," => positioned(COMMA)
  | ";" => positioned(SEMI)
  | "as" => positioned(AS)
  | "and" => positioned(AND)
  | "(" => positioned(LPAREN)
  | ")" => positioned(RPAREN)
  | "{" => positioned(LBRACE)
  | "}" => positioned(RBRACE)
  | "[" => positioned(LBRACK)
  | "[>" => positioned(LBRACKRCARET)
  | "]" => positioned(RBRACK)
  | "<" => positioned(LCARET)
  /* We do not lex >> or >>> as a single token as type vectors can contain
     these, e.g. List<Option<a>>. An operator like `>>>>` is lexed as four
     seperate tokens and the parser sorts it out. */
  | ">" => positioned(RCARET)
  /* The order of these is somewhat important and is why they are
     not sorted by precedence */
  | "+="
  | "-="
  | "*="
  | "/="
  | "%=" =>
    positioned(INFIX_ASSIGNMENT_10(Sedlexing.Utf8.sub_lexeme(lexbuf, 0, 1)))
  | ("==" | "!=", operator_chars)
  | "is"
  | "isnt" => positioned(INFIX_80(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("<<", operator_chars)
  | (">>", rcaret_operator_chars) =>
    positioned(INFIX_100(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("<", lcaret_operator_chars)
  | (">", rcaret_operator_chars) =>
    positioned(INFIX_90(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("^", operator_chars) =>
    positioned(INFIX_60(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("+" | "-", operator_chars) =>
    positioned(INFIX_110(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("*" | "%", operator_chars)
  | ("/", slash_operator_chars) =>
    positioned(INFIX_120(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("&&", operator_chars) =>
    positioned(INFIX_40(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("&", operator_chars) =>
    positioned(INFIX_70(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("||" | "??", operator_chars) =>
    positioned(INFIX_30(Sedlexing.Utf8.lexeme(lexbuf)))
  | ("|", operator_chars) =>
    positioned(INFIX_50(Sedlexing.Utf8.lexeme(lexbuf)))
  | "!" => positioned(PREFIX_150(Sedlexing.Utf8.lexeme(lexbuf)))
  | "@" => positioned(AT)
  | "b\"" =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    let buf = Buffer.create(16);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_bytes(start_p, buf, lexbuf);
  | '"' =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    let buf = Buffer.create(16);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str(start_p, buf, lexbuf);
  | "'" =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    let buf = Buffer.create(4);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_char(start_p, buf, lexbuf);
  | "_" => positioned(UNDERSCORE)
  | lident => positioned(LIDENT(Sedlexing.Utf8.lexeme(lexbuf)))
  | uident => positioned(UIDENT(Sedlexing.Utf8.lexeme(lexbuf)))
  | eof => positioned(EOF)
  | _ => raise(Error(lexbuf_loc(lexbuf), UnrecognizedToken))
  };
}
and read_bytes = (start_p, buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  // Eat all `\\` to ensure we handle `\"` correctly
  | "\\\\" =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str(start_p, buf, lexbuf);
  // Eat all `\"` to ensure the entire value is eaten
  | "\\\"" =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str(start_p, buf, lexbuf);
  | '"' =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    (BYTES(Buffer.contents(buf)), start_p, end_p);
  | any =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_bytes(start_p, buf, lexbuf);
  | _ =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    raise(
      Error(
        Location.of_positions(start_p, end_p),
        UnclosedBytes(start_p.pos_lnum),
      ),
    );
  };
}
and read_str = (start_p, buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  // Eat all `\\` to ensure we handle `\"` correctly
  | "\\\\" =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str(start_p, buf, lexbuf);
  // Eat all `\"` to ensure the entire value is eaten
  | "\\\"" =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str(start_p, buf, lexbuf);
  | '"' =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    (STRING(Buffer.contents(buf)), start_p, end_p);
  | any =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str(start_p, buf, lexbuf);
  | _ =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    raise(
      Error(
        Location.of_positions(start_p, end_p),
        UnclosedString(start_p.pos_lnum),
      ),
    );
  };
}
and read_char = (start_p, buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  // Eat all `\\` to ensure we handle `\'` correctly
  | "\\\\" =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_char(start_p, buf, lexbuf);
  // Eat all `\'` to ensure the entire value is eaten
  | "\\'" =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_char(start_p, buf, lexbuf);
  | "'" =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    (CHAR(Buffer.contents(buf)), start_p, end_p);
  | any =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_char(start_p, buf, lexbuf);
  | _ =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    raise(
      Error(
        Location.of_positions(start_p, end_p),
        UnclosedChar(start_p.pos_lnum),
      ),
    );
  };
}
and read_block_comment = (start_p, buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  | any =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_block_comment(start_p, buf, lexbuf);
  | "*/" =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    let source = Buffer.contents(buf);
    let loc = Location.of_positions(start_p, end_p);
    collect_comment(make_block_comment, source, loc, lexbuf);
    token(lexbuf);
  | _ =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    raise(
      Error(
        Location.of_positions(start_p, end_p),
        UnclosedBlockComment(start_p.pos_lnum),
      ),
    );
  };
}
and read_doc_comment = (start_p, buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  | any =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_doc_comment(start_p, buf, lexbuf);
  | "*/" =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    let source = Buffer.contents(buf);
    let loc = Location.of_positions(start_p, end_p);
    collect_comment(make_doc_comment, source, loc, lexbuf);
    token(lexbuf);
  | _ =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
    raise(
      Error(
        Location.of_positions(start_p, end_p),
        UnclosedDocComment(start_p.pos_lnum),
      ),
    );
  };
};
