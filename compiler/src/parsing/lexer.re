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
  | UnclosedChar(int)
  | UnclosedBlockComment(int)
  | UnclosedDocComment(int)
  | IllegalUnicodeCodePoint(string);

exception Error(Location.t, error);

let report_error = (ppf, err) =>
  switch (err) {
  | UnrecognizedToken =>
    Format.fprintf(ppf, "The Grain lexer doesn't recognize this token.")
  | UnclosedString(line) =>
    Format.fprintf(ppf, "Unclosed string literal, opened on line %d", line)
  | UnclosedChar(line) =>
    Format.fprintf(ppf, "Unclosed character literal, opened on line %d", line)
  | UnclosedBlockComment(line) =>
    Format.fprintf(ppf, "Unclosed block comment, opened on line %d", line)
  | UnclosedDocComment(line) =>
    Format.fprintf(ppf, "Unclosed doc comment, opened on line %d", line)
  | IllegalUnicodeCodePoint(cp) =>
    Format.fprintf(ppf, "Illegal unicode code point: %S", cp)
  };

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );

let add_code_point = (buf, str, loc) => {
  let (esc, numstr) = (
    String.sub(str, 1, 1),
    String.sub(str, 2, String.length(str) - 2),
  );
  let code_point =
    switch (esc) {
    | "u" when numstr.[0] == '{' =>
      Scanf.sscanf(String.sub(numstr, 1, String.length(numstr) - 1), "%x", x =>
        x
      )
    | "u"
    | "x" => Scanf.sscanf(numstr, "%x", x => x)
    | _ => Scanf.sscanf(esc ++ numstr, "%o", x => x)
    };
  if (Uchar.is_valid(code_point)) {
    Buffer.add_utf_8_uchar(buf, Uchar.of_int(code_point));
  } else {
    raise(Error(loc, IllegalUnicodeCodePoint(str)));
  };
};

let comments = ref([]);

let consume_comments = () => {
  let out_comments = comments^;
  comments := [];
  List.rev(out_comments);
};

let collect_comment = (comment_type, source, loc, lexbuf) => {
  comments := [comment_type(source, loc), ...comments^];
};

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
let dec_float_decimal = [%sedlex.regexp? ('.', Star(dec_digit | '_'))];
let dec_float_decimal_explicit = [%sedlex.regexp?
  ('.', dec_digit, Star(dec_digit | '_'))
];
let dec_float_integral = [%sedlex.regexp?
  (dec_digit, Star(dec_digit | '_'))
];

let dec_float = [%sedlex.regexp?
  (dec_float_integral, dec_float_decimal, Opt(dec_float_exp)) |
  (dec_float_decimal_explicit, Opt(dec_float_exp)) |
  (dec_float_integral, dec_float_exp)
];

let unsigned_float = [%sedlex.regexp? dec_float];

let uident = [%sedlex.regexp? (lu, Star(xid_continue))];
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

// Tabs and space separators (https://www.compart.com/en/unicode/category/Zs)
let blank = [%sedlex.regexp? Plus(zs | '\t')];

let unicode_esc = [%sedlex.regexp? ("\\u{", Rep(hex_digit, 1 .. 6), "}")];
let unicode4_esc = [%sedlex.regexp? ("\\u", Rep(hex_digit, 4))];
let hex_esc = [%sedlex.regexp? ("\\x", Rep(hex_digit, 1 .. 2))];
let oct_esc = [%sedlex.regexp? ("\\", Rep(oct_digit, 1 .. 3))];
let num_esc = [%sedlex.regexp? unicode_esc | unicode4_esc | hex_esc | oct_esc];

let newline_char = [%sedlex.regexp? "\r\n" | '\n'];
let newline_chars = [%sedlex.regexp?
  (Star(newline_char | blank), newline_char)
];

let line_comment = [%sedlex.regexp? ("//", Star(Compl('\r' | '\n')))];
let shebang_comment = [%sedlex.regexp? ("#!", Star(Compl('\r' | '\n')))];

let sub_lexeme = (lexbuf, first, last) => {
  // We use this implementation over Sedlexing's sub_lexeme since it supports negative indexing
  Grain_utils.String_utils.slice(
    ~first,
    ~last,
    Sedlexing.Utf8.lexeme(lexbuf),
  );
};

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
  | newline_chars => positioned(EOL)
  | (unsigned_float, 'f') => positioned(FLOAT32(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_float, 'd') => positioned(FLOAT64(sub_lexeme(lexbuf, 0, -1)))
  | unsigned_float =>
    positioned(NUMBER_FLOAT(Sedlexing.Utf8.lexeme(lexbuf)))
  | (unsigned_int, 'l') => positioned(INT32(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_int, 'L') => positioned(INT64(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_int, 'n') => positioned(WASMI32(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_int, 'N') => positioned(WASMI64(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_float, 'w') => positioned(WASMF32(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_float, 'W') => positioned(WASMF64(sub_lexeme(lexbuf, 0, -1)))
  | (unsigned_int, 't') => positioned(BIGINT(sub_lexeme(lexbuf, 0, -1)))
  | unsigned_int => positioned(NUMBER_INT(Sedlexing.Utf8.lexeme(lexbuf)))
  | "primitive" => positioned(PRIMITIVE)
  | "foreign" => positioned(FOREIGN)
  | "wasm" => positioned(WASM)
  | "while" => positioned(WHILE)
  | "for" => positioned(FOR)
  | "continue" => positioned(CONTINUE)
  | "break" => positioned(BREAK)
  | "if" => positioned(IF)
  | "when" => positioned(WHEN)
  | "else" => positioned(ELSE)
  | "true" => positioned(TRUE)
  | "false" => positioned(FALSE)
  | "void" => positioned(VOID)
  | "import" => positioned(IMPORT)
  | "export" => positioned(EXPORT)
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
  | "..." => positioned(ELLIPSIS)
  | "." => positioned(DOT)
  | "::" => positioned(COLONCOLON)
  | ":=" => positioned(GETS)
  | ":" => positioned(COLON)
  | "=" => positioned(EQUAL)
  | "," => positioned(COMMA)
  | ";" => positioned(SEMI)
  | "as" => positioned(AS)
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
  | '"' =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    read_str(start_p, Buffer.create(16), lexbuf);
  | "'" =>
    let (start_p, _) = Sedlexing.lexing_positions(lexbuf);
    read_char(start_p, Buffer.create(4), lexbuf);
  | "_" => positioned(UNDERSCORE)
  | lident => positioned(LIDENT(Sedlexing.Utf8.lexeme(lexbuf)))
  | uident => positioned(UIDENT(Sedlexing.Utf8.lexeme(lexbuf)))
  | eof => positioned(EOF)
  | _ => raise(Error(lexbuf_loc(lexbuf), UnrecognizedToken))
  };
}
and read_str = (start_p, buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  | ('\\', newline_char) => read_str(start_p, buf, lexbuf)
  | "\\b" =>
    Buffer.add_char(buf, '\b');
    read_str(start_p, buf, lexbuf);
  | "\\f" =>
    Buffer.add_char(buf, '\012');
    read_str(start_p, buf, lexbuf);
  | "\\n" =>
    Buffer.add_char(buf, '\n');
    read_str(start_p, buf, lexbuf);
  | "\\r" =>
    Buffer.add_char(buf, '\r');
    read_str(start_p, buf, lexbuf);
  | "\\t" =>
    Buffer.add_char(buf, '\t');
    read_str(start_p, buf, lexbuf);
  | "\\v" =>
    Buffer.add_char(buf, '\011');
    read_str(start_p, buf, lexbuf);
  | "\\\"" =>
    Buffer.add_char(buf, '"');
    read_str(start_p, buf, lexbuf);
  | "\\\\" =>
    Buffer.add_char(buf, '\\');
    read_str(start_p, buf, lexbuf);
  | num_esc =>
    add_code_point(buf, Sedlexing.Utf8.lexeme(lexbuf), lexbuf_loc(lexbuf));
    read_str(start_p, buf, lexbuf);
  | '"' =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
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
  | "\\b" =>
    Buffer.add_char(buf, '\b');
    read_char(start_p, buf, lexbuf);
  | "\\f" =>
    Buffer.add_char(buf, '\012');
    read_char(start_p, buf, lexbuf);
  | "\\n" =>
    Buffer.add_char(buf, '\n');
    read_char(start_p, buf, lexbuf);
  | "\\r" =>
    Buffer.add_char(buf, '\r');
    read_char(start_p, buf, lexbuf);
  | "\\t" =>
    Buffer.add_char(buf, '\t');
    read_char(start_p, buf, lexbuf);
  | "\\v" =>
    Buffer.add_char(buf, '\011');
    read_char(start_p, buf, lexbuf);
  | "\\'" =>
    Buffer.add_char(buf, '\'');
    read_char(start_p, buf, lexbuf);
  | "\\\\" =>
    Buffer.add_char(buf, '\\');
    read_char(start_p, buf, lexbuf);
  | num_esc =>
    add_code_point(buf, Sedlexing.Utf8.lexeme(lexbuf), lexbuf_loc(lexbuf));
    read_char(start_p, buf, lexbuf);
  | "'" =>
    let (_, end_p) = Sedlexing.lexing_positions(lexbuf);
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
