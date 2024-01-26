open Grain_parsing;
open Graindoc_parser;

let lexbuf_loc = lexbuf => {
  let (loc_start, loc_end) = Sedlexing.lexing_positions(lexbuf);
  Location.{loc_start, loc_end, loc_ghost: false};
};

let ident = [%sedlex.regexp? (xid_start | '_', Star(xid_continue))];
let semver = [%sedlex.regexp?
  (
    Opt('v'),
    Plus('0' .. '9'),
    '.',
    Plus('0' .. '9'),
    '.',
    Plus('0' .. '9'),
    Opt((
      '-',
      Star('0' .. '9' | 'A' .. 'z' | '-' | '.'),
      Opt(('+', Star('0' .. '9' | 'A' .. 'z' | '-'))),
    )),
  )
];

let normalize_semver = semver =>
  if (semver.[0] == 'v') {
    Grain_utils.String_utils.slice(
      ~first=1,
      ~last=String.length(semver),
      semver,
    );
  } else {
    semver;
  };

// HORIZONTAL TABULATION
// VERTICAL TABULATION
// SPACE
// LEFT-TO-RIGHT MARK
// RIGHT-TO-LEFT MARK
let blank = [%sedlex.regexp? 0x09 | 0x0B | 0x20 | 0x200E | 0x200F];

// LINE FEED
// FORM FEED
// CARRIAGE RETURN
// NEXT LINE
// LINE SEPARATOR
// PARAGRAPH SEPARATOR
let newline = [%sedlex.regexp? 0x0A | 0x0C | 0x0D | 0x85 | 0x2028 | 0x2029];

// A colon eats whitespace after it
let colon = [%sedlex.regexp? (':', Star(blank))];

let dec_digit = [%sedlex.regexp? '0' .. '9'];
let dec_int = [%sedlex.regexp? (dec_digit, Star(dec_digit))];

// A asterisk eats whitespace before it and up to 1 after it
let asterisk = [%sedlex.regexp? (Star(blank), '*', Opt(blank))];

type lexer_state = {mutable lexer_mode}
and lexer_mode =
  | Start
  | Default
  | Param
  | Since
  | History
  | Throws
  | FreeTextAttribute;

let rec token = (state, lexbuf) => {
  switch (state.lexer_mode) {
  | Start => start(state, lexbuf)
  | Default => default(state, lexbuf)
  | Param => param(state, lexbuf)
  | Since => since(state, lexbuf)
  | History => history(state, lexbuf)
  | Throws => throws(state, lexbuf)
  | FreeTextAttribute => free_text_attribute(state, lexbuf)
  };
}

and start = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | ("/**", Star(blank)) =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and default = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | asterisk => default(state, lexbuf)
  | "@param" =>
    state.lexer_mode = Param;
    PARAM;
  | "@returns" =>
    state.lexer_mode = FreeTextAttribute;
    RETURNS;
  | "@example" =>
    state.lexer_mode = FreeTextAttribute;
    EXAMPLE;
  | "@deprecated" =>
    state.lexer_mode = FreeTextAttribute;
    DEPRECATED;
  | "@since" =>
    state.lexer_mode = Since;
    SINCE;
  | "@history" =>
    state.lexer_mode = History;
    HISTORY;
  | "@throws" =>
    state.lexer_mode = Throws;
    THROWS;
  | newline => EOL
  | eof => EOF
  | _ => text(state, lexbuf)
  };
}

and free_text_attribute = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | blank => free_text_attribute(state, lexbuf)
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and param = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | blank => param(state, lexbuf)
  | ident => IDENT(Sedlexing.Utf8.lexeme(lexbuf))
  | dec_int => INT(Sedlexing.Utf8.lexeme(lexbuf))
  | colon =>
    state.lexer_mode = Default;
    COLON;
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and section = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | blank => section(state, lexbuf)
  | (Compl(blank | newline | ':' | eof), Star(Compl(newline | ':' | eof))) =>
    TEXT(Sedlexing.Utf8.lexeme(lexbuf))
  | colon =>
    state.lexer_mode = Default;
    COLON;
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and since = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | blank => since(state, lexbuf)
  | semver =>
    state.lexer_mode = Default;
    SEMVER(normalize_semver(Sedlexing.Utf8.lexeme(lexbuf)));
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and history = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | blank => history(state, lexbuf)
  | semver => SEMVER(normalize_semver(Sedlexing.Utf8.lexeme(lexbuf)))
  | colon =>
    state.lexer_mode = Default;
    COLON;
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and throws = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | blank => throws(state, lexbuf)
  | (Compl(blank | newline | ':' | eof), Star(Compl(newline | ':' | eof))) =>
    CONSTRUCTOR(Sedlexing.Utf8.lexeme(lexbuf))
  | colon =>
    state.lexer_mode = Default;
    COLON;
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
}

and text = (state, lexbuf) => {
  switch%sedlex (lexbuf) {
  | Plus(Compl(newline | eof)) => TEXT(Sedlexing.Utf8.lexeme(lexbuf))
  | _ =>
    state.lexer_mode = Default;
    default(state, lexbuf);
  };
};

let make_lexer = () => {
  let state = {lexer_mode: Start};
  lexbuf => token(state, lexbuf);
};
