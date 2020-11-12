type t;

let nil: t;
let text: string => t;
let concat: list(t) => t;
let group: t => t;
let break_group: (~should_break: bool, t) => t;
let if_break: (t, t) => t;
let break_parent: t;
let line: t;
let softline: t;
let hardline: t;
let line_suffix: t => t;
let custom_layout: list(t) => t;
let indent: t => t;

let space: t;
let comma: t;
let dot: t;
let elipsis: t;
let less_than: t;
let greater_than: t;
let lbrace: t;
let rbrace: t;
let lparen: t;
let rparen: t;
let lbracket: t;
let rbracket: t;
let colon: t;
let equal: t;
let trailing_comma: t;
let single_quote: t;
let double_quote: t;

let join: (~sep: t, list(t)) => t;

let to_string: (~width: int, ~indent: int, t) => string;
