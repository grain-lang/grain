type line_style =
  | Medium
  | Soft
  | Hard;

type t =
  | Nil
  | Text(string)
  | Concat(list(t))
  | Group(bool, t)
  | IfBreak(t, t)
  | BreakParent
  | LineBreak(line_style)
  | LineSuffix(t)
  | CustomLayout(list(t))
  | Indent(t);

let nil = Nil;
let text = s => Text(s);
let concat = t => Concat(t);
let group = t => Group(false, t);
let break_group = (~should_break, t) => Group(should_break, t);
let if_break = (y, n) => IfBreak(y, n);
let break_parent = BreakParent;
let line = LineBreak(Medium);
let softline = LineBreak(Soft);
let hardline = LineBreak(Hard);
let line_suffix = t => LineSuffix(t);
let custom_layout = t => CustomLayout(t);
let indent = t => Indent(t);

let space = Text(" ");
let comma = Text(",");
let dot = Text(".");
let elipsis = Text("...");
let less_than = Text("<");
let greater_than = Text(">");
let lbrace = Text("{");
let rbrace = Text("}");
let lparen = Text("(");
let rparen = Text(")");
let lbracket = Text("[");
let rbracket = Text("]");
let colon = Text(":");
let equal = Text("=");
let trailing_comma = IfBreak(comma, nil);
let single_quote = Text("'");
let double_quote = Text("\"");

let join = (~sep, docs) =>
  List.fold_left((acc, curr) => [curr, sep, ...acc], [], docs)
  |> List.rev
  |> List.tl
  |> concat;
