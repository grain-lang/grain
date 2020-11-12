type break_mode =
  | Break
  | Flat;

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

let to_string = (~width, ~indent, doc) => {
  let buffer = Buffer.create(1000);
  let rec process = (~pos, lineSuffices, stack) =>
    switch (stack) {
    | [(ind, mode, doc) as cmd, ...rest] =>
      switch (doc) {
      | Nil
      | BreakParent => process(~pos, lineSuffices, rest)
      | Text(s) =>
        Buffer.add_string(buffer, s);
        process(~pos=String.length(s) + pos, lineSuffices, rest);
      | Concat(ds) =>
        let ops = List.map(d => (ind, mode, d), ds);
        process(~pos, lineSuffices, List.append(ops, rest));
      | Group(b, d) =>
        if (b) {
          process(~pos, lineSuffices, [(ind, Break, d), ...rest]);
        } else {
          process(~pos, lineSuffices, [(ind, Flat, d), ...rest]);
        }
      | IfBreak(y, n) =>
        if (mode == Break) {
          process(~pos, lineSuffices, [(ind, mode, y), ...rest]);
        } else {
          process(~pos, lineSuffices, [(ind, mode, n), ...rest]);
        }
      | LineBreak(ls) =>
        if (mode == Break) {
          switch (lineSuffices) {
          | [] =>
            Buffer.add_char(buffer, '\n'); // TODO
            Buffer.add_string(buffer, String.make(ind, ' '));
            process(~pos=ind, [], rest);
          | _ =>
            process(
              ~pos=ind,
              [],
              List.append(List.rev(lineSuffices), [cmd, ...rest]),
            )
          };
        } else {
          let pos =
            switch (ls) {
            | Medium =>
              Buffer.add_string(buffer, " ");
              pos + 1;
            | Soft => pos
            | Hard =>
              Buffer.add_char(buffer, '\n'); // TODO
              0;
            };
          process(~pos, lineSuffices, rest);
        }
      | LineSuffix(d) =>
        process(~pos, [(ind, mode, d), ...lineSuffices], rest)
      | CustomLayout(_) => () // TODO
      | Indent(d) =>
        process(~pos, lineSuffices, [(ind + indent, mode, d), ...rest])
      }
    | [] => ()
    };
  process(~pos=0, [], [(0, Flat, doc)]);
  Buffer.contents(buffer);
};
