type t = SmartPrint.t;

let (++) = SmartPrint.append;
// We want to use `space` for non-breakable space
let breakable_space = SmartPrint.space;
let hardline = SmartPrint.hardline;
let softline = SmartPrint.softline;
let string = SmartPrint.string;
let ifBreaks = SmartPrint.ifBreaks;
let empty = SmartPrint.empty;
let nest = SmartPrint.nest;
let nest_all = SmartPrint.nest_all;
let group = SmartPrint.group;
let group_all = SmartPrint.group_all;
let indent = SmartPrint.indent;
let concat = SmartPrint.concat;
let double_quotes = SmartPrint.double_quotes;

let concat_map = (~sep, ~lead=first => empty, ~trail=last => empty, ~f, l) => {
  let (last, list) =
    List.fold_left(
      ((prev, res), next_item) =>
        switch (prev) {
        | Some(prev_item) => (
            Some(next_item),
            [sep(prev_item, next_item) ++ f(next_item), ...res],
          )
        | None =>
          // If there was no previous, there won't be any in ...res so it isn't spread
          (Some(next_item), [lead(next_item) ++ f(next_item)])
        },
      (None, []),
      l,
    );
  switch (l) {
  | [] => empty
  | _ =>
    switch (last) {
    | Some(last) => SmartPrint.concat(List.rev(list)) ++ trail(last)
    | None => failwith("Impossible: No last item")
    }
  };
};

let parens = d => SmartPrint.parens(nest_all(softline ++ d ++ softline));
let angle_brakets = d =>
  SmartPrint.angle_brakets(nest_all(softline ++ d ++ softline));
let block_braces = d => SmartPrint.braces(hardline ++ indent(d) ++ hardline);
let braces = d =>
  SmartPrint.braces(nest_all(breakable_space ++ d ++ breakable_space));
let list_brakets = d =>
  SmartPrint.brakets(nest_all(softline ++ d ++ softline));
let array_brakets = d =>
  SmartPrint.brakets(
    string(">") ++ nest_all(breakable_space ++ d ++ softline),
  );
let brackets = d => SmartPrint.brakets(softline ++ d ++ softline);

let space = string(" ");
let comma = string(",");
let comma_breakable_space = comma ++ breakable_space;
let comma_hardline = comma ++ hardline;

let to_string = (~width, ~tab, ~newline, doc) =>
  SmartPrint.to_string(width, tab, newline, doc);
