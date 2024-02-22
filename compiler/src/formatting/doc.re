/**
  The Doc module implements a document IR and engine for pretty-printing code.
  Concatenation of Doc.t nodes is O(1) and printing a document is O(n) to the
  size of the document.

  The most important aspect of the engine are groups and how breaks interact
  with them. By default, the engine will print a group by either breaking none
  of the break hints in that group if the entire group would fit on that line
  (known as Flat mode) or all of the break hints in that group if the group
  would not fit if printed in Flat mode (known as Breaking mode). This covers
  95% of formatting use cases, and users should tend to reach for default
  groups before considering one of the alternatives. For the remaining 5% of
  use cases, groups can also be created in FitGroups mode or FitAll mode. In
  FitGroups mode, the engine will attempt to print as many subgroups in Flat
  mode as possible on each line, breaking only when necessary. In FitAll mode,
  the engine will attempt to print as many subgroups in Breaking mode as
  possible on each line.

  Hardlines should be avoided. Instead, emit break hints and allow the engine
  to decide when breaks should be made. If hardlines must be used, consider
  using the group's ~print_width parameter to manually specify how wide the
  engine should consider the group. By default, a group is only considered as
  wide as the content leading to the first hardline.

  That's most of what you need to know to effectively use this module! Further
  details on each node are provided below for maintainers or curious consumers.

  IR nodes:
    • Empty
      Has no effect on the output of the printing engine.
    • GroupBreaker
      Causes the enclosing group to be printed in Breaking mode.
    • String
      Prints the string as-is. The `string` function is Utf8-aware.
    • Blank
      Prints the specified number of spaces.
    • BreakHint
      Tells the engine that a break can be made here. If the engine decides not
      to print a break, it prints the supplied document instead.
    • Hardline
      Forces the engine to print a newline character. Width calculations for
      the current line are truncated at the Hardline. If the `phantom` field is
      set to `true`, instead the Hardline is calculated as a zero-width non-
      breaking character (the newline is emitted in the output, but
      calculations assume it's just not there).
    • IfBroken
      If the engine has broken the current group, prints the `breaking`
      document and prints the `flat` document otherwise. Note that for FitAll
      and FitGroups groups, the `flat` document would be printed if the
      IfBroken node appears before the point at which the group is broken, as
      the engine makes that decision while printing the group (unlike default
      groups, where the engine makes this decision before printing the group).
    • Indent
      Introduces indentation equal to the number of spaces specified when the
      enclosing group is broken. When newline characters are emitted, they are
      followed by space characters equal to the amount of indentation that has
      been applied by all groups, unless this would lead to trailing
      whitespace. Note that if the enclosing group has not been broken, the
      indentation will not apply. For example, in this document,
        group(~kind=FitGroups, indent(2,
          group(indent(2, string("foo") ++ break ++ string("bar")))
        ))
      if the break hint is broken by the engine, `bar`'s indentation level will
      only be two spaces, as the outer group could never be broken be broken by
      the engine.
    • Group
      ~kind=Auto
        The engine checks if the group would fit on the current line if printed
        in Flat mode. If so, it prints the group in Flat mode and Breaking mode
        otherwise.
      ~kind=FitGroups
        The engine begins printing the group. When it encounters a break hint,
        it checks if the following node would fit on the current line. If that
        node is a Group, its Flat mode width is used for the check. If the node
        would not fit, a break is emitted.
      ~kind=FitAll
        The engine begins printing the group. When it encounters a break hint,
        it checks if the following node would fit on the current line. If that
        node is a Group, its Breaking mode width is used for the check. If the
        node would not fit, a break is emitted.
    • Concat
      Prints the first document followed by the second document. Keeps track of
      the combined width to allow the engine to make constant-time decisions
      about line breaks.
*/
type t =
  | Empty
  | GroupBreaker
  | String({
      value: string,
      width,
    })
  | Blank({count: int})
  | BreakHint({
      doc: t,
      flat_width: width,
    })
  | Hardline({phantom: bool})
  | IfBroken({
      flat: t,
      breaking: t,
      flat_width: width,
      breaking_width: width,
    })
  | Indent({
      count: int,
      doc: t,
      has_group_breaker: bool,
      flat_width: width,
      breaking_width: width,
    })
  | Group({
      group_type,
      doc: t,
      flat_width: width,
      breaking_width: width,
    })
  | Concat({
      left: t,
      right: t,
      has_group_breaker: bool,
      flat_width: width,
      breaking_width: width,
    })
and group_type =
  | Auto
  | FitGroups
  | FitAll
and width =
  | WithBreak(int)
  | WithoutBreak(int);

let breaking_width = doc =>
  switch (doc) {
  | Empty
  | GroupBreaker => WithoutBreak(0)
  | String({width}) => width
  | Indent({breaking_width})
  | Group({breaking_width})
  | Concat({breaking_width})
  | IfBroken({breaking_width}) => breaking_width
  | Blank({count}) => WithoutBreak(count)
  | BreakHint(_)
  | Hardline({phantom: false}) => WithBreak(0)
  | Hardline({phantom: true}) => WithoutBreak(0)
  };

let flat_width = doc =>
  switch (doc) {
  | Empty
  | GroupBreaker => WithoutBreak(0)
  | String({width}) => width
  | Indent({flat_width})
  | Group({flat_width})
  | Concat({flat_width})
  | IfBroken({flat_width})
  | BreakHint({flat_width}) => flat_width
  | Blank({count}) => WithoutBreak(count)
  | Hardline({phantom: false}) => WithBreak(0)
  | Hardline({phantom: true}) => WithoutBreak(0)
  };

let has_group_breaker = doc =>
  switch (doc) {
  | GroupBreaker => true
  | Empty
  | IfBroken(_)
  | BreakHint(_)
  | Blank(_)
  | Hardline(_)
  | Group(_)
  | String(_) => false
  | Concat({has_group_breaker})
  | Indent({has_group_breaker}) => has_group_breaker
  };

let width_value = width =>
  switch (width) {
  | WithBreak(w)
  | WithoutBreak(w) => w
  };

let group_breaker = GroupBreaker;
let string = s =>
  String({value: s, width: WithoutBreak(Utf8.countInString(s))});
let blank = c => Blank({count: c});
let break = doc => BreakHint({doc, flat_width: flat_width(doc)});
let hardline = Hardline({phantom: false});
let phantom_hardline = Hardline({phantom: true});
let if_broken = (breaking, flat) =>
  IfBroken({
    flat,
    breaking,
    flat_width: flat_width(flat),
    breaking_width: breaking_width(breaking),
  });
let indent = (~count=2, doc) =>
  Indent({
    count,
    doc,
    has_group_breaker: has_group_breaker(doc),
    flat_width: flat_width(doc),
    breaking_width: breaking_width(doc),
  });
let group = (~print_width=?, ~kind=Auto, doc) => {
  let (flat_width, breaking_width) =
    switch (print_width) {
    | Some(width) => (WithoutBreak(width), WithoutBreak(width))
    | None => (flat_width(doc), breaking_width(doc))
    };
  Group({group_type: kind, doc, flat_width, breaking_width});
};

let concat = (left, right) => {
  let add = (left, right) => {
    switch (left, right) {
    | (WithBreak(_), _) => left
    | (WithoutBreak(l), WithoutBreak(r)) => WithoutBreak(l + r)
    | (WithoutBreak(l), WithBreak(r)) => WithBreak(l + r)
    };
  };

  let has_group_breaker =
    has_group_breaker(left) || has_group_breaker(right);

  let (flat_width, breaking_width) =
    if (has_group_breaker) {
      let breaking_width = add(breaking_width(left), breaking_width(right));
      (breaking_width, breaking_width);
    } else {
      (
        add(flat_width(left), flat_width(right)),
        add(breaking_width(left), breaking_width(right)),
      );
    };

  Concat({left, right, has_group_breaker, flat_width, breaking_width});
};
let (++) = concat;

let breakable_space = break(blank(1));
let break = break(Empty);
let space = blank(1);
let empty = Empty;

let comma = string(",");
let comma_breakable_space = comma ++ breakable_space;

let concat_map = (~sep, ~lead, ~trail, ~f: (~final: bool, 'a) => t, l) => {
  switch (l) {
  | [] => empty
  | [first, ..._] =>
    let rec concat_map = (acc, l) => {
      switch (l) {
      | [] => failwith("Impossible: empty list")
      | [ultimate] =>
        // one element list
        acc ++ f(~final=true, ultimate) ++ trail(ultimate)
      | [penultimate, ultimate] =>
        acc
        ++ f(~final=false, penultimate)
        ++ sep(penultimate, ultimate)
        ++ f(~final=true, ultimate)
        ++ trail(ultimate)
      | [elem, next, ...rest] =>
        concat_map(
          acc ++ f(~final=false, elem) ++ sep(elem, next),
          [next, ...rest],
        )
      };
    };
    concat_map(lead(first), l);
  };
};

let parens = (~wrap=doc => group(doc), doc) =>
  wrap(string("(") ++ doc ++ string(")"));
let braces = (~wrap=doc => group(doc), doc) =>
  wrap(string("{") ++ doc ++ string("}"));
let array_brackets = (~wrap=doc => group(doc), doc) =>
  wrap(string("[>") ++ doc ++ string("]"));
let list_brackets = (~wrap=doc => group(doc), doc) =>
  wrap(string("[") ++ doc ++ string("]"));
let angle_brackets = (~wrap=doc => group(doc), doc) =>
  wrap(string("<") ++ doc ++ string(">"));

let double_quotes = doc => string("\"") ++ doc ++ string("\"");

let trailing_comma = if_broken(string(","), empty);

module Engine = {
  type mode =
    | Flat
    | Breaking
    | FitFlat
    | FitBreaking;

  type group = {
    mode,
    mutable global_indent: int,
    mutable local_indent: int,
    mutable broken: bool,
  };

  let print = (~write, ~eol, ~line_width, doc) => {
    // The current column we're writing to
    let column = ref(0);
    // Queue for indentation to prevent lines with just spaces
    let write_queue = ref(None);
    // Continuation for Fit mode calculations that depend on the size of the next node
    let k = ref(None);

    let eol =
      switch (eol) {
      | Grain_utils.Fs_access.CRLF => "\r\n"
      | LF => "\n"
      };

    let flush_write_queue = () => {
      switch (write_queue^) {
      | Some(queued) =>
        write(queued);
        write_queue := None;
      | None => ()
      };
    };

    let rec print = (~group, doc) => {
      switch (k^) {
      | Some(f) =>
        k := None;
        f(doc);
      | None => ()
      };

      switch (doc) {
      | Empty
      | GroupBreaker => ()
      | String({value, width}) =>
        flush_write_queue();
        write(value);
        column := column^ + width_value(width);
      | Blank({count}) =>
        flush_write_queue();
        write(String.make(count, ' '));
        column := column^ + count;
      | BreakHint({doc, flat_width: width}) =>
        let break = () => {
          group.broken = true;
          group.global_indent = group.global_indent + group.local_indent;
          group.local_indent = 0;
          write(eol);
          write_queue := Some(String.make(group.global_indent, ' '));
          column := group.global_indent;
        };
        switch (group.mode) {
        | Flat => print(~group, doc)
        | Breaking => break()
        | FitFlat =>
          k :=
            Some(
              next_doc => {
                let next_width = width_value(flat_width(next_doc));
                let hint_width = width_value(width);
                if (column^ + hint_width + next_width > line_width) {
                  break();
                } else {
                  print(~group, doc);
                };
              },
            )
        | FitBreaking =>
          k :=
            Some(
              next_doc => {
                let next_width = width_value(breaking_width(next_doc));
                let hint_width = width_value(width);
                if (column^ + hint_width + next_width > line_width) {
                  break();
                } else {
                  print(~group, doc);
                };
              },
            )
        };
      | Hardline(_) =>
        group.broken = true;
        group.global_indent = group.global_indent + group.local_indent;
        group.local_indent = 0;
        write(eol);
        write_queue := Some(String.make(group.global_indent, ' '));
        column := group.global_indent;
      | IfBroken({flat, breaking}) =>
        if (group.broken) {
          print(~group, breaking);
        } else {
          print(~group, flat);
        }
      | Indent({count, doc}) =>
        let global_indent = group.global_indent;
        let local_indent = group.local_indent;
        group.local_indent = local_indent + count;
        print(~group, doc);
        group.global_indent = global_indent;
        group.local_indent = local_indent;
      | Group({doc, group_type, flat_width}) =>
        let width = width_value(flat_width);
        let mode =
          switch (group_type) {
          | _ when has_group_breaker(doc) => Breaking
          | Auto when column^ + width > line_width => Breaking
          | Auto => Flat
          | FitGroups => FitFlat
          | FitAll => FitBreaking
          };

        let group = {
          mode,
          global_indent: group.global_indent,
          local_indent: 0,
          broken: has_group_breaker(doc),
        };
        print(~group, doc);
      | Concat({left, right}) =>
        print(~group, left);
        print(~group, right);
      };
    };

    let group = {
      mode: Flat,
      global_indent: 0,
      local_indent: 0,
      broken: false,
    };
    print(~group, doc);
  };

  let to_string = (~eol, ~line_width, doc) => {
    let b = Buffer.create(2048);
    let write = Buffer.add_string(b);
    print(~write, ~eol, ~line_width, doc);
    Buffer.contents(b);
  };
};
