/**
 * Based on the excellent smart-print library: https://github.com/clarus/smart-print
 *
 * Copyright (c) 2013, Guillaume Claret
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or
 * other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/* Separators. */
module Break = {
  /* A break can be a whitespace or a newline if the text has to be splited. */
  type t =
    | Space
    | Softline
    | Hardline;
};

module Comment = {
  open Grain_parsing;

  type t =
    | Doc(string)
    | Block(string)
    | Line(string)
    | Shebang(string);
};

let max_width = 80;
let tab_width = 2;

/* The internal representation of a document and the engine. */
module Atom = {
  /* An atom is the low-level tree describing a document. */
  type t =
    | StringIfBreaks(string, int, int)
    /* A non-breaking string. It should be newlines free. Represented as a sub-string of an other string, with an offset and a length. */
    | String(string, int, int)
    | Comment(Comment.t)
    /* A separator. */
    | Break(Break.t)
    /* A list of atoms. Only the necessary number of breaks are splited. The boolean is true if nesting is activated. */
    | GroupOne(bool, list(t))
    /* A list of atoms. No or all the breaks are splited. The boolean is true if nesting is activated. */
    | GroupAll(bool, list(t))
    /* Indents by [n] tabulations the atom. Can be negative. */
    | Indent(int, t);

  /* If we overflow a line. */
  exception Overflow;

  /* Print "at best" an atom [a].
     [indent] is the indentation level, [column] the current column position (in number
     of spaces), [last_break] the last break printed if any (so we can collapse
     spaces). It returns the same atom where spaces have been evaluated to
     newlines, the new current column position and the last break printed if any.
     Must succeed (no uncaught [Overflow] exception). */
  let rec eval =
          (~indent: int, ~column: int, ~last_break: option(Break.t), atom: t)
          : (t, int, option(Break.t)) =>
    switch (atom) {
    | String(_, _, l)
    | StringIfBreaks(_, _, l) => (
        atom,
        if (last_break == Some(Break.Hardline)) {
          column + indent + l;
        } else {
          column + l;
        },
        None,
      )
    // TODO: Implement comments
    | Comment(_) => (atom, column, last_break)
    | Break(Break.Space) =>
      if (last_break == None) {
        (atom, column + 1, Some(Break.Space));
      } else {
        (atom, column, last_break);
      }
    | Break(Break.Softline) =>
      if (last_break == None) {
        (atom, column, Some(Break.Softline));
      } else {
        (atom, column, last_break);
      }
    | Break(Break.Hardline) => (atom, 0, Some(Break.Hardline))
    | GroupOne(can_nest, atoms) =>
      let (atoms, column, last_break) =
        try_eval_list_one(
          ~indent,
          ~column,
          ~last_break,
          ~can_fail=false,
          ~can_nest,
          ~in_nest=false,
          atoms,
        );
      (GroupOne(can_nest, atoms), column, last_break);
    | GroupAll(can_nest, atoms) =>
      let (atoms, column, last_break) =
        try({
          let (column, last_break) =
            try_eval_list_flat(
              ~indent=indent + tab_width,
              ~column,
              ~last_break,
              atoms,
            );
          (atoms, column, last_break);
        }) {
        | Overflow =>
          eval_list_all(~indent, ~column, ~last_break, ~can_nest, atoms)
        };
      (GroupAll(can_nest, atoms), column, last_break);
    | Indent(n, atom) =>
      let (atom, column, last_break) =
        eval(~indent=indent + n * tab_width, ~column, ~last_break, atom);
      (Indent(n, atom), column, last_break);
    }

  /* Try to print an atom without evaluating the spaces. May raise [Overflow] if we overflow the line [width]. */
  and try_eval_flat =
      (~indent: int, ~column: int, ~last_break: option(Break.t), atom: t)
      : (int, option(Break.t)) => {
    let try_return = ((column, last_break)) =>
      if (column > max_width) {
        raise(Overflow);
      } else {
        (column, last_break);
      };
    switch (atom) {
    | String(str, _, l)
    | StringIfBreaks(str, _, l) =>
      try_return((
        if (last_break == Some(Break.Hardline)) {
          column + indent + l;
        } else {
          column + l;
        },
        None,
      ))
    // TODO: Implement comments
    | Comment(_) => (column, last_break)
    | Break(Break.Space) =>
      if (last_break == None) {
        try_return((column + 1, Some(Break.Space)));
      } else {
        try_return((column, last_break));
      }
    | Break(Break.Softline) =>
      if (last_break == None) {
        try_return((column, Some(Break.Softline)));
      } else {
        try_return((column, last_break));
      }
    | Break(Break.Hardline) => raise(Overflow)
    | GroupOne(can_nest, atoms) =>
      let (column, last_break) =
        try_eval_list_flat(
          ~indent=indent + tab_width,
          ~column,
          ~last_break,
          atoms,
        );
      (column, last_break);
    | GroupAll(can_nest, atoms) =>
      let (column, last_break) =
        try_eval_list_flat(
          ~indent=indent + tab_width,
          ~column,
          ~last_break,
          atoms,
        );
      (column, last_break);
    | Indent(_, atom) => try_eval_flat(~indent, ~column, ~last_break, atom)
    };
  }

  /* Like [try_eval_flat] but for a list of atoms. */
  and try_eval_list_flat =
      (
        ~indent: int,
        ~column: int,
        ~last_break: option(Break.t),
        atoms: list(t),
      )
      : (int, option(Break.t)) =>
    switch (atoms) {
    | [] => (column, last_break)
    | [atom, ...atoms] =>
      let (column, last_break) =
        try_eval_flat(~indent, ~column, ~last_break, atom);
      let (column, last_break) =
        try_eval_list_flat(~indent, ~column, ~last_break, atoms);
      (column, last_break);
    }

  /* Eval "at best" a list of atoms using the "split only when necessary" policy. The [can_fail]
     flag controls if we can raise an [Overflow], the [can_nest] if we can nest when we break,
     [in_nest] if we have already nested. */
  and try_eval_list_one =
      (
        ~indent: int,
        ~column: int,
        ~last_break: option(Break.t),
        // TODO: It'd be nice to have `eval_list_one` that is unfallible and `try_eval_list_one` that is only fallible
        ~can_fail: bool,
        ~can_nest: bool,
        ~in_nest: bool,
        atoms: list(t),
      )
      : (list(t), int, option(Break.t)) =>
    switch (atoms) {
    | [] => (atoms, column, last_break)
    | [Break(Break.Space), ...atoms] =>
      if (last_break == None) {
        /* If it is not possible in flat mode, switch back to "at best". */
        try({
          let (atoms, column, last_break) =
            try_eval_list_one(
              ~indent,
              ~column=column + 1,
              ~last_break=Some(Break.Space),
              ~can_fail=true,
              ~can_nest,
              ~in_nest,
              atoms,
            );
          ([Break(Break.Space), ...atoms], column, last_break);
        }) {
        | Overflow =>
          let do_indent = can_nest && !in_nest;
          let (atoms, column, last_break) =
            try_eval_list_one(
              ~indent=
                if (do_indent) {
                  indent + tab_width;
                } else {
                  indent;
                },
              ~column=0,
              ~last_break=Some(Break.Hardline),
              ~can_fail=false,
              ~can_nest,
              ~in_nest,
              atoms,
            );
          if (do_indent) {
            (
              [Break(Break.Hardline), Indent(1, GroupOne(false, atoms))],
              column,
              last_break,
            );
          } else {
            ([Break(Break.Hardline), ...atoms], column, last_break);
          };
        };
      } else {
        try_eval_list_one(
          ~indent,
          ~column,
          ~last_break,
          ~can_fail,
          ~can_nest,
          ~in_nest,
          atoms,
        );
      }
    | [Break(Break.Softline), ...atoms] =>
      if (last_break == None) {
        /* If it is not possible in flat mode, switch back to "at best". */
        try({
          let (atoms, column, last_break) =
            try_eval_list_one(
              ~indent,
              ~column,
              ~last_break=Some(Break.Softline),
              ~can_fail=true,
              ~can_nest,
              ~in_nest,
              atoms,
            );
          ([Break(Break.Softline), ...atoms], column, last_break);
        }) {
        | Overflow =>
          let do_indent = can_nest && !in_nest;
          let (atoms, column, last_break) =
            try_eval_list_one(
              ~indent=
                if (do_indent) {
                  indent + tab_width;
                } else {
                  indent;
                },
              ~column=0,
              ~last_break=Some(Break.Hardline),
              ~can_fail=false,
              ~can_nest,
              ~in_nest,
              atoms,
            );
          if (do_indent) {
            (
              [Break(Break.Hardline), Indent(1, GroupOne(false, atoms))],
              column,
              last_break,
            );
          } else {
            ([Break(Break.Hardline), ...atoms], column, last_break);
          };
        };
      } else {
        try_eval_list_one(
          ~indent,
          ~column,
          ~last_break,
          ~can_fail,
          ~can_nest,
          ~in_nest,
          atoms,
        );
      }
    | [Break(Break.Hardline), ...atoms] =>
      let (atoms, column, last_break) =
        /* If there is an explicit newline we always undo the nesting. */
        if (in_nest) {
          try_eval_list_one(
            ~indent=indent - tab_width,
            ~column=0,
            ~last_break=Some(Break.Hardline),
            ~can_fail=false,
            ~can_nest,
            ~in_nest=false,
            atoms,
          );
        } else {
          try_eval_list_one(
            ~indent,
            ~column=0,
            ~last_break=Some(Break.Hardline),
            ~can_fail=false,
            ~can_nest,
            ~in_nest=false,
            atoms,
          );
        };
      if (in_nest) {
        (
          [Break(Break.Hardline), Indent(-1, GroupOne(false, atoms))],
          column,
          last_break,
        );
      } else {
        ([Break(Break.Hardline), ...atoms], column, last_break);
      };
    | [atom, ...atoms] =>
      let (atom, column, last_break) =
        /* If [Overflow] is possible we try in flat mode, else "at best". */
        if (can_fail) {
          let (column, last_break) =
            try_eval_flat(~indent, ~column, ~last_break, atom);
          (atom, column, last_break);
        } else {
          eval(~indent, ~column, ~last_break, atom);
        };
      let (atoms, column, last_break) =
        try_eval_list_one(
          ~indent,
          ~column,
          ~last_break,
          ~can_fail,
          ~can_nest,
          ~in_nest,
          atoms,
        );
      ([atom, ...atoms], column, last_break);
    }

  /* Eval "at best" a list of atoms splitting all the spaces. The flag [can_nest] sets if we indent when we break lines. */
  and eval_list_all =
      (
        ~indent: int,
        ~column: int,
        ~last_break: option(Break.t),
        ~can_nest: bool,
        atoms: list(t),
      )
      : (list(t), int, option(Break.t)) =>
    switch (atoms) {
    | [] => (atoms, column, last_break)
    | [Break(Break.Space), ...atoms]
    | [Break(Break.Softline), ...atoms] =>
      if (last_break == None) {
        let (atoms, column, last_break) =
          eval_list_all(
            ~indent=
              if (can_nest) {
                indent + 1;
              } else {
                indent;
              },
            ~column=if (can_nest) {2} else {0},
            ~last_break=Some(Break.Hardline),
            ~can_nest=false,
            atoms,
          );
        if (can_nest) {
          (
            [Break(Break.Hardline), Indent(1, GroupAll(false, atoms))],
            column,
            last_break,
          );
        } else {
          ([Break(Break.Hardline), ...atoms], column, last_break);
        };
      } else {
        eval_list_all(~indent, ~column, ~last_break, ~can_nest, atoms);
      }
    | [atom, ...atoms] =>
      let (atom, column, last_break) =
        eval(~indent, ~column, ~last_break, atom);
      let (atoms, column, last_break) =
        eval_list_all(~indent, ~column, ~last_break, ~can_nest, atoms);
      ([atom, ...atoms], column, last_break);
    };

  /* Evaluate the breaks. */
  let render = (atoms: list(t)): t => {
    let (atom, _, _) =
      eval(
        ~indent=0,
        ~column=0,
        ~last_break=Some(Break.Hardline),
        GroupOne(false, atoms),
      );
    atom;
  };

  /* A buffer eating trailing spaces. */
  module NonTrailingBuffer = {
    type t = {
      add_char: char => unit,
      add_string: string => unit,
      add_sub_string: (string, int, int) => unit,
      add_newline: unit => unit,
      mutable nb_spaces: int,
    };

    /* A new buffer. */
    let make =
        (
          add_char: char => unit,
          add_string: string => unit,
          add_sub_string: (string, int, int) => unit,
          add_newline: unit => unit,
        )
        : t => {
      add_char,
      add_string,
      add_sub_string,
      add_newline,
      nb_spaces: 0 /* A number of spaces we may print if they are not trailing. */,
    };

    /* Forget previous spaces which appear to be trailing. */
    let forget_spaces = (b: t): unit => b.nb_spaces = 0;

    /* Spaces are not trailing: print all of them. */
    let flush_spaces = (b: t): unit => {
      b.add_string(String.make(b.nb_spaces, ' '));
      forget_spaces(b);
    };

    /* Indent by [i] spaces. Indentation spaces are not printed when on an empty line */
    let indent = (b: t, i: int): unit => {
      forget_spaces(b);
      b.add_string(String.make(i, ' '));
    };

    /* Print a sub-string. */
    let sub_string = (b: t, s: string, o: int, l: int): unit => {
      flush_spaces(b);
      b.add_sub_string(s, o, l);
    };

    /* Add one space in the buffer. */
    let space = (b: t): unit => b.nb_spaces = b.nb_spaces + 1;

    /* Print a newline, with no trailing space before it. */
    let newline = (b: t): unit => {
      forget_spaces(b);
      b.add_newline();
    };
  };

  /* Write to something, given the [add_char] and [add_string] functions. */
  let to_something =
      (
        add_char: char => unit,
        add_string: string => unit,
        add_sub_string: (string, int, int) => unit,
        add_newline: unit => unit,
        atom: t,
      )
      : unit => {
    open NonTrailingBuffer;
    let b = make(add_char, add_string, add_sub_string, add_newline);
    let rec aux = (atom, i, last_break: option(Break.t)): option(Break.t) =>
      switch (atom) {
      | String("", o, l) =>
        /* If we have an emptry string, we don't want to indent */
        if (last_break == Some(Break.Hardline)) {
          forget_spaces(b);
        };
        None;
      | String(s, o, l) =>
        if (last_break == Some(Break.Hardline)) {
          indent(b, i);
        };
        sub_string(b, s, o, l);
        None;
      | StringIfBreaks(s, o, l) =>
        if (last_break !== None) {
          sub_string(b, s, o, l);
        };
        None;
      // TODO: Implement comments
      | Comment(_) => last_break
      | Break(Break.Space) =>
        if (last_break == None || last_break == Some(Break.Softline)) {
          space(b);
          Some(Break.Space);
        } else {
          last_break;
        }
      | Break(Break.Softline) =>
        if (last_break == None) {
          Some(Break.Softline);
        } else {
          last_break;
        }
      | Break(Break.Hardline) =>
        newline(b);
        Some(Break.Hardline);
      | GroupOne(_, atoms)
      | GroupAll(_, atoms) =>
        let last_break = ref(last_break);
        atoms |> List.iter(atom => last_break := aux(atom, i, last_break^));
        last_break^;
      | Indent(n, atom) => aux(atom, i + n * tab_width, last_break)
      };
    ignore(aux(atom, 0, Some(Break.Hardline)));
  };
};

/* A document is a binary tree of atoms so that concatenation happens in O(1). */
type t =
  | Empty
  | Leaf(Atom.t)
  | Node(t, t);

let empty: t = (Empty: t);

let string = (s: string): t =>
  if (s == "") {
    empty;
  } else {
    Leaf(Atom.String(s, 0, Utf8.countInString(s)));
  };

let ifBreaks = (s: string): t =>
  if (s == "") {
    empty;
  } else {
    Leaf(Atom.StringIfBreaks(s, 0, Utf8.countInString(s)));
  };

let (!^) = string;

let sub_string = (s: string, o: int, l: int): t =>
  if (l == 0) {
    empty;
  } else {
    Leaf(Atom.String(s, o, l));
  };

let doc_comment = (cmt: string): t => Leaf(Atom.Comment(Comment.Doc(cmt)));
let block_comment = (cmt: string): t =>
  Leaf(Atom.Comment(Comment.Block(cmt)));
let line_comment = (cmt: string): t =>
  Leaf(Atom.Comment(Comment.Line(cmt)));
let shebang_comment = (cmt: string): t =>
  Leaf(Atom.Comment(Comment.Shebang(cmt)));

let breakable_space: t = (Leaf(Atom.Break(Break.Space)): t);
let softline: t = (Leaf(Atom.Break(Break.Softline)): t);
let hardline: t = (Leaf(Atom.Break(Break.Hardline)): t);

let append = (d1: t, d2: t): t => Node(d1, d2);

let (++) = append;

let concat_with_space = (d1: t, d2: t): t => d1 ++ breakable_space ++ d2;

let (^^) = concat_with_space;

/* Convert a document, which is a tree of atoms, to a list of atoms. In O(n). */
let to_atoms = (d: t): list(Atom.t) => {
  let rec aux = (d: t, atoms: list(Atom.t)): list(Atom.t) =>
    switch (d) {
    | Empty => atoms
    | Leaf(atom) => [atom, ...atoms]
    | Node(d1, d2) => aux(d1, aux(d2, atoms))
    };
  aux(d, []);
};

let rec indent = (d: t): t =>
  switch (d) {
  | Empty => Empty
  | Leaf(atom) => Leaf(Atom.Indent(1, atom))
  | Node(d1, d2) => Node(indent(d1), indent(d2))
  };

let nest = (d: t): t => Leaf(Atom.GroupOne(true, to_atoms(d)));

let nest_all = (d: t): t => Leaf(Atom.GroupAll(true, to_atoms(d)));

let group = (d: t): t => Leaf(Atom.GroupOne(false, to_atoms(d)));

let group_all = (d: t): t => Leaf(Atom.GroupAll(false, to_atoms(d)));

let parens = (d: t): t => !^"(" ++ d ++ !^")";

let braces = (d: t): t => !^"{" ++ d ++ !^"}";

let brackets = (d: t): t => !^"[" ++ d ++ !^"]";

let angle_brackets = (d: t): t => !^"<" ++ d ++ !^">";

let single_quotes = (d: t): t => !^"'" ++ d ++ !^"'";

let double_quotes = (d: t): t => !^"\"" ++ d ++ !^"\"";

let concat = (ds: list(t)): t => List.fold_left(append, empty, ds);

let separate = (separator: t, ds: list(t)): t => {
  let rec aux = ds =>
    switch (ds) {
    | [] => empty
    | [d, ...ds] => separator ++ d ++ aux(ds)
    };
  switch (ds) {
  | [] => empty
  | [d, ...ds] => d ++ aux(ds)
  };
};

/* Split a non-unicode string in a list of offsets / lengths according to a predicate [f]. */
let rec split =
        (s: string, f: char => bool, o: int, l: int): list((int, int)) =>
  if (o + l == Utf8.countInString(s)) {
    [(o, l)];
  } else if (f(s.[o + l])) {
    [(o, l), ...split(s, f, o + l + 1, 0)];
  } else {
    split(s, f, o, l + 1);
  };

let words = (s: string): t =>
  group @@
  separate(breakable_space) @@
  List.map(
    ((o, l)) => sub_string(s, o, l),
    split(s, c => c == ' ' || c == '\n' || c == '\t', 0, 0),
  );

let lines = (s: string): t =>
  separate(hardline) @@
  List.map(
    ((o, l)) => sub_string(s, o, l),
    split(s, c => c == '\n', 0, 0),
  );

module Debug = {
  let bool = (b: bool): t => !^string_of_bool(b);

  let int = (i: int): t => !^string_of_int(i);

  let string = (s: string): t => double_quotes(!^String.escaped(s));

  let list = (d: 'a => t, l: list('a)): t =>
    brackets @@
    nest_all(
      breakable_space
      ^^ separate(!^";" ^^ breakable_space, List.map(d, l))
      ^^ breakable_space,
    );

  /* Pretty-print an atom. */
  let rec pp_atom = (atom: Atom.t): t =>
    switch (atom) {
    | Atom.String(s, o, l) =>
      string(Grain_utils.String_utils.Utf8.sub(s, o, l))
    | Atom.StringIfBreaks(s, o, l) =>
      string(Grain_utils.String_utils.Utf8.sub(s, o, l))
    | Atom.Comment(Doc(comment)) =>
      nest(!^"DocComment" ^^ parens(string(comment)))
    | Atom.Comment(Block(comment)) =>
      nest(!^"BlockComment" ^^ parens(string(comment)))
    | Atom.Comment(Line(comment)) =>
      nest(!^"LineComment" ^^ parens(string(comment)))
    | Atom.Comment(Shebang(comment)) =>
      nest(!^"ShebangComment" ^^ parens(string(comment)))
    | Atom.Break(Break.Space) => !^"Space"
    | Atom.Break(Break.Softline) => !^"Softline"
    | Atom.Break(Break.Hardline) => !^"Hardline"
    | Atom.GroupOne(can_nest, atoms) =>
      nest(
        !^"GroupOne" ^^ parens(bool(can_nest) ++ !^"," ^^ pp_atoms(atoms)),
      )
    | Atom.GroupAll(can_nest, atoms) =>
      nest(
        !^"GroupAll" ^^ parens(bool(can_nest) ++ !^"," ^^ pp_atoms(atoms)),
      )
    | Atom.Indent(n, atom) =>
      nest(!^"Indent" ^^ parens(int(n) ++ !^"," ^^ pp_atom(atom)))
    }

  /* Pretty-print a list of atoms. */
  and pp_atoms = (atoms: list(Atom.t)): t =>
    group_all(separate(!^"," ^^ breakable_space, List.map(pp_atom, atoms)));

  let pp_document = (d: t): t => list(pp_atom, to_atoms(d));

  let pp_document_after_rendering = (d: t): t =>
    pp_atom @@ Atom.render @@ to_atoms(d);
};

let to_something =
    (
      add_char: char => unit,
      add_string: string => unit,
      add_sub_string: (string, int, int) => unit,
      add_newline: unit => unit,
      d: t,
    )
    : unit =>
  Atom.to_something(add_char, add_string, add_sub_string, add_newline) @@
  Atom.render @@
  to_atoms(d);

let to_buffer = (newline: string, b: Buffer.t, d: t): unit => {
  let output_newline = () => Buffer.add_string(b, newline);
  let output_sub_string = (b, s: string, o: int, l: int): unit =>
    Buffer.add_string(b, Grain_utils.String_utils.Utf8.sub(s, o, l));
  to_something(
    Buffer.add_char(b),
    Buffer.add_string(b),
    output_sub_string(b),
    output_newline,
    d,
  );
};

let to_string = (~newline: string, doc: t): string => {
  let b = Buffer.create(10);
  to_buffer(newline, b, doc);
  Buffer.contents(b);
};

let to_out_channel = (newline: string, c: out_channel, d: t): unit => {
  let output_sub_string = (s: string, o: int, l: int): unit =>
    output_string(c, Grain_utils.String_utils.Utf8.sub(s, o, l));
  let output_newline = () => output_string(c, newline);
  to_something(
    output_char(c),
    output_string(c),
    output_sub_string,
    output_newline,
    d,
  );
};

let to_stdout = (newline: string, d: t): unit =>
  to_out_channel(newline, stdout, d);

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
    | Some(last) => concat(List.rev(list)) ++ trail(last)
    | None => failwith("Impossible: No last item")
    }
  };
};

let parens = d => parens(nest(softline ++ d ++ softline));
let parens_all = d => parens(nest_all(softline ++ d ++ softline));
let angle_brackets = d =>
  angle_brackets(nest_all(softline ++ d ++ softline));
let block_braces = d => braces(hardline ++ indent(d) ++ hardline);
let braces = d => braces(nest_all(breakable_space ++ d ++ breakable_space));
let list_brackets = d => brackets(nest_all(softline ++ d ++ softline));
let array_brackets = d =>
  brackets(string(">") ++ nest_all(breakable_space ++ d ++ softline));
let brackets = d => brackets(softline ++ d ++ softline);

let space = string(" ");
let comma = string(",");
let comma_breakable_space = comma ++ breakable_space;
let comma_hardline = comma ++ hardline;
