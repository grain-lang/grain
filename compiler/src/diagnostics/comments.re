open Grain_parsing;
open Grain_typed;
open Grain_utils;

// Attributes in a comment are prefixed with `@` symbol, such as `@param`
module Attribute = {
  open Comment_attributes;

  type attributes = list(t);

  type error =
    | GraindocSyntaxError(string, string);

  exception Error(Location.t, error);

  let report_error = (ppf, err) =>
    switch (err) {
    | GraindocSyntaxError(msg, where) =>
      Format.fprintf(ppf, "Graindoc syntax error %s: %s", where, msg)
    };

  let () =
    Location.register_error_of_exn(
      fun
      | Error(loc, err) =>
        Some(Location.error_of_printer(loc, report_error, err))
      | _ => None,
    );

  let extract = (comment_source, comment_content, comment_loc) => {
    // strip trailing */ to allow block comments in examples
    let modified_source =
      String.trim(
        Grain_utils.String_utils.slice(~first=0, ~last=-2, comment_source),
      );
    let lexbuf = Sedlexing.Utf8.from_string(modified_source);

    Sedlexing.set_position(lexbuf, comment_loc.Location.loc_start);
    Sedlexing.set_filename(lexbuf, comment_loc.Location.loc_start.pos_fname);

    let parse_graindoc =
      MenhirLib.Convert.Simplified.traditional2revised(
        Graindoc_parser.graindoc,
      );
    let (buffer, graindoc_lexer) =
      MenhirLib.ErrorReports.wrap_supplier(
        Sedlexing.with_tokenizer(Graindoc_lexer.make_lexer(), lexbuf),
      );

    try(parse_graindoc(graindoc_lexer)) {
    | Graindoc_parser.Error(state) =>
      open Lexing;
      let chunk = ((start_p, end_p)) =>
        String_utils.Utf8.sub(
          comment_source,
          start_p.pos_cnum - comment_loc.loc_start.pos_cnum,
          end_p.pos_cnum - start_p.pos_cnum,
        );
      let where = MenhirLib.ErrorReports.show(chunk, buffer);
      raise(
        Error(
          Location.curr(lexbuf),
          GraindocSyntaxError(
            Graindoc_parser_messages.message(state),
            where,
          ),
        ),
      );
    };
  };

  let is_param = attr => {
    switch (attr) {
    | Param(_) => true
    | _ => false
    };
  };

  let is_returns = attr => {
    switch (attr) {
    | Returns(_) => true
    | _ => false
    };
  };

  let is_example = attr => {
    switch (attr) {
    | Example(_) => true
    | _ => false
    };
  };

  let is_deprecated = attr => {
    switch (attr) {
    | Deprecated(_) => true
    | _ => false
    };
  };

  let is_since = attr => {
    switch (attr) {
    | Since(_) => true
    | _ => false
    };
  };

  let is_history = attr => {
    switch (attr) {
    | History(_) => true
    | _ => false
    };
  };

  let is_throws = attr => {
    switch (attr) {
    | Throws(_) => true
    | _ => false
    };
  };

  let () =
    Printexc.register_printer(exc =>
      switch (exc) {
      | InvalidAttribute(attr_name) =>
        Some(
          Printf.sprintf(
            "No DocBlock attribute defined for `@%s`",
            attr_name,
          ),
        )
      | MalformedAttribute(attr_name, example) =>
        Some(
          Printf.sprintf(
            "Incorrect formatting of `@%s` DocBlock—must be `%s`",
            attr_name,
            example,
          ),
        )
      | _ => None
      }
    );
};

type description = option(string);
type attributes = Attribute.attributes;

module type OrderedComments = {
  type comment = (Typedtree.comment, description, attributes);

  type comments;

  let comments: comments;

  let find_starting_on_lnum: int => option(comment);
  let find_ending_on_lnum: int => option(comment);

  let iter: ((int, comment) => unit) => unit;
};

module MakeOrderedComments =
       (Raw: {
          let comments: list(Typedtree.comment);
        })
       : OrderedComments => {
  module IntMap = Map.Make(Int);

  type comment = (Typedtree.comment, description, attributes);

  type comments = {
    mutable by_start_lnum:
      IntMap.t((Typedtree.comment, description, attributes)),
    mutable by_end_lnum:
      IntMap.t((Typedtree.comment, description, attributes)),
  };

  let comments = {by_start_lnum: IntMap.empty, by_end_lnum: IntMap.empty};

  List.iter(
    (comment: Typedtree.comment) => {
      let (start_lnum, end_lnum, data) =
        switch (comment) {
        | Line({cmt_loc, _})
        | Shebang({cmt_loc, _})
        | Block({cmt_loc, _}) =>
          let data = (comment, None, []);
          (cmt_loc.loc_start.pos_lnum, cmt_loc.loc_end.pos_lnum, data);
        | Doc({cmt_source, cmt_content, cmt_loc}) =>
          let (description, attributes) =
            Attribute.extract(cmt_source, cmt_content, cmt_loc);
          let data = (comment, description, attributes);
          (cmt_loc.loc_start.pos_lnum, cmt_loc.loc_end.pos_lnum, data);
        };
      comments.by_start_lnum =
        IntMap.add(start_lnum, data, comments.by_start_lnum);
      comments.by_end_lnum = IntMap.add(end_lnum, data, comments.by_end_lnum);
    },
    Raw.comments,
  );

  let find_starting_on_lnum = lnum =>
    IntMap.find_opt(lnum, comments.by_start_lnum);
  let find_ending_on_lnum = lnum =>
    IntMap.find_opt(lnum, comments.by_end_lnum);

  let iter = fn => IntMap.iter(fn, comments.by_start_lnum);
};

let to_ordered = (comments): (module OrderedComments) =>
  (module
   MakeOrderedComments({
     let comments = comments;
   }));

let start_line = (comment: Typedtree.comment) => {
  switch (comment) {
  | Line({cmt_loc})
  | Shebang({cmt_loc})
  | Block({cmt_loc})
  | Doc({cmt_loc}) => cmt_loc.loc_start.pos_lnum
  };
};

let end_line = (comment: Typedtree.comment) => {
  switch (comment) {
  | Line({cmt_loc})
  | Shebang({cmt_loc})
  | Block({cmt_loc})
  | Doc({cmt_loc}) => cmt_loc.loc_end.pos_lnum
  };
};

module Doc = {
  let starting_on = (~lnum, module C: OrderedComments) => {
    let data = C.find_starting_on_lnum(lnum);
    switch (data) {
    | Some((Doc({cmt_content}), _, _)) => data
    | _ => None
    };
  };

  let ending_on = (~lnum, module C: OrderedComments) => {
    let data = C.find_ending_on_lnum(lnum);
    switch (data) {
    | Some((Doc({cmt_content}), _, _)) => data
    | _ => None
    };
  };

  let ending_on_including_attribute = (~lnum, module C: OrderedComments) => {
    let rec ending_on_lnum_help = (lnum, check_prev) => {
      let data = C.find_ending_on_lnum(lnum);
      switch (data) {
      | Some((Doc({cmt_content}), _, _)) => data
      // Hack to handle code that has an attribute on the line before, such as `@disableGC`
      | None when check_prev => ending_on_lnum_help(lnum - 1, false)
      | _ => None
      };
    };
    ending_on_lnum_help(lnum, true);
  };
};

let print_comment = (comment: Parsetree.comment) => {
  let endloc =
    switch (comment) {
    | Line(cmt)
    | Block(cmt)
    | Doc(cmt)
    | Shebang(cmt) => cmt.cmt_loc.loc_end
    };

  let startloc =
    switch (comment) {
    | Line(cmt)
    | Block(cmt)
    | Doc(cmt)
    | Shebang(cmt) => cmt.cmt_loc.loc_start
    };

  let (_file, stmtstartline, startchar, _sbol) =
    Locations.get_raw_pos_info(startloc);
  let (_file, stmtendline, endchar, _sbol) =
    Locations.get_raw_pos_info(endloc);

  Printf.printf(
    "%d:%d,%d:%d -",
    stmtstartline,
    startchar,
    stmtendline,
    endchar,
  );

  switch (comment) {
  | Line(cmt)
  | Block(cmt)
  | Doc(cmt)
  | Shebang(cmt) => print_endline(cmt.cmt_source)
  };
};

let get_comment_source = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt)
  | Block(cmt)
  | Doc(cmt)
  | Shebang(cmt) => cmt.cmt_source
  };
