open Grain_parsing;
open Grain_typed;
open Grain_utils;

// Attributes in a comment are prefixed with `@` symbol, such as `@param`
module Attribute = {
  exception InvalidAttribute(string);
  exception MalformedAttribute(string, string);

  type attr_name = string;
  type attr_desc = string;
  // The `attr_type` always starts as `None` and is applied later by something like Graindoc
  type attr_type = option(string);
  type attr_version = string;

  type t =
    | Param({
        attr_name,
        attr_type,
        attr_desc,
      })
    | Returns({
        attr_desc,
        attr_type,
      })
    | Module({
        attr_name,
        attr_desc,
      })
    // Currently only accepts single-line examples
    | Example({attr_desc})
    | Section({
        attr_name,
        attr_desc,
      })
    | Deprecated({attr_desc})
    | Since({attr_version})
    | History({
        attr_version,
        attr_desc,
      })
    | Throws({
        attr_type,
        attr_desc,
      });

  let parse_param = (~attr, content) => {
    let re = Str.regexp({|^\([^:]+\):[ ]+\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_name = Str.matched_group(1, content);
      let attr_desc = Str.matched_group(2, content);
      Param({attr_name, attr_desc, attr_type: None});
    } else {
      raise(
        MalformedAttribute(
          attr,
          "@param ParamName: Description of param value",
        ),
      );
    };
  };

  let parse_returns = (~attr, content) => {
    let re = Str.regexp({|^\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_desc = Str.matched_group(1, content);
      Returns({attr_desc, attr_type: None});
    } else {
      raise(
        MalformedAttribute(attr, "@returns Description of return value"),
      );
    };
  };

  let parse_module = (~attr, content) => {
    let re = Str.regexp({|^\([^:]+\):[ ]+\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_name = Str.matched_group(1, content);
      let attr_desc = Str.matched_group(2, content);
      Module({attr_name, attr_desc});
    } else {
      raise(
        MalformedAttribute(attr, "@module ModuleName: Description of module"),
      );
    };
  };

  let parse_example = (~attr, content) => {
    let re = Str.regexp({|^\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_desc = Str.matched_group(1, content);
      Example({attr_desc: attr_desc});
    } else {
      raise(MalformedAttribute(attr, "@example single-line code example"));
    };
  };

  let parse_section = (~attr, content) => {
    let re = Str.regexp({|^\([^:]+\):[ ]+\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_name = Str.matched_group(1, content);
      let attr_desc = Str.matched_group(2, content);
      Section({attr_name, attr_desc});
    } else {
      raise(
        MalformedAttribute(
          attr,
          "@section SectionName: Description of section",
        ),
      );
    };
  };

  let parse_deprecated = (~attr, content) => {
    let re = Str.regexp({|^\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_desc = Str.matched_group(1, content);
      Deprecated({attr_desc: attr_desc});
    } else {
      raise(
        MalformedAttribute(attr, "@deprecated Description of deprecation"),
      );
    };
  };

  let parse_since = (~attr, content) => {
    let re = Str.regexp({|^v?\([0-9]+\.[0-9]+\.[0-9]+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_version = Str.matched_group(1, content);
      Since({attr_version: attr_version});
    } else {
      raise(MalformedAttribute(attr, "@since vX.Y.Z"));
    };
  };

  let parse_history = (~attr, content) => {
    let re = Str.regexp({|^v?\([0-9]+\.[0-9]+\.[0-9]+\):[ ]+\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_version = Str.matched_group(1, content);
      let attr_desc = Str.matched_group(2, content);
      History({attr_version, attr_desc});
    } else {
      raise(
        MalformedAttribute(attr, "@history vX.Y.Z: Description of history"),
      );
    };
  };

  let parse_throws = (~attr, content) => {
    let re = Str.regexp({|^\([^:]+\):[ ]+\(.+\)$|});
    if (Str.string_match(re, content, 0)) {
      let attr_type = Str.matched_group(1, content);
      let attr_desc = Str.matched_group(2, content);
      Throws({attr_type: Some(attr_type), attr_desc});
    } else {
      raise(
        MalformedAttribute(
          attr,
          "@throws ExceptionType: Explanation of exceptional case",
        ),
      );
    };
  };

  let extract = comment => {
    let attrs = ref([]);
    let attr_line_re = Str.regexp({|^@\([a-zA-Z_]+\)\b\(.*\)$|});

    // TODO: We should probably transition to a lexer, instead of 2-passes of RegExp
    let out =
      Str.global_substitute(
        attr_line_re,
        _ => {
          let attr = Str.matched_group(1, comment);
          // Trim this since we only match word boundaries now
          let content = String.trim(Str.matched_group(2, comment));
          switch (attr) {
          | "param" =>
            let param_attr = parse_param(~attr, content);
            attrs := [param_attr, ...attrs^];
          | "returns" =>
            let returns_attr = parse_returns(~attr, content);
            attrs := [returns_attr, ...attrs^];
          | "module" =>
            let module_attr = parse_module(~attr, content);
            attrs := [module_attr, ...attrs^];
          | "example" =>
            let example_attr = parse_example(~attr, content);
            attrs := [example_attr, ...attrs^];
          | "section" =>
            let section_attr = parse_section(~attr, content);
            attrs := [section_attr, ...attrs^];
          | "deprecated" =>
            let deprecated_attr = parse_deprecated(~attr, content);
            attrs := [deprecated_attr, ...attrs^];
          | "since" =>
            let since_attr = parse_since(~attr, content);
            attrs := [since_attr, ...attrs^];
          | "history" =>
            let history_attr = parse_history(~attr, content);
            attrs := [history_attr, ...attrs^];
          | "throws" =>
            let throws_attr = parse_throws(~attr, content);
            attrs := [throws_attr, ...attrs^];
          | _ => raise(InvalidAttribute(attr))
          };

          // Replace it with nothing
          "";
        },
        comment,
      );

    let desc = String.trim(out);
    let desc_opt =
      if (desc != "") {
        Some(desc);
      } else {
        None;
      };
    (desc_opt, List.rev(attrs^));
  };

  let is_param = (attr: t) => {
    switch (attr) {
    | Param(_) => true
    | _ => false
    };
  };

  let is_returns = (attr: t) => {
    switch (attr) {
    | Returns(_) => true
    | _ => false
    };
  };

  let is_module = (attr: t) => {
    switch (attr) {
    | Module(_) => true
    | _ => false
    };
  };

  let is_example = (attr: t) => {
    switch (attr) {
    | Example(_) => true
    | _ => false
    };
  };

  let is_section = (attr: t) => {
    switch (attr) {
    | Section(_) => true
    | _ => false
    };
  };

  let is_deprecated = (attr: t) => {
    switch (attr) {
    | Deprecated(_) => true
    | _ => false
    };
  };

  let is_since = (attr: t) => {
    switch (attr) {
    | Since(_) => true
    | _ => false
    };
  };

  let is_history = (attr: t) => {
    switch (attr) {
    | History(_) => true
    | _ => false
    };
  };

  let is_throws = (attr: t) => {
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
type attributes = list(Attribute.t);

module type OrderedComments = {
  type comment = (Typedtree.comment, description, attributes);

  type comments;

  let comments: comments;

  let find_starting_on_lnum: int => option(comment);
  let find_ending_on_lnum: int => option(comment);

  let iter: ((int, comment) => unit) => unit;
};

module MakeOrderedComments =
       (Raw: {let comments: list(Typedtree.comment);})
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
        | Doc({cmt_loc, cmt_content}) =>
          let (description, attributes) = Attribute.extract(cmt_content);
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

  let find_module = (module C: OrderedComments) => {
    let module_comments = ref([]);
    C.iter((_, (_comment, _desc, attrs) as comment) =>
      if (List.exists(Attribute.is_module, attrs)) {
        module_comments := [comment, ...module_comments^];
      }
    );
    if (List.length(module_comments^) > 1) {
      failwith("More than one @module block is not supported");
    } else {
      List.nth_opt(module_comments^, 0);
    };
  };

  let find_sections = (module C: OrderedComments) => {
    let section_comments = ref([]);
    C.iter((_, (_comment, _desc, attrs) as comment) =>
      if (List.exists(Attribute.is_section, attrs)) {
        section_comments := [comment, ...section_comments^];
      }
    );
    List.rev(section_comments^);
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
