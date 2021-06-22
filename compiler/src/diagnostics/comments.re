open Grain_parsing;
open Grain_typed;

// Attributes in a comment are prefixed with `@` symbol, such as `@param`
module Attribute = {
  exception InvalidAttribute(string);
  exception MalformedAttribute(string, string);

  type attr_name = string;
  type attr_desc = string;
  // The `attr_type` always starts as `None` and is applied later by something like Graindoc
  type attr_type = option(string);

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
      });

  let try_or_malformed = (~attr, ~hint, fn) =>
    try(fn()) {
    | _ => raise(MalformedAttribute(attr, hint))
    };

  let extract = comment => {
    let attrs = ref([]);
    // TODO: We should probably be using a less-janky RegExp library
    // https://regexper.com/#%5E%40%28%5Ba-zA-Z_%5D%2B%29%28%5B%20%5D%2B%28%5B%5E%3A%5D%2B%29%3A%29%3F%5B%20%5D%2B%28.*%29%24
    let re = Str.regexp({|^@\([a-zA-Z_]+\)\([ ]+\([^:]+\):\)?[ ]+\(.*\)$|});
    let out =
      Str.global_substitute(
        re,
        _ => {
          let attr = Str.matched_group(1, comment);
          switch (attr) {
          | "param" =>
            try_or_malformed(
              ~attr,
              ~hint="@param ParamName: Description of param value",
              () => {
                let attr_name = Str.matched_group(3, comment);
                let attr_desc = Str.matched_group(4, comment);
                attrs :=
                  [Param({attr_name, attr_desc, attr_type: None}), ...attrs^];
              },
            )
          | "returns" =>
            try_or_malformed(
              ~attr,
              ~hint="@returns Description of return value",
              () => {
                let attr_desc = Str.matched_group(4, comment);
                attrs := [Returns({attr_desc, attr_type: None}), ...attrs^];
              },
            )
          | "module" =>
            try_or_malformed(
              ~attr,
              ~hint="@module ModuleName: Description of module",
              () => {
                let attr_name = Str.matched_group(3, comment);
                let attr_desc = Str.matched_group(4, comment);
                attrs := [Module({attr_name, attr_desc}), ...attrs^];
              },
            )
          | "example" =>
            try_or_malformed(
              ~attr,
              ~hint="@example single-line code example",
              () => {
                let attr_desc = Str.matched_group(4, comment);
                attrs := [Example({attr_desc: attr_desc}), ...attrs^];
              },
            )
          | "section" =>
            try_or_malformed(
              ~attr,
              ~hint="@section SectionName: Description of section",
              () => {
                let attr_name = Str.matched_group(3, comment);
                let attr_desc = Str.matched_group(4, comment);
                attrs := [Section({attr_name, attr_desc}), ...attrs^];
              },
            )
          | _ => raise(InvalidAttribute(attr))
          };

          // Replace it with nothing
          "";
        },
        comment,
      );

    (String.trim(out), List.rev(attrs^));
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

type attributes = list(Attribute.t);

module IntMap = Map.Make(Int);

type comments = {
  mutable by_start_lnum: IntMap.t((Typedtree.comment, string, attributes)),
  mutable by_end_lnum: IntMap.t((Typedtree.comment, string, attributes)),
};

let comments = {by_start_lnum: IntMap.empty, by_end_lnum: IntMap.empty};

let setup_comments = (raw_comments: list(Typedtree.comment)) => {
  List.iter(
    (comment: Typedtree.comment) => {
      switch (comment) {
      | Line({cmt_loc, cmt_content})
      | Shebang({cmt_loc, cmt_content})
      | Block({cmt_loc, cmt_content})
      | Doc({cmt_loc, cmt_content}) =>
        let (description, attributes) = Attribute.extract(cmt_content);
        let data = (comment, description, attributes);

        comments.by_start_lnum =
          IntMap.add(
            cmt_loc.loc_start.pos_lnum,
            data,
            comments.by_start_lnum,
          );
        comments.by_end_lnum =
          IntMap.add(cmt_loc.loc_end.pos_lnum, data, comments.by_end_lnum);
      }
    },
    raw_comments,
  );
};

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
  let starting_on_lnum = lnum => {
    let data = IntMap.find_opt(lnum, comments.by_start_lnum);
    switch (data) {
    | Some((Doc({cmt_content}), _, _)) => data
    | _ => None
    };
  };

  let ending_on_lnum = lnum => {
    let rec ending_on_lnum_help = (lnum, check_prev) => {
      let data = IntMap.find_opt(lnum, comments.by_end_lnum);
      switch (data) {
      | Some((Doc({cmt_content}), _, _)) => data
      // Hack to handle code that has an attribute on the line before, such as `@disableGC`
      | None when check_prev => ending_on_lnum_help(lnum - 1, false)
      | _ => None
      };
    };
    ending_on_lnum_help(lnum, true);
  };

  let find_module = () => {
    let module_comments = ref([]);
    IntMap.iter(
      (_, (_comment, _desc, attrs) as comment) =>
        if (List.exists(Attribute.is_module, attrs)) {
          module_comments := [comment, ...module_comments^];
        },
      comments.by_start_lnum,
    );
    if (List.length(module_comments^) > 1) {
      failwith("More than one @module block is not supported");
    } else {
      List.nth_opt(module_comments^, 0);
    };
  };

  let find_sections = () => {
    let section_comments = ref([]);
    IntMap.iter(
      (_, (_comment, _desc, attrs) as comment) =>
        if (List.exists(Attribute.is_section, attrs)) {
          section_comments := [comment, ...section_comments^];
        },
      comments.by_start_lnum,
    );
    List.rev(section_comments^);
  };
};
