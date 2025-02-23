open Location;
open Identifier;
open Parsetree;
open Ast_helper;
open Grain_utils;

let make_line_comment = (source, loc) => {
  let content = String_utils.slice(~first=2, source) |> String.trim;
  Line({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let make_shebang_comment = (source, loc) => {
  let content = String_utils.slice(~first=2, source) |> String.trim;
  Shebang({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let make_block_comment = (source, loc) => {
  let content =
    String_utils.slice(~first=2, ~last=-2, source)
    |> String_utils.deasterisk_each_line
    |> String_utils.trim_each_line;
  Block({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let make_doc_comment = (source, loc) => {
  let content =
    String_utils.slice(~first=3, ~last=-2, source)
    |> String_utils.deasterisk_each_line
    |> String_utils.trim_each_line(~style=String_utils.KeepIndent);
  Doc({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let to_loc = ((loc_start, loc_end)) => {
  {loc_start, loc_end, loc_ghost: false};
};

let mkid = ns => {
  let help = ns => {
    let rec help = (ns, (acc_ident, acc_str)) => {
      let ident =
        Option.fold(
          ~some=i => IdentExternal(i, acc_str),
          ~none=IdentName(acc_str),
          acc_ident,
        );
      switch (ns) {
      | [] => ident
      | [n, ...tl] => help(tl, (Some(ident), n))
      };
    };
    switch (ns) {
    | [] => failwith("Should be impossible")
    | [n, ...tl] => help(tl, (None, n))
    };
  };
  mkloc @@ help(ns);
};

let mkid_expr = (loc, ns) => {
  let loc = to_loc(loc);
  Expression.ident(~loc, ~core_loc=loc, mkid(ns, loc));
};

let mkstr = (loc, s) => mkloc(s, to_loc(loc));

let make_include_ident = ident => {
  switch (ident.txt) {
  | IdentName(name) => name
  | IdentExternal(_) =>
    raise(
      SyntaxError(
        ident.loc,
        "A module include name cannot contain `.` as that would reference a binding within another module.",
      ),
    )
  };
};

let make_include_alias = ident => {
  switch (ident.txt) {
  | IdentName(name) => name
  | IdentExternal(_) =>
    raise(
      SyntaxError(
        ident.loc,
        "A module alias cannot contain `.` as that would reference a binding within another module.",
      ),
    )
  };
};

let make_program = (~loc, ~core_loc, ~attributes, module_name, statements) => {
  // Ensure the program loc starts at the beginning of the file even if
  // there's whitespace or comments
  let loc_start = {...loc.loc_start, pos_lnum: 1, pos_cnum: 0, pos_bol: 0};
  let prog_loc = {...loc, loc_start};

  {
    attributes,
    module_name,
    statements,
    comments: [],
    prog_loc,
    prog_core_loc: core_loc,
  };
};

let parse_program = (program, token, lexbuf) => {
  program(token);
};
