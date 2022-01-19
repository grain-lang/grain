open Location;
open Identifier;
open Parsetree;
open Ast_helper;
open Grain_utils;

/* Used for error messages. */
let first_loc = ref(Location.dummy_loc);
let last_loc = ref(Location.dummy_loc);

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
    |> String_utils.trim_each_line;
  Doc({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let to_loc = ((loc_start, loc_end)) => {
  let ret = {loc_start, loc_end, loc_ghost: false};
  last_loc := ret;
  ret;
};

let fix_tyvar_mapper = super => {
  open Ast_mapper;
  open Ast_helper;
  let typ = (mapper, {ptyp_desc, ptyp_loc} as t) =>
    switch (ptyp_desc) {
    | PTyVar(v)
        when
          v != ""
          && (
            switch (v.[0]) {
            | 'A' .. 'Z' => true
            | _ => false
            }
          ) =>
      let id = mkloc(IdentName(v), ptyp_loc);
      {...t, ptyp_desc: PTyConstr(id, [])};
    | _ => super.typ(mapper, t)
    };
  {...super, typ};
};

let fix_blocks = ({statements} as prog) => {
  open Ast_mapper;
  let mapper = default_mapper |> fix_tyvar_mapper;
  {...prog, statements: List.map(mapper.toplevel(mapper), statements)};
};

let is_uppercase_ident = name => {
  Char_utils.is_uppercase_letter(name.[0]);
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

let mkid_expr = (loc, ns) =>
  Exp.ident(~loc=to_loc(loc), mkid(ns, to_loc(loc)));

let mkstr = (loc, s) => mkloc(s, to_loc(loc));

let make_program = statements => {
  let prog_loc = {
    loc_start: first_loc^.loc_end,
    loc_end: last_loc^.loc_end,
    loc_ghost: false,
  };
  fix_blocks({statements, comments: [], prog_loc});
};

let parse_program = (program, t, lexbuf) => {
  first_loc := Location.curr(lexbuf);
  program(t, lexbuf);
};
