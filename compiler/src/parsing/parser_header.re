open Location;
open Identifier;
open Dyp;
open Parsetree;
open Ast_helper;
open Grain_utils;

/* Used for error messages and as a default, in case anything slips through
   without an explicit loc. */
let first_loc = ref(Location.dummy_loc);
let last_loc = ref(Location.dummy_loc);
let dyp_merge = keep_all;

let last_state_printer = ref(() => ());

let when_debug = (~n=?, thunk) =>
  switch (n) {
  | Some(n) =>
    if (Grain_utils.Config.parser_debug_level^ >= n) {
      thunk();
    }
  | None => ()
  };

let debug_print_state = () => when_debug(last_state_printer^);

let prerr_string = s => when_debug(~n=1, () => Stdlib.prerr_string(s));

let make_line_comment = (source, loc) => {
  let content = String_utils.slice(~first=2, source) |> String.trim;
  Line({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let make_shebang_comment = (source, loc) => {
  let content = String_utils.slice(~first=2, source) |> String.trim;
  Shebang({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let make_block_comment = (source, loc) => {
  let content = String_utils.slice(~first=2, ~last=-2, source) |> String.trim;
  Block({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let make_doc_comment = (source, loc) => {
  let content = String_utils.slice(~first=3, ~last=-2, source) |> String.trim;
  Doc({cmt_content: content, cmt_source: source, cmt_loc: loc});
};

let symbol_rloc = dyp => {
  let ret = {
    loc_start: dyp.symbol_start_pos(),
    loc_end: dyp.symbol_end_pos(),
    loc_ghost: false,
  };
  last_state_printer := (() => dyp.print_state(stderr));
  when_debug(~n=1, last_state_printer^);
  last_loc := ret;
  ret;
};

let symbol_gloc = dyp => {
  let ret = {
    loc_start: dyp.symbol_start_pos(),
    loc_end: dyp.symbol_end_pos(),
    loc_ghost: true,
  };
  last_state_printer := (() => dyp.print_state(stderr));
  when_debug(~n=1, last_state_printer^);
  last_loc := ret;
  ret;
};

let rhs_loc = (dyp, n) => {
  let ret = {
    loc_start: dyp.rhs_start_pos(n),
    loc_end: dyp.rhs_end_pos(n),
    loc_ghost: false,
  };
  last_state_printer := (() => dyp.print_state(stderr));
  when_debug(~n=1, last_state_printer^);
  last_loc := ret;
  ret;
};

let fix_block_mapper = super => {
  open Ast_mapper;
  let expr = (mapper, {pexp_desc, pexp_loc} as e) =>
    switch (pexp_desc) {
    | PExpBlock([]) => super.expr(mapper, e)
    | PExpBlock(elts) =>
      let elts = List.map(mapper.expr(mapper), elts);
      /* Properly nest let bindings */
      let elts =
        List.fold_right(
          ({pexp_desc} as stmt, elts) =>
            switch (pexp_desc) {
            | PExpLet(r, m, binds, _) => [
                {
                  ...stmt,
                  pexp_desc:
                    PExpLet(
                      r,
                      m,
                      binds,
                      {...stmt, pexp_desc: PExpBlock(elts)},
                    ),
                },
              ]
            | _ => [stmt, ...elts]
            },
          elts,
          [],
        );
      {...e, pexp_desc: PExpBlock(elts)};
    | _ => super.expr(mapper, e)
    };
  {...super, expr};
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
  let mapper = default_mapper |> fix_block_mapper |> fix_tyvar_mapper;
  {...prog, statements: List.map(mapper.toplevel(mapper), statements)};
};

let no_record_block = exprs =>
  switch (exprs) {
  | [{pexp_desc: PExpId({txt: IdentName(_)})}] => raise(Dyp.Giveup)
  | _ => ()
  };

let no_brace_expr = expr =>
  switch (expr.pexp_desc) {
  | PExpBlock(_)
  | PExpRecord(_) => raise(Dyp.Giveup)
  | _ => ()
  };

let no_uppercase_ident = expr =>
  switch (expr.pexp_desc) {
  | PExpId({txt: id}) =>
    let first_char = Identifier.last(id).[0];
    if (Char_utils.is_uppercase_letter(first_char)) {
      raise(Dyp.Giveup);
    };
  | _ => ()
  };

let no_array_access = expr =>
  switch (expr.pexp_desc) {
  | PExpArrayGet(_) => raise(Dyp.Giveup)
  | _ => ()
  };

let no_rational_literal = (expr1, expr2) =>
  switch (expr1.pexp_desc, expr2.pexp_desc) {
  | (
      PExpConstant(PConstNumber(PConstNumberInt(_))),
      PExpConstant(PConstNumber(PConstNumberInt(_))),
    ) =>
    raise(Dyp.Giveup)
  | _ => ()
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

let mkid_expr = (dyp, ns) =>
  Exp.ident(~loc=symbol_rloc(dyp), mkid(ns, symbol_rloc(dyp)));

let mkstr = (dyp, s) => mkloc(s, symbol_rloc(dyp));

let make_program = statements => {
  let prog_loc = {
    loc_start: first_loc^.loc_end,
    loc_end: last_loc^.loc_end,
    loc_ghost: false,
  };
  fix_blocks({statements, comments: [], prog_loc});
};

let parse_program = (program, t, lexbuf) => {
  Dyp.dypgen_verbose := Grain_utils.Config.parser_debug_level^;
  first_loc := Location.curr(lexbuf);
  with_default_loc_src(() => last_loc^, () => program(t, lexbuf));
};

let print_syntax_error =
  Printf.(
    Location.(
      fun
      | Syntax_error => {
          debug_print_state();
          Some(errorf(~loc=last_loc^, "Syntax error"));
        }
      | _ => None
    )
  );

let () = {
  Dyp.dypgen_verbose := Grain_utils.Config.parser_debug_level^;
  Location.register_error_of_exn(print_syntax_error);
};
