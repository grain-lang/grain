/* This file is mostly copied from OCaml's parsing/location.ml.
   The original copyright notice is reproduced below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Sexplib.Conv;
open Lexing;
open Grain_utils;
/* NOTE: A lot of this file is taken from OCaml's parsing/location.ml.
   Perhaps we should just go ahead and copy the whole thing. */
let absname = ref(false);

let sexp_of_position = (p: position) =>
  Sexplib.Sexp.List([
    Sexplib.Sexp.Atom("position"),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("file"),
      Sexplib.Conv.sexp_of_string(p.pos_fname),
    ]),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("line"),
      Sexplib.Conv.sexp_of_int(p.pos_lnum),
    ]),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("col"),
      Sexplib.Conv.sexp_of_int(p.pos_cnum),
    ]),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("bol"),
      Sexplib.Conv.sexp_of_int(p.pos_bol),
    ]),
  ]);

let position_to_yojson = (p: position): Yojson.Safe.t =>
  `Assoc([
    ("file", `String(p.pos_fname)),
    ("line", `Int(p.pos_lnum)),
    ("col", `Int(p.pos_cnum)),
    ("bol", `Int(p.pos_bol)),
  ]);

let position_of_sexp = (sexp: Sexplib.Sexp.t) =>
  Sexplib.Conv.(
    Sexplib.Sexp.(
      switch (sexp) {
      | Atom(str) => of_sexp_error("position_of_sexp: list needed", sexp)
      | List([Atom("position"), List(sexp_fields)])
          when List.length(sexp_fields) == 4 =>
        let fields =
          List.map(
            fun
            | List([Atom(str), Atom(_) as value]) => (str, value)
            | sexp => of_sexp_error("position_of_sexp: invalid field", sexp),
            sexp_fields,
          );
        let (pos_fname, pos_lnum, pos_cnum, pos_bol) =
          try((
            string_of_sexp(List.assoc("file", fields)),
            int_of_sexp(List.assoc("line", fields)),
            int_of_sexp(List.assoc("col", fields)),
            int_of_sexp(List.assoc("bol", fields)),
          )) {
          | Not_found =>
            of_sexp_error("position_of_sexp: invalid fields", sexp)
          };
        {pos_fname, pos_lnum, pos_cnum, pos_bol};
      | List([Atom("position"), ..._]) =>
        of_sexp_error("position_of_sexp: invalid fields", sexp)
      | List(_) =>
        of_sexp_error("position_of_sexp: invalid s-expression", sexp)
      }
    )
  );

let position_of_yojson = (yj: Yojson.Safe.t): result(position, string) =>
  switch (yj) {
  | `Assoc(contents) =>
    let map = Hashtbl.create(4);
    List.iter(((key, value)) => Hashtbl.add(map, key, value), contents);
    if (!List.for_all(Hashtbl.mem(map), ["file", "line", "col", "bol"])) {
      Result.Error(
        "position_of_yojson: invalid json object: "
        ++ Yojson.Safe.to_string(yj),
      );
    } else {
      let (file, line, col, bol) =
        switch (List.map(Hashtbl.find(map), ["file", "line", "col", "bol"])) {
        | [a, b, c, d] => (a, b, c, d)
        | _ => failwith("impossible")
        };

      let res_map = f => (
        fun
        | Result.Ok(x) => f(x)
        | Result.Error(y) => Result.Error(y)
      );

      switch (file) {
      | `String(pos_fname) =>
        switch (
          List.fold_right(
            ((cur_name, cur), acc) =>
              res_map(
                acc_list =>
                  switch (cur) {
                  | `Int(x) => Result.Ok([x, ...acc_list])
                  | `Intlit(x) => Result.Ok([int_of_string(x), ...acc_list])
                  | _ =>
                    Result.Error(
                      "position_of_yojson '" ++ cur_name ++ "' is not an int",
                    )
                  },
                acc,
              ),
            [("line", line), ("col", col), ("bol", bol)],
            Result.Ok([]),
          )
        ) {
        | Result.Ok([pos_lnum, pos_cnum, pos_bol]) =>
          Result.Ok({pos_fname, pos_lnum, pos_cnum, pos_bol})
        | Result.Ok(_) => failwith("position_of_yojson: impossible")
        | Result.Error(x) => Result.Error(x)
        }
      | _ => Result.Error("position_of_yojson: 'file' is not a string")
      };
    };
  | _ =>
    Result.Error(
      "position_of_yojson: invalid json object: " ++ Yojson.Safe.to_string(yj),
    )
  };

[@deriving (sexp, yojson)]
type t =
  Warnings.loc = {
    loc_start: position,
    loc_end: position,
    loc_ghost: bool,
  };

let in_file = name => {
  let loc = {pos_fname: name, pos_lnum: 1, pos_bol: 0, pos_cnum: (-1)};
  {loc_start: loc, loc_end: loc, loc_ghost: true};
};

let dummy_loc = {loc_start: dummy_pos, loc_end: dummy_pos, loc_ghost: true};

let sexp_of_t = loc =>
  if (loc == dummy_loc) {
    Sexplib.Sexp.Atom("dummy_loc");
  } else {
    sexp_of_t(loc);
  };

let t_of_sexp = sexp =>
  switch (sexp) {
  | Sexplib.Sexp.Atom("dummy_loc") => dummy_loc
  | _ => t_of_sexp(sexp)
  };

let curr = lexbuf => {
  loc_start: lexbuf.lex_start_p,
  loc_end: lexbuf.lex_curr_p,
  loc_ghost: false,
};

let init = (lexbuf, fname) =>
  lexbuf.lex_curr_p = {
    pos_fname: fname,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: 0,
  };

let symbol_rloc = () => {
  loc_start: Parsing.symbol_start_pos(),
  loc_end: Parsing.symbol_end_pos(),
  loc_ghost: false,
};

let symbol_gloc = () => {
  loc_start: Parsing.symbol_start_pos(),
  loc_end: Parsing.symbol_end_pos(),
  loc_ghost: true,
};

let rhs_loc = n => {
  loc_start: Parsing.rhs_start_pos(n),
  loc_end: Parsing.rhs_end_pos(n),
  loc_ghost: false,
};

let input_name = ref("_none_");
let input_lexbuf = ref(None: option(lexbuf));

/* Terminal info */

let status = ref(Terminfo.Uninitialised);

let num_loc_lines = ref(0); /* number of lines already printed after input */

let print_updating_num_loc_lines = (ppf, f, arg) => {
  open Format;
  let out_functions = pp_get_formatter_out_functions(ppf, ());
  let out_string = (str, start, len) => {
    let rec count = (i, c) =>
      if (i == start + len) {
        c;
      } else if (str.[i] == '\n') {
        count(succ(i), succ(c));
      } else {
        count(succ(i), c);
      };
    num_loc_lines := num_loc_lines^ + count(start, 0);
    out_functions.out_string(str, start, len);
  };
  pp_set_formatter_out_functions(ppf, {...out_functions, out_string});
  f(ppf, arg);
  pp_print_flush(ppf, ());
  pp_set_formatter_out_functions(ppf, out_functions);
};

let reset = () => num_loc_lines := 0;

open Format;

let (msg_file, msg_line, msg_chars, msg_char, msg_to, msg_colon) = (
  "File \"",
  "\", line ",
  ", characters ",
  ", character ",
  "-",
  ":",
);

/** Returns (file, line, char) */

let get_pos_info = pos => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
);

let setup_colors = () =>
  Misc.Color.setup @@
  Some(
    if (Grain_utils.Config.color_enabled^) {
      Misc.Color.Auto;
    } else {
      Misc.Color.Never;
    },
  );

let absolute_path = s => {
  open Filename; /* This function could go into Filename */

  let s =
    if (is_relative(s)) {
      concat(Sys.getcwd(), s);
    } else {
      s;
    };
  /* Now simplify . and .. components */
  let rec aux = s => {
    let base = basename(s);
    let dir = dirname(s);
    if (dir == s) {
      dir;
    } else if (base == current_dir_name) {
      aux(dir);
    } else if (base == parent_dir_name) {
      dirname(aux(dir));
    } else {
      concat(aux(dir), base);
    };
  };

  aux(s);
};

let show_filename = file =>
  if (absname^) {
    absolute_path(file);
  } else {
    file;
  };
let print_filename = (ppf, file) => fprintf(ppf, "%s", show_filename(file));

let print_loc = (ppf, loc) => {
  setup_colors();
  let (file, line, startchar) = get_pos_info(loc.loc_start);
  let (_, endline, endchar) = get_pos_info(loc.loc_end);
  /*let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in*/
  fprintf(
    ppf,
    "%s@{<loc>%a%s%i",
    msg_file,
    print_filename,
    file,
    msg_line,
    line,
  );
  if (startchar >= 0) {
    if (line == endline) {
      fprintf(ppf, "%s%i%s%i", msg_chars, startchar, msg_to, endchar);
    } else {
      fprintf(
        ppf,
        "%s%i%sline %i%s%i",
        msg_char,
        startchar,
        msg_to,
        endline,
        msg_char,
        endchar,
      );
    };
  };
  fprintf(ppf, "@}");
};

let default_printer = (ppf, loc) => {
  setup_colors();
  fprintf(ppf, "@{<loc>%a@}%s@,", print_loc, loc, msg_colon);
};

let printer = ref(default_printer);
let print = (ppf, loc) => printer^(ppf, loc);

let error_prefix = "Error";
let warning_prefix = "Warning";

let print_error_prefix = ppf => {
  setup_colors();
  fprintf(ppf, "@{<error>%s@}", error_prefix);
};

let print_compact = (ppf, loc) => {
  let (file, line, startchar) = get_pos_info(loc.loc_start);
  let (_, endline, endchar) = get_pos_info(loc.loc_end);
  fprintf(ppf, "%a:%i", print_filename, file, line);
  if (startchar >= 0) {
    if (line == endline) {
      fprintf(ppf, ",%i--%i", startchar, endchar);
    } else {
      fprintf(ppf, ",%i--%i,%i", startchar, endline, endchar);
    };
  };
};

let print_error = (ppf, loc) =>
  fprintf(ppf, "%a%t:", print, loc, print_error_prefix);

let print_error_cur_file = (ppf, ()) =>
  print_error(ppf, in_file(input_name^));

let default_warning_printer = (loc, ppf, w) =>
  switch (Warnings.report(w)) {
  | `Inactive => ()
  | `Active({Warnings.number, message, is_error, sub_locs}) =>
    setup_colors();
    fprintf(ppf, "@[<v>");
    print(ppf, loc);
    if (is_error) {
      fprintf(
        ppf,
        "%t (%s %d): %s@,",
        print_error_prefix,
        String.uncapitalize_ascii(warning_prefix),
        number,
        message,
      );
    } else {
      fprintf(
        ppf,
        "@{<warning>%s@} %d: %s@,",
        warning_prefix,
        number,
        message,
      );
    };
    List.iter(
      ((loc, msg)) =>
        if (loc != dummy_loc) {
          fprintf(ppf, "  %a  %s@,", print, loc, msg);
        },
      sub_locs,
    );
    fprintf(ppf, "@]");
  };

let warning_printer = ref(default_warning_printer);

let print_warning = (loc, ppf, w) =>
  print_updating_num_loc_lines(ppf, warning_printer^(loc), w);

let formatter_for_warnings = ref(err_formatter);
let prerr_warning = (loc, w) =>
  print_warning(loc, formatter_for_warnings^, w);

let echo_eof = () => {
  print_newline();
  incr(num_loc_lines);
};

type loc('a) = {
  txt: 'a,
  loc: t,
};

let mkloc = (txt, loc) => {txt, loc};
let mknoloc = txt => mkloc(txt, dummy_loc);

type error = {
  loc: t,
  msg: string,
  sub: list(error),
  if_highlight: string /* alternative message if locations are highlighted */
};

let pp_ksprintf = (~before=?, k, fmt) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Misc.Color.set_color_tag_handling(ppf);
  switch (before) {
  | None => ()
  | Some(f) => f(ppf)
  };
  kfprintf(
    _ => {
      pp_print_flush(ppf, ());
      let msg = Buffer.contents(buf);
      k(msg);
    },
    ppf,
    fmt,
  );
};

/* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted */
let print_phantom_error_prefix = ppf =>
  Format.pp_print_as(ppf, String.length(error_prefix) + 2 /* ": " */, "");

let errorf = (~loc=dummy_loc, ~sub=[], ~if_highlight="", fmt) =>
  pp_ksprintf(
    ~before=print_phantom_error_prefix,
    msg => {loc, msg, sub, if_highlight},
    fmt,
  );

let error = (~loc=dummy_loc, ~sub=[], ~if_highlight="", msg) => {
  loc,
  msg,
  sub,
  if_highlight,
};

let error_of_exn: ref(list(exn => option(error))) = (
  ref([]): ref(list(exn => option(error)))
);

let register_error_of_exn = f => error_of_exn := [f, ...error_of_exn^];

exception Already_displayed_error = Warnings.Errors;

let error_of_exn = exn =>
  switch (exn) {
  | Already_displayed_error => Some(`Already_displayed)
  | _ =>
    let rec loop = (
      fun
      | [] => None
      | [f, ...rest] =>
        switch (f(exn)) {
        | Some(error) => Some(`Ok(error))
        | None => loop(rest)
        }
    );

    loop(error_of_exn^);
  };

let rec default_error_reporter = (ppf, {loc, msg, sub, if_highlight}) => {
  fprintf(ppf, "@[<v>%a %s", print_error, loc, msg);
  List.iter(Format.fprintf(ppf, "@,@[<2>%a@]", default_error_reporter), sub);
  fprintf(ppf, "@]");
};

let error_reporter = ref(default_error_reporter);

let report_error = (ppf, err) =>
  print_updating_num_loc_lines(ppf, error_reporter^, err);

let error_of_printer = (loc, print, x) => errorf(~loc, "%a@?", print, x);

let error_of_printer_file = (print, x) =>
  error_of_printer(in_file(input_name^), print, x);

let () =
  register_error_of_exn(
    fun
    | Sys_error(msg) =>
      Some(errorf(~loc=in_file(input_name^), "I/O error: %s", msg))

    /* [This exception was removed in OCaml 4.09, so we need to decide if we want to add back support for it]
       | Misc.HookExnWrapper {error = e; hook_name;
                           hook_info={Misc.sourcefile}} ->
        let sub = match error_of_exn e with
          | None | Some `Already_displayed -> error (Printexc.to_string e)
          | Some (`Ok err) -> err
        in
        Some
          (errorf ~loc:(in_file sourcefile)
             "In hook %S:" hook_name
             ~sub:[sub]) */
    | _ => None,
  );

external reraise: exn => 'a = "%reraise";

let rec report_exception = (ppf, exn) => {
  let rec loop = (n, exn) =>
    switch (error_of_exn(exn)) {
    | None => reraise(exn)
    | Some(`Already_displayed) => ()
    | Some(`Ok(err)) => Format.fprintf(ppf, "@[%a@]@.", report_error, err)
    | exception exn when n > 0 => loop(n - 1, exn)
    };

  loop(10, exn);
};

exception Error(error);

let () =
  register_error_of_exn(
    fun
    | Error(e) => Some(e)
    | _ => None,
  );

let raise_errorf = (~loc=dummy_loc, ~sub=[], ~if_highlight="") =>
  pp_ksprintf(~before=print_phantom_error_prefix, msg =>
    raise(Error({loc, msg, sub, if_highlight}))
  );
