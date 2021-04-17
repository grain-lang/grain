/** External frontend for running the parser. */
open Lexing;
open Location;

type error =
  | AmbiguousParse(list(Parsetree.parsed_program))
  | NoValidParse;

let report_error = ppf =>
  fun
  | AmbiguousParse(parses) =>
    if (Grain_utils.Config.verbose^
        || Grain_utils.Config.parser_debug_level^ > 0) {
      Format.fprintf(
        ppf,
        "The Grain compiler had trouble parsing your program. Here were the potential parses:@\n@[<v>%a@]",
        ppf =>
          List.iter(x =>
            Format.fprintf(
              ppf,
              "@[%s@]%,",
              Sexplib.Sexp.to_string_hum @@
              Parsetree.sexp_of_parsed_program(x),
            )
          ),
        parses,
      );
    } else {
      Format.fprintf(
        ppf,
        "The Grain compiler had trouble parsing your program.",
      );
    }
  | NoValidParse =>
    Format.fprintf(
      ppf,
      "The Grain compiler was unable to parse your program. If you see this message, please file an issue at https://github.com/grain-lang/grain",
    );

exception Error(Location.t, error);

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );

let parse = (~name=?, lexbuf): Parsetree.parsed_program => {
  Option.iter(
    n => {
      lexbuf.lex_curr_p = {...lexbuf.lex_curr_p, pos_fname: n};
      Location.input_name := n;
    },
    name,
  );
  let loc_start =
    Option.fold(
      ~some=n => {...lexbuf.lex_start_p, pos_fname: n},
      ~none=lexbuf.lex_start_p,
      name,
    );
  let loc_end = lexbuf.lex_curr_p;
  let startpos = {loc_start, loc_end, loc_ghost: true};
  switch (Parser.parse_program(Lexer.token, lexbuf)) {
  | [] => raise(Error(startpos, NoValidParse))
  | [(x, _)] => {...x, comments: Lexer.consume_comments()}
  | parses => raise(Error(startpos, AmbiguousParse(List.map(fst, parses))))
  };
};

let scan_for_imports = (~defer_errors=true, filename: string): list(string) => {
  let ic = open_in(filename);
  let lexbuf = Lexing.from_channel(ic);
  try({
    let {Parsetree.comments, Parsetree.statements} = parse(lexbuf);
    let implicit_opens =
      List.map(
        o => {
          switch (o) {
          | Grain_utils.Config.Pervasives_mod => "pervasives"
          | Grain_utils.Config.Gc_mod => "runtime/gc"
          }
        },
        switch (comments) {
        | [Block({cmt_content}), ..._] =>
          Grain_utils.Config.with_inline_flags(
            ~on_error=_ => (),
            cmt_content,
            Grain_utils.Config.get_implicit_opens,
          )
        | _ => Grain_utils.Config.get_implicit_opens()
        },
      );
    let found_imports = ref([]);
    let iter_mod = (self, import) =>
      found_imports := [import.Parsetree.pimp_path.txt, ...found_imports^];
    let iterator = {...Ast_iterator.default_iterator, import: iter_mod};
    List.iter(iterator.toplevel(iterator), statements);
    close_in(ic);
    List.sort_uniq(
      String.compare,
      List.append(implicit_opens, found_imports^),
    );
  }) {
  | e =>
    close_in(ic);
    if (!defer_errors) {
      raise(e);
    };
    []; // <- defer parse error until we try to compile this dependency
  };
};
