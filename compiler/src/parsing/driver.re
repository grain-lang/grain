/** External frontend for running the parser. */
open Lexing;
open Location;

let apply_filename_to_lexbuf = (name, lexbuf) => {
  Sedlexing.set_filename(lexbuf, name);
  Location.input_name := name;
};

// This parse is very fast, but cannot report useful errors.
// parse_program_for_syntax_error is 2-5x slower, but can provide proper
// syntax error information.
let parse_program =
  Parser_header.parse_program(
    MenhirLib.Convert.Simplified.traditional2revised(Parser.program),
  );

module E = MenhirLib.ErrorReports;
module L = MenhirLib.LexerUtil;
module I = UnitActionsParser.MenhirInterpreter;

let env = checkpoint =>
  switch (checkpoint) {
  | I.HandlingError(env) => env
  | _ =>
    failwith(
      "Impossible: Non- error handling state when handling parser error",
    )
  };

let state = checkpoint =>
  switch (I.top(env(checkpoint))) {
  | Some(I.Element(s, _, _, _)) => I.number(s)
  | None =>
    // The parser hasn't left its initial state. The state number is probably
    // zero, but it's not guaranteed. A future version of Menhir is supposed
    // to provide a real solution for this.
    0
  };

let extract = (text, (pos1, pos2)) => {
  Grain_utils.String_utils.Utf8.sub(
    text,
    pos1.pos_cnum,
    pos2.pos_cnum - pos1.pos_cnum,
  );
};

let show = (text, positions) =>
  extract(text, positions) |> E.sanitize |> E.compress |> E.shorten(20);

let get = (text, checkpoint, i) =>
  switch (I.get(i, env(checkpoint))) {
  | Some(I.Element(_, _, pos1, pos2)) => show(text, (pos1, pos2))
  | None => failwith("Impossible: Syntax error outside of program")
  };

let succeed = _v =>
  failwith(
    "Impossible: Successful parse by slow parser after unsuccessful parse by fast parser",
  );

let fail = (text, buffer, checkpoint: I.checkpoint(_)) => {
  /* Indicate where in the input file the error occurred. */
  let (loc_start, loc_end) = E.last(buffer);
  let location = {loc_start, loc_end, loc_ghost: false};
  /* Show the tokens just before and just after the error. */
  let indication =
    Printf.sprintf("Syntax error %s.\n", E.show(show(text), buffer));
  /* Fetch an error message from the database. */
  let message = Parser_messages.message(state(checkpoint));
  /* Expand away the $i keywords that might appear in the message. */
  let message = E.expand(get(text, checkpoint), message);
  /* Show these three components. */
  raise(
    Ast_helper.SyntaxError(
      location,
      Printf.sprintf("%s%s%!", indication, message),
    ),
  );
};

// This parser is only meant to be invoked when you know a parse error
// will occur
let parse_program_for_syntax_error = (~name=?, lexbuf, source) => {
  Sedlexing.set_position(lexbuf, Location.start_pos);
  Option.iter(n => apply_filename_to_lexbuf(n, lexbuf), name);
  /* Allocate and initialize a lexing buffer. */
  /* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. */
  let lexer = Wrapped_lexer.init(lexbuf);
  let supplier = _ => Wrapped_lexer.token(lexer);
  /* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. */
  let (buffer, supplier) = E.wrap_supplier(supplier);
  /* Fetch the parser's initial checkpoint. */
  let (initial_pos, _) = Sedlexing.lexing_positions(lexbuf);
  let checkpoint = UnitActionsParser.Incremental.program(initial_pos);
  /* Run the parser. */
  /* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. */
  I.loop_handle(succeed, fail(source, buffer), supplier, checkpoint);
};

let cached_parsetrees = Hashtbl.create(64);
let reset = () => Hashtbl.clear(cached_parsetrees);

let get_cached_parsetree = name => {
  Option.fold(~none=None, ~some=Hashtbl.find_opt(cached_parsetrees), name);
};

let parse = (~name=?, lexbuf, source): Parsetree.parsed_program => {
  switch (get_cached_parsetree(name)) {
  | Some(cached) => cached
  | None =>
    Sedlexing.set_position(lexbuf, Location.start_pos);
    Option.iter(n => apply_filename_to_lexbuf(n, lexbuf), name);
    let lexer = Wrapped_lexer.init(lexbuf);
    let token = _ => Wrapped_lexer.token(lexer);
    Lexer.reset();
    let program =
      try({
        ...parse_program(token, lexbuf),
        comments: Lexer.consume_comments(),
      }) {
      | Sedlexing.MalFormed =>
        raise(Ast_helper.BadEncoding(Location.curr(lexbuf)))
      | Parser.Error =>
        // Fast parse failed, so now we do a slow, thoughtful parse to produce a
        // good error message.
        let source = source();
        ignore @@
        parse_program_for_syntax_error(
          ~name?,
          Sedlexing.Utf8.from_string(source),
          source,
        );
        // This should never be hit, but if it does someone will see and report
        failwith("Impossible: Program with syntax error raised no error");
      };
    switch (name) {
    | Some(name) => Hashtbl.add(cached_parsetrees, name, program)
    | None => ()
    };
    program;
  };
};

let read_imports = (program: Parsetree.parsed_program) => {
  open Parsetree_iter;

  let module_has_attr = name =>
    List.exists(
      attr => attr.Asttypes.attr_name.txt == name,
      program.attributes,
    );
  let implicit_opens =
    List.map(
      o => {
        switch (o) {
        | Grain_utils.Config.Pervasives_mod =>
          Location.mknoloc("pervasives.gr")
        | Grain_utils.Config.Gc_mod => Location.mknoloc("runtime/gc.gr")
        }
      },
      Grain_utils.Config.with_attribute_flags(
        ~no_pervasives=module_has_attr("noPervasives"),
        ~runtime_mode=module_has_attr("runtimeMode"),
        Grain_utils.Config.get_implicit_opens,
      ),
    );
  let found_includes = ref([]);

  let enter_include = inc => {
    found_includes := [inc.Parsetree.pinc_path, ...found_includes^];
  };

  iter_parsed_program({...default_hooks, enter_include}, program);

  List.sort_uniq(
    (a, b) => String.compare(a.txt, b.txt),
    List.append(implicit_opens, found_includes^),
  );
};

let scan_for_imports =
    (~defer_errors=true, filename: string): list(loc(string)) => {
  let ic = open_in(filename);
  let lexbuf = Sedlexing.Utf8.from_channel(ic);
  try({
    let source = () => {
      let ic = open_in_bin(filename);
      let source = really_input_string(ic, in_channel_length(ic));
      close_in(ic);
      source;
    };
    let prog = parse(~name=filename, lexbuf, source);
    close_in(ic);
    read_imports(prog);
  }) {
  | e =>
    close_in(ic);
    if (!defer_errors) {
      raise(e);
    };
    []; // <- defer parse error until we try to compile this dependency
  };
};

let print_syntax_error =
  Printf.(
    Location.(
      fun
      | Ast_helper.BadEncoding(loc) =>
        Some(errorf(~loc, "Grain programs must be UTF-8 encoded."))
      | Ast_helper.SyntaxError(loc, msg) => {
          Some(errorf(~loc, "%s", msg));
        }
      | _ => None
    )
  );

let _ = Location.register_error_of_exn(print_syntax_error);
