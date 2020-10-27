type lsp_error = {
  file: string,
  line: int,
  startchar: int,
  endline: int,
  endchar: int,
  lsp_message: string,
};

let lsp_error_to_yojson = (e: lsp_error): Yojson.t =>
  `Assoc([
    ("file", `String(e.file)),
    ("line", `Int(e.line)),
    ("startchar", `Int(e.startchar)),
    ("endline", `Int(e.endline)),
    ("endchar", `Int(e.endchar)),
    ("lsp_message", `String(e.lsp_message)),
  ]);

/* output error in a format friendly for LSP processing */
// let error_to_json = ({loc, msg, sub, if_highlight}) => {
//   let (file, line, startchar) =
//     Grain_parsing.Location.get_pos_info(loc.loc_start);
//   let (_, endline, endchar) =
//     Grain_parsing.Location.get_pos_info(loc.loc_end);

//   let error_json: lsp_error = {
//     file,
//     line,
//     startchar,
//     endline,
//     endchar,
//     lsp_message: msg,
//   };

//   lsp_error_to_yojson(error_json);
// };

/* lsp - print error to stdout */
//let rec print_exception = exn => {
//   let rec loop = (n, exn) =>
//     switch (error_of_exn(exn)) {
//     | None => ""
//     | Some(`Already_displayed) => ""
//     | Some(`Ok(err)) => error_to_json(err)
//     | exception exn when n > 0 => loop(n - 1, exn)
//     };
//   loop(10, exn);
// };

//let exnToJson = (exn) =>

let result_to_yojson =
    (~errors: list(lsp_error), ~lenses: list(Lenses.lens_t)): Yojson.t =>
  `Assoc([
    ("lenses", `List(List.map(l => Lenses.lens_to_yojson(l), lenses))),
    ("errors", `List(List.map(e => lsp_error_to_yojson(e), errors))),
  ]);

let result_to_json =
    (~errors: list(lsp_error), ~lenses: list(Lenses.lens_t)) =>
  Yojson.to_string(result_to_yojson(~errors, ~lenses));
