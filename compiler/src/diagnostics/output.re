[@deriving yojson]
type lsp_error = {
  file: string,
  line: int,
  startchar: int,
  endline: int,
  endchar: int,
  lsp_message: string,
};

[@deriving yojson]
type lsp_warning = {
  file: string,
  line: int,
  startchar: int,
  endline: int,
  endchar: int,
  number: int,
  lsp_message: string,
};

[@deriving yojson]
type lsp_result = {
  errors: list(lsp_error),
  warnings: list(lsp_warning),
  values: list(Lenses.lens_t),
  lenses: list(Lenses.lens_t),
};
let exn_to_lsp_error = (exn: exn): option(lsp_error) => {
  let error = Grain_parsing.Location.error_of_exn(exn);

  switch (error) {
  | Some(err) =>
    switch (err) {
    | `Ok(e) =>
      let (file, line, startchar) =
        Grain_parsing.Location.get_pos_info(e.loc.loc_start);
      let (_, endline, endchar) =
        Grain_parsing.Location.get_pos_info(e.loc.loc_end);
      let error_json: lsp_error = {
        file,
        line,
        startchar,
        endline,
        endchar,
        lsp_message: e.msg,
      };
      Some(error_json);
    | _ => None
    }
  | _ => None
  };
};

// kept lenses as an empty array so older LSPs don't crash

let result_to_json =
    (
      ~errors: list(lsp_error),
      ~warnings: list(lsp_warning),
      ~values: list(Lenses.lens_t),
    ) => {
  let result: lsp_result = {errors, warnings, values, lenses: []};
  Yojson.Basic.pretty_to_string(
    Yojson.Safe.to_basic(lsp_result_to_yojson(result)),
  );
};

let convert_warnings = (warnings_this_run, topLevelFileName) => {
  List.map(
    w => {
      let (loc: Grain_utils.Warnings.loc, warn: Grain_utils.Warnings.t) = w;
      let (file, line, startchar) =
        Grain_parsing.Location.get_pos_info(loc.loc_start);
      let (_, endline, endchar) =
        Grain_parsing.Location.get_pos_info(loc.loc_end);
      let warning: lsp_warning = {
        file,
        line,
        startchar,
        endline,
        endchar,
        number: Grain_utils.Warnings.number(warn),
        lsp_message: Grain_utils.Warnings.message(warn),
      };
      warning;
    },
    warnings_this_run,
  );
};
