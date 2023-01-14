open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;
open Lsp_types;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#definitionParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    position: Protocol.position,
  };
};

// // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
// module ResponseResult = {
//   [@deriving yojson]
//   type t = {
//     uri: Protocol.uri,
//     range: Protocol.range,
//   };
// };

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#locationLink

module ResponseResult = {
  [@deriving yojson]
  type t = {
    [@key "originSelectionRange"]
    origin_selection_range: Protocol.range,
    [@key "targetUri"]
    target_uri: Protocol.uri,
    [@key "targetRange"]
    target_range: Protocol.range,
    [@key "targetSelectionRange"]
    target_selection_range: Protocol.range,
  };
};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let send_definition =
    (
      ~id: Protocol.message_id,
      ~range: Protocol.range,
      ~target_uri: Protocol.uri,
      target_range: Protocol.range,
    ) => {
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({
      origin_selection_range: range,
      target_uri,
      target_range,
      target_selection_range: target_range,
    }),
  );
};

// copied from hover, make a util function
let loc_to_range = (pos: Location.t): Protocol.range => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(pos.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(pos.loc_end);

  {
    range_start: {
      line: startline - 1,
      character: startchar,
    },
    range_end: {
      line: endline - 1,
      character: endchar,
    },
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => send_no_result(~id)
  | Some({program, sourcetree}) =>
    let results = Sourcetree.query(params.position, sourcetree);

    let loc_start: Lexing.position = {
      pos_fname: "",
      pos_lnum: 1,
      pos_bol: 0,
      pos_cnum: 0,
    };

    let loc_end: Lexing.position = {
      pos_fname: "",
      pos_lnum: 1,
      pos_bol: 0,
      pos_cnum: 3,
    };

    let target_location: Location.t = {loc_start, loc_end, loc_ghost: false};

    let target_range = loc_to_range(target_location);

    switch (results) {
    | [Expression(exp, desc), ..._] =>
      send_definition(
        ~id,
        ~range=loc_to_range(exp.exp_loc),
        ~target_uri=params.text_document.uri,
        target_range,
      )
    | [Pattern(pat), ..._] =>
      send_definition(
        ~id,
        ~range=loc_to_range(pat.pat_loc),
        ~target_uri=params.text_document.uri,
        target_range,
      )
    | [Type(ty), ..._] =>
      send_definition(
        ~id,
        ~range=loc_to_range(ty.ctyp_loc),
        ~target_uri=params.text_document.uri,
        target_range,
      )
    | [Declaration(decl), ..._] =>
      send_definition(
        ~id,
        ~range=loc_to_range(decl.data_loc),
        ~target_uri=params.text_document.uri,
        target_range,
      )
    | [Module(path, loc), ..._] =>
      send_definition(
        ~id,
        ~range=loc_to_range(loc),
        ~target_uri=params.text_document.uri,
        target_range,
      )
    | _ => send_no_result(~id)
    };
  };
};
