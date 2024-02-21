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
type check_position =
  | Forward
  | Backward;
let rec find_definition =
        (
          ~check_position=Forward,
          sourcetree: Sourcetree.sourcetree,
          position: Protocol.position,
        ) => {
  let results = Sourcetree.query(position, sourcetree);

  let result =
    switch (results) {
    | [Value({definition}), ..._]
    | [Pattern({definition}), ..._]
    | [Type({definition}), ..._]
    | [Declaration({definition}), ..._]
    | [Exception({definition}), ..._]
    | [Module({definition}), ..._] =>
      switch (definition) {
      | None => None
      | Some(loc) =>
        let uri = Utils.filename_to_uri(loc.loc_start.pos_fname);
        Some((loc, uri));
      }
    | _ => None
    };
  switch (result) {
  | None =>
    if (check_position == Forward && position.character > 0) {
      // If a user selects from left to right, their pointer ends up after the identifier
      // this tries to check if the identifier was selected.
      find_definition(
        ~check_position=Backward,
        sourcetree,
        {line: position.line, character: position.character - 1},
      );
    } else {
      None;
    }
  | Some((loc, uri)) => result
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
    let result = find_definition(sourcetree, params.position);
    switch (result) {
    | None => send_no_result(~id)
    | Some((loc, uri)) =>
      send_definition(
        ~id,
        ~range=Utils.loc_to_range(loc),
        ~target_uri=uri,
        Utils.loc_to_range(loc),
      )
    };
  };
};
