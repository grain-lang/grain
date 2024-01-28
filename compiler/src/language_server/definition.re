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

    switch (results) {
    | [Value({definition}), ..._]
    | [Pattern({definition}), ..._]
    | [Type({definition}), ..._]
    | [Declaration({definition}), ..._]
    | [Exception({definition}), ..._]
    | [Module({definition}), ..._] =>
      switch (definition) {
      | None => send_no_result(~id)
      | Some(loc) =>
        let uri = Utils.filename_to_uri(loc.loc_start.pos_fname);

        send_definition(
          ~id,
          ~range=Utils.loc_to_range(loc),
          ~target_uri=uri,
          Utils.loc_to_range(loc),
        );
      }
    | _ => send_no_result(~id)
    };
  };
};
