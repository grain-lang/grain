open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;
open Lsp_types;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#inlayHintParams

module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    range: Protocol.range,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint
module ResponseResult = {
 

  [@deriving yojson]
  type inlay_hint = {
    label: string,
    position: Protocol.position,
  };

  [@deriving yojson]
  type t = list(inlay_hint);

};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {

  Trace.log("Inlay hint request received");
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => 
    Trace.log("No compiled code")
    send_no_result(~id)
  | Some({program, sourcetree}) =>
   // let results = Sourcetree.query(params.position, sourcetree);
  //  send_no_result(~id);

  Trace.log("Sending dumy result")


  let p : Protocol.position = {
    line:1,
    character:1
  }

  let r : ResponseResult.inlay_hint = {
    label: "inline test",
    position: p

  }

  Protocol.response(~id, ResponseResult.to_yojson([r]))

  }
}