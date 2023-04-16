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

let find_hints = program => {
  let hints = ref([]);
  open Typedtree;
  open Protocol;
  module Iterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;
      let enter_toplevel_stmt = (stmt: toplevel_stmt) => {
        switch (stmt.ttop_desc) {
        | TTopInclude(inc) =>
          let name = Path.name(inc.tinc_path);

          let stmt_loc = stmt.ttop_loc;
          let stmt_end = stmt_loc.loc_end;

          let p: Protocol.position = {
            line: stmt_end.pos_lnum - 1,
            character: stmt_end.pos_cnum - stmt_end.pos_bol + 1 + 1,
          };

          let r: ResponseResult.inlay_hint = {
            label: ": " ++ name,
            position: p,
          };
          hints := [r, ...hints^];
        | _ => ()
        };
      };
    });
  Iterator.iter_typed_program(program);
  hints^;
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
  | None => send_no_result(~id)
  | Some({program, sourcetree}) =>
    let hints = find_hints(program);
    Protocol.response(~id, ResponseResult.to_yojson(hints));
  };
};
