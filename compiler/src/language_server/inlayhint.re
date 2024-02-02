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

let build_hint =
    (position: Protocol.position, message: string): ResponseResult.inlay_hint => {
  {label: ": " ++ message, position};
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
          hints := [build_hint(p, name), ...hints^];
        | _ => ()
        };
      };

      let enter_binding = ({vb_pat}: value_binding) => {
        switch (vb_pat.pat_extra) {
        | [] =>
          switch (vb_pat.pat_desc) {
          | TPatVar(_, {loc}) =>
            let bind_end = loc.loc_end;
            let p: Protocol.position = {
              line: bind_end.pos_lnum - 1,
              character: bind_end.pos_cnum - bind_end.pos_bol,
            };
            let typeSignature =
              Printtyp.string_of_type_scheme(vb_pat.pat_type);
            hints := [build_hint(p, typeSignature), ...hints^];
          | _ => ()
          }
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
