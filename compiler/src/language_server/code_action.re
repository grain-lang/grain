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
  type code_action_context = {diagnostics: list(Protocol.diagnostic)};

  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    range: Protocol.range,
    context: code_action_context,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#locationLink
module ResponseResult = {
  [@deriving yojson]
  type code_action = {
    title: string,
    kind: string,
    edit: Protocol.workspace_edit,
  };

  [@deriving yojson]
  type t = option(list(code_action));
};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let explicit_type_annotation = (range, uri, type_str) => {
  ResponseResult.{
    title: "Annotate type",
    kind: "annotate-type",
    edit: {
      document_changes: [
        {
          text_document: {
            uri,
            version: None,
          },
          edits: [{range, new_text: ": " ++ type_str}],
        },
      ],
    },
  };
};

let named_arg_label = (range, uri, arg_label) => {
  ResponseResult.{
    title: "Use argument label",
    kind: "use-argument-label",
    edit: {
      document_changes: [
        {
          text_document: {
            uri,
            version: None,
          },
          edits: [{range, new_text: arg_label ++ "="}],
        },
      ],
    },
  };
};

let send_code_actions =
    (id: Protocol.message_id, code_actions: list(ResponseResult.code_action)) => {
  Protocol.response(~id, ResponseResult.to_yojson(Some(code_actions)));
};

let rec process_explicit_type_annotation =
        (uri, results: list(Sourcetree.node)) => {
  switch (results) {
  | [
      Pattern({pattern: {pat_extra: [], pat_desc: TPatVar(_)} as pattern}),
      ..._,
    ] =>
    let loc = {...pattern.pat_loc, loc_start: pattern.pat_loc.loc_end};
    let type_str = Printtyp.string_of_type_scheme(pattern.pat_type);
    Some(explicit_type_annotation(Utils.loc_to_range(loc), uri, type_str));
  | [_, ...rest] => process_explicit_type_annotation(uri, rest)
  | _ => None
  };
};

let rec process_named_arg_label = (uri, results: list(Sourcetree.node)) => {
  switch (results) {
  | [Argument({arg_label, label_specified, loc}), ..._] when !label_specified =>
    let loc = {...loc, loc_end: loc.loc_start};
    let arg_label =
      switch (arg_label) {
      | Unlabeled =>
        failwith("Impossible: unlabeled argument after typechecking")
      | Labeled({txt})
      | Default({txt}) => txt
      };
    Some(named_arg_label(Utils.loc_to_range(loc), uri, arg_label));
  | [_, ...rest] => process_named_arg_label(uri, rest)
  | _ => None
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
    let results = Sourcetree.query(params.range.range_start, sourcetree);

    let code_actions =
      List.filter_map(
        x => x,
        [
          process_explicit_type_annotation(params.text_document.uri, results),
          process_named_arg_label(params.text_document.uri, results),
        ],
      );

    switch (code_actions) {
    | [] => send_no_result(~id)
    | _ => send_code_actions(id, code_actions)
    };
  };
};
