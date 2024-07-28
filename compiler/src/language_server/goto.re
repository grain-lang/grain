open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;
open Lsp_types;

type goto_request_type =
  | Definition
  | TypeDefinition;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#definitionParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = Protocol.text_document_position_params;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#locationLink
module ResponseResult = {
  [@deriving yojson]
  type t = Protocol.location_link;
};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let send_location_link =
    (
      ~id: Protocol.message_id,
      ~range: Protocol.range,
      ~target_uri: Protocol.uri,
      target_range: Protocol.range,
    ) => {
  Protocol.response(
    ~id,
    Protocol.location_link_to_yojson({
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

let rec find_location =
        (
          ~check_position=Forward,
          get_location: list(Sourcetree.node) => option(Location.t),
          sourcetree: Sourcetree.sourcetree,
          position: Protocol.position,
        ) => {
  let results = Sourcetree.query(position, sourcetree);

  let result =
    switch (get_location(results)) {
    | None => None
    | Some(loc) =>
      let uri = Utils.filename_to_uri(loc.loc_start.pos_fname);
      Some((loc, uri));
    };
  switch (result) {
  | None =>
    if (check_position == Forward && position.character > 0) {
      // If a user selects from left to right, their pointer ends up after the identifier
      // this tries to check if the identifier was selected.
      find_location(
        ~check_position=Backward,
        get_location,
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
      goto_request_type: goto_request_type,
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => send_no_result(~id)
  | Some({program, sourcetree}) =>
    let get_location =
      switch (goto_request_type) {
      | Definition => (
          results => {
            switch (results) {
            | [Sourcetree.Value({definition}), ..._]
            | [Pattern({definition}), ..._]
            | [Type({definition}), ..._]
            | [Declaration({definition}), ..._]
            | [Exception({definition}), ..._]
            | [Module({definition}), ..._] => definition
            | _ => None
            };
          }
        )
      | TypeDefinition => (
          results => {
            switch (results) {
            | [Value({env, value_type: type_expr}), ..._] =>
              Env.get_type_definition_loc(type_expr, env)
            | [Pattern({definition}), ..._] => definition
            | _ => None
            };
          }
        )
      };

    let result = find_location(get_location, sourcetree, params.position);
    switch (result) {
    | None => send_no_result(~id)
    | Some((loc, uri)) =>
      send_location_link(
        ~id,
        ~range=Utils.loc_to_range(loc),
        ~target_uri=uri,
        Utils.loc_to_range(loc),
      )
    };
  };
};
