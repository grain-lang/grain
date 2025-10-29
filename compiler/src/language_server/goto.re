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
      ~origin_range: Protocol.range,
      ~target_uri: Protocol.uri,
      ~target_range: Protocol.range,
    ) => {
  Protocol.response(
    ~id,
    Protocol.location_link_to_yojson({
      origin_selection_range: origin_range,
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
          get_location:
            list(Sourcetree.node) => option((Location.t, Location.t)),
          sourcetree: Sourcetree.sourcetree,
          position: Protocol.position,
        ) => {
  let results = Sourcetree.query(position, sourcetree);

  let result =
    switch (get_location(results)) {
    | None => None
    | Some((origin_loc, target_loc)) =>
      let uri = Utils.filename_to_uri(target_loc.loc_start.pos_fname);
      Some((origin_loc, target_loc, uri));
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
        {
          line: position.line,
          character: position.character - 1,
        },
      );
    } else {
      None;
    }
  | Some(_) => result
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
            | [Sourcetree.Value({loc, definition: Some(definition)}), ..._]
            | [
                Pattern({
                  pattern: {pat_loc: loc},
                  definition: Some(definition),
                }),
                ..._,
              ]
            | [
                Type({
                  core_type: {ctyp_loc: loc},
                  definition: Some(definition),
                }),
                ..._,
              ]
            | [Declaration({loc, definition: Some(definition)}), ..._]
            | [Exception({loc, definition: Some(definition)}), ..._]
            | [Module({loc, definition: Some(definition)}), ..._] =>
              Some((loc, definition))
            | _ => None
            };
          }
        )
      | TypeDefinition => (
          results => {
            switch (results) {
            | [Value({loc, env, value_type: type_expr}), ..._] =>
              Option.bind(Env.get_type_definition_loc(type_expr, env), l =>
                Some((loc, l))
              )
            | [
                Pattern({
                  pattern: {pat_loc: loc},
                  definition: Some(definition),
                }),
                ..._,
              ] =>
              Some((loc, definition))
            | _ => None
            };
          }
        )
      };

    let result = find_location(get_location, sourcetree, params.position);
    switch (result) {
    | None => send_no_result(~id)
    | Some((origin_loc, target_loc, target_uri)) =>
      send_location_link(
        ~id,
        ~origin_range=Utils.loc_to_range(origin_loc),
        ~target_uri,
        ~target_range=Utils.loc_to_range(target_loc),
      )
    };
  };
};
