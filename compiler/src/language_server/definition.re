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

let send_definition =
    (~id: Protocol.message_id, ~range: Protocol.range, ~target: Location.t) => {
  let target_start = target.loc_start;
  let filename = target_start.pos_fname;
  let target_uri = Protocol.filename_to_uri(filename);

  let target_range = loc_to_range(target);

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

let print_loc_string = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = Locations.get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = Locations.get_raw_pos_info(loc.loc_end);

  if (startchar >= 0) {
    if (line == endline) {
      Printf.sprintf("%s %d:%d,%d\n", msg, line, startchar, endchar);
    } else {
      Printf.sprintf(
        "%s %d:%d - %d:%d\n",
        msg,
        line,
        startchar,
        endline,
        endchar,
      );
    };
  } else {
    Printf.sprintf("Empty loc? %s %d:%d,%d\n", msg, line, startchar, endchar);
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  Trace.log("Processing go to definition");

  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => send_no_result(~id)
  | Some({program, sourcetree}) =>
    let results = Sourcetree.query(params.position, sourcetree);

    switch (results) {
    | [Expression(exp, desc), ..._] =>
      Trace.log("is an expression");
      switch (desc) {
      | None =>
        Trace.log("no expression desc");
        send_no_result(~id);
      | Some(d) =>
        send_definition(
          ~id,
          ~range=loc_to_range(exp.exp_loc),
          ~target=d.val_loc,
        )
      };
    | [Pattern(pat), ..._] =>
      Trace.log("is a pattern");
      send_no_result(~id);

    | [Type(ty), ..._] =>
      Trace.log("is a type");

      switch (ty.ctyp_type.desc) {
      | TTyConstr(path, _, _) =>
        Trace.log("is a TTyConstr");

        let search = Env.find_type(path, program.env);
        Trace.log(
          print_loc_string("Type location from search", search.type_loc),
        );

        let loc = search.type_loc;

        // not sure how to get type declaration location

        // let target_loc =

        //  send_definition(
        //   ~id,
        //   ~range=loc_to_range(loc),
        //   ~target=target_loc,
        // )
        send_no_result(~id);

      | _ =>
        Trace.log("Is not a TTyConstr");
        send_no_result(~id);
      };

    | [Declaration(decl), ..._] =>
      Trace.log("is a declaration");
      send_no_result(~id);

    | [Module(path, loc), ..._] =>
      Trace.log("is a module");

      let modsearch = Env.find_module(path, None, program.env);

      let mod_loc = modsearch.md_loc;

      // seems to come back as dummy
      Trace.log(print_loc_string("Module location from search", mod_loc));

      //  send_definition(
      //   ~id,
      //   ~range=loc_to_range(loc),
      //   ~target=mod_loc,
      // )

      send_no_result(~id);

    | _ =>
      Trace.log("nothing we process for definitions");
      send_no_result(~id);
    };
  };
};
