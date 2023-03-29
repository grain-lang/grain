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
      // let process_value_description = (id, instance_ty, ty) => {
      //   // Never consider special idents when deciding to display generalized
      //   // types, i.e. always display instance types for lists
      //   Parsetree.(
      //     Identifier.(
      //       switch (id) {
      //       | {txt: IdentName({txt: "[]" | "[...]"})} => instance_ty
      //       | _ => ty
      //       }
      //     )
      //   );
      // };
      // let enter_expression = exp => {
      //   Parsetree.(
      //     Path.(
      //       switch (exp.exp_desc) {
      //       | TExpIdent(
      //           PExternal(path, _),
      //           {txt: IdentExternal(IdentName({loc}), _)},
      //           desc,
      //         ) =>
      //         segments :=
      //           [
      //             (
      //               loc_to_interval(loc),
      //               Module(
      //                 path,
      //                 Env.find_module(path, None, exp.exp_env),
      //                 loc,
      //               ),
      //             ),
      //             (
      //               loc_to_interval(exp.exp_loc),
      //               Value(exp.exp_env, desc.val_type, exp.exp_loc),
      //             ),
      //             ...segments^,
      //           ]
      //       | TExpIdent(_, id, desc) =>
      //         segments :=
      //           [
      //             (
      //               loc_to_interval(exp.exp_loc),
      //               Value(
      //                 exp.exp_env,
      //                 process_value_description(
      //                   id,
      //                   exp.exp_type,
      //                   desc.val_type,
      //                 ),
      //                 exp.exp_loc,
      //               ),
      //             ),
      //             ...segments^,
      //           ]
      //       | TExpUse(module_, items) =>
      //         segments :=
      //           [
      //             (
      //               loc_to_interval(module_.loc),
      //               Module(
      //                 module_.txt,
      //                 Env.find_module(module_.txt, None, exp.exp_env),
      //                 module_.loc,
      //               ),
      //             ),
      //             (
      //               loc_to_interval(exp.exp_loc),
      //               Value(exp.exp_env, exp.exp_type, exp.exp_loc),
      //             ),
      //             ...switch (items) {
      //                | TUseAll => []
      //                | TUseItems(items) =>
      //                  List.map(
      //                    item => {
      //                      switch (item) {
      //                      | Types.TUseType({name, declaration, loc}) => (
      //                          loc_to_interval(loc),
      //                          Declaration(
      //                            Ident.create(name),
      //                            declaration,
      //                            loc,
      //                          ),
      //                        )
      //                      | TUseModule({name, declaration, loc}) => (
      //                          loc_to_interval(loc),
      //                          Module(
      //                            PIdent(Ident.create(name)),
      //                            declaration,
      //                            loc,
      //                          ),
      //                        )
      //                      | TUseValue({value, loc}) => (
      //                          loc_to_interval(loc),
      //                          Value(exp.exp_env, value.val_type, loc),
      //                        )
      //                      }
      //                    },
      //                    items,
      //                  )
      //                },
      //           ]
      //           @ segments^
      //       | _ =>
      //         segments :=
      //           [
      //             (
      //               loc_to_interval(exp.exp_loc),
      //               Value(exp.exp_env, exp.exp_type, exp.exp_loc),
      //             ),
      //             ...segments^,
      //           ]
      //       }
      //     )
      //   );
      // };
      // let enter_pattern = pat => {
      //   segments :=
      //     [(loc_to_interval(pat.pat_loc), Pattern(pat)), ...segments^];
      // };
      // let enter_core_type = ty => {
      //   segments :=
      //     [(loc_to_interval(ty.ctyp_loc), Type(ty)), ...segments^];
      // };
      // let enter_data_declaration = decl => {
      //   segments :=
      //     [
      //       (
      //         loc_to_interval(decl.data_loc),
      //         Declaration(decl.data_id, decl.data_type, decl.data_loc),
      //       ),
      //       ...segments^,
      //     ];
      // };

      //     type include_declaration = {
      //   pinc_path: loc(string),
      //   pinc_alias: option(loc(string)),
      //   [@sexp_drop_if sexp_locs_disabled]
      //   pinc_loc: Location.t,
      // };

      let enter_toplevel_stmt = (stmt: toplevel_stmt) => {
        switch (stmt.ttop_desc) {
        | TTopInclude(inc) =>
          Trace.log("found an include");
          let path = inc.tinc_path;
          let loc = inc.tinc_loc;

          let name =
            switch (path) {
            | PIdent(i) => i.name
            | _ => ""
            };

          let stmt_loc = stmt.ttop_loc;

          let stmt_end = stmt_loc.loc_end;

          let p: Protocol.position = {
            line: stmt_end.pos_lnum - 1,
            character: stmt_end.pos_cnum - stmt_end.pos_bol + 1 + 1,
          };

          let r: ResponseResult.inlay_hint = {
            label: " : " ++ name,
            position: p,
          };
          hints := [r, ...hints^];
        | _ => ()
        };
      };
    });
  Iterator.iter_typed_program(program);
  //create(segments^);
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
  | None =>
    Trace.log("No compiled code");
    send_no_result(~id);
  | Some({program, sourcetree}) =>
    // let results = Sourcetree.query(params.position, sourcetree);
    //  send_no_result(~id);

    Trace.log("Sending hint result");

    let p: Protocol.position = {line: 2, character: 16};

    let r: ResponseResult.inlay_hint = {label: "inline test", position: p};

    let hints = find_hints(program);

    Trace.log("Sending " ++ string_of_int(List.length(hints)) ++ " hints");

    Protocol.response(~id, ResponseResult.to_yojson(hints));
  };
};
