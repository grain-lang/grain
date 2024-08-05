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

let rec resolve_typ = (typ: Types.type_expr) => {
  switch (typ.desc) {
  | TTyLink(type_expr)
  | TTySubst(type_expr) => resolve_typ(type_expr)
  | _ => typ
  };
};
let string_of_typ = (typ: Types.type_expr) => {
  Printtyp.string_of_type_scheme(resolve_typ(typ));
};

let is_func_typ = (typ: Types.type_expr) => {
  switch (resolve_typ(typ).desc) {
  | TTyArrow(_, _, _) => true
  | _ => false
  };
};

let find_hints = (~toggle_type_hints: bool, program) => {
  let hints = ref([]);
  open Typedtree;
  open Protocol;
  module Iterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;

      let enter_expression = ({exp_desc, exp_type}: expression) =>
        if (toggle_type_hints) {
          switch (exp_desc) {
          | TExpLambda(bindings, _) =>
            List.iter(
              ({mb_pat, mb_loc}: match_branch) => {
                switch (mb_pat.pat_desc) {
                | TPatTuple(args) =>
                  switch (resolve_typ(exp_type).desc) {
                  | TTyArrow(typ_args, _, _) =>
                    let argument_typs =
                      List.map(
                        ((arg, typ: Types.type_expr)) =>
                          switch (arg) {
                          | Default(_) => None
                          | _ => Some(typ)
                          },
                        typ_args,
                      );
                    w;
                    if (List.length(argument_typs) == List.length(args)) {
                      List.iter(
                        ((arg: pattern, typ: option(Types.type_expr))) => {
                          switch (arg.pat_desc, typ) {
                          | (TPatVar(_, _), Some(typ)) =>
                            let bind_end = arg.pat_loc.loc_end;
                            let p: Protocol.position = {
                              line: bind_end.pos_lnum - 1,
                              character: bind_end.pos_cnum - bind_end.pos_bol,
                            };
                            let typeSignature = string_of_typ(typ);
                            hints :=
                              [build_hint(p, typeSignature), ...hints^];
                          | _ => ()
                          }
                        },
                        List.combine(args, argument_typs),
                      );
                    };
                  | _ => ()
                  }
                | _ => ()
                }
              },
              bindings,
            )
          | _ => ()
          };
        };

      let enter_binding = ({vb_pat, vb_expr}: value_binding, toplevel: bool) =>
        if (!toplevel && toggle_type_hints) {
          switch (vb_pat) {
          | {pat_extra: [], pat_desc: TPatVar(_, {loc})} =>
            let bind_end = loc.loc_end;
            let p: Protocol.position = {
              line: bind_end.pos_lnum - 1,
              character: bind_end.pos_cnum - bind_end.pos_bol,
            };
            let typ = vb_pat.pat_type;
            if (!is_func_typ(typ)) {
              let typeSignature = string_of_typ(typ);
              hints := [build_hint(p, typeSignature), ...hints^];
            };
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
      ~toggle_type_hints: bool,
      params: RequestParams.t,
    ) => {
  Trace.log("Inlay hint request received");
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => send_no_result(~id)
  | Some({program, sourcetree}) =>
    let hints = find_hints(~toggle_type_hints, program);
    Protocol.response(~id, ResponseResult.to_yojson(hints));
  };
};
