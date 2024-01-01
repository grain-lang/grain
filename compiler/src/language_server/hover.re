open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;
open Lsp_types;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hoverParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = Protocol.text_document_position_params;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover
module ResponseResult = {
  [@deriving yojson]
  type markup_content = {
    kind: string,
    value: string,
  };

  [@deriving yojson]
  type t = {
    contents: markup_content,
    range: Protocol.range,
  };
};

let send_hover = (~id: Protocol.message_id, ~range: Protocol.range, result) => {
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({
      contents: {
        kind: "markdown",
        value: result,
      },
      range,
    }),
  );
};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let module_lens = (decl: Types.module_declaration) => {
  Document.grain_code_block(Document.print_mod_type(decl));
};

let value_lens = (env: Env.t, ty: Types.type_expr) => {
  Document.print_type(env, ty);
};

let pattern_lens = (p: Typedtree.pattern) => {
  Document.print_type(p.pat_env, p.pat_type);
};

let type_lens = (ty: Typedtree.core_type) => {
  Document.grain_type_code_block(
    Printtyp.string_of_type_scheme(ty.ctyp_type),
  );
};

let declaration_lens = (ident: Ident.t, decl: Types.type_declaration) => {
  Document.grain_type_code_block(
    Printtyp.string_of_type_declaration(~ident, decl),
  );
};

let include_lens = (env: Env.t, path: Path.t) => {
  let header = grain_code_block("module " ++ Path.name(path));
  let decl = Env.find_module(path, None, env);
  let module_decl =
    switch (Modules.get_provides(decl)) {
    | [_, ..._] => Some(module_lens(decl))
    | [] => None
    };
  switch (module_decl) {
  | Some(mod_sig) => markdown_join(header, mod_sig)
  | None => header
  };
};

let exception_declaration_lens =
    (ident: Ident.t, ext: Types.extension_constructor) => {
  grain_type_code_block(
    Printtyp.string_of_extension_constructor(~ident, ext),
  );
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => send_no_result(~id)
  | Some({program, sourcetree}) =>
    let results = Sourcetree.query(params.position, sourcetree);
    switch (results) {
    | [Value({env, value_type, loc}), ..._] =>
      send_hover(
        ~id,
        ~range=Utils.loc_to_range(loc),
        value_lens(env, value_type),
      )
    | [Pattern({pattern}), ..._] =>
      send_hover(
        ~id,
        ~range=Utils.loc_to_range(pattern.pat_loc),
        pattern_lens(pattern),
      )
    | [Type({core_type}), ..._] =>
      send_hover(
        ~id,
        ~range=Utils.loc_to_range(core_type.ctyp_loc),
        type_lens(core_type),
      )
    | [Declaration({ident, decl, loc}), ..._] =>
      send_hover(
        ~id,
        ~range=Utils.loc_to_range(loc),
        declaration_lens(ident, decl),
      )
    | [Exception({ident, ext, loc}), ..._] =>
      send_hover(
        ~id,
        ~range=Utils.loc_to_range(loc),
        exception_declaration_lens(ident, ext),
      )
    | [Module({decl, loc}), ..._] =>
      send_hover(~id, ~range=Utils.loc_to_range(loc), module_lens(decl))
    | [Include({path, loc}), ..._] =>
      let hover_lens =
        try(Some(include_lens(program.env, path))) {
        | Not_found => None
        };
      switch (hover_lens) {
      | Some(lens) => send_hover(~id, ~range=Utils.loc_to_range(loc), lens)
      | None => send_no_result(~id)
      };

    | _ => send_no_result(~id)
    };
  };
};
