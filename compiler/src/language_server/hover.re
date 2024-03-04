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
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    position: Protocol.position,
  };
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

// We need to use the "grain-type" markdown syntax to have correct coloring on hover items
let grain_type_code_block = Markdown.code_block(~syntax="grain-type");
// Used for module hovers
let grain_code_block = Markdown.code_block(~syntax="grain");

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

let markdown_join = (a, b) => {
  // Horizonal rules between code blocks render a little funky
  // so we manually add linebreaks
  Printf.sprintf(
    "%s\n---\n<br><br>\n%s",
    a,
    b,
  );
};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let supressed_types = [Builtin_types.path_void, Builtin_types.path_bool];

let print_type = (env, ty) => {
  let instance = grain_type_code_block(Printtyp.string_of_type_scheme(ty));
  try({
    let (path, _, decl) = Ctype.extract_concrete_typedecl(env, ty);
    // Avoid showing the declaration for supressed types
    if (List.exists(
          supressed_type => Path.same(path, supressed_type),
          supressed_types,
        )) {
      raise(Not_found);
    };
    markdown_join(
      grain_code_block(
        Printtyp.string_of_type_declaration(
          ~ident=Ident.create(Path.last(path)),
          decl,
        ),
      ),
      instance,
    );
  }) {
  | Not_found => instance
  };
};

let module_lens = (decl: Types.module_declaration) => {
  let vals = Modules.get_provides(decl);
  let signatures =
    List.map(
      (v: Modules.export) =>
        switch (v.kind) {
        | Function
        | Value => Format.sprintf("let %s", v.signature)
        | Record
        | Enum
        | Abstract
        | Exception => v.signature
        },
      vals,
    );
  grain_code_block(String.concat("\n", signatures));
};

let value_lens = (env: Env.t, ty: Types.type_expr) => {
  print_type(env, ty);
};

let pattern_lens = (p: Typedtree.pattern) => {
  print_type(p.pat_env, p.pat_type);
};

let type_lens = (ty: Typedtree.core_type) => {
  grain_type_code_block(Printtyp.string_of_type_scheme(ty.ctyp_type));
};

let declaration_lens = (ident: Ident.t, decl: Types.type_declaration) => {
  grain_type_code_block(Printtyp.string_of_type_declaration(~ident, decl));
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
