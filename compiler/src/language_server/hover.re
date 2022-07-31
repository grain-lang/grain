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

let module_lens = (~program: Typedtree.typed_program, path: Path.t) => {
  let vals = Modules.get_exports(~path, program);
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

let expression_lens =
    (e: Typedtree.expression, desc: option(Types.value_description)) => {
  let ty =
    switch (desc) {
    | Some({val_type}) => val_type
    | None => e.exp_type
    };
  print_type(e.exp_env, ty);
};

let pattern_lens = (p: Typedtree.pattern) => {
  print_type(p.pat_env, p.pat_type);
};

let type_lens = (ty: Typedtree.core_type) => {
  grain_type_code_block(Printtyp.string_of_type_scheme(ty.ctyp_type));
};

let declaration_lens = (decl: Typedtree.data_declaration) => {
  grain_type_code_block(
    Printtyp.string_of_type_declaration(~ident=decl.data_id, decl.data_type),
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
    | [Expression(exp, desc), ..._] =>
      send_hover(
        ~id,
        ~range=loc_to_range(exp.exp_loc),
        expression_lens(exp, desc),
      )
    | [Pattern(pat), ..._] =>
      send_hover(~id, ~range=loc_to_range(pat.pat_loc), pattern_lens(pat))
    | [Type(ty), ..._] =>
      send_hover(~id, ~range=loc_to_range(ty.ctyp_loc), type_lens(ty))
    | [Declaration(decl), ..._] =>
      send_hover(
        ~id,
        ~range=loc_to_range(decl.data_loc),
        declaration_lens(decl),
      )
    | [Module(path, loc), ..._] =>
      send_hover(~id, ~range=loc_to_range(loc), module_lens(~program, path))
    | _ => send_no_result(~id)
    };
  };
};
