open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentSymbolParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentSymbol
module ResponseResult = {
  /** A symbol kind. */
  [@deriving (enum, yojson)]
  type symbol_kind =
    | [@value 1] File
    | [@value 2] Module
    | [@value 3] Namespace
    | [@value 4] Package
    | [@value 5] Class
    | [@value 6] Method
    | [@value 7] Property
    | [@value 8] Field
    | [@value 9] Constructor
    | [@value 10] Enum
    | [@value 11] Interface
    | [@value 12] Function
    | [@value 13] Variable
    | [@value 14] Constant
    | [@value 15] String
    | [@value 16] Number
    | [@value 17] Boolean
    | [@value 18] Array
    | [@value 19] Object
    | [@value 20] Key
    | [@value 21] Null
    | [@value 22] EnumMember
    | [@value 23] Struct
    | [@value 24] Event
    | [@value 25] Operator
    | [@value 26] TypeParameter;
  let symbol_kind_to_yojson = symbol_kind =>
    symbol_kind_to_enum(symbol_kind) |> [%to_yojson: int];
  let symbol_kind_of_yojson = json =>
    Result.bind(json |> [%of_yojson: int], value => {
      switch (symbol_kind_of_enum(value)) {
      | Some(symbol_kind) => Ok(symbol_kind)
      | None => Result.Error("Invalid enum value")
      }
    });

  /**
   * Symbol tags are extra annotations that tweak the rendering of a symbol.
   */
  [@deriving (enum, yojson)]
  type symbol_tag =
    | /**
     * Render a symbol as obsolete, usually using a strike-out.
     */
      [@value 1] Deprecated;
  /**
   * Represents programming constructs like variables, classes, interfaces etc.
   * that appear in a document. Document symbols can be hierarchical and they
   * have two ranges: one that encloses its definition and one that points to its
   * most interesting range, e.g. the range of an identifier.
   */
  [@deriving yojson]
  type document_symbol = {
    /**
     * The name of this symbol. Will be displayed in the user interface and
     * therefore must not be an empty string or a string only consisting of
     * white spaces.
     */
    name: string,
    /**
     * More detail for this symbol, e.g the signature of a function.
     */
    [@default None]
    detail: option(string),
    /** The kind of this symbol. */
    kind: symbol_kind,
    /** Tags for this document symbol. */
    [@default None]
    tags: option(list(symbol_tag)),
    /**
     * The range enclosing this symbol not including leading/trailing whitespace
     * but everything else like comments. This information is typically used to
     * determine if the clients cursor is inside the symbol to reveal it  in the
     * UI.
     */
    range: Protocol.range,
    /**
     * The range that should be selected and revealed when this symbol is being
     * picked, e.g. the name of a function. Must be contained by the `range`.
     */
    [@key "selectionRange"]
    selection_range: Protocol.range,
    /**
     * Children of this symbol, e.g. properties of a class.
     */
    [@default None]
    children: option(list(document_symbol)),
  };
  [@deriving yojson]
  type t = list(document_symbol);
};

// Outline building functions
let rec build_toplevel_outline = (statement: Typedtree.toplevel_stmt) => {
  switch (statement.ttop_desc) {
  | TTopForeign(_)
  | TTopInclude(_)
  | TTopProvide(_)
  | TTopException(_)
  | TTopExpr(_) => None
  | TTopData(data_decls) =>
    Some(
      List.filter_map(
        (data_decl: Typedtree.data_declaration) => {
          let kind: option(ResponseResult.symbol_kind) =
            switch (data_decl.data_kind) {
            | TDataRecord(_) => Some(Struct)
            | TDataVariant(_) => Some(Enum)
            | TDataAbstract => None
            };
          switch (kind) {
          | Some(kind) =>
            Some(
              {
                name: Ident.name(data_decl.data_id),
                detail: None,
                kind,
                tags: None,
                range: Utils.loc_to_range(data_decl.data_loc),
                selection_range: Utils.loc_to_range(data_decl.data_loc),
                children: None,
              }: ResponseResult.document_symbol,
            )
          | None => None
          };
        },
        data_decls,
      ),
    )
  | TTopLet(_, _, binds) =>
    Some(
      List.filter_map(
        (bind: Typedtree.value_binding) => {
          switch (bind.vb_pat.pat_desc) {
          | TPatVar(ident, _) =>
            Some(
              {
                name: Ident.name(ident),
                detail:
                  Some(
                    Printtyp.string_of_type_scheme(bind.vb_expr.exp_type),
                  ),
                kind:
                  switch (bind.vb_expr.exp_desc) {
                  | TExpLambda(_, _) => Function
                  | _ => Variable
                  },
                tags: None,
                range: Utils.loc_to_range(bind.vb_loc),
                selection_range: Utils.loc_to_range(bind.vb_loc),
                children: None,
              }: ResponseResult.document_symbol,
            )
          | _ => None
          }
        },
        binds,
      ),
    )
  | TTopModule(module_decl) =>
    Some([
      (
        {
          name: Ident.name(module_decl.tmod_id),
          detail: None,
          kind: Module,
          tags: None,
          range: Utils.loc_to_range(statement.ttop_loc),
          selection_range: Utils.loc_to_range(statement.ttop_loc),
          children:
            Some(
              List.fold_left(
                (acc, statement) => {
                  switch (build_toplevel_outline(statement)) {
                  | None => acc
                  | Some(symbol) => List.append(acc, symbol)
                  }
                },
                [],
                module_decl.tmod_statements,
              ),
            ),
        }: ResponseResult.document_symbol
      ),
    ])
  };
};

let build_program_outline = (program: Typedtree.typed_program) => {
  [
    (
      {
        name: program.module_name.txt,
        detail: None,
        kind: Module,
        tags: None,
        range: Utils.loc_to_range(program.mod_loc),
        selection_range: Utils.loc_to_range(program.mod_loc),
        children:
          Some(
            List.fold_left(
              (acc, statement) => {
                switch (build_toplevel_outline(statement)) {
                | None => acc
                | Some(symbol) => List.append(acc, symbol)
                }
              },
              [],
              program.statements,
            ),
          ),
      }: ResponseResult.document_symbol
    ),
  ];
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => Protocol.response(~id, `Null)
  | Some({program}) =>
    let outline = build_program_outline(program);
    Protocol.response(~id, ResponseResult.to_yojson(outline));
  };
};
