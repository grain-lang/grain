open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;

// This is the full enumeration of all CompletionItemKind as declared by the language server
// protocol (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind),
// but not all will be used by Grain LSP
[@deriving (enum, yojson)]
type completion_item_kind =
  // Since these are using ppx_deriving enum, order matters
  | [@value 1] CompletionItemKindText
  | CompletionItemKindMethod
  | CompletionItemKindFunction
  | CompletionItemKindConstructor
  | CompletionItemKindField
  | CompletionItemKindVariable
  | CompletionItemKindClass
  | CompletionItemKindInterface
  | CompletionItemKindModule
  | CompletionItemKindProperty
  | CompletionItemKindUnit
  | CompletionItemKindValue
  | CompletionItemKindEnum
  | CompletionItemKindKeyword
  | CompletionItemKindSnippet
  | CompletionItemKindColor
  | CompletionItemKindFile
  | CompletionItemKindReference
  | CompletionItemKindFolder
  | CompletionItemKindEnumMember
  | CompletionItemKindConstant
  | CompletionItemKindStruct
  | CompletionItemKindEvent
  | CompletionItemKindOperator
  | CompletionItemKindTypeParameter;

[@deriving (enum, yojson)]
type completion_trigger_kind =
  // Since these are using ppx_deriving enum, order matters
  | [@value 1] CompletionTriggerInvoke
  | CompletionTriggerCharacter
  | CompletionTriggerForIncompleteCompletions;

let completion_item_kind_to_yojson = severity =>
  completion_item_kind_to_enum(severity) |> [%to_yojson: int];
let completion_item_kind_of_yojson = json =>
  Result.bind(json |> [%of_yojson: int], value => {
    switch (completion_item_kind_of_enum(value)) {
    | Some(severity) => Ok(severity)
    | None => Result.Error("Invalid enum value")
    }
  });

let completion_trigger_kind_to_yojson = kind =>
  completion_trigger_kind_to_enum(kind) |> [%to_yojson: int];
let completion_trigger_kind_of_yojson = json =>
  Result.bind(json |> [%of_yojson: int], value => {
    switch (completion_trigger_kind_of_enum(value)) {
    | Some(kind) => Ok(kind)
    | None => Result.Error("Invalid enum value")
    }
  });

[@deriving yojson]
type completion_item = {
  label: string,
  kind: completion_item_kind,
  detail: string,
  documentation: string,
};

[@deriving yojson({strict: false})]
type completion_context = {
  [@key "triggerKind"]
  trigger_kind: completion_trigger_kind,
  [@key "triggerCharacter"] [@default None]
  trigger_character: option(string),
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    position: Protocol.position,
    [@default None]
    context: option(completion_context),
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
module ResponseResult = {
  [@deriving yojson]
  type t = {
    isIncomplete: bool,
    items: list(completion_item),
  };
};

// maps Grain types to LSP CompletionItemKind
let rec get_kind = (desc: Types.type_desc) =>
  switch (desc) {
  | TTyVar(_) => CompletionItemKindVariable
  | TTyArrow(_) => CompletionItemKindFunction
  | TTyTuple(_) => CompletionItemKindStruct
  | TTyRecord(_) => CompletionItemKindStruct
  | TTyConstr(_) => CompletionItemKindConstructor
  | TTySubst(s) => get_kind(s.desc)
  | TTyLink(t) => get_kind(t.desc)
  | _ => CompletionItemKindText
  };

let send_completion =
    (~id: Protocol.message_id, completions: list(completion_item)) => {
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({isIncomplete: false, items: completions}),
  );
};

module Resolution = {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem
  module RequestParams = {
    // TODO: implement the rest of the fields
    [@deriving yojson({strict: false})]
    type t = {label: string};
  };

  // As per https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
  // If computing full completion items is expensive, servers can additionally provide a handler for
  // the completion item resolve request (‘completionItem/resolve’). This request is sent when a
  // completion item is selected in the user interface.
  let process =
      (
        ~id: Protocol.message_id,
        ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
        ~documents: Hashtbl.t(Protocol.uri, string),
        params: RequestParams.t,
      ) => {
    Trace.log("Completable: Resolution Request Recieved");
    // Right now we just resolve nothing to clear the client's request
    // In future we may want to send more details back with Graindoc details for example
    send_completion(
      ~id,
      [
        {
          label: "testing",
          kind: CompletionItemKindValue,
          detail: "Where does this go deatil",
          documentation: "test item",
        },
      ],
    );
  };
};

type completionState =
  | ComletableCode(string)
  | CompletableExpr(string)
  | CompletableType(string);

let find_completable_state = (documents, uri, positon: Protocol.position) => {
  // try and find the code we are completing in the original source
  switch (Hashtbl.find_opt(documents, uri)) {
  | None => None
  | Some(source_code) =>
    // TODO: We need to handle crlf, and also mutability == bad
    // Get Document
    let lines = String.split_on_char('\n', source_code);
    // TODO: Handle Multiple Lines, so we need to convert to an index
    let index = positon.character;
    // Calculate Current Position
    Trace.log(
      "Completable: Completable Line " ++ List.nth(lines, positon.line),
    );
    // Search File To Grab Context
    let rec searchForContext = (search_code, offset) => {
      // If Last Element is = then we are in expr
      // If Last Element is : then we are in type
      // If Last Element was {
      // If we detect a use before than we are in a from
      // Otherwise we are in a ComletableCode
      // TODO: Support other places, improve this half parser
      switch (String_utils.char_at(search_code, offset)) {
      | Some('=') =>
        CompletableExpr(
          String.trim(
            String_utils.slice(~first=offset + 2, ~last=index, search_code),
          ),
        )
      | Some(':') =>
        CompletableType(
          String.trim(
            String_utils.slice(~first=offset, ~last=index, search_code),
          ),
        )
      // TODO: Search for using statements
      // | Some('{') =>
      //   CompletableExpr(
      //     String_utils.slice(~first=offset, ~last=index, search_code),
      //   )
      | _ when offset <= 0 => ComletableCode(search_code)
      | _ => searchForContext(search_code, offset - 1)
      };
    };
    let context =
      searchForContext(List.nth(lines, positon.line), positon.character);
    Some(context);
  };
};

let build_keyword_completion = keyword => {
  {
    // TODO: Would be better if these were actual snippet completions
    label: keyword,
    kind: CompletionItemKindKeyword,
    detail: "",
    documentation: "",
  };
};
let build_value_completion =
    (~env, (value_name: string, value_desc: Types.value_description)) => {
  {
    label: value_name,
    // TODO: Consider Making This More Fine Grained, based off the type
    kind: CompletionItemKindValue,
    detail: Doc.print_type(env, value_desc.val_type),
    // TODO: Maybe generate grain doc
    documentation: "",
  };
};
let build_module_completion =
    (~env, (module_name: string, mod_desc: Types.module_declaration)) => {
  {
    label: module_name,
    kind: CompletionItemKindModule,
    detail: Doc.print_mod_type(mod_desc),
    // TODO: Maybe generate grain doc
    documentation: "",
  };
};

let build_type_completion =
    (~env, (type_name: string, type_desc: Types.type_declaration)) => {
  {
    label: type_name,
    kind: CompletionItemKindTypeParameter,
    // TODO: Add a kind here
    detail: "",
    // TODO: Maybe generate grain doc
    documentation: "",
  };
};

let rec get_completions =
        (
          ~include_values,
          ~include_types,
          env,
          root_decl: Types.module_declaration,
          path: list(string),
        ) => {
  // Get The Modules Exports
  let provides =
    switch (root_decl.md_type) {
    | TModSignature(provides) =>
      List.map(
        (s: Types.signature_item) => {
          switch (s) {
          // Enabled
          | TSigValue(ident, decl) when include_values =>
            Some(build_value_completion(~env, (ident.name, decl)))
          | TSigType(ident, decl, _) when include_types =>
            Some(build_type_completion(~env, (ident.name, decl)))
          | TSigModule(ident, decl, _) =>
            Some(build_module_completion(~env, (ident.name, decl)))
          // Dissabled
          | TSigValue(_, _)
          | TSigType(_, _, _)
          | TSigTypeExt(_, _, _)
          | TSigModType(_, _) => None
          }
        },
        provides,
      )
    | _ => []
    };
  // Filter
  switch (path) {
  | [_]
  | [] => List.filter_map(x => x, provides)
  | [pathItem, ...path] =>
    // Find the desired module
    let subMod =
      switch (root_decl.md_type) {
      | TModSignature(provides) =>
        List.find_opt(
          (s: Types.signature_item) => {
            switch (s) {
            | TSigModule(ident, decl, _) when ident.name == pathItem => true
            | _ => false
            }
          },
          provides,
        )
      | _ => None
      };
    switch (subMod) {
    | Some(TSigModule(_, decl, _)) =>
      get_completions(env, decl, path, ~include_values, ~include_types)
    | _ => []
    };
  };
};
let get_top_level_completions =
    (
      ~include_keywords,
      ~include_values,
      ~include_types,
      program: Typedtree.typed_program,
      compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      search: list(string),
    ) => {
  // Keyword Completions
  // TODO: Include all keywords, Maybe figure out if i can grab these from some place
  let keyword_completions =
    if (include_keywords) {
      let keywords = [
        "let",
        "type",
        "module",
        "include",
        "from",
        "record",
        "rec",
        "enum",
        "provide",
        "abstract",
        "if",
        "while",
        "for",
      ];
      List.map(build_keyword_completion, keywords);
    } else {
      [];
    };
  // Value Completions
  // TODO: add Compiler Built ins, maybe there is a list somewhere
  let value_completions =
    if (include_values) {
      let values =
        Env.fold_values(
          (tag, path, decl, acc) => {List.append(acc, [(tag, decl)])},
          None,
          program.env,
          [],
        );
      List.map(build_value_completion(~env=program.env), values);
    } else {
      [];
    };
  // Module Completions
  let modules =
    Env.fold_modules(
      (tag, path, decl, acc) => {List.append(acc, [(tag, decl)])},
      None,
      program.env,
      [],
    );
  let module_completions =
    List.map(build_module_completion(~env=program.env), modules);
  // Merge Completions
  let completions =
    List.concat([keyword_completions, value_completions, module_completions]);
  // Find The Top Level Modules
  let completions =
    switch (search) {
    // User Wrote Nothing
    | []
    // User Wrote A Single Word, i.e Top Level Search
    | [_] => completions
    // User Wrote A Path
    | [search, ...path] =>
      Trace.log(Printf.sprintf("Completable: Root %s", search));
      // Find Module Root
      switch (List.find_opt(((mod_name, _)) => mod_name == search, modules)) {
      | Some((_, root_decl)) =>
        get_completions(
          program.env,
          root_decl,
          path,
          ~include_values,
          ~include_types,
        )
      | None => []
      };
    };
  Trace.log(
    Printf.sprintf(
      "Completable: Completions Count %d",
      List.length(completions),
    ),
  );
  completions;
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => send_completion(~id, [])
  | Some({program, sourcetree}) =>
    // Get Completable state and filter
    let completableState =
      find_completable_state(
        documents,
        params.text_document.uri,
        params.position,
      );
    // Collect Completables
    let completions =
      switch (completableState) {
      | None => []
      | Some(completableState) =>
        switch (completableState) {
        | ComletableCode(str) =>
          Trace.log("Completable: Code " ++ str);
          let completionPath = String.split_on_char('.', str);
          get_top_level_completions(
            ~include_keywords=true,
            ~include_values=true,
            ~include_types=false,
            program,
            compiled_code,
            completionPath,
          );
        | CompletableExpr(str) =>
          Trace.log("Completable: Expr " ++ str);
          let completionPath = String.split_on_char('.', str);
          get_top_level_completions(
            ~include_keywords=false,
            ~include_values=true,
            ~include_types=true,
            program,
            compiled_code,
            completionPath,
          );
        | CompletableType(str) =>
          Trace.log("Completable: Type " ++ str);
          let completionPath = String.split_on_char('.', str);
          get_top_level_completions(
            ~include_keywords=false,
            ~include_values=false,
            ~include_types=true,
            program,
            compiled_code,
            completionPath,
          );
        }
      };
    send_completion(~id, completions);
  };
};
