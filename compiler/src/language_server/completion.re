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

type completionState =
  | ComletableCode(string)
  | CompletableExpr(string)
  | CompletableType(string);

let find_completable_state = (documents, uri, positon: Protocol.position) => {
  // try and find the code we are completing in the original source
  switch (Hashtbl.find_opt(documents, uri)) {
  | None => None
  | Some(source_code) =>
    // Get Document
    let source =
      Str.global_replace(Str.regexp({|\r\n|}), {|\n|}, source_code);
    let lines = String.split_on_char('\n', source);
    let index = positon.character;
    // Search File To Grab Context
    let rec searchForContext = (search_code, offset, cut, hasHitSep) => {
      switch (String_utils.char_at(search_code, offset)) {
      // Find Context
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
      // Context Seperators
      | Some('(')
      | Some(',')
      | Some('[') =>
        searchForContext(
          search_code,
          offset - 1,
          hasHitSep ? cut : offset - 1,
          true,
        )
      // Must be something else
      | _ when offset <= 0 => ComletableCode(search_code)
      | _ =>
        searchForContext(
          search_code,
          offset - 1,
          hasHitSep ? cut : offset - 1,
          true,
        )
      };
    };
    let context =
      searchForContext(
        List.nth(lines, positon.line),
        positon.character,
        positon.character,
        false,
      );
    Some(context);
  };
};

let build_keyword_completion = keyword => {
  {
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
    kind: CompletionItemKindValue,
    detail: Doc.print_type(env, value_desc.val_type),
    documentation: "",
  };
};
let build_module_completion =
    (~env, (module_name: string, mod_desc: Types.module_declaration)) => {
  {
    label: module_name,
    kind: CompletionItemKindModule,
    detail: Doc.print_mod_type(mod_desc),
    documentation: "",
  };
};

let build_type_completion =
    (~env, (type_name: string, type_desc: Types.type_declaration)) => {
  {
    label: type_name,
    kind: CompletionItemKindTypeParameter,
    detail: "",
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
  let keyword_completions =
    if (include_keywords) {
      let keywords = [
        "primitive",
        "foreign",
        "wasm",
        "while",
        "for",
        "continue",
        "break",
        "return",
        "if",
        "when",
        "else",
        "include",
        "use",
        "provide",
        "abstract",
        "except",
        "from",
        "type",
        "enum",
        "record",
        "module",
        "let",
        "mut",
        "rec",
        "match",
        "assert",
        "fail",
        "exception",
        "throw",
      ];
      List.map(build_keyword_completion, keywords);
    } else {
      [];
    };
  // Value Completions
  let value_completions =
    if (include_values) {
      let values =
        Env.fold_values(
          (tag, path, decl, acc) => {List.append(acc, [(tag, decl)])},
          None,
          program.env,
          [],
        );
      let builtins = [
        ("None", CompletionItemKindEnumMember),
        ("Some", CompletionItemKindEnumMember),
        ("Ok", CompletionItemKindEnumMember),
        ("Err", CompletionItemKindEnumMember),
        ("true", CompletionItemKindConstant),
        ("false", CompletionItemKindConstant),
        ("void", CompletionItemKindConstant),
      ];
      let builtins =
        List.map(
          ((label, kind)) => {label, kind, detail: "", documentation: ""},
          builtins,
        );
      List.append(
        List.map(build_value_completion(~env=program.env), values),
        builtins,
      );
    } else {
      [];
    };
  // Type Completions
  let type_completions =
    if (include_types) {
      let types =
        Env.fold_types(
          (tag, path, (decl, descr), acc) => {
            List.append(acc, [(tag, decl)])
          },
          None,
          program.env,
          [],
        );
      List.map(build_type_completion(~env=program.env), types);
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
    List.concat([
      keyword_completions,
      value_completions,
      type_completions,
      module_completions,
    ]);
  // Find The Top Level Modules
  let completions =
    switch (search) {
    // User Wrote Nothing
    | []
    // User Wrote A Single Word, i.e Top Level Search
    | [_] => completions
    // User Wrote A Path
    | [search, ...path] =>
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
      }
    };
  // Remove Operators
  let operators = [
    '$',
    '&',
    '*',
    '/',
    '+',
    '-',
    '=',
    '>',
    '<',
    '^',
    '|',
    '!',
    '?',
    '%',
    ':',
    '.',
  ];
  let completions =
    List.filter(
      ({label}: completion_item) =>
        !List.exists(o => String.contains(label, o), operators),
      completions,
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
            ~include_types=false,
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
