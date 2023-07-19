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
      switch (String_utils.char_at(search_code, offset)) {
      // TODO: Return Proper Inside
      | Some('=') =>
        CompletableExpr(
          String_utils.slice(~first=offset, ~last=index, search_code),
        )
      | Some(':') =>
        CompletableType(
          String_utils.slice(~first=offset, ~last=index, search_code),
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
let get_completions =
    (
      program: Typedtree.typed_program,
      compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      root: list(string),
      current_search: string,
      include_keywords: bool,
      include_modules: bool,
      include_values: bool,
    ) => {
  // Find Keywords
  let keyword_completions =
    if (include_keywords) {
      // TODO: Include all keywords, Maybe figure out if i can grab these from some place
      let keywords = [
        "let",
        "type",
        "module",
        "include",
        "from",
        "record",
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
  // TODO: Get The Current Environment, using the path
  let getEnvironment = (env, path: list(string)) => {
    let rec getModuleEnv = (env, module_path, path: list(string)) => {
      switch (path) {
      | [pathItem] =>
        // Get The Module's Exports
        Some(([], []))
      | [pathItem, ...path] =>
        // Get The Module's Exports
        let provides = Env.find_modtype(module_path, env).mtd_type;
        switch (provides) {
        // Figure out why this can occur
        | None => None
        | Some(TModSignature(provides)) =>
          // Scan the List, try and find module exports
          let needed_module = List.find(provide => true, provides);
          Some(([], []));
        | Some(_) =>
          Trace.log("Completable: Not Found Direct Module Signature");
          None;
        };
      // TODO: I think this is an impossible case
      | [] => None
      };
    };
    // If we are going down to modules we go one way, otherwise we go the other
    Some(
      switch (path) {
      // Base
      | [] =>
        let modules =
          Env.fold_modules(
            (tag, path, decl, acc) => {List.append(acc, [(tag, decl)])},
            None,
            env,
            [],
          );
        let values =
          Env.fold_values(
            (tag, path, decl, acc) => {List.append(acc, [(tag, decl)])},
            None,
            env,
            [],
          );
        (modules, values);
      // We Need to go deeper
      | [baseModule, ...rest] =>
        // TODO: There has to be a better way todo this
        let modules =
          Env.fold_modules(
            (tag, path, decl, acc) =>
              if (String_utils.starts_with(tag, baseModule)) {
                List.append(acc, [path]);
              } else {
                acc;
              },
            None,
            env,
            [],
          );
        switch (modules) {
        // We did not find the module
        | [] => ([], [])
        // We found at least one matching module
        | [mod_path, ..._] =>
          switch (getModuleEnv(env, mod_path, rest)) {
          | None => ([], [])
          | Some(x) => x
          }
        };
      },
    );
  };
  let environment_completions =
    switch (getEnvironment(program.env, root)) {
    | None => []
    | Some((modules, values)) =>
      // TODO: Find Modules
      let module_completions =
        if (include_modules) {
          List.map(
            ((tag, decl)) =>
              {
                label: tag,
                kind: CompletionItemKindModule,
                detail: "",
                documentation: "",
              },
            modules,
          );
        } else {
          [];
        };
      // TODO: Find Values
      let value_completions =
        if (include_values) {
          List.map(
            ((tag, decl: Types.value_description)) =>
              {
                label: tag,
                kind: get_kind(decl.val_type.desc),
                detail: Printtyp.string_of_type_scheme(decl.val_type),
                documentation: "",
              },
            values,
          );
        } else {
          [];
        };
      // Merge Our Results
      List.concat([module_completions, value_completions]);
    };
  // Merge Our Results
  let completions: list(completion_item) =
    List.concat([keyword_completions, environment_completions]);
  // Filter Our Results
  let filtered_completions =
    List.filter(
      ({label}) =>
        String_utils.starts_with(
          StringLabels.lowercase_ascii(label),
          StringLabels.lowercase_ascii(current_search),
        ),
      completions,
    );
  // Return Our Results
  filtered_completions;
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
          switch (List.rev(completionPath)) {
          | []
          | ["", ""] => []
          | [search] =>
            get_completions(
              program,
              compiled_code,
              [],
              search,
              true,
              true,
              true,
            )
          | [search, ...root] =>
            get_completions(
              program,
              compiled_code,
              List.rev(root),
              search,
              false,
              true,
              true,
            )
          };
        // switch (pathRoot) {
        // // Just A .
        // | None => []
        // // A word no dot
        // | Some(true) =>
        //   // TODO: Suggest Keywords
        //   let module_completions =
        //     List.map(
        //       (i: string) => {
        //         let item: completion_item = {
        //           label: i,
        //           kind: CompletionItemKindModule,
        //           detail: "",
        //           documentation: "",
        //         };
        //         item;
        //       },
        //       modules,
        //     );
        //   let values: list((string, Types.value_description)) =
        //     Env.fold_values(
        //       (tag, path, vd, acc) => {List.append(acc, [(tag, vd)])},
        //       None,
        //       program.env,
        //       [],
        //     );
        //   let valueCompletions =
        //     List.map(
        //       ((i: string, l: Types.value_description)) => {
        //         let item: completion_item = {
        //           label: i,
        //           kind: get_kind(l.val_type.desc),
        //           detail: Printtyp.string_of_type_scheme(l.val_type),
        //           documentation: "",
        //         };
        //         item;
        //       },
        //       values,
        //     );
        //   let completions =
        //     List.concat([module_completions, valueCompletions]);
        //   List.filter(
        //     ({label}) =>
        //       String.starts_with(
        //         ~prefix=StringLabels.lowercase_ascii(str),
        //         StringLabels.lowercase_ascii(label),
        //       ),
        //     completions,
        //   );
        // // A Module Path
        // | Some(false) => []
        // };
        | CompletableExpr(str) =>
          Trace.log("Completable: Expr " ++ str);
          // TODO: Build Path
          // TODO: Get Module If Available
          // TODO: Filter Out Operators
          [];
        | CompletableType(str) =>
          Trace.log("Completable: Type " ++ str);
          // TODO: Suggest Type Info
          [];
        }
      };
    send_completion(~id, completions);
  };
};
