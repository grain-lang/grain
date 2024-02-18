open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Grain_parsing;
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

[@deriving (enum, yojson)]
type insert_text_format =
  // Since these are using ppx_deriving enum, order matters
  | [@value 1] InsertTextFormatPlainText
  | InsertTextFormatSnippet;

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

let insert_text_format_to_yojson = value =>
  insert_text_format_to_enum(value) |> [%to_yojson: int];
let insert_text_format_of_yojson = json =>
  Result.bind(json |> [%of_yojson: int], value => {
    switch (insert_text_format_of_enum(value)) {
    | Some(value) => Ok(value)
    | None => Result.Error("Invalid enum value")
    }
  });

[@deriving yojson]
type completion_item = {
  label: string,
  kind: completion_item_kind,
  detail: string,
  [@key "insertText"]
  insert_text: option(string),
  [@key "insertTextFormat"]
  insert_text_format,
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

let send_completion =
    (~id: Protocol.message_id, completions: list(completion_item)) => {
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({isIncomplete: false, items: completions}),
  );
};

// completions helpers
let build_completion =
    (
      ~detail="",
      ~documentation="",
      ~insert_text_format=InsertTextFormatPlainText,
      ~insert_text=?,
      label: string,
      kind: completion_item_kind,
    ) => {
  {label, kind, detail, insert_text, insert_text_format, documentation};
};

// TODO: This is for debugging only
let debug_stringify_tkn = (token: Parser.token) => {
  switch (token) {
  | Parser.RATIONAL(c) => Printf.sprintf("Token.Rational(%s)", c)
  | Parser.NUMBER_INT(c) => Printf.sprintf("Token.NUMBER_INT(%s)", c)
  | Parser.NUMBER_FLOAT(c) => Printf.sprintf("Token.NUMBER_FLOAT(%s)", c)
  | Parser.INT8(c) => Printf.sprintf("Token.INT8(%s)", c)
  | Parser.INT16(c) => Printf.sprintf("Token.INT16(%s)", c)
  | Parser.INT32(c) => Printf.sprintf("Token.INT32(%s)", c)
  | Parser.INT64(c) => Printf.sprintf("Token.INT64(%s)", c)
  | Parser.UINT8(c) => Printf.sprintf("Token.UINT8(%s)", c)
  | Parser.UINT16(c) => Printf.sprintf("Token.UINT16(%s)", c)
  | Parser.UINT32(c) => Printf.sprintf("Token.UINT32(%s)", c)
  | Parser.UINT64(c) => Printf.sprintf("Token.UINT64(%s)", c)
  | Parser.FLOAT32(c) => Printf.sprintf("Token.FLOAT32(%s)", c)
  | Parser.FLOAT64(c) => Printf.sprintf("Token.FLOAT64(%s)", c)
  | Parser.BIGINT(c) => Printf.sprintf("Token.BIGINT(%s)", c)
  | Parser.WASMI32(c) => Printf.sprintf("Token.WASMI32(%s)", c)
  | Parser.WASMI64(c) => Printf.sprintf("Token.WASMI64(%s)", c)
  | Parser.WASMF32(c) => Printf.sprintf("Token.WASMF32(%s)", c)
  | Parser.WASMF64(c) => Printf.sprintf("Token.WASMF64(%s)", c)
  | Parser.LIDENT(c) => Printf.sprintf("Token.LIDENT(%s)", c)
  | Parser.UIDENT(c) => Printf.sprintf("Token.UIDENT(%s)", c)
  | Parser.STRING(c) => Printf.sprintf("Token.STRING(%s)", c)
  | Parser.BYTES(c) => Printf.sprintf("Token.BYTES(%s)", c)
  | Parser.CHAR(c) => Printf.sprintf("Token.CHAR(%s)", c)
  | Parser.LBRACK => "Token.LBRACK"
  | Parser.LBRACKRCARET => "Token.LBRACKRCARET"
  | Parser.RBRACK => "Token.RBRACK"
  | Parser.LPAREN => "Token.LPAREN"
  | Parser.RPAREN => "Token.RPAREN"
  | Parser.LBRACE => "Token.LBRACE"
  | Parser.RBRACE => "Token.RBRACE"
  | Parser.LCARET => "Token.LCARET"
  | Parser.RCARET => "Token.RCARET"
  | Parser.COMMA => "Token.COMMA"
  | Parser.SEMI => "Token.SEMI"
  | Parser.AS => "Token.AS"
  | Parser.THICKARROW => "Token.THICKARROW"
  | Parser.ARROW => "Token.ARROW"
  | Parser.EQUAL => "Token.EQUAL"
  | Parser.GETS => "Token.GETS"
  | Parser.UNDERSCORE => "Token.UNDERSCORE"
  | Parser.COLON => "Token.COLON"
  | Parser.QUESTION => "Token.QUESTION"
  | Parser.DOT => "Token.DOT"
  | Parser.ELLIPSIS => "Token.ELLIPSIS"
  | Parser.ASSERT => "Token.ASSERT"
  | Parser.FAIL => "Token.FAIL"
  | Parser.EXCEPTION => "Token.EXCEPTION"
  | Parser.THROW => "Token.THROW"
  | Parser.TRUE => "Token.TRUE"
  | Parser.FALSE => "Token.FALSE"
  | Parser.VOID => "Token.VOID"
  | Parser.LET => "Token.LET"
  | Parser.MUT => "Token.MUT"
  | Parser.REC => "Token.REC"
  | Parser.IF => "Token.IF"
  | Parser.WHEN => "Token.WHEN"
  | Parser.ELSE => "Token.ELSE"
  | Parser.MATCH => "Token.MATCH"
  | Parser.WHILE => "Token.WHILE"
  | Parser.FOR => "Token.FOR"
  | Parser.CONTINUE => "Token.CONTINUE"
  | Parser.BREAK => "Token.BREAK"
  | Parser.RETURN => "Token.RETURN"
  | Parser.AT => "Token.AT"
  | Parser.INFIX_10(c) => Printf.sprintf("Token.INFIX_10(%s)", c)
  | Parser.INFIX_30(c) => Printf.sprintf("Token.INFIX_30(%s)", c)
  | Parser.INFIX_40(c) => Printf.sprintf("Token.INFIX_40(%s)", c)
  | Parser.INFIX_50(c) => Printf.sprintf("Token.INFIX_50(%s)", c)
  | Parser.INFIX_60(c) => Printf.sprintf("Token.INFIX_60(%s)", c)
  | Parser.INFIX_70(c) => Printf.sprintf("Token.INFIX_70(%s)", c)
  | Parser.INFIX_80(c) => Printf.sprintf("Token.INFIX_80(%s)", c)
  | Parser.INFIX_90(c) => Printf.sprintf("Token.INFIX_90(%s)", c)
  | Parser.INFIX_100(c) => Printf.sprintf("Token.INFIX_100(%s)", c)
  | Parser.INFIX_110(c) => Printf.sprintf("Token.INFIX_110(%s)", c)
  | Parser.INFIX_120(c) => Printf.sprintf("Token.INFIX_120(%s)", c)
  | Parser.PREFIX_150(c) => Printf.sprintf("Token.PREFIX_150(%s)", c)
  | Parser.INFIX_ASSIGNMENT_10(c) =>
    Printf.sprintf("Token.INFIX_ASSIGNMENT_10(%s)", c)
  | Parser.ENUM => "Token.ENUM"
  | Parser.RECORD => "Token.RECORD"
  | Parser.TYPE => "Token.TYPE"
  | Parser.MODULE => "Token.MODULE"
  | Parser.INCLUDE => "Token.INCLUDE"
  | Parser.USE => "Token.USE"
  | Parser.PROVIDE => "Token.PROVIDE"
  | Parser.ABSTRACT => "Token.ABSTRACT"
  | Parser.FOREIGN => "Token.FOREIGN"
  | Parser.WASM => "Token.WASM"
  | Parser.PRIMITIVE => "Token.PRIMITIVE"
  | Parser.AND => "Token.AND"
  | Parser.EXCEPT => "Token.EXCEPT"
  | Parser.FROM => "Token.FROM"
  | Parser.STAR => "Token.STAR"
  | Parser.SLASH => "Token.SLASH"
  | Parser.DASH => "Token.DASH"
  | Parser.PIPE => "Token.PIPE"
  | Parser.EOL => "Token.EOL"
  | Parser.EOF => "Token.EOF"
  | Parser.TRY => "Token.TRY"
  | Parser.CATCH => "Token.CATCH"
  | Parser.COLONCOLON => "Token.COLONCOLON"
  | Parser.MACRO => "Token.MACRO"
  | Parser.YIELD => "Token.YIELD"
  | Parser.FUN => "Token.FUN"
  };
};

let debug_stringify_tkn_loc =
    (token: Parser.token, start_loc: int, end_loc: int) => {
  Printf.sprintf(
    "Token: %s, Start: %d, End: %d",
    debug_stringify_tkn(token),
    start_loc,
    end_loc,
  );
};
// Completion Info

type completion_value =
  | PlainText(string)
  | Snippet(string, string);

let toplevel_keywords = [
  PlainText("exception"),
  PlainText("enum"),
  PlainText("record"),
  PlainText("type"),
  PlainText("module"),
  PlainText("provide"),
  PlainText("abstract"),
  Snippet("include", "include \"$1\"$0"),
  Snippet("from", "from \"$1\" use { $0 }"),
];

let expression_keywods = [
  PlainText("let"),
  Snippet("if", "if ($1) $0"),
  Snippet("match", "match ($1) {\n  $0\n}"),
  Snippet("while", "while ($1) {\n  $0\n}"),
  Snippet("for", "for ($1; $2; $3) {\n  $0\n}"),
];

// context helpers
type lex_token = {
  token: Parser.token,
  start_loc: int,
  end_loc: int,
};

type completable_context =
  | CompletableInclude(string)
  | CompletableStatement(bool)
  | CompletableExpressionWithReturn
  | CompletableExpression
  | CompletableExpressionPath(Path.t, bool)
  | CompletableAs
  | CompletableAfterLet
  | CompletableUnknown;

// TODO: This is only for debugging atm
let print_string_of_context = context => {
  let ctx =
    switch (context) {
    | CompletableInclude(_) => "CompleteInclude"
    | CompletableAs => "CompleteAs"
    | CompletableAfterLet => "CompletableAfterLet"
    | CompletableExpressionWithReturn => "CompletableExpressionWithReturn"
    | CompletableExpression => "CompleteExpression"
    | CompletableExpressionPath(_, _) => "CompleteExpressionPath"
    | CompletableStatement(true) => "CompletableStatement(Module)"
    | CompletableStatement(false) => "CompletableStatement(Statement | value)"
    | CompletableUnknown => "CompleteUnknown"
    };
  Trace.log(Printf.sprintf("Context: %s", ctx));
};

let convert_position_to_offset = (source: string, position: Protocol.position) => {
  let (_, _, offset) =
    List.fold_left(
      ((line_num, col_num, offset), c) =>
        if (line_num == position.line && col_num == position.character) {
          (line_num, col_num, offset);
        } else {
          switch (c) {
          | '\r' => (line_num, 0, offset + 1)
          | '\n' => (line_num + 1, 0, offset + 1)
          | _ => (line_num, col_num + 1, offset + 1)
          };
        },
      (0, 0, 0),
      List.of_seq(String.to_seq(source)),
    );
  offset;
};

let in_range = (range_start: int, range_end: int, pos: int) => {
  range_start < pos && pos < range_end;
};
let after_range = (range_end: int, pos: int) => {
  range_end < pos;
};

let last_token_eq = (token: Parser.token, token_list: list(Parser.token)) => {
  switch (token_list) {
  | [tkn, ..._] => tkn == token
  | [] => false
  };
};

let rec token_non_breaking_lst = (token_list: list(Parser.token)) => {
  switch (token_list) {
  | [Parser.EOF, ...rest]
  | [Parser.EOL, ...rest]
  | [Parser.COMMA, ...rest] => token_non_breaking_lst(rest)
  | [_, ..._] => false
  | [] => true
  };
};

let rec collect_idents =
        (
          acc: option(Path.t),
          last_dot: bool,
          token_list: list(Parser.token),
        ) => {
  switch (token_list) {
  | _ when !last_dot => (acc, false)
  | [Parser.UIDENT(str), ...rest]
  | [Parser.LIDENT(str), ...rest] =>
    let ident =
      switch (acc) {
      | Some(acc) => Path.PExternal(acc, str)
      | None => Path.PIdent(Ident.create(str))
      };
    collect_idents(Some(ident), false, rest);
  | [Parser.DOT, ...rest] => collect_idents(acc, true, rest)
  | [_, ..._]
  | [] => (acc, last_dot)
  };
};

let get_completion_context = (documents, uri, position: Protocol.position) => {
  // try and find the code we are completing in the original source
  switch (Hashtbl.find_opt(documents, uri)) {
  | None => CompletableUnknown
  | Some(source_code) =>
    // Get Document
    let offset = convert_position_to_offset(source_code, position);
    Trace.log(Printf.sprintf("Offset: %d", offset));
    // Collect Tokens until offset
    let lexbuf = Sedlexing.Utf8.from_string(source_code);
    let lexer = Wrapped_lexer.init(lexbuf);
    let token = _ => Wrapped_lexer.token(lexer);
    Lexer.reset();
    let rec get_tokens = (tokens: list(lex_token)) => {
      let (current_tok, start_loc, end_loc) = token();
      let current_token = {
        token: current_tok,
        start_loc: start_loc.pos_cnum,
        end_loc: end_loc.pos_cnum,
      };
      switch (current_tok) {
      | _ when current_token.start_loc > offset => tokens
      | Parser.EOF => [current_token, ...tokens]
      | _ => get_tokens([current_token, ...tokens])
      };
    };
    let tokens =
      try(get_tokens([])) {
      | _ => []
      };
    List.iter(
      current_token => {
        Trace.log(
          Printf.sprintf(
            "Token(%s)",
            debug_stringify_tkn_loc(
              current_token.token,
              current_token.start_loc,
              current_token.end_loc,
            ),
          ),
        )
      },
      tokens,
    );
    // Determine Context
    let rec determine_if_in_block = tokens => {
      switch (tokens) {
      | [{token: Parser.LBRACE, start_loc}, ..._] when start_loc < offset =>
        true
      | [{token: Parser.RBRACE, start_loc}, ..._] when start_loc < offset =>
        false
      | [_, ...rest] => determine_if_in_block(rest)
      | [] => false
      };
    };
    let in_block = determine_if_in_block(tokens);
    let rec build_context =
            (
              ~hit_eol: bool,
              token_list: list(Parser.token),
              tokens: list(lex_token),
            ) => {
      switch (tokens) {
      // TODO: Add a state for when we are at from |
      // TODO: Add a state for when we are at from XXXX use { | }
      // TODO: Add a state for when we are at match (XXXX) { | } <- This could be very useful also could not be
      // TODO: Add a state for type XXXX = |
      // Tokens that we care about
      | [{token: Parser.LET}, ..._]
          when !hit_eol && token_non_breaking_lst(token_list) =>
        CompletableAfterLet
      | [{token: Parser.STRING(_), end_loc}, {token: Parser.INCLUDE}, ..._]
          when
            !hit_eol
            && after_range(end_loc, offset)
            && !last_token_eq(Parser.AS, token_list) =>
        CompletableAs
      | [
          {token: Parser.STRING(str), start_loc, end_loc},
          {token: Parser.INCLUDE},
          ..._,
        ]
          when in_range(start_loc, end_loc, offset) =>
        CompletableInclude(str)
      | [{token: Parser.DOT}, {token: Parser.EOL}, ..._]
          when !hit_eol && !last_token_eq(Parser.DOT, token_list) =>
        // TODO: Support test().label on records somehow
        // TODO: Implement path collection
        let (path, expr_start) = collect_idents(None, true, token_list);
        switch (path) {
        | Some(path) => CompletableExpressionPath(path, expr_start)
        | None => CompletableUnknown
        };
      | [{token: Parser.LIDENT(str), start_loc}, {token: Parser.EOL}, ..._]
          when !hit_eol && start_loc < offset =>
        if (!in_block) {
          CompletableStatement(false);
        } else {
          CompletableExpression;
        }
      | [{token: Parser.UIDENT(_), start_loc}, {token: Parser.EOL}, ..._]
          when !hit_eol && start_loc < offset =>
        if (!in_block) {
          CompletableStatement(true);
        } else {
          CompletableExpression;
        }
      | [{token: Parser.THICKARROW}, ..._] =>
        // TODO: Determine if this is a type or expression
        CompletableExpression
      | [{token: Parser.LPAREN}, ..._]
          when token_non_breaking_lst(token_list) =>
        CompletableExpressionWithReturn
      | [{token: Parser.EQUAL}, ..._]
          when token_non_breaking_lst(token_list) =>
        CompletableExpressionWithReturn
      | [{token: Parser.EOL, start_loc}, ...rest] when start_loc < offset =>
        build_context(~hit_eol=true, [Parser.EOL, ...token_list], rest)
      | [] => CompletableUnknown
      // Most tokens we can skip
      | [tok, ...rest] =>
        build_context(~hit_eol, [tok.token, ...token_list], rest)
      };
    };
    build_context(~hit_eol=false, [], tokens);
  };
};

let rec resolve_type = (type_desc: Types.type_desc) => {
  switch (type_desc) {
  | TTySubst({desc})
  | TTyLink({desc}) => resolve_type(desc)
  | _ => type_desc
  };
};

let build_keyword_completions = (values: list(completion_value)) => {
  List.map(
    keyword =>
      switch (keyword) {
      | PlainText(label) =>
        build_completion(label, CompletionItemKindKeyword)
      | Snippet(label, snippet) =>
        build_completion(
          ~insert_text_format=InsertTextFormatSnippet,
          ~insert_text=snippet,
          label,
          CompletionItemKindKeyword,
        )
      },
    values,
  );
};

let get_expression_completions =
    (desire_non_void: bool, program: option(Typedtree.typed_program)) => {
  // TODO: Consider using source tree to better infer the env
  // builtins
  let builtins = [
    build_completion("Ok", CompletionItemKindEnumMember),
    build_completion("Err", CompletionItemKindEnumMember),
    build_completion("Some", CompletionItemKindEnumMember),
    build_completion("None", CompletionItemKindEnumMember),
    build_completion("true", CompletionItemKindValue),
    build_completion("false", CompletionItemKindValue),
    build_completion("void", CompletionItemKindValue),
  ];
  // values
  let value_completions =
    switch (program) {
    | Some({env}) =>
      Env.fold_values(
        (tag, _, decl, acc) => {
          let (kind, typ) =
            switch (resolve_type(decl.val_type.desc)) {
            | TTyArrow(_, typ, _) => (CompletionItemKindFunction, typ.desc)
            | typ => (CompletionItemKindValue, typ)
            };
          switch (List.of_seq(String.to_seq(tag))) {
          | [
              '$' | '&' | '*' | '/' | '+' | '-' | '=' | '>' | '<' | '^' | '|' |
              '!' |
              '?' |
              '%' |
              ':' |
              '.',
              ..._,
            ] => acc
          | _ =>
            switch (resolve_type(typ)) {
            | TTyConstr(id, _, _)
                when Path.same(id, Builtin_types.path_void) && desire_non_void => acc
            | _ => [
                build_completion(
                  ~detail=Document.print_type_raw(decl.val_type),
                  tag,
                  kind,
                ),
                ...acc,
              ]
            }
          };
        },
        None,
        env,
        [],
      )
    | None => []
    };
  // modules
  let module_completions =
    switch (program) {
    | Some({env}) =>
      Env.fold_modules(
        (tag, _, _, acc) => {
          [
            build_completion(
              ~detail=Printf.sprintf("module %s", tag),
              tag,
              CompletionItemKindModule,
            ),
            ...acc,
          ]
        },
        None,
        env,
        [],
      )
    | None => []
    };
  // merge them all
  List.concat([builtins, value_completions, module_completions]);
};

let get_completions_from_context =
    (context: completable_context, program: option(Typedtree.typed_program)) => {
  // TODO: Consider using the sourcetree to provide some extra env context, thinking type signatures
  switch (context) {
  | CompletableInclude(str) =>
    // TODO: Add all paths in Includes
    // TODO: Add all relative paths
    [build_completion("number", CompletionItemKindFile)]
  | CompletableStatement(true) =>
    switch (program) {
    | Some({env}) =>
      Env.fold_modules(
        (tag, _, _, acc) => {
          [
            build_completion(
              ~detail=Printf.sprintf("module %s", tag),
              tag,
              CompletionItemKindModule,
            ),
            ...acc,
          ]
        },
        None,
        env,
        [],
      )
    | None => []
    }
  | CompletableStatement(false) =>
    let toplevel_completions = build_keyword_completions(toplevel_keywords);
    let expression_completions =
      build_keyword_completions(expression_keywods);
    let value_completions =
      switch (program) {
      | Some({env}) =>
        Env.fold_values(
          (tag, _, decl, acc) => {
            switch (resolve_type(decl.val_type.desc)) {
            | TTyArrow(_, _, _) => [
                build_completion(
                  ~detail=Document.print_type_raw(decl.val_type),
                  tag,
                  CompletionItemKindFunction,
                ),
                ...acc,
              ]
            | _ => acc
            }
          },
          None,
          env,
          [],
        )
      | None => []
      };
    List.concat([
      toplevel_completions,
      expression_completions,
      value_completions,
    ]);
  | CompletableExpressionWithReturn =>
    get_expression_completions(true, program)
  | CompletableExpression =>
    let keyword_completions = build_keyword_completions(expression_keywods);
    let expression_completions = get_expression_completions(false, program);
    List.concat([keyword_completions, expression_completions]);
  | CompletableExpressionPath(path, expr_start) =>
    switch (expr_start) {
    | false =>
      switch (program) {
      | Some({env}) =>
        // TODO: Idk if this is the right function here
        []
      | None => []
      }
    // TODO: Handle this
    | true =>
      Trace.log("Unknown Behaviour of expr.XXX");
      [];
    }
  | CompletableAs => [build_completion("as", CompletionItemKindKeyword)]
  | CompletableAfterLet => [
      build_completion("mut", CompletionItemKindKeyword),
      build_completion("rec", CompletionItemKindKeyword),
    ]
  | CompletableUnknown => []
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  let program =
    switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
    | None => None
    | Some({program}) => Some(program)
    };
  let context =
    get_completion_context(
      documents,
      params.text_document.uri,
      params.position,
    );
  print_string_of_context(context);
  let completions = get_completions_from_context(context, program);
  send_completion(~id, completions);
};
