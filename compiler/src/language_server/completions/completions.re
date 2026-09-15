open Grain_typed;
open Lsp_types;
open Completion_types;
open Pipeline;
open Request;

module RequestParams = Completion_types.RequestParams;
module ResponseResult = Completion_types.ResponseResult;

let send_completion = (~id: Protocol.message_id, ~is_incomplete, items) =>
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({
      is_incomplete,
      items,
    }),
  );

let module_in_scope = (request: Request.t) => {
  let filename = Utils.uri_to_filename(request.uri);
  let pos: Protocol.position = {
    line: request.position.line,
    character: request.position.character,
  };
  (decl: Types.module_declaration) =>
    decl.md_loc.loc_start.pos_fname != filename
    || loc_ends_before_position(decl.md_loc, pos);
};

let local_candidates = (request: Request.t) =>
  Sources.Locals.local_candidates(
    ~context=request.context,
    request.local_bindings,
  );

let expression_start_expected_type = (request: Request.t) =>
  switch (request.context.keyword_slot) {
  | Some(ExpressionStart) => Request.expected_type(request)
  | _ => None
  };

let keyword_candidates = (request: Request.t) =>
  switch (request.context.keyword_slot) {
  | None => []
  | Some(slot) =>
    let expected_type = expression_start_expected_type(request);
    let env =
      switch (request.typed) {
      | Some({env}) => Some(env)
      | None => None
      };
    Sources.Keywords.keyword_candidates(
      ~context=request.context,
      ~env?,
      ~expected_type?,
      slot,
    );
  };

let let_header_candidates = (request: Request.t) =>
  switch (request.context.keyword_slot) {
  | Some(LetHeader) =>
    (request.context.prefix == "" ? local_candidates(request) : [])
    @ keyword_candidates(request)
  | Some(LetAfterModifier) => local_candidates(request)
  | _ => []
  };

let typed_candidates = (request: Request.t, make_candidates) =>
  switch (request.typed) {
  | None => []
  | Some(typed) => make_candidates(typed)
  };

let type_reference_candidates = (request: Request.t) =>
  switch (request.typed) {
  | None =>
    Sources.Typed_env.env_types(
      ~context=request.context,
      ~include_variants=true,
      Env.initial_env,
    )
  | Some({env, module_names}) =>
    switch (type_reference_qualifier(request)) {
    | Some(qualifier) =>
      Sources.Member_access.type_candidates(
        ~context=request.context,
        ~qualifier,
        env,
      )
    | None =>
      Sources.Typed_env.env_types(
        ~context=request.context,
        ~include_variants=true,
        env,
      )
      @ Sources.Typed_env.env_modules(
          ~context=request.context,
          ~sort_group=Sort_group.ModuleMember,
          ~exclude_module_names=module_names,
          env,
        )
    }
  };

let in_scope_candidates = (request: Request.t) => {
  let expected_type = expression_start_expected_type(request);
  let at_expression_start =
    request.context.keyword_slot == Some(ExpressionStart);
  let locals =
    switch (at_expression_start, expected_type) {
    | (true, Some(_)) => []
    | _ => local_candidates(request)
    };
  let typed =
    typed_candidates(request, ({env, module_names}) =>
      if (at_expression_start) {
        Sources.Typed_env.expression_start_candidates(
          ~context=request.context,
          ~expected_type?,
          ~exclude_module_names=module_names,
          env,
        );
      } else {
        Sources.Typed_env.in_scope_candidates(
          ~context=request.context,
          ~exclude_module_names=module_names,
          env,
        );
      }
    );
  locals @ typed @ keyword_candidates(request);
};

let collect_candidates = (request: Request.t) =>
  switch (request.context.keyword_slot) {
  | Some(AliasKeyword) => keyword_candidates(request)
  | Some(LetHeader)
  | Some(LetAfterModifier) => let_header_candidates(request)
  | _ =>
    switch (request.context.kind) {
    | Suppressed => []
    | DocblockContext =>
      Sources.Docblock.docblock_attribute_candidates(~context=request.context)
    | TypeReference => type_reference_candidates(request)
    | MemberAccess(qualifier) =>
      typed_candidates(request, ({env}) =>
        Sources.Member_access.module_candidates(
          ~context=request.context,
          ~qualifier,
          env,
        )
      )
    | CallArgument =>
      typed_candidates(request, ({env}) =>
        Sources.Call_arguments.argument_candidates(
          ~context=request.context,
          ~env,
          ~callee_fallback=request.callee_fallback,
          Request.application(request),
        )
      )
    | PatternContext =>
      let expected_type = Request.expected_type(request);
      let locals =
        switch (expected_type) {
        | Some(_) => []
        | None => local_candidates(request)
        };
      locals
      @ typed_candidates(request, ({env}) =>
          Sources.Typed_env.pattern_candidates(
            ~context=request.context,
            ~expected_type?,
            env,
          )
        )
      @ keyword_candidates(request);
    | MatchGuardKeyword => keyword_candidates(request)
    | ImportPath =>
      typed_candidates(request, ({env, module_names}) =>
        Sources.Typed_env.import_path_candidates(
          ~context=request.context,
          ~exclude_module_names=module_names,
          ~keep_module=module_in_scope(request),
          env,
        )
      )
    | UseModulePath(qualifier) =>
      typed_candidates(request, ({env}) =>
        Sources.Member_access.use_path_candidates(
          ~context=request.context,
          ~qualifier,
          env,
        )
      )
    | ImportFilePath =>
      Sources.Import_paths.file_path_candidates(
        ~context=request.context,
        ~uri=request.uri,
      )
    | ImportModuleName(import_path) =>
      Sources.Import_paths.include_module_candidates(
        ~context=request.context,
        ~uri=request.uri,
        ~import_path,
      )
    | UseItems(qualifier, slot) =>
      typed_candidates(request, ({env}) =>
        Sources.Member_access.use_item_candidates(
          ~context=request.context,
          ~qualifier,
          ~slot,
          env,
        )
      )
    | UseShape =>
      Sources.Member_access.use_shape_snippet_candidate(
        ~context=request.context,
      )
    | InScope => in_scope_candidates(request)
    }
  };

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      ~parse_trees:
         Hashtbl.t(Protocol.uri, (int, Grain_tree_sitter.parse_tree)),
      params: RequestParams.t,
    ) => {
  let uri = params.text_document.uri;
  switch (Hashtbl.find_opt(documents, uri)) {
  | None => send_completion(~id, ~is_incomplete=false, [])
  | Some(source) =>
    let tree = Grain_tree_sitter.cached_tree(~parse_trees, uri, source);
    let request =
      Request.make(
        ~uri,
        ~lsp_position=params.position,
        ~source,
        ~tree,
        ~compiled_code,
      );
    let candidates = collect_candidates(request);
    let (is_incomplete, items) =
      finalize_candidates(~prefix=request.context.prefix, candidates);
    send_completion(~id, ~is_incomplete, items);
  };
};
