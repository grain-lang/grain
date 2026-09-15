open Grain_typed;
open Protocol;
open Sourcetree;
open Grain_tree_sitter;
open Syntax.Grammar;

type compiler_query = {
  env: Env.t,
  module_names: list(string),
  sourcetree: Sourcetree.sourcetree,
  sourcetree_results: list(Sourcetree.node),
};

type t = {
  uri,
  position,
  source: string,
  tree: parse_tree,
  context: Syntax.Types.t,
  local_bindings: list(Syntax.Types.local_binding),
  callee_fallback: option(string),
  typed: option(compiler_query),
};

let value_env_of_results = (results: list(Sourcetree.node)) =>
  List.find_map(
    fun
    | Sourcetree.Value({env}) => Some(env)
    | _ => None,
    results,
  );

let application_env_of_results = (results: list(Sourcetree.node)) =>
  List.find_map(
    fun
    | Sourcetree.Application({fun_expr}) => Some(fun_expr.Typedtree.exp_env)
    | _ => None,
    results,
  );

let pattern_env_of_results = (results: list(Sourcetree.node)) =>
  List.find_map(
    fun
    | Sourcetree.Pattern({env}) => Some(env)
    | _ => None,
    results,
  );

let env_from_results = (~results: list(Sourcetree.node)) =>
  switch (
    value_env_of_results(results),
    application_env_of_results(results),
    pattern_env_of_results(results),
  ) {
  | (Some(env), _, _)
  | (_, Some(env), _)
  | (_, _, Some(env)) => Some(env)
  | (None, None, None) => None
  };

let module_name_from_declaration = (tree: parse_tree, node: Tree.node) =>
  switch (Tree.node_child_by_field_name(node, Field.name)) {
  | Some(name) => Some(Tree.node_text(tree, name))
  | None => None
  };

let compare_positions = (a: Protocol.position, b: Protocol.position) =>
  if (a.line == b.line) {
    Stdlib.compare(a.character, b.character);
  } else {
    Stdlib.compare(a.line, b.line);
  };

let position_of_lexing_pos = (lexing_pos): Protocol.position => {
  let (_, line, character) = Grain_parsing.Location.get_pos_info(lexing_pos);
  {
    line: line - 1,
    character,
  };
};

let loc_start_position = (loc: Grain_parsing.Location.t) =>
  position_of_lexing_pos(loc.loc_start);

let loc_end_position = (loc: Grain_parsing.Location.t) =>
  position_of_lexing_pos(loc.loc_end);

let loc_contains_position =
    (loc: Grain_parsing.Location.t, pos: Protocol.position) =>
  compare_positions(loc_start_position(loc), pos) <= 0
  && compare_positions(pos, loc_end_position(loc)) <= 0;

let loc_ends_before_position =
    (loc: Grain_parsing.Location.t, pos: Protocol.position) =>
  compare_positions(loc_end_position(loc), pos) <= 0;

let enclosing_module_names_from_syntax = (tree: parse_tree, node: Tree.node) => {
  let rec collect = (node, acc) => {
    let acc =
      if (Tree.node_kind(node) == Kind.module_declaration) {
        switch (module_name_from_declaration(tree, node)) {
        | Some(name) => [name, ...acc]
        | None => acc
        };
      } else {
        acc;
      };
    switch (Tree.node_parent(node)) {
    | Some(parent) => collect(parent, acc)
    | None => acc
    };
  };
  List.rev(collect(node, []));
};

let rec enclosing_modules_in_statements = (pos: Protocol.position, statements) =>
  List.find_map(
    (stmt: Typedtree.toplevel_stmt) =>
      switch (stmt.ttop_desc) {
      | TTopModule(decl) when loc_contains_position(decl.tmod_loc, pos) =>
        Some([
          decl,
          ...enclosing_modules_in_statements(pos, decl.tmod_statements),
        ])
      | _ => None
      },
    statements,
  )
  |> Option.value(~default=[]);

let apply_toplevel_stmt = (env, stmt: Typedtree.toplevel_stmt) =>
  switch (stmt.ttop_desc) {
  | TTopForeign(desc) => Env.add_value(desc.tvd_id, desc.tvd_val, env)
  | TTopData(decls) =>
    List.fold_left(
      (env, decl: Typedtree.data_declaration) =>
        Env.add_type(~check=false, decl.data_id, decl.data_type, env),
      env,
      decls,
    )
  | TTopModule(decl) =>
    Env.add_module_declaration(
      ~check=false,
      decl.tmod_id,
      decl.tmod_decl,
      env,
    )
  | TTopException(ext) =>
    Env.add_extension(~check=false, ext.ext_id, ext.ext_type, env)
  | _ => env
  };

let env_for_module_body = (~base_env, pos: Protocol.position, decl) => {
  let rec loop = (env, statements) =>
    switch (statements) {
    | [] => env
    | [stmt, ...rest] =>
      if (loc_contains_position(stmt.Typedtree.ttop_loc, pos)) {
        stmt.ttop_env;
      } else if (loc_ends_before_position(stmt.Typedtree.ttop_loc, pos)) {
        loop(apply_toplevel_stmt(env, stmt), rest);
      } else {
        env;
      }
    };
  loop(base_env, decl.Typedtree.tmod_statements);
};

let make =
    (
      ~uri,
      ~lsp_position: Protocol.position,
      ~source,
      ~tree,
      ~compiled_code: Hashtbl.t(uri, Lsp_types.code),
    ) => {
  let ts_pos: Grain_tree_sitter.position = {
    line: lsp_position.line,
    character: lsp_position.character,
  };
  let context = Syntax.At_point.at(tree, ts_pos);
  let local_bindings = Syntax.Bindings.local_bindings_before(tree, ts_pos);
  let callee_fallback = Syntax.Calls.callee_name_before_call(tree, ts_pos);
  let typed =
    switch (Hashtbl.find_opt(compiled_code, uri)) {
    | None => None
    | Some({program: compiled_program, sourcetree}) =>
      let sourcetree_results =
        Sourcetree.query(lsp_position: Protocol.position, sourcetree);
      let enclosing_modules =
        enclosing_modules_in_statements(
          lsp_position,
          compiled_program.statements,
        );
      let node = Syntax.Util.node_at_cursor(tree, ts_pos);
      let enclosing_module_names =
        enclosing_module_names_from_syntax(tree, node);
      let in_module_toplevel =
        Syntax.Keyword_slots.Statements.in_module_toplevel_statement(node);
      let result_env = env_from_results(~results=sourcetree_results);
      let env = Option.value(~default=compiled_program.env, result_env);
      let env =
        if (Syntax.Keyword_slots.Match_slot.cursor_in_when_guard_expression(
              source,
              ts_pos,
              node,
            )) {
          value_env_of_results(sourcetree_results)
          |> Option.value(~default=compiled_program.env);
        } else {
          env;
        };
      let env =
        if (in_module_toplevel || Option.is_none(result_env)) {
          List.fold_left(
            (env, decl: Typedtree.module_declaration) =>
              env_for_module_body(~base_env=env, lsp_position, decl),
            env,
            enclosing_modules,
          );
        } else {
          env;
        };
      Some({
        env,
        module_names: [
          compiled_program.module_name.txt,
          ...enclosing_module_names,
        ],
        sourcetree,
        sourcetree_results,
      });
    };
  {
    uri,
    position: ts_pos,
    source,
    tree,
    context,
    local_bindings,
    callee_fallback,
    typed,
  };
};

let find_value_type_in_results = results =>
  List.find_map(
    fun
    | Sourcetree.Value({value_type}) => Some(value_type)
    | _ => None,
    results,
  );

let expected_type_from_scrutinee = (tree, pos, sourcetree) =>
  switch (Syntax.Match_tree.match_scrutinee_position(tree, pos)) {
  | None => None
  | Some(scrutinee_pos) =>
    let scrutinee_position: Protocol.position = {
      line: scrutinee_pos.line,
      character: scrutinee_pos.character,
    };
    find_value_type_in_results(
      Sourcetree.query(scrutinee_position, sourcetree),
    );
  };

let collection_element_type = (env, value_type, collection_path) =>
  switch (Ctype.expand_head(env, value_type).desc) {
  | TTyConstr(path, [element_type], _) when Path.same(path, collection_path) =>
    Some(element_type)
  | _ => None
  };

let expected_type_from_expression_results = results => {
  let rec find = results =>
    switch (results) {
    | [] => None
    | [
        Sourcetree.Value({env, value_type, exp, loc: _, definition: _}),
        ...rest,
      ] =>
      switch (exp.Typedtree.exp_desc) {
      | TExpList(_) =>
        switch (
          collection_element_type(env, value_type, Builtin_types.path_list)
        ) {
        | Some(expected_type) => Some(expected_type)
        | None => find(rest)
        }
      | TExpArray(_) =>
        switch (
          collection_element_type(env, value_type, Builtin_types.path_array)
        ) {
        | Some(expected_type) => Some(expected_type)
        | None => find(rest)
        }
      | _ => find(rest)
      }
    | [_, ...rest] => find(rest)
    };
  find(results);
};

let type_name_from_let_binding = (tree, node) =>
  switch (Tree.node_child_by_field_name(node, Field.pattern)) {
  | Some(pattern) when Tree.node_kind(pattern) == Kind.typed_pattern =>
    switch (Tree.node_named_child(pattern, 1)) {
    | Some(type_node) =>
      let text = Tree.node_text(tree, type_node);
      let base_name =
        switch (String.index_opt(text, '<')) {
        | Some(idx) => String.sub(text, 0, idx)
        | None => text
        };
      let base_name = String.trim(base_name);
      base_name == "" ? None : Some(base_name);
    | None => None
    }
  | _ => None
  };

let rec find_value_binding_on_line = (pos: Grain_tree_sitter.position, node) =>
  if (Tree.node_kind(node) == Kind.value_binding) {
    let (row, _) = Tree.node_start_point(node);
    row == pos.line ? Some(node) : None;
  } else {
    Syntax.Util.find_named_child_map(node, find_value_binding_on_line(pos));
  };

let expected_type_from_let_annotation = (tree, pos, env) => {
  let node = Syntax.Util.node_at_cursor(tree, pos);
  let binding =
    if (Tree.node_kind(node) == Kind.value_binding) {
      Some(node);
    } else {
      switch (
        Tree.ancestor(node, n => Tree.node_kind(n) == Kind.value_binding)
      ) {
      | Some(_) as found => found
      | None => find_value_binding_on_line(pos, Tree.root(tree))
      };
    };
  switch (binding) {
  | None => None
  | Some(binding_node) =>
    switch (type_name_from_let_binding(tree, binding_node)) {
    | None => None
    | Some(type_name) =>
      try({
        let path =
          Env.lookup_type(
            ~mark=false,
            Grain_parsing.Identifier.parse(type_name),
            env,
          );
        let decl = Env.find_type(path, env);
        let params = List.map(_ => Ctype.newvar(), decl.Types.type_params);
        Some(Ctype.newconstr(path, params));
      }) {
      | Not_found => None
      | exn =>
        Trace.log(
          "expected_type_from_let_annotation: " ++ Printexc.to_string(exn),
        );
        None;
      }
    }
  };
};

let expected_type = request =>
  switch (request.typed) {
  | None => None
  | Some({sourcetree_results, sourcetree, env}) =>
    let find_expected_type_in_results = results =>
      List.find_map(
        fun
        | Sourcetree.Pattern({expected_type: Some(expected_type)}) =>
          Some(expected_type)
        | _ => None,
        results,
      );
    switch (find_expected_type_in_results(sourcetree_results)) {
    | Some(expected_type) => Some(expected_type)
    | None =>
      switch (request.context.kind) {
      | PatternContext =>
        expected_type_from_scrutinee(
          request.tree,
          request.position,
          sourcetree,
        )
      | _ =>
        switch (request.context.keyword_slot) {
        | Some(ExpressionStart) =>
          switch (expected_type_from_expression_results(sourcetree_results)) {
          | Some(_) as result => result
          | None =>
            expected_type_from_let_annotation(
              request.tree,
              request.position,
              env,
            )
          }
        | _ => None
        }
      }
    };
  };

let application = request =>
  switch (request.typed) {
  | None => None
  | Some({sourcetree_results}) =>
    List.find_map(
      fun
      | Sourcetree.Application({fun_expr, args}) => Some((fun_expr, args))
      | _ => None,
      sourcetree_results,
    )
  };

let type_reference_qualifier = (request: t) =>
  switch (request.context.kind) {
  | TypeReference =>
    let node = Syntax.Util.node_at_cursor(request.tree, request.position);
    let qualified_type =
      if (Tree.node_kind(node) == Kind.qualified_type_identifier) {
        Some(node);
      } else {
        Tree.ancestor(node, n =>
          Tree.node_kind(n) == Kind.qualified_type_identifier
        );
      };
    switch (qualified_type) {
    | Some(qualified) =>
      Syntax.Text.qualifier_from_node(
        request.tree,
        qualified,
        Syntax.Text.byte_offset(request.source, request.position),
      )
    | None =>
      Syntax.Text.qualifier_from_line(
        ~require_module_name=true,
        request.source,
        request.position,
      )
    };
  | _ => None
  };
