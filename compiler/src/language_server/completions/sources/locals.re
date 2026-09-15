open Completion_types;
open Completion_types.ResponseResult;
open Builder;

/* Locals come from the parsetree. Users are more likely to want them so given a large boost unless they are in an import path. */
let in_scope_local_boost = 2500;

let local_relevance_boost = context =>
  switch (context.Syntax.Types.kind) {
  | ImportPath => 0
  | _ => in_scope_local_boost
  };

let local_candidates = (~context, bindings) =>
  List.map(
    (binding: Syntax.Types.local_binding) =>
      make_candidate(
        ~source=TreeSitter,
        ~label=binding.name,
        ~kind=Variable,
        ~sort_group=Sort_group.Local,
        ~context,
        ~relevance_boost=local_relevance_boost(context),
        (),
      ),
    bindings,
  );
