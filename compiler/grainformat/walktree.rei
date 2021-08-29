let partition_comments:
  (Grain_parsing.Location.t, option(Grain_parsing.Location.t)) =>
  (
    list(Grain_parsing.Parsetree.comment),
    list(Grain_parsing.Parsetree.comment),
  );

let remove_used_comments:
  (
    list(Grain_parsing__Parsetree.comment),
    list(Grain_parsing__Parsetree.comment)
  ) =>
  unit;

let remove_nodes_before: Grain_utils__Warnings.loc => unit;

let walktree:
  (
    list(Grain_parsing__Parsetree.toplevel_stmt),
    list(Grain_parsing__Parsetree.comment)
  ) =>
  unit;

let remove_comments_in_ignore_block: Grain_utils__Warnings.loc => unit;
