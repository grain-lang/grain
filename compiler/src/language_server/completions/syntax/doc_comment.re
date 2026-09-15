open Types;
open Grammar;

let cursor_in_doc_comment = (tree: parse_tree, pos: position) =>
  switch (Tree.node_descendant_for_point(tree, pos)) {
  | None => false
  | Some(node) =>
    Tree.node_kind(node) == Kind.doc_comment
    || Option.is_some(Util.ancestor_of_kind(node, Kind.doc_comment))
  };
