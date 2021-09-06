/*

   This module provides brings the AST and Comments together in an ordered list.
   This lets us make a decision on which AST nodes a comment lies betweem, to know if the
   comment comes in between expressions or patterns, at the end of a line or on its own line.

   We remove comments from the list once used to ensure we don't use them twice.

   We remove comments and nodes from the list once we've passed them in the AST parse.
   This really helps performance and keeps the search to the first few items in the list each search,
   rather than searching from start to finish every time

 */

/*
   Given a location, find the comments before and after that location, bounded by AST nodes

   We do this by searching forward from the start of the list.  Each time we find a node that isn't the
   partition location, we clear the list.

   Once we find the location, every comment after is is added to the post list until we hit another AST node

   N C1 C2 N C3 L C4 C5 N

   If we partition on L, we want C3 pre, C4,C5 post


 */
let partition_comments:
  (Grain_parsing.Location.t, option(Grain_parsing.Location.t)) =>
  (
    list(Grain_parsing.Parsetree.comment),
    list(Grain_parsing.Parsetree.comment),
  );

/*

   To avoid us re-using comments, we remove them from the list one consumed
 */

let remove_used_comments:
  (
    list(Grain_parsing__Parsetree.comment),
    list(Grain_parsing__Parsetree.comment)
  ) =>
  unit;

/*
   remove_nodes_before is purely for performance.   Once we've partioned on a location and comnsumed
   the comments, all the nodes before this location are redundant in the seach, so we remove them from
   the list

 */

let remove_nodes_before: Grain_utils__Warnings.loc => unit;

/* walktree generate a linear list of AST nodes sorted by location.  We pass in the AST and the comments list
   provided by the parser, and interleave them in sorted order */

let walktree:
  (
    list(Grain_parsing__Parsetree.toplevel_stmt),
    list(Grain_parsing__Parsetree.comment)
  ) =>
  unit;

/*
   For formatter-ignore (or syntaxt we don't understand) we just substitute the original source code
   rather than re-write the AST.  This could contain comments, so we remove them from the list to
   avoid them being used when seen as preceeding a later node


 */
let remove_comments_in_ignore_block: Grain_utils__Warnings.loc => unit;

/* return the comments inside a location */

let get_comments_inside_location:
  Grain_parsing.Location.t => list(Grain_parsing.Parsetree.comment);
