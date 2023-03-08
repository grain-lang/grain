open Grain_parsing;
open Segment_tree;

/*
   This module implements a data structure used for efficient querying of
   Typedtree nodes given a location.

   The current Commenttree implementation returns _all_ nodes at a given point,
   sorted by specificity. In the future this may change to return just the most
   specific node if performance is a concern.
 */

module type Commenttree = {
  include SegmentTree with type point = Lexing.position;
  type node = Parsetree.comment;

  type commentree = t(node);

  let from_comments: list(node) => t(node);
};

module Commenttree: Commenttree = {
  include Make({
    type t = Lexing.position;
    type segment = (Lexing.position, Lexing.position);

    let center = (x, y) => {
      Lexing.{...x, pos_cnum: (x.pos_cnum + y.pos_cnum) / 2};
    };

    let compare = (x, y) => {
      Lexing.(Stdlib.compare(x.pos_cnum, y.pos_cnum));
    };

    let compare_segments = ((x1, x2), (y1, y2)) => {
      Lexing.(
        Stdlib.compare(x2.pos_cnum - x1.pos_cnum, y2.pos_cnum - y1.pos_cnum)
      );
    };
  });

  type node = Parsetree.comment;

  type commentree = t(node);

  let loc_to_interval = loc => {
    Location.(loc.loc_start, loc.loc_end);
  };

  let from_comments = comments => {
    create(
      List.map(
        comment => {
          open Parsetree;
          let loc =
            switch (comment) {
            | Line({cmt_loc})
            | Shebang({cmt_loc})
            | Block({cmt_loc})
            | Doc({cmt_loc}) => cmt_loc
            };
          (loc_to_interval(loc), comment);
        },
        comments,
      ),
    );
  };
};
