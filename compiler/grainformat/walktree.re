open Grain_parsing;

type node_t =
  | Code(Grain_parsing.Location.t)
  | Comment((Grain_parsing.Location.t, Grain_parsing.Parsetree.comment));

let allLocations: ref(list(node_t)) = ref([]);

let getNodeLoc = (node: node_t): Grain_parsing.Location.t =>
  switch (node) {
  | Code(loc) => loc
  | Comment((loc, _)) => loc
  };

let getCommentLoc = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => cmt.cmt_loc
  | Block(cmt) => cmt.cmt_loc
  | Doc(cmt) => cmt.cmt_loc
  | Shebang(cmt) => cmt.cmt_loc
  };

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let getComment = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => cmt.cmt_content
  | Block(cmt) => cmt.cmt_content
  | Doc(cmt) => cmt.cmt_content
  | Shebang(cmt) => cmt.cmt_content
  };

let comparePoints = (line1, char1, line2, char2) =>
  if (line1 == line2 && char1 == char2) {
    0;
  } else if (line1 <= line2) {
    if (line1 == line2) {
      if (char1 < char2) {
        (-1);
      } else {
        1;
      };
    } else {
      (-1);
    };
  } else {
    1;
  };

let compare_locations =
    (loc1: Grain_parsing.Location.t, loc2: Grain_parsing.Location.t) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
  let (_, raw2l, raw2c, _) = get_raw_pos_info(loc2.loc_start);

  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);
  let (_, raw2le, raw2ce, _) = get_raw_pos_info(loc2.loc_end);

  // compare the leading points

  let res =
    if (comparePoints(raw1l, raw1c, raw2l, raw2c) == 0
        && comparePoints(raw1le, raw1ce, raw2le, raw2ce) == 0) {
      0;
    } else {
      comparePoints(raw1l, raw1c, raw2l, raw2c);
    };

  res;
};

let compare_partition_locations =
    (loc1: Grain_parsing.Location.t, loc2: Grain_parsing.Location.t) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
  let (_, raw2l, raw2c, _) = get_raw_pos_info(loc2.loc_start);

  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);
  let (_, raw2le, raw2ce, _) = get_raw_pos_info(loc2.loc_end);

  // compare the leading points

  let res =
    if (comparePoints(raw1l, raw1c, raw2l, raw2c) == 0
        && comparePoints(raw1le, raw1ce, raw2le, raw2ce) == 0) {
      0;
    } else if
      // is loc2 inside loc1
      (comparePoints(raw1l, raw1c, raw2l, raw2c) < 1
       && comparePoints(raw2le, raw2ce, raw1le, raw1ce) < 1) {
      0;
    } else {
      comparePoints(raw1l, raw1c, raw2l, raw2c);
    };

  res;
};

let walktree =
    (
      statements: list(Grain_parsing.Parsetree.toplevel_stmt),
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let comment_locations =
    List.map(c => Comment((getCommentLoc(c), c)), comments);

  allLocations := comment_locations;

  let iter_location = (self, location) =>
    if (!List.mem(Code(location), allLocations^)) {
      allLocations := List.append(allLocations^, [Code(location)]);
    };

  let iterator = {...Ast_iterator.default_iterator, location: iter_location};

  List.iter(iterator.toplevel(iterator), statements);

  allLocations :=
    List.sort(
      (node1: node_t, node2: node_t) => {
        let loc1 = getNodeLoc(node1);
        let loc2 = getNodeLoc(node2);
        compare_locations(loc1, loc2);
      },
      allLocations^,
    );
};

let partitionComments =
    (loc: Grain_parsing.Location.t)
    : (
        list(Grain_parsing.Parsetree.comment),
        list(Grain_parsing.Parsetree.comment),
      ) => {
  let skip = ref(false);

  let (preceeding, following) =
    List.fold_left(
      (acc, node) =>
        if (skip^) {
          acc;
        } else {
          let (accPreceeding, accFollowing) = acc;
          let nodeLoc = getNodeLoc(node);

          let comparedLoc = compare_partition_locations(loc, nodeLoc);

          if (comparedLoc == 0) {
            // hit the node we are looking for
            acc;
          } else if (comparedLoc > 0) {
            switch (node) {
            | Code(_) => ([], accFollowing)
            | Comment((l, c)) => (accPreceeding @ [c], accFollowing)
            };
          } else {
            switch (node) {
            | Code(_) =>
              skip := true;
              acc;
            | Comment((l, c)) => (accPreceeding, accFollowing @ [c])
            };
          };
        },
      ([], []),
      allLocations^,
    );

  (preceeding, following);
};

let removeUsedComments = (preComments, postComments) => {
  let cleanedList =
    List.filter(
      n =>
        switch (n) {
        | Code(_) => true
        | Comment((loc, comment)) =>
          if (List.mem(comment, preComments)
              || List.mem(comment, postComments)) {
            false;
          } else {
            true;
          }
        },
      allLocations^,
    );

  allLocations := cleanedList;
};
