open Grain_parsing;
open Grain_diagnostics;

type node_t =
  | Code(Grain_parsing.Location.t)
  | Comment((Grain_parsing.Location.t, Grain_parsing.Parsetree.comment));

let all_locations: ref(list(node_t)) = ref([]);

let get_node_loc = (node: node_t): Grain_parsing.Location.t =>
  switch (node) {
  | Code(loc) => loc
  | Comment((loc, _)) => loc
  };

let compare_points = (line1, char1, line2, char2) =>
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
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc1.loc_start);
  let (_, raw2l, raw2c, _) = Locations.get_raw_pos_info(loc2.loc_start);

  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc1.loc_end);
  let (_, raw2le, raw2ce, _) = Locations.get_raw_pos_info(loc2.loc_end);

  // compare the leading points

  let res =
    if (compare_points(raw1l, raw1c, raw2l, raw2c) == 0
        && compare_points(raw1le, raw1ce, raw2le, raw2ce) == 0) {
      0;
    } else {
      compare_points(raw1l, raw1c, raw2l, raw2c);
    };

  res;
};

// is loc1 inside loc2
// assumes loc2 ends at the end of the line,
let is_first_inside_second =
    (loc1: Grain_parsing.Location.t, loc2: Grain_parsing.Location.t) => {
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc1.loc_start);
  let (_, raw2l, raw2c, _) = Locations.get_raw_pos_info(loc2.loc_start);

  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc1.loc_end);
  let (_, raw2le, raw2ce, _) = Locations.get_raw_pos_info(loc2.loc_end);

  if (raw1l < raw2l) {
    false;
  } else if (raw1le > raw2le) {
    false;
  } else {
    let begins_inside =
      if (raw1l > raw2l) {
        true;
      } else if (raw1c >= raw2c && raw1c <= raw2ce) {
        true;
      } else {
        false;
      };
    let ends_inside =
      if (raw1le < raw2le) {
        true;
      } else if (raw1le == raw2le) {
        if (raw1ce <= raw2ce) {
          true;
        } else {
          false;
        };
      } else {
        false;
      };

    begins_inside && ends_inside;
  };
};

let compare_partition_locations =
    (loc1: Grain_parsing.Location.t, loc2: Grain_parsing.Location.t) => {
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc1.loc_start);
  let (_, raw2l, raw2c, _) = Locations.get_raw_pos_info(loc2.loc_start);

  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc1.loc_end);
  let (_, raw2le, raw2ce, _) = Locations.get_raw_pos_info(loc2.loc_end);

  // compare the leading points

  if (compare_points(raw1l, raw1c, raw2l, raw2c) == 0
      && compare_points(raw1le, raw1ce, raw2le, raw2ce) == 0) {
    0;
  } else if
    // is loc2 inside loc1
    (compare_points(raw1l, raw1c, raw2l, raw2c) < 1
     && compare_points(raw2le, raw2ce, raw1le, raw1ce) < 1) {
    0;
  } else {
    compare_points(raw1l, raw1c, raw2l, raw2c);
  };
};

let walktree =
    (
      statements: list(Grain_parsing.Parsetree.toplevel_stmt),
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let comment_locations =
    List.map(c => Comment((Locations.get_comment_loc(c), c)), comments);

  all_locations := comment_locations;

  let iter_location = (self, location) =>
    if (!List.mem(Code(location), all_locations^)) {
      all_locations := List.append(all_locations^, [Code(location)]);
    };

  let iterator = {...Ast_iterator.default_iterator, location: iter_location};

  List.iter(iterator.toplevel(iterator), statements);

  all_locations :=
    List.sort(
      (node1: node_t, node2: node_t) => {
        let loc1 = get_node_loc(node1);
        let loc2 = get_node_loc(node2);
        compare_locations(loc1, loc2);
      },
      all_locations^,
    );
  // useful for dumping for debug
  // List.iter(
  //   n =>
  //     switch (n) {
  //     | Code(_) => Debug.print_loc("code", get_node_loc(n))
  //     | Comment(_) => Debug.print_loc("comment", get_node_loc(n))
  //     },
  //   all_locations^,
  // );
};

let partition_comments =
    (loc: Grain_parsing.Location.t, range: option(Grain_parsing.Location.t))
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
          let (acc_preceeding, acc_following) = acc;
          let nodeLoc = get_node_loc(node);

          let inRange =
            switch (range) {
            | None => true
            | Some(rangeloc) =>
              if (is_first_inside_second(nodeLoc, rangeloc)) {
                true;
              } else {
                false;
              }
            };

          if (!inRange) {
            acc;
          } else {
            let comparedLoc = compare_partition_locations(loc, nodeLoc);

            if (comparedLoc == 0) {
              // hit the node we are looking for
              acc;
            } else if (comparedLoc > 0) {
              switch (node) {
              | Code(_) => ([], acc_following)
              | Comment((l, c)) => (acc_preceeding @ [c], acc_following)
              };
            } else {
              switch (node) {
              | Code(_) =>
                skip := true;
                acc;
              | Comment((l, c)) => (acc_preceeding, acc_following @ [c])
              };
            };
          };
        },
      ([], []),
      all_locations^,
    );
  (preceeding, following);
};

let remove_used_comments = (pre_comments, post_comments) => {
  let cleaned_list =
    List.filter(
      n =>
        switch (n) {
        | Code(_) => true
        | Comment((loc, comment)) =>
          if (List.mem(comment, pre_comments)
              || List.mem(comment, post_comments)) {
            false;
          } else {
            true;
          }
        },
      all_locations^,
    );

  all_locations := cleaned_list;
};

let remove_nodes_before = (loc: Location.t) => {
  let skip = ref(false);
  let cleaned_list =
    List.filter(
      n =>
        if (skip^) {
          true;
        } else {
          let node_loc = get_node_loc(n);
          let compared_loc = compare_partition_locations(node_loc, loc);

          if (compared_loc == 0) {
            skip := true;
            true;
          } else {
            false;
          };
        },
      all_locations^,
    );

  all_locations := cleaned_list;
};

let remove_comments_in_ignore_block = (loc: Location.t) => {
  let cleaned_list =
    List.filter(
      n => {
        switch (n) {
        | Code(_) => true
        | Comment((commentloc, _)) =>
          !is_first_inside_second(commentloc, loc)
        }
      },
      all_locations^,
    );

  all_locations := cleaned_list;
};

let get_comments_inside_location = (loc: Grain_parsing.Location.t) => {
  List.fold_left(
    (acc, n) =>
      switch (n) {
      | Code(_) => acc
      | Comment((commentloc, c)) =>
        if (is_first_inside_second(commentloc, loc)) {
          acc @ [c];
        } else {
          acc;
        }
      },
    [],
    all_locations^,
  );
};
