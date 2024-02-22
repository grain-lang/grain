open Grain_parsing;

// This structure isn't a tree at all, but we use a binary search algorithm to
// efficiently find comments, which is tree-like in spirit.

type t = {
  comments: array(Parsetree.comment),
  line_map: Hashtbl.t(int, Parsetree.comment),
};

let empty: t = {comments: [||], line_map: Hashtbl.create(0)};

let loc = cmt => {
  switch (cmt) {
  | Parsetree.Doc({cmt_loc})
  | Block({cmt_loc})
  | Line({cmt_loc})
  | Shebang({cmt_loc}) => cmt_loc
  };
};

let from_comments = x => {
  // The array allows us to do a binary search for comments within a range.
  let comments = Array.of_list(x);
  // This map stores the last comment on a line, allowing us to quickly check
  // for formatter-ignore comments.
  let line_map = Hashtbl.create(Array.length(comments));
  List.iter(
    comment =>
      Hashtbl.add(line_map, loc(comment).loc_start.pos_lnum, comment),
    x,
  );
  {comments, line_map};
};

let rec find_start_index = (array, point, ans, left, right) =>
  if (left <= right) {
    let middle = (left + right) / 2;
    if (loc(array[middle]).loc_start.pos_cnum >= point) {
      find_start_index(array, point, Some(middle), left, middle - 1);
    } else {
      find_start_index(array, point, ans, middle + 1, right);
    };
  } else {
    ans;
  };

let rec collect_range = (array, start, stop) =>
  if (start == Array.length(array)) {
    [];
  } else {
    let elem = array[start];
    if (loc(elem).loc_end.pos_cnum <= stop) {
      [elem, ...collect_range(array, start + 1, stop)];
    } else {
      [];
    };
  };

let query =
    (
      tree,
      {Location.loc_start: {pos_cnum: start}, loc_end: {pos_cnum: finish}},
    ) => {
  let array = tree.comments;
  let start_index =
    find_start_index(array, start, None, 0, Array.length(array) - 1);
  switch (start_index) {
  | None => []
  | Some(start_index) => collect_range(array, start_index, finish)
  };
};

let query_line = (tree, line) => Hashtbl.find_opt(tree.line_map, line);
