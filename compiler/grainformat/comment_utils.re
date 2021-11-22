open Grain_diagnostics;
module Doc = Res_doc;

let rec get_comments_on_line = (line: int, comments) =>
  // print_endline("get_comments_before_line:" ++ string_of_int(line));
  if (List.length(comments) == 0) {
    [];
  } else {
    let c: Grain_parsing.Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) = Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtline == line) {
      [c] @ get_comments_on_line(line, List.tl(comments));
    } else {
      []; // can stop early as there will be no more
    };
  };

let rec get_comments_before_line = (line: int, comments) =>
  // print_endline("get_comments_before_line:" ++ string_of_int(line));
  if (List.length(comments) == 0) {
    [];
  } else {
    let c: Grain_parsing.Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) = Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtline < line) {
      [c] @ get_comments_before_line(line, List.tl(comments));
    } else {
      []; // can stop early as there will be no more
    };
  };

let rec get_comments_after_line = (line: int, comments) =>
  // print_endline("get_comments_after_line:" ++ string_of_int(line));
  if (List.length(comments) == 0) {
    [];
  } else {
    let c: Grain_parsing.Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) = Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtline > line) {
      [c] @ get_comments_after_line(line, List.tl(comments));
    } else {
      get_comments_after_line(
        line,
        List.tl(comments) // gotta keep checking
      );
    };
  };

let rec get_comments_between_lines = (line1: int, line2: int, comments) =>
  // print_endline(
  //   "get_comments_between_lines:"
  //   ++ string_of_int(line1)
  //   ++ "-"
  //   ++ string_of_int(line2),
  // );
  if (List.length(comments) == 0) {
    [];
  } else {
    let c: Grain_parsing.Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtsline, cmtschar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    let (_, cmteline, cmtechar, _) =
      Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtsline > line1) {
      if (cmteline < line2) {
        [c] @ get_comments_between_lines(line1, line2, List.tl(comments));
      } else
        {
          [];
        }; // can stop early
    } else {
      // it's before, so keep going
      get_comments_between_lines(
        line1,
        line2,
        List.tl(comments),
      );
    };
  };

let get_comments_between_statements =
    (
      ~stmt1: option(Grain_parsing.Location.t),
      ~stmt2: option(Grain_parsing.Location.t),
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  // if stmt1 is None get all comments before
  // if stmt2 is None get all comments after
  switch (stmt1, stmt2) {
  | (None, Some(stmt)) =>
    let (_, stmtStartLine, stmsStartChar, _) =
      Locations.get_raw_pos_info(stmt.loc_start);
    get_comments_before_line(stmtStartLine, comments);

  | (Some(stmt), None) =>
    let (_, stmtEndine, stmsEndtChar, _) =
      Locations.get_raw_pos_info(stmt.loc_end);

    get_comments_after_line(stmtEndine, comments);

  | (Some(s1), Some(s2)) =>
    let (_, stmtEndine, stmsEndtChar, _) =
      Locations.get_raw_pos_info(s1.loc_end);
    let (_, stmtStartLine, stmsStartChar, _) =
      Locations.get_raw_pos_info(s2.loc_start);

    get_comments_between_lines(stmtEndine, stmtStartLine, comments);
  | (None, None) => []
  };
};

let rec get_comments_inside_location =
        (
          ~location: Grain_parsing.Location.t,
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
  Debug.print_loc("get_comments_inside_location", location);
  let (_, stmtStartLine, stmsStartChar, _) =
    Locations.get_raw_pos_info(location.loc_start);
  let (_, stmtEndLine, stmsEndChar, _) =
    Locations.get_raw_pos_info(location.loc_end);

  if (List.length(comments) > 0) {
    let cmt: Grain_parsing__Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(cmt);

    let (_, cmtsline, cmtschar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    let (_, cmteline, cmtechar, _) =
      Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtsline > stmtEndLine) {
      []; // can stop now
    } else if (cmteline < stmtStartLine) {
      get_comments_inside_location(location, List.tl(comments));
    } else if
      // need the clever comparisons here
      (cmtsline > stmtStartLine
       || cmtsline == stmtStartLine
       && cmtschar >= stmsStartChar) {
      if (cmteline < stmtEndLine
          || cmteline == stmtEndLine
          && cmtechar <= stmsEndChar) {
        [cmt] @ get_comments_inside_location(location, List.tl(comments));
      } else {
        get_comments_inside_location(location, List.tl(comments));
      };
    } else {
      get_comments_inside_location(location, List.tl(comments));
    };
  } else {
    [];
  };
};

let comment_to_doc = (comment: Grain_parsing.Parsetree.comment) => {
  let comment_string = Comments.get_comment_source(comment);
  Doc.text(String.trim(comment_string));
};

let comments_to_docs = (comments: list(Grain_parsing.Parsetree.comment)) => {
  Doc.concat([
    Doc.join(Doc.hardLine, List.map(c => comment_to_doc(c), comments)),
    Doc.hardLine,
  ]);
};

let print_comments = (comments: list(Grain_parsing.Parsetree.comment)) => {
  List.map(c => Comments.print_comment(c), comments);
};

let get_after_brace_comments =
    (
      loc: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, startline, startc, _) = Locations.get_raw_pos_info(loc.loc_start);

  get_comments_on_line(startline, comments);
};

let line_of_comments_to_doc =
    (comments: list(Grain_parsing.Parsetree.comment)) =>
  if (List.length(comments) > 0) {
    let cmts = List.map(c => comment_to_doc(c), comments);
    Doc.concat([Doc.space, Doc.join(Doc.space, cmts)]);
  } else {
    Doc.nil;
  };
