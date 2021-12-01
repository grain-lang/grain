open Grain_diagnostics;
module Doc = Res_doc;

let rec get_comments_on_line =
        (line: int, comments: list(Grain_parsing__Parsetree.comment)) =>
  switch (comments) {
  | [] => []
  | [c, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    if (cmtline == line) {
      [c, ...get_comments_on_line(line, remaining_comments)];
    } else if (cmtline < line) {
      get_comments_on_line(line, remaining_comments);
    } else {
      [];
    }; // can stop early as there will be no more
  };

let rec get_comments_before_line =
        (line: int, comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => []
  | [c, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) = Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtline < line) {
      [c, ...get_comments_before_line(line, remaining_comments)];
    } else {
      []; // can stop early as there will be no more
    };
  };

let rec get_comments_after_line =
        (line: int, comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => []
  | [c, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) = Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtline > line) {
      [c, ...get_comments_after_line(line, remaining_comments)];
    } else {
      get_comments_after_line(
        line,
        List.tl(comments) // gotta keep checking
      );
    };
  };

let rec get_comments_between_lines =
        (
          line1: int,
          line2: int,
          comments: list(Grain_parsing.Parsetree.comment),
        ) =>
  switch (comments) {
  | [] => []
  | [c, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtsline, cmtschar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    let (_, cmteline, cmtechar, _) =
      Locations.get_raw_pos_info(c_loc.loc_end);

    if (cmtsline > line1) {
      if (cmteline < line2) {
        [c, ...get_comments_between_lines(line1, line2, remaining_comments)];
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

let rec get_comments_inside_location =
        (
          ~location: Grain_parsing.Location.t,
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
  let (_, stmt_start_line, stm_start_char, _) =
    Locations.get_raw_pos_info(location.loc_start);
  let (_, stmt_end_line, stmt_end_char, _) =
    Locations.get_raw_pos_info(location.loc_end);

  switch (comments) {
  | [] => []
  | [cmt, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(cmt);

    let (_, cmtsline, cmtschar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    let (_, cmteline, cmtechar, _) =
      Locations.get_raw_pos_info(c_loc.loc_end);
    if (cmtsline > stmt_end_line) {
      []; // can stop now
    } else if (cmteline < stmt_start_line) {
      get_comments_inside_location(location, remaining_comments);
    } else if
      // need the complex comparisons here
      (cmtsline > stmt_start_line
       || cmtsline == stmt_start_line
       && cmtschar >= stm_start_char) {
      if (cmteline < stmt_end_line
          || cmteline == stmt_end_line
          && cmtechar <= stmt_end_char) {
        [cmt, ...get_comments_inside_location(location, remaining_comments)];
      } else {
        get_comments_inside_location(location, remaining_comments);
      };
    } else {
      get_comments_inside_location(location, remaining_comments);
    };
  };
};

let get_comments_between_locations =
    (
      ~loc1: Grain_parsing.Location.t,
      ~loc2: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmt_end_line, stmt_end_char, _) =
    Locations.get_raw_pos_info(loc1.loc_end);
  let (_, stmt_start_line, stm_start_char, _) =
    Locations.get_raw_pos_info(loc2.loc_start);

  // invert the request to look inside the location in the gap

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmt_end_line,
    pos_bol: 0,
    pos_cnum: stmt_end_char,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmt_start_line,
    pos_bol: 0,
    pos_cnum: stm_start_char,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: false,
  };
  get_comments_inside_location(~location, comments);
};

let get_comments_enclosed_and_before_location =
    (
      ~loc1: Grain_parsing.Location.t,
      ~loc2: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, loc_start_line, loc_start_char, _) =
    Locations.get_raw_pos_info(loc1.loc_start);
  let (_, stmt_start_line, stm_start_char, _) =
    Locations.get_raw_pos_info(loc2.loc_start);

  // invert the request to look inside the location in the gap

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: loc_start_line,
    pos_bol: 0,
    pos_cnum: loc_start_char,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmt_start_line,
    pos_bol: 0,
    pos_cnum: stm_start_char,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: false,
  };

  get_comments_inside_location(~location, comments);
};

let get_comments_to_end_of_enclosing_location =
    (
      ~wrapper: Grain_parsing.Location.t,
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, wrap_end_line, wrap_end_char, _) =
    Locations.get_raw_pos_info(wrapper.loc_end);
  let (_, loc_end_line, loc_end_char, _) =
    Locations.get_raw_pos_info(location.loc_end);

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: loc_end_line,
    pos_bol: 0,
    pos_cnum: loc_end_char,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: wrap_end_line,
    pos_bol: 0,
    pos_cnum: wrap_end_char,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: false,
  };
  get_comments_inside_location(~location, comments);
};

let get_comments_from_start_of_enclosing_location =
    (
      ~wrapper: Grain_parsing.Location.t,
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, wrap_start_line, wrap_start_char, _) =
    Locations.get_raw_pos_info(wrapper.loc_start);
  let (_, loc_start_line, loc_start_char, _) =
    Locations.get_raw_pos_info(location.loc_start);

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: wrap_start_line,
    pos_bol: 0,
    pos_cnum: wrap_start_char,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: loc_start_line,
    pos_bol: 0,
    pos_cnum: loc_start_char,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: false,
  };
  get_comments_inside_location(~location, comments);
};

let get_comments_between_locs =
    (
      ~loc1: Grain_parsing.Location.t,
      ~loc2: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmt_end_line, stmt_end_char, _) =
    Locations.get_raw_pos_info(loc1.loc_end);
  let (_, stmt_start_line, stm_start_char, _) =
    Locations.get_raw_pos_info(loc2.loc_start);

  // invert the request to look inside the location in the gap

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmt_end_line,
    pos_bol: 0,
    pos_cnum: stmt_end_char,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmt_start_line,
    pos_bol: 0,
    pos_cnum: stm_start_char,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: false,
  };
  get_comments_inside_location(~location, comments);
};

let rec get_comments_on_line_end =
        (
          line: int,
          char: int,
          comments: list(Grain_parsing.Parsetree.comment),
        ) =>
  switch (comments) {
  | [] => []
  | [c, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    if (cmtline > line) {
      []; // can stop early as there will be no more
    } else if (cmtline == line && cmtchar >= char) {
      [c, ...get_comments_on_line_end(line, char, List.tl(comments))];
    } else {
      get_comments_on_line_end(line, char, List.tl(comments));
    };
  };

let rec get_comments_on_line_start = (line: int, char: int, comments) =>
  switch (comments) {
  | [] => []
  | [c, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    if (cmtline > line) {
      []; // can stop early as there will be no more
    } else if (cmtline == line && cmtchar <= char) {
      [c, ...get_comments_on_line_start(line, char, List.tl(comments))];
    } else {
      get_comments_on_line_start(line, char, List.tl(comments));
    };
  };

let get_comments_to_end_of_line =
    (
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmt_end_line, stmt_end_char, _) =
    Locations.get_raw_pos_info(location.loc_end);

  get_comments_on_line_end(stmt_end_line, stmt_end_char, comments);
};

let get_comments_to_end_of_src =
    (
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmt_end_line, _, _) =
    Locations.get_raw_pos_info(location.loc_end);

  get_comments_after_line(stmt_end_line, comments);
};
let comment_to_doc = (comment: Grain_parsing.Parsetree.comment) => {
  let comment_string = Comments.get_comment_source(comment);
  let newline =
    switch (comment) {
    | Line(_)
    | Shebang(_) => Doc.hardLine
    | Doc(_) => Doc.hardLine
    | _ => Doc.nil
    };
  Doc.concat([Doc.text(String.trim(comment_string)), newline]);
};

let get_after_brace_comments =
    (
      loc: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, startline, startc, _) = Locations.get_raw_pos_info(loc.loc_start);

  get_comments_on_line(startline, comments);
};

let rec line_of_comments_inner =
        (
          prev: option(Grain_parsing.Parsetree.comment),
          comments: list(Grain_parsing.Parsetree.comment),
          comment_to_doc: Grain_parsing.Parsetree.comment => Doc.t,
        ) =>
  switch (comments) {
  | [] => Doc.nil
  | [cmt] =>
    switch (prev) {
    | None => comment_to_doc(cmt)
    | Some(c) =>
      let (_, prevCmt, _, _) =
        Locations.get_raw_pos_info(Locations.get_comment_loc(c).loc_end);
      let (_, thisCmt, _, _) =
        Locations.get_raw_pos_info(Locations.get_comment_loc(cmt).loc_start);

      if (prevCmt != thisCmt) {
        Doc.concat([Doc.hardLine, comment_to_doc(cmt)]);
      } else {
        comment_to_doc(cmt);
      };
    }
  | [cmt, ...remainder] =>
    Doc.concat([
      switch (prev) {
      | None => comment_to_doc(cmt)
      | Some(c) =>
        let (_, prevCmt, _, _) =
          Locations.get_raw_pos_info(Locations.get_comment_loc(c).loc_end);
        let (_, thisCmt, _, _) =
          Locations.get_raw_pos_info(
            Locations.get_comment_loc(cmt).loc_start,
          );

        if (prevCmt != thisCmt) {
          Doc.concat([Doc.hardLine, comment_to_doc(cmt)]);
        } else {
          comment_to_doc(cmt);
        };
      },
      line_of_comments_inner(Some(cmt), List.tl(comments), comment_to_doc),
    ])
  };

let no_breakcomment_to_doc = (comment: Grain_parsing.Parsetree.comment) => {
  let comment_string = Comments.get_comment_source(comment);
  Doc.text(String.trim(comment_string));
};

let comments_to_docs =
    (~offset: bool, comments: list(Grain_parsing.Parsetree.comment)) => {
  let listLength = List.length(comments);
  if (listLength > 0) {
    if (offset) {
      Doc.concat([
        Doc.space,
        line_of_comments_inner(None, comments, comment_to_doc),
      ]);
    } else {
      Doc.concat([line_of_comments_inner(None, comments, comment_to_doc)]);
    };
  } else {
    Doc.nil;
  };
};

let line_of_comments_to_doc =
    (comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => Doc.nil
  | _remaining_comments =>
    line_of_comments_inner(None, comments, comment_to_doc)
  };

let line_ending_comments =
    (~offset: bool, comments: list(Grain_parsing__Parsetree.comment)) => {
  switch (comments) {
  | [] => Doc.nil
  | _remaining_comments =>
    let num_trailing_comments = List.length(comments);
    let last = List.nth(comments, num_trailing_comments - 1);
    switch (last) {
    | Block(_) =>
      Doc.concat([comments_to_docs(~offset, comments), Doc.hardLine])
    | _ => comments_to_docs(~offset, comments)
    };
  };
};

let block_ending_comments =
    (~offset: bool, comments: list(Grain_parsing__Parsetree.comment)) => {
  switch (comments) {
  | [] => Doc.nil
  | _remaining_comments =>
    let num_trailing_comments = List.length(comments);
    let last = List.nth(comments, num_trailing_comments - 1);
    switch (last) {
    | Block(_) =>
      Doc.concat([comments_to_docs(~offset, comments), Doc.hardLine])
    | _ => comments_to_docs(~offset, comments)
    };
  };
};

let hard_line_needed =
    (~separator, comments: list(Grain_parsing__Parsetree.comment)) => {
  switch (comments) {
  | [] => separator
  | _remaining_comments =>
    let num_comments = List.length(comments);
    let last = List.nth(comments, num_comments - 1);
    switch (last) {
    | Line(_) => Doc.hardLine
    | _ => separator
    };
  };
};

let line_of_comments_to_doc_no_break =
    (~offset: bool, comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => Doc.nil
  | _remaining_comments =>
    if (offset) {
      Doc.concat([
        Doc.space,
        line_of_comments_inner(None, comments, no_breakcomment_to_doc),
      ]);
    } else {
      line_of_comments_inner(None, comments, no_breakcomment_to_doc);
    }
  };
