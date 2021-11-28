open Grain_diagnostics;
module Doc = Res_doc;

let rec get_comments_on_line = (line: int, comments) =>
  if (List.length(comments) == 0) {
    [];
  } else {
    let c: Grain_parsing.Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    if (cmtline == line) {
      [c] @ get_comments_on_line(line, List.tl(comments));
    } else {
      []; // can stop early as there will be no more
    };
  };

let rec get_comments_before_line = (line: int, comments) =>
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

let rec get_comments_inside_location =
        (
          ~location: Grain_parsing.Location.t,
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
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
      // need the complex comparisons here
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

let get_comments_between_locations =
    (
      ~loc1: Grain_parsing.Location.t,
      ~loc2: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  // if loc1 is None get all comments before
  // if loc2 is None get all comments after

  let (_, stmtEndine, stmsEndtChar, _) =
    Locations.get_raw_pos_info(loc1.loc_end);
  let (_, stmtStartLine, stmsStartChar, _) =
    Locations.get_raw_pos_info(loc2.loc_start);

  // invert the request to look inside the location in the gap

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmtEndine,
    pos_bol: 0,
    pos_cnum: stmsEndtChar,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmtStartLine,
    pos_bol: 0,
    pos_cnum: stmsStartChar,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: true,
  };
  get_comments_inside_location(~location, comments);
};

let get_comments_to_end_of_enclosing_location =
    (
      ~wrapper: Grain_parsing.Location.t,
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, wrapEndLine, wrapEndChar, _) =
    Locations.get_raw_pos_info(wrapper.loc_end);
  let (_, locEndLine, locEndChar, _) =
    Locations.get_raw_pos_info(location.loc_end);

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: locEndLine,
    pos_bol: 0,
    pos_cnum: locEndChar,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: wrapEndLine,
    pos_bol: 0,
    pos_cnum: wrapEndChar,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: true,
  };
  let cmts = get_comments_inside_location(~location, comments);

  cmts;
};

let get_comments_from_start_of_enclosing_location =
    (
      ~wrapper: Grain_parsing.Location.t,
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, wrapStartLine, wrapStartChar, _) =
    Locations.get_raw_pos_info(wrapper.loc_start);
  let (_, locStartLine, locStartChar, _) =
    Locations.get_raw_pos_info(location.loc_start);

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: wrapStartLine,
    pos_bol: 0,
    pos_cnum: wrapStartChar,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: locStartLine,
    pos_bol: 0,
    pos_cnum: locStartChar,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: true,
  };
  let cmts = get_comments_inside_location(~location, comments);

  cmts;
};

let get_comments_between_locs =
    (
      ~loc1: Grain_parsing.Location.t,
      ~loc2: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmtEndine, stmsEndtChar, _) =
    Locations.get_raw_pos_info(loc1.loc_end);
  let (_, stmtStartLine, stmsStartChar, _) =
    Locations.get_raw_pos_info(loc2.loc_start);

  // invert the request to look inside the location in the gap

  let start_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmtEndine,
    pos_bol: 0,
    pos_cnum: stmsEndtChar,
  };
  let end_loc: Lexing.position = {
    pos_fname: "",
    pos_lnum: stmtStartLine,
    pos_bol: 0,
    pos_cnum: stmsStartChar,
  };

  let location: Grain_parsing.Location.t = {
    loc_start: start_loc,
    loc_end: end_loc,
    loc_ghost: true,
  };
  let cmts = get_comments_inside_location(~location, comments);

  cmts;
};

let rec get_comments_on_line_end = (line: int, char: int, comments) =>
  if (List.length(comments) == 0) {
    [];
  } else {
    let c: Grain_parsing.Parsetree.comment = List.hd(comments);
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(c);
    let (_, cmtline, cmtchar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    // print_endline("comment is on line " ++ string_of_int(cmtline));
    // print_endline("comment is on char " ++ string_of_int(cmtchar));

    if (cmtline > line) {
      []; // can stop early as there will be no more
    } else if (cmtline == line && cmtchar >= char) {
      [c] @ get_comments_on_line_end(line, char, List.tl(comments));
    } else {
      get_comments_on_line_end(line, char, List.tl(comments));
    };
  };

let get_comments_to_end_of_line =
    (
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmtEndine, stmsEndtChar, _) =
    Locations.get_raw_pos_info(location.loc_end);

  get_comments_on_line_end(stmtEndine, stmsEndtChar, comments);
};

let get_comments_to_end_of_src =
    (
      ~location: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, stmtEndine, _, _) = Locations.get_raw_pos_info(location.loc_end);

  get_comments_after_line(stmtEndine, comments);
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

let rec line_of_comments_inner =
        (
          prev: option(Grain_parsing.Parsetree.comment),
          comments: list(Grain_parsing.Parsetree.comment),
          comment_to_doc: Grain_parsing.Parsetree.comment => Doc.t,
        ) =>
  if (List.length(comments) == 1) {
    let cmt = List.hd(comments);

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
    };
  } else {
    let cmt = List.hd(comments);
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
    ]);
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
  if (List.length(comments) > 0) {
    line_of_comments_inner(None, comments, comment_to_doc);
  } else {
    Doc.nil;
  };

let line_ending_comments =
    (~offset: bool, comments: list(Grain_parsing__Parsetree.comment)) => {
  let num_trailing_comments = List.length(comments);

  switch (num_trailing_comments) {
  | 0 => Doc.nil
  | _ =>
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
  let num_trailing_comments = List.length(comments);

  switch (num_trailing_comments) {
  | 0 => Doc.nil
  | _ =>
    let num_trailing_comments = List.length(comments);
    let last = List.nth(comments, num_trailing_comments - 1);
    switch (last) {
    | Block(_) =>
      Doc.concat([comments_to_docs(~offset, comments), Doc.hardLine])
    | _ => comments_to_docs(~offset, comments)
    };
  };
};

let hard_line_needed = (comments: list(Grain_parsing__Parsetree.comment)) => {
  let num_comments = List.length(comments);

  switch (num_comments) {
  | 0 => Doc.line
  | _ =>
    let last = List.nth(comments, num_comments - 1);
    switch (last) {
    | Line(_) => Doc.hardLine
    | _ => Doc.hardLine
    };
  };
};

let line_needed = (comments: list(Grain_parsing__Parsetree.comment)) => {
  let num_comments = List.length(comments);

  switch (num_comments) {
  | 0 => Doc.softLine
  | _ =>
    let last = List.nth(comments, num_comments - 1);
    switch (last) {
    | Line(_) => Doc.hardLine
    | _ => Doc.hardLine
    };
  };
};

let line_of_comments_to_doc_no_break =
    (~offset: bool, comments: list(Grain_parsing.Parsetree.comment)) =>
  if (List.length(comments) > 0) {
    if (offset) {
      Doc.concat([
        Doc.space,
        line_of_comments_inner(None, comments, no_breakcomment_to_doc),
      ]);
    } else {
      line_of_comments_inner(None, comments, no_breakcomment_to_doc);
    };
  } else {
    Doc.nil;
  };
