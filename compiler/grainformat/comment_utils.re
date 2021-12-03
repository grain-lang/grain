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

let nobreak_comment_to_doc = (comment: Grain_parsing.Parsetree.comment) => {
  let comment_string = Comments.get_comment_source(comment);

  Doc.concat([Doc.text(String.trim(comment_string))]);
};

let get_after_brace_comments =
    (
      loc: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, startline, startc, _) = Locations.get_raw_pos_info(loc.loc_start);

  get_comments_on_line(startline, comments);
};

let no_breakcomment_to_doc = (comment: Grain_parsing.Parsetree.comment) => {
  let comment_string = Comments.get_comment_source(comment);
  Doc.text(String.trim(comment_string));
};

let rec comments_inner =
        (
          prev: option(Grain_parsing.Parsetree.comment),
          bracket_line_opt: option(int),
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
  switch (prev) {
  | None =>
    switch (comments) {
    | [] => [Doc.nil]
    | [cmt, ...rem] =>
      switch (bracket_line_opt) {
      | None => [
          comment_to_doc(cmt),
          ...comments_inner(Some(cmt), bracket_line_opt, rem),
        ]
      | Some(bracket_line) =>
        let (_, this_line, _, _) =
          Locations.get_raw_pos_info(
            Locations.get_comment_loc(cmt).loc_start,
          );

        if (bracket_line == this_line) {
          [
            Doc.space,
            comment_to_doc(cmt),
            ...comments_inner(Some(cmt), bracket_line_opt, rem),
          ];
        } else {
          [
            Doc.hardLine,
            comment_to_doc(cmt),
            ...comments_inner(Some(cmt), bracket_line_opt, rem),
          ];
        };
      }
    }
  | Some(prev_cmt) =>
    switch (comments) {
    | [] => [Doc.nil]
    | [cmt, ...rem] =>
      let (_, prev_line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(prev_cmt).loc_end,
        );
      let (_, this_line, _, _) =
        Locations.get_raw_pos_info(Locations.get_comment_loc(cmt).loc_start);

      switch (this_line - prev_line) {
      | 0 => [
          Doc.space,
          comment_to_doc(cmt),
          ...comments_inner(Some(cmt), bracket_line_opt, rem),
        ]

      | 1 => [
          switch (prev_cmt) {
          | Line(_) => Doc.nil
          | _ => Doc.hardLine
          },
          comment_to_doc(cmt),
          ...comments_inner(Some(cmt), bracket_line_opt, rem),
        ]
      | _ => [
          Doc.hardLine,
          comment_to_doc(cmt),
          ...comments_inner(Some(cmt), bracket_line_opt, rem),
        ]
      };
    }
  };
};

let inbetween_comments_to_docs =
    (
      ~offset: bool,
      ~bracket_line: option(int),
      comments: list(Grain_parsing.Parsetree.comment),
    ) =>
  switch (comments) {
  | [] =>
    switch (bracket_line) {
    | None => Doc.nil
    | Some(_) => Doc.softLine
    }
  | _remaining_comments =>
    if (offset) {
      Doc.concat([
        Doc.space,
        Doc.concat(comments_inner(None, bracket_line, comments)),
      ]);
    } else {
      Doc.concat(comments_inner(None, bracket_line, comments));
    }
  };

let rec get_comments_after_location =
        (
          ~location: Grain_parsing.Location.t,
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
  let (_, stmt_end_line, stmt_end_char, _) =
    Locations.get_raw_pos_info(location.loc_end);

  switch (comments) {
  | [] => []
  | [cmt, ...remaining_comments] =>
    let c_loc: Grain_parsing.Location.t = Locations.get_comment_loc(cmt);

    let (_, cmtsline, cmtschar, _) =
      Locations.get_raw_pos_info(c_loc.loc_start);

    if (cmtsline > stmt_end_line
        || cmtsline == stmt_end_line
        && cmtschar > stmt_end_char) {
      [cmt, ...get_comments_after_location(~location, remaining_comments)];
    } else {
      get_comments_after_location(~location, remaining_comments);
    };
  };
};

let rec trailing_comments_inner =
        (
          prev: option(Grain_parsing.Parsetree.comment),
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
  switch (prev) {
  | None =>
    switch (comments) {
    | [] => [Doc.nil]
    | [cmt] => [nobreak_comment_to_doc(cmt)]
    | [cmt, ...rem] => [
        comment_to_doc(cmt),
        ...trailing_comments_inner(Some(cmt), rem),
      ]
    }
  | Some(prev_cmt) =>
    switch (comments) {
    | [] => [Doc.nil]
    | [cmt, ...rem] =>
      let (_, prev_line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(prev_cmt).loc_end,
        );
      let (_, this_line, _, _) =
        Locations.get_raw_pos_info(Locations.get_comment_loc(cmt).loc_start);

      let comment_printer =
        switch (rem) {
        | [] => nobreak_comment_to_doc
        | _ => comment_to_doc
        };

      switch (this_line - prev_line) {
      | 0 => [
          Doc.space,
          comment_printer(cmt),
          ...trailing_comments_inner(Some(cmt), rem),
        ]

      | 1 => [
          switch (prev_cmt) {
          | Line(_) => Doc.nil
          | _ => Doc.hardLine
          },
          comment_printer(cmt),
          ...trailing_comments_inner(Some(cmt), rem),
        ]
      | _ => [
          Doc.hardLine,
          comment_printer(cmt),
          ...trailing_comments_inner(Some(cmt), rem),
        ]
      };
    }
  };
};

let block_trailing_comments_docs =
    (comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => Doc.nil
  | _remaining_comments =>
    Doc.concat(trailing_comments_inner(None, comments))
  };

let single_line_of_comments =
    (comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => Doc.nil
  | _ =>
    Doc.concat([
      Doc.space,
      Doc.join(
        Doc.space,
        List.map(c => {nobreak_comment_to_doc(c)}, comments),
      ),
    ])
  };
