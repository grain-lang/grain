open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;

let walk_tree_add_comments = (parsed_program: Parsetree.parsed_program) => {
  [];
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

let getCommentLoc = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => cmt.cmt_loc
  | Block(cmt) => cmt.cmt_loc
  | Doc(cmt) => cmt.cmt_loc
  | Shebang(cmt) => cmt.cmt_loc
  };

let print_loc = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = get_raw_pos_info(loc.loc_end);
  /*let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in*/

  if (startchar >= 0) {
    if (line == endline) {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ ","
        ++ string_of_int(endchar),
      );
    } else {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ " - "
        ++ string_of_int(endline)
        ++ ":"
        ++ string_of_int(endchar),
      );
    };
  };
};
let debug_expression = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpConstant(x) => print_loc("PExpConstant", expr.pexp_loc)
  | PExpId({txt: id}) => print_loc("PExpId", expr.pexp_loc)

  | PExpLet(rec_flag, mut_flag, vbs) => print_loc("PExpLet", expr.pexp_loc)

  | PExpTuple(expressions) => print_loc("PExpTuple", expr.pexp_loc)

  | PExpArray(expressions) => print_loc("PExpArray", expr.pexp_loc)

  | PExpArrayGet(expression1, expression2) =>
    print_loc("PExpArrayGet", expr.pexp_loc)

  | PExpArraySet(expression1, expression2, expression3) =>
    print_loc("PExpArraySet", expr.pexp_loc)

  | PExpRecord(record) => print_loc("PExpRecord", expr.pexp_loc)

  | PExpRecordGet(expression, {txt, _}) =>
    print_loc("PExpRecordGet", expr.pexp_loc)

  | PExpRecordSet(expression, {txt, _}, expression2) =>
    print_loc("PExpRecordSet", expr.pexp_loc)

  | PExpMatch(expression, match_branches) =>
    print_loc("PExpMatch", expr.pexp_loc)

  | PExpPrim1(prim1, expression) => print_loc("PExpPrim1", expr.pexp_loc)

  | PExpPrim2(prim2, expression, expression1) =>
    print_loc("PExpPrim2", expr.pexp_loc)

  | PExpPrimN(primn, expressions) => print_loc("PExpPrimN", expr.pexp_loc)

  | PExpIf(condition, trueExpr, falseExpr) =>
    print_loc("PExpIf", expr.pexp_loc)

  | PExpWhile(expression, expression1) =>
    print_loc("PExpWhile", expr.pexp_loc)

  | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
    print_loc("PExpFor", expr.pexp_loc)

  | PExpContinue => print_loc("PExpContinue", expr.pexp_loc)

  | PExpBreak => print_loc("PExpBreak", expr.pexp_loc)

  | PExpConstraint(expression, parsed_type) =>
    print_loc("PExpConstraint", expr.pexp_loc)

  | PExpLambda(patterns, expression) =>
    print_loc("PExpLambda", expr.pexp_loc)

  | PExpApp(func, expressions) => print_loc("PExpApp", expr.pexp_loc)

  | PExpBlock(expressions) => print_loc("PExpBlock", expr.pexp_loc)

  | PExpBoxAssign(expression, expression1) =>
    print_loc("PExpBoxAssign", expr.pexp_loc)

  | PExpAssign(expression, expression1) =>
    print_loc("PExpAssign", expr.pexp_loc)

  | /** Used for modules without body expressions */ PExpNull =>
    print_loc("PExpNull", expr.pexp_loc)
  };
};

let debug_pattern = (pat: Parsetree.pattern) => {
  switch (pat.ppat_desc) {
  | PPatAny => print_endline("PPatAny")
  | PPatConstant(c) => print_endline("PPatAny")
  | PPatVar({txt, _}) => print_endline("PPatVar " ++ txt)
  | PPatTuple(patterns) => print_endline("PPatTuple")
  | PPatArray(patterns) => print_endline("PPatArray")

  | PPatRecord(patternlocs, closedflag) => print_endline("PPatRecord")
  | PPatConstraint(pattern, parsed_type) => print_endline("PPatConstraint")
  | PPatConstruct(location, patterns) => print_endline("PPatConstruct")

  | PPatOr(pattern1, pattern2) => print_endline("PPatOr")
  | PPatAlias(pattern, loc) => print_endline("PPatAlias")
  };
};

let partitionByLoc = (comments, loc) => {
  let rec loop = ((leading, inside, trailing), comments) =>
    Location.(
      switch (comments) {
      | [comment, ...rest] =>
        let cmtLoc = getCommentLoc(comment);
        if (cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum) {
          loop(([comment, ...leading], inside, trailing), rest);
        } else if (cmtLoc.loc_start.pos_cnum >= loc.loc_end.pos_cnum) {
          loop((leading, inside, [comment, ...trailing]), rest);
        } else {
          loop((leading, [comment, ...inside], trailing), rest);
        };
      | [] => (List.rev(leading), List.rev(inside), List.rev(trailing))
      }
    );

  loop(([], [], []), comments);
};

// let partitionAdjacentTrailing = (loc1, comments) => {
//   open Location;
//   open Lexing;
//   let rec loop = (~prevEndPos, afterLoc1, comments) =>
//     switch (comments) {
//     | [] => (List.rev(afterLoc1), [])
//     | [comment, ...rest] as comments =>
//       let cmtPrevEndPos = Comment.prevTokEndPos(comment);
//       if (prevEndPos.Lexing.pos_cnum === cmtPrevEndPos.pos_cnum) {
//         let commentEnd = Comment.loc(comment).loc_end;
//         loop(~prevEndPos=commentEnd, [comment, ...afterLoc1], rest);
//       } else {
//         (List.rev(afterLoc1), comments);
//       };
//     };

//   loop(~prevEndPos=loc1.loc_end, [], comments);
// };
