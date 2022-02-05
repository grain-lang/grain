open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_diagnostics;

let print_loc_string = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = Locations.get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = Locations.get_raw_pos_info(loc.loc_end);

  if (startchar >= 0) {
    if (line == endline) {
      Printf.sprintf("%s %d:%d,%d\n", msg, line, startchar, endchar);
    } else {
      Printf.sprintf(
        "%s %d:%d - %d:%d\n",
        msg,
        line,
        startchar,
        endline,
        endchar,
      );
    };
  } else {
    "";
  };
};

let print_loc = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = Locations.get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = Locations.get_raw_pos_info(loc.loc_end);

  if (startchar >= 0) {
    if (line == endline) {
      Printf.printf("%s %d:%d,%d\n", msg, line, startchar, endchar);
    } else {
      Printf.printf(
        "%s %d:%d - %d:%d\n",
        msg,
        line,
        startchar,
        endline,
        endchar,
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
  | PExpPrim0(prim0) => print_loc("PExpPrim0", expr.pexp_loc)
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

let print_comments = (comments: list(Grain_parsing.Parsetree.comment)) => {
  List.map(Comments.print_comment, comments);
};
