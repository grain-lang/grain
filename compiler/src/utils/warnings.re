/* See copyright information in warnings.mli */

type loc = {
  loc_start: Lexing.position,
  loc_end: Lexing.position,
  loc_ghost: bool,
};

type t =
  | LetRecNonFunction(string)
  | AmbiguousName(list(string), list(string), bool)
  | NotPrincipal(string)
  | NameOutOfScope(string, list(string), bool)
  | StatementType
  | NonreturningStatement
  | AllClausesGuarded
  | PartialMatch(string)
  | FragileMatch(string)
  | UnusedMatch
  | UnusedPat
  | NonClosedRecordPattern(string)
  | UnreachableCase
  | ShadowConstructor(string)
  | NoCmiFile(string, option(string));

let number =
  fun
  | LetRecNonFunction(_) => 1
  | NotPrincipal(_) => 2
  | AmbiguousName(_) => 3
  | NameOutOfScope(_) => 4
  | StatementType => 5
  | NonreturningStatement => 6
  | AllClausesGuarded => 7
  | PartialMatch(_) => 8
  | FragileMatch(_) => 9
  | UnusedMatch => 10
  | UnusedPat => 11
  | UnreachableCase => 12
  | ShadowConstructor(_) => 13
  | NoCmiFile(_) => 14
  | NonClosedRecordPattern(_) => 15;

let last_warning_number = 15;

let message =
  fun
  | LetRecNonFunction(name) =>
    Printf.sprintf(
      "'%s' is not a function, but is bound with 'let rec'",
      name,
    )
  | NotPrincipal(s) => s ++ " is not principal."
  | [@implicit_arity] NameOutOfScope(ty, [nm], false) =>
    nm
    ++ " was selected from type "
    ++ ty
    ++ ".\nIt is not visible in the current scope, and will not \nbe selected if the type becomes unknown."
  | [@implicit_arity] NameOutOfScope(_, _, false) => assert(false)
  | [@implicit_arity] NameOutOfScope(ty, slist, true) =>
    "this record of type "
    ++ ty
    ++ " contains fields that are \nnot visible in the current scope: "
    ++ String.concat(" ", slist)
    ++ ".\nThey will not be selected if the type becomes unknown."
  | [@implicit_arity] AmbiguousName([s], tl, false) =>
    s
    ++ " belongs to several types: "
    ++ String.concat(" ", tl)
    ++ "\nThe first one was selected. Please disambiguate if this is wrong."
  | [@implicit_arity] AmbiguousName(_, _, false) => assert(false)
  | [@implicit_arity] AmbiguousName(_slist, tl, true) =>
    "these field labels belong to several types: "
    ++ String.concat(" ", tl)
    ++ "\nThe first one was selected. Please disambiguate if this is wrong."
  | StatementType => "this expression should have type void."
  | NonreturningStatement => "this statement never returns (or has an unsound type)."
  | AllClausesGuarded => "this pattern-matching is not exhaustive.\nAll clauses in this pattern-matching are guarded."
  | PartialMatch("") => "this pattern-matching is not exhaustive."
  | PartialMatch(s) =>
    "this pattern-matching is not exhaustive.\nHere is an example of a case that is not matched:\n"
    ++ s
  | FragileMatch("") => "this pattern-matching is fragile."
  | FragileMatch(s) =>
    "this pattern-matching is fragile.\nIt will remain exhaustive when constructors are added to type "
    ++ s
    ++ "."
  | UnusedMatch => "this match case is unused."
  | UnusedPat => "this sub-pattern is unused."
  | UnreachableCase => "this mach case is unreachable."
  | ShadowConstructor(s) =>
    "the pattern variable " ++ s ++ " shadows a constructor of the same name."
  | [@implicit_arity] NoCmiFile(name, None) =>
    "no cmi file was found in path for module " ++ name
  | [@implicit_arity] NoCmiFile(name, Some(msg)) =>
    Printf.sprintf(
      "no valid cmi file was found in path for module %s. %s",
      name,
      msg,
    )
  | NonClosedRecordPattern(s) =>
    "the following fields are missing from the record pattern: " ++ s;

let sub_locs =
  fun
  | _ => [];

type state = {
  active: array(bool),
  error: array(bool),
};

let current =
  ref({
    active: Array.make(last_warning_number + 1, false),
    error: Array.make(last_warning_number + 1, false),
  });

let backup = () => current^;

let restore = x => current := x;

let is_active = x => current^.active[number(x)];
let is_error = x => current^.error[number(x)];

let nerrors = ref(0);

let defaults = [
  LetRecNonFunction(""),
  [@implicit_arity] AmbiguousName([], [], false),
  NotPrincipal(""),
  [@implicit_arity] NameOutOfScope("", [], false),
  StatementType,
  NonreturningStatement,
  AllClausesGuarded,
  PartialMatch(""),
  UnusedMatch,
  UnusedPat,
  NonClosedRecordPattern(""),
  UnreachableCase,
  ShadowConstructor(""),
  [@implicit_arity] NoCmiFile("", None),
];

let _ = List.iter(x => current^.active[number(x)] = true, defaults);

type reporting_information = {
  number: int,
  message: string,
  is_error: bool,
  sub_locs: list((loc, string)),
};

let report = w => {
  if (is_error(w)) {
    incr(nerrors);
  };
  `Active({
    number: number(w),
    message: message(w),
    is_error: is_error(w),
    sub_locs: sub_locs(w),
  });
};

exception Errors;

let reset_fatal = () => nerrors := 0;

let check_fatal = () =>
  if (nerrors^ > 0) {
    nerrors := 0;
    raise(Errors);
  };
