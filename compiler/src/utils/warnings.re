/* See copyright information in warnings.mli */

type loc = {
  loc_start: Lexing.position,
  loc_end: Lexing.position,
  loc_ghost: bool,
};

type number_type =
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float32
  | Float64
  | Rational
  | BigInt;

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
  | UnusedExtension
  | NonClosedRecordPattern(string)
  | UnreachableCase
  | ShadowConstructor(string)
  | NoCmiFile(string, option(string))
  | FuncWasmUnsafe(string, string, string)
  | FromNumberLiteral(number_type, string, string)
  | UselessRecordSpread
  | PrintUnsafe(string)
  | ToStringUnsafe(string)
  | ArrayIndexNonInteger(string);

let last_warning_number = 22;

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
  | NonClosedRecordPattern(_) => 15
  | UnusedExtension => 16
  | FuncWasmUnsafe(_, _, _) => 17
  | FromNumberLiteral(_, _, _) => 18
  | UselessRecordSpread => 19
  | PrintUnsafe(_) => 20
  | ToStringUnsafe(_) => 21
  | ArrayIndexNonInteger(_) => last_warning_number;

let message =
  fun
  | LetRecNonFunction(name) =>
    Printf.sprintf(
      "'%s' is not a function, but is bound with 'let rec'",
      name,
    )
  | NotPrincipal(s) => s ++ " is not principal."
  | NameOutOfScope(ty, [nm], false) =>
    nm
    ++ " was selected from type "
    ++ ty
    ++ ".\nIt is not visible in the current scope, and will not \nbe selected if the type becomes unknown."
  | NameOutOfScope(_, _, false) => assert(false)
  | NameOutOfScope(ty, slist, true) =>
    "this record of type "
    ++ ty
    ++ " contains fields that are \nnot visible in the current scope: "
    ++ String.concat(" ", slist)
    ++ ".\nThey will not be selected if the type becomes unknown."
  | AmbiguousName([s], tl, false) =>
    s
    ++ " belongs to several types: "
    ++ String.concat(" ", tl)
    ++ "\nThe first one was selected. Please disambiguate if this is wrong."
  | AmbiguousName(_, _, false) => assert(false)
  | AmbiguousName(_slist, tl, true) =>
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
  | UnusedExtension => "this type extension is unused."
  | UnreachableCase => "this match case is unreachable."
  | ShadowConstructor(s) =>
    "the pattern variable " ++ s ++ " shadows a constructor of the same name."
  | NoCmiFile(name, None) =>
    "no cmi file was found in path for module " ++ name
  | NoCmiFile(name, Some(msg)) =>
    Printf.sprintf(
      "no valid cmi file was found in path for module %s. %s",
      name,
      msg,
    )
  | NonClosedRecordPattern(s) =>
    "the following fields are missing from the record pattern: "
    ++ s
    ++ "\nUse `_` to ignore unused fields."
  | FuncWasmUnsafe(func, f, m) =>
    "it looks like you are using "
    ++ func
    ++ " on two unsafe Wasm values here.\nThis is generally unsafe and will cause errors. Use "
    ++ f
    ++ " from the `"
    ++ m
    ++ "` module the instead."
  | FromNumberLiteral(mod_type, mod_name, n) => {
      let literal =
        switch (mod_type) {
        | Int8 => Printf.sprintf("%ss", n)
        | Int16 => Printf.sprintf("%sS", n)
        | Int32 => Printf.sprintf("%sl", n)
        | Int64 => Printf.sprintf("%sL", n)
        | Uint8 => Printf.sprintf("%sus", n)
        | Uint16 => Printf.sprintf("%suS", n)
        | Uint32 => Printf.sprintf("%sul", n)
        | Uint64 => Printf.sprintf("%suL", n)
        | Float32 =>
          Printf.sprintf("%sf", String.contains(n, '.') ? n : n ++ ".")

        | Float64 =>
          Printf.sprintf("%sd", String.contains(n, '.') ? n : n ++ ".")
        | Rational =>
          Printf.sprintf("%sr", String.contains(n, '/') ? n : n ++ "/1")
        | BigInt => Printf.sprintf("%st", n)
        };
      Printf.sprintf(
        "it looks like you are calling %s.fromNumber() with a constant number. Try using the literal syntax (e.g. `%s`) instead.",
        mod_name,
        literal,
      );
    }
  | UselessRecordSpread => "this record spread is useless as all of the record's fields are overridden."
  | PrintUnsafe(typ) =>
    "it looks like you are using `print` on an unsafe Wasm value here.\nThis is generally unsafe and will cause errors. Use `DebugPrint.print`"
    ++ typ
    ++ " from the `runtime/debugPrint` module instead."
  | ToStringUnsafe(typ) =>
    "it looks like you are using `toString` on an unsafe Wasm value here.\nThis is generally unsafe and will cause errors. Use `DebugPrint.toString`"
    ++ typ
    ++ " from the `runtime/debugPrint` module instead."
  | ArrayIndexNonInteger(idx) =>
    "Array index should be an integer, but found `" ++ idx ++ "`.";

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
  AmbiguousName([], [], false),
  // TODO(#681): Look into reenabling these
  //NotPrincipal(""),
  //NameOutOfScope("", [], false),
  StatementType,
  NonreturningStatement,
  AllClausesGuarded,
  PartialMatch(""),
  UnusedMatch,
  UnusedPat,
  NonClosedRecordPattern(""),
  UnreachableCase,
  ShadowConstructor(""),
  NoCmiFile("", None),
  FuncWasmUnsafe("", "", ""),
  FromNumberLiteral(Int8, "", ""),
  UselessRecordSpread,
  PrintUnsafe(""),
  ToStringUnsafe(""),
  ArrayIndexNonInteger(""),
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

// track the warnings for the LSP
let warnings_this_run: ref(list((loc, t))) = ref([]);

let add_warning = (l: loc, w: t) =>
  warnings_this_run := List.cons((l, w), warnings_this_run^);

let get_warnings = () => warnings_this_run^;

let reset_warnings = () => warnings_this_run := [];

let with_preserve_warnings = thunk => {
  let saved = warnings_this_run^;
  warnings_this_run := [];
  try({
    let r = thunk();
    warnings_this_run := saved;
    r;
  }) {
  | exn =>
    warnings_this_run := saved;
    raise(exn);
  };
};
