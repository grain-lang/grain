(* See copyright information in warnings.mli *)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type t =
  | LetRecNonFunction of string
  | AmbiguousName of string list * string list * bool
  | NotPrincipal of string
  | NameOutOfScope of string * string list * bool
  | StatementType
  | NonreturningStatement
  | AllClausesGuarded
  | PartialMatch of string
  | FragileMatch of string
  | UnusedMatch
  | UnusedPat
  | UnreachableCase
  | ShadowConstructor of string

let number = function
  | LetRecNonFunction _ -> 1
  | NotPrincipal _ -> 2
  | AmbiguousName _ -> 3
  | NameOutOfScope _ -> 4
  | StatementType -> 5
  | NonreturningStatement -> 6
  | AllClausesGuarded -> 7
  | PartialMatch _ -> 8
  | FragileMatch _ -> 9
  | UnusedMatch -> 10
  | UnusedPat -> 11
  | UnreachableCase -> 12
  | ShadowConstructor _ -> 13

let last_warning_number = 13

let message = function
  | LetRecNonFunction(name) ->
    Printf.sprintf "'%s' is not a function, but is bound with 'let rec'" name
  | NotPrincipal s -> s^" is not principal."
  | NameOutOfScope (ty, [nm], false) ->
    nm ^ " was selected from type " ^ ty ^
    ".\nIt is not visible in the current scope, and will not \n\
     be selected if the type becomes unknown."
  | NameOutOfScope (_, _, false) -> assert false
  | NameOutOfScope (ty, slist, true) ->
    "this record of type "^ ty ^" contains fields that are \n\
    not visible in the current scope: "
    ^ String.concat " " slist ^ ".\n\
    They will not be selected if the type becomes unknown."
  | AmbiguousName ([s], tl, false) ->
    s ^ " belongs to several types: " ^ String.concat " " tl ^
    "\nThe first one was selected. Please disambiguate if this is wrong."
  | AmbiguousName (_, _, false) -> assert false
  | AmbiguousName (_slist, tl, true) ->
    "these field labels belong to several types: " ^
      String.concat " " tl ^
    "\nThe first one was selected. Please disambiguate if this is wrong."
  | StatementType -> "this expression should have type void."
  | NonreturningStatement -> "this statement never returns (or has an unsound type)."
  | AllClausesGuarded ->
    "this pattern-matching is not exhaustive.\n\
     All clauses in this pattern-matching are guarded."
  | PartialMatch "" -> "this pattern-matching is not exhaustive."
  | PartialMatch s ->
    "this pattern-matching is not exhaustive.\n\
     Here is an example of a case that is not matched:\n" ^ s
  | FragileMatch "" -> "this pattern-matching is fragile."
  | FragileMatch s ->
    "this pattern-matching is fragile.\n\
     It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | UnusedMatch -> "this match case is unused."
  | UnusedPat -> "this sub-pattern is unused."
  | UnreachableCase -> "this mach case is unreachable."
  | ShadowConstructor s -> "the pattern variable " ^ s ^ " shadows a constructor of the same name."

let sub_locs = function
  | _ -> []

type state = {
  active: bool array;
  error: bool array;
}

let current = ref {
    active = Array.make (last_warning_number + 1) true;
    error = Array.make (last_warning_number + 1) false;
  }

let backup () = !current

let restore x = current := x

let is_active x = (!current).active.(number x)
let is_error x = (!current).error.(number x)

let nerrors = ref 0

type reporting_information = {
  number : int;
  message : string;
  is_error : bool;
  sub_locs : (loc * string) list;
}

let report w =
  if is_error w then incr nerrors;
  `Active { number = number w; message = message w; is_error = is_error w;
            sub_locs = sub_locs w; }


exception Errors

let reset_fatal () =
  nerrors := 0

let check_fatal () =
  if !nerrors > 0 then begin
    nerrors := 0;
    raise Errors;
  end;

