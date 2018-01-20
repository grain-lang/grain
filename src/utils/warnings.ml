(* See copyright information in warnings.mli *)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type t =
  | LetRecNonFunction of string

let number = function
  | LetRecNonFunction _ -> 1

let last_warning_number = 1

let message = function
  | LetRecNonFunction(name) ->
    Printf.sprintf "'%s' is not a function, but is bound with 'let rec'" name

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

