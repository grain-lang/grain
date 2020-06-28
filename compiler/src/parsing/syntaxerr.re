/* See copyright information in syntaxerr.mli */

type stxerr =
  | LetWithoutBody(Location.t)
  | Other(Location.t);

exception Error(stxerr);

let prepare_error =
  fun
  | LetWithoutBody(loc) =>
    Location.errorf(~loc, "Missing expression after let binding")
  | Other(loc) => Location.errorf(~loc, "Syntax error");

let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(prepare_error(err))
    | _ => None,
  );

let location_of_error =
  fun
  | LetWithoutBody(l)
  | Other(l) => l;
