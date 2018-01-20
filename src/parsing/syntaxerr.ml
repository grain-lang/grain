(* See copyright information in syntaxerr.mli *)

type stxerr =
  | Other of Location.t

exception Error of stxerr

let prepare_error = function
  | Other loc ->
    Location.errorf ~loc "Syntax error"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | _ -> None
    )

let location_of_error = function
  | Other l -> l


