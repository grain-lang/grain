open Errors
open Types

(** Definitions for interacting with the standard library. *)

(** Path to the default grain standard library. This path must be included
    in calls to load_libraries in order to be searched (this is so that the
    standard library can be disabled). If the grain root is not set, this will return None. *)
val stdlib_directory : unit -> string option

(** Loads the libraries which are included in the given program.
    The given environment will be used to check the well-formedness
    of any loaded libraries, and the given list of directories will
    be searched to find included libraries. *)
val load_libraries : sourcespan envt -> string list -> sourcespan program -> (exn list, sourcespan program) either
