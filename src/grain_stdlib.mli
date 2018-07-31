(** Definitions for interacting with the standard library. *)

(** Loads the libraries which are included in the given program.
    The given environment will be used to check the well-formedness
    of any loaded libraries, and the given list of directories will
    be searched to find included libraries. *)
val load_libraries : Grain_parsing.Parsetree.parsed_program -> Grain_parsing.Parsetree.parsed_program
