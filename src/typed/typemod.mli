open Grain_parsing
open Types
open Typedtree

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Identifier.t
  | With_mismatch of Identifier.t * Includemod.error list
  | With_makes_applicative_functor_ill_typed of
      Identifier.t * Path.t * Includemod.error list
  | With_changes_module_alias of Identifier.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Identifier.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val type_implementation : Parsetree.parsed_program -> Typedtree.typed_program
