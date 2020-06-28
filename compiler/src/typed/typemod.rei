open Grain_parsing;
open Types;
open Typedtree;

type error =
  | Cannot_apply(module_type)
  | Not_included(list(Includemod.error))
  | Cannot_eliminate_dependency(module_type)
  | Signature_expected
  | Structure_expected(module_type)
  | With_no_component(Identifier.t)
  | With_mismatch(Identifier.t, list(Includemod.error))
  | With_makes_applicative_functor_ill_typed(
      Identifier.t,
      Path.t,
      list(Includemod.error),
    )
  | With_changes_module_alias(Identifier.t, Ident.t, Path.t)
  | With_cannot_remove_constrained_type
  | Repeated_name(string, string)
  | Non_generalizable(type_expr)
  | Non_generalizable_module(module_type)
  | Implementation_is_required(string)
  | Interface_not_compiled(string)
  | Not_allowed_in_functor_body
  | Not_a_packed_module(type_expr)
  | Incomplete_packed_module(type_expr)
  | Scoping_pack(Identifier.t, type_expr)
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias(Path.t);

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

let type_implementation: Parsetree.parsed_program => Typedtree.typed_program;
