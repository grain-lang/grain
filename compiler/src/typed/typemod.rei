open Grain_parsing;
open Types;
open Typedtree;

type error =
  | Include_module_name_mismatch(string, string, string)
  | Signature_expected
  | Structure_expected(module_type)
  | Repeated_name(string, string)
  | Non_generalizable(type_expr)
  | Non_generalizable_module(module_type)
  | Cannot_scrape_alias(Path.t)
  | Nonrecursive_type_with_recursion(Identifier.t);

exception Error(Location.t, Env.t, error);

let type_implementation: Parsetree.parsed_program => Typedtree.typed_program;
