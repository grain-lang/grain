/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Typechecking of type expressions for the core language */

open Grain_parsing;
open Types;

let transl_simple_type:
  (Env.t, bool, Parsetree.parsed_type) => Typedtree.core_type;
let transl_simple_type_univars:
  (Env.t, Parsetree.parsed_type) => Typedtree.core_type;
let transl_simple_type_delayed:
  (Env.t, Parsetree.parsed_type) => (Typedtree.core_type, unit => unit);
/* Translate a type, but leave type variables unbound. Returns
   the type and a function that binds the type variable. */
let transl_type_scheme: (Env.t, Parsetree.parsed_type) => Typedtree.core_type;
let reset_type_variables: unit => unit;
let type_variable: (Location.t, string) => type_expr;
let transl_type_param: (Env.t, Parsetree.parsed_type) => Typedtree.core_type;

type variable_context;
let narrow: unit => variable_context;
let widen: variable_context => unit;

exception Already_bound;

type error =
  | Unbound_type_variable(string)
  | Unbound_type_constructor(Identifier.t)
  | Unbound_type_constructor_2(Path.t)
  | Type_arity_mismatch(Identifier.t, int, int)
  | Bound_type_variable(string)
  | Recursive_type
  | Unbound_row_variable(Identifier.t)
  | Type_mismatch(list((type_expr, type_expr)))
  | Alias_type_mismatch(list((type_expr, type_expr)))
  | Present_has_conjunction(string)
  | Present_has_no_type(string)
  | Constructor_mismatch(type_expr, type_expr)
  | Not_a_variant(type_expr)
  | Variant_tags(string, string)
  | Invalid_variable_name(string)
  | Cannot_quantify(string, type_expr)
  | Multiple_constraints_on_type(Identifier.t)
  | Method_mismatch(string, type_expr, type_expr)
  | Unbound_value(Identifier.t)
  | Unbound_value_in_module(Identifier.t, string)
  | Unbound_constructor(Identifier.t)
  | Unbound_exception(Identifier.t)
  | Unbound_label(Identifier.t)
  | Unbound_module(Identifier.t)
  | Unbound_class(Identifier.t)
  | Unbound_modtype(Identifier.t)
  | Unbound_cltype(Identifier.t)
  | Ill_typed_functor_application(
      Identifier.t,
      Identifier.t,
      option(list(Includemod.error)),
    )
  | Illegal_reference_to_recursive_module
  | Wrong_use_of_module(
      Identifier.t,
      [
        | `Structure_used_as_functor
        | `Abstract_used_as_functor
        | `Functor_used_as_structure
        | `Abstract_used_as_structure
        | `Generative_used_as_applicative
      ],
    )
  | Cannot_scrape_alias(Identifier.t, Path.t)
  | Opened_object(option(Path.t))
  | Not_an_object(type_expr);

exception Error(Location.t, Env.t, error);

let report_error: (Env.t, Format.formatter, error) => unit;

/* Support for first-class modules. */
let transl_modtype_identifier:
  /* from Typemod */
  ref((Location.t, Env.t, Identifier.t) => Path.t);

let find_type:
  (Env.t, Location.t, Identifier.t) => (Path.t, type_declaration);
let find_constructor:
  (Env.t, Location.t, Identifier.t) => constructor_description;
let find_exception: (Env.t, Location.t, Identifier.t) => extension_constructor;
let find_all_constructors:
  (Env.t, Location.t, Identifier.t) =>
  list((constructor_description, unit => unit));
let find_value:
  (Env.t, Location.t, Identifier.t) => (Path.t, value_description);
let find_module:
  (Env.t, Location.t, Identifier.t) => (Path.t, module_declaration);
let lookup_module:
  (~load: bool=?, Env.t, Location.t, Identifier.t, option(string)) => Path.t;
let find_modtype:
  (Env.t, Location.t, Identifier.t) => (Path.t, modtype_declaration);

let type_attributes: Asttypes.attributes => Typedtree.attributes;

let unbound_label_error: (Env.t, Location.loc(Identifier.t)) => 'a;
let unbound_constructor_error: (Env.t, Location.loc(Identifier.t)) => 'a;
