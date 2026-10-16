open Grain_parsing;
open Grain_typed;
open Docir;
open Errors;

module Docir = Docir;
module Errors = Errors;

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                               AST GENERATION                             │
// └──────────────────────────────────────────────────────────────────────────┘

/**
 * Generates a docir from a type description.
 *
 * @param namespace The root namespace of the type
 * @param ident The identifier of the type
 * @param type_desc The type description to generate from
 * @return The docir for the given type description
 */
let from_type_description:
  (~namespace: option(string), ~ident: Ident.t, Types.type_declaration) =>
  type_info;

/**
 * Generates a docir from a value description.
 *
 * @param namespace The root namespace of the value
 * @param ident The identifier of the value
 * @param value_desc The value description to generate from
 * @return The docir for the given value description
 */
let from_value_description:
  (~namespace: option(string), ~ident: Ident.t, Types.value_description) =>
  value_info;

/**
 * Generates a docir from a module signature.
 *
 * @param namespace The root namespace of the module
 * @param name The name of the module
 * @param loc The location of the module
 * @param signature The module signature to generate from
 * @return The docir for the given module signature
 */
let from_module_signature:
  (
    ~namespace: option(string),
    ~name: string,
    ~loc: Location.t,
    Types.signature
  ) =>
  module_info;

/**
 * Generates a docir from a program.
 *
 * @param program The program to generate from
 * @return The docir for the given module program
 */
let from_program: Typedtree.typed_program => module_info;
