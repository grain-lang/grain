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

/* Inclusion checks for the module language */

open Grain_parsing;
open Typedtree;
open Types;
open Format;

/** Type describing which arguments of an inclusion to consider as used
    for the usage warnings. [Mark_both] is the default. */

type mark =
  | /** Mark definitions used from both arguments */
    Mark_both
  | /** Mark definitions used from the positive (first) argument */
    Mark_positive
  | /** Mark definitions used from the negative (second) argument */
    Mark_negative
  | /** Do not mark definitions used from either argument */
    Mark_neither;

/* Stub; may need to port this over at some point */
type module_coercion = option(unit);

let modtypes:
  (~loc: Location.t, Env.t, ~mark: mark=?, module_type, module_type) =>
  module_coercion;

/** [check_modtype_inclusion ~loc env mty1 path1 mty2] checks that the
    functor application F(M) is well typed, where mty2 is the type of
    the argument of F and path1/mty1 is the path/unstrenghened type of M. */

let check_modtype_inclusion:
  (~loc: Location.t, Env.t, Types.module_type, Path.t, Types.module_type) =>
  unit;

let signatures:
  (Env.t, ~mark: mark=?, signature, signature) => module_coercion;

let compunit:
  (Env.t, ~mark: mark=?, string, signature, string, signature) =>
  module_coercion;

let type_declarations:
  (
    ~loc: Location.t,
    Env.t,
    ~mark: mark=?,
    Ident.t,
    type_declaration,
    type_declaration
  ) =>
  unit;

type symptom =
  | Missing_field(Ident.t, Location.t, string) /* kind */
  | Value_descriptions(Ident.t, value_description, value_description)
  | Type_declarations(
      Ident.t,
      type_declaration,
      type_declaration,
      list(Includecore.type_mismatch),
    )
  | Module_types(module_type, module_type)
  | Modtype_infos(Ident.t, modtype_declaration, modtype_declaration)
  | Modtype_permutation
  | Interface_mismatch(string, string)
  | Unbound_modtype_path(Path.t)
  | Unbound_module_path(Path.t)
  | Invalid_module_alias(Path.t);

type pos =
  | Module(Ident.t)
  | Modtype(Ident.t)
  | Arg(Ident.t)
  | Body(Ident.t);
type error = (list(pos), Env.t, symptom);

exception Error(list(error));

let report_error: (formatter, list(error)) => unit;
/* val expand_module_alias: Env.t -> pos list -> Path.t -> Types.module_type */
