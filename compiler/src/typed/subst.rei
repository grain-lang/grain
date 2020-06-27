/* Modified from OCaml. */
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

/* Substitutions */

open Types;

type t;

/*
    Substitutions are used to translate a type from one context to
    another.  This requires substituting paths for identifiers, and
    possibly also lowering the level of non-generic variables so that
    they are inferior to the maximum level of the new context.

    Substitutions can also be used to create a "clean" copy of a type.
    Indeed, non-variable node of a type are duplicated, with their
    levels set to generic level.  That way, the resulting type is
    well-formed (decreasing levels), even if the original one was not.
 */

let identity: t;

let add_type: (Ident.t, Path.t, t) => t;
let add_type_path: (Path.t, Path.t, t) => t;
let add_type_function:
  (Path.t, ~params: list(type_expr), ~body: type_expr, t) => t;
let add_module: (Ident.t, Path.t, t) => t;
let add_module_path: (Path.t, Path.t, t) => t;
let add_modtype: (Ident.t, module_type, t) => t;
let for_saving: t => t;
let reset_for_saving: unit => unit;

let module_path: (t, Path.t) => Path.t;
let type_path: (t, Path.t) => Path.t;

let type_expr: (t, type_expr) => type_expr;
let value_description: (t, value_description) => value_description;
let type_declaration: (t, type_declaration) => type_declaration;
let modtype: (t, module_type) => module_type;
let signature: (t, signature) => signature;
let modtype_declaration: (t, modtype_declaration) => modtype_declaration;
let module_declaration: (t, module_declaration) => module_declaration;
let typexp: (t, Types.type_expr) => Types.type_expr;

/* Composition of substitutions:
   apply (compose s1 s2) x = apply s2 (apply s1 x) */
let compose: (t, t) => t;

/* A forward reference to be filled in ctype.ml. */
let ctype_apply_env_empty:
  ref((list(type_expr), type_expr, list(type_expr)) => type_expr);
