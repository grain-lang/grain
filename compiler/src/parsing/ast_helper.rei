/* This file is largely copied from OCaml's parsing/ast_helper.mli.
   The original copyright notice is reproduced below. */
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

open Parsetree;

type id = loc(Identifier.t);
type str = loc(string);
type loc = Location.t;

/** Default value for all optional location arguments. */

let default_loc_src: ref(unit => loc);

/** Set the [default_loc] within the scope of the execution
        of the provided function. */

let with_default_loc: (loc, unit => 'a) => 'a;

let with_default_loc_src: (unit => loc, unit => 'a) => 'a;

module Const: {
  let string: string => constant;
  let int: int => constant;
  let int32: int32 => constant;
  let int64: int64 => constant;
  let bool: bool => constant;
  let void: constant;
};

module Typ: {
  let mk: (~loc: loc=?, parsed_type_desc) => parsed_type;
  let any: (~loc: loc=?, unit) => parsed_type;
  let var: (~loc: loc=?, string) => parsed_type;
  let arrow: (~loc: loc=?, list(parsed_type), parsed_type) => parsed_type;
  let tuple: (~loc: loc=?, list(parsed_type)) => parsed_type;
  let constr: (~loc: loc=?, id, list(parsed_type)) => parsed_type;
  let poly: (~loc: loc=?, list(str), parsed_type) => parsed_type;
  let force_poly: parsed_type => parsed_type;
};

module CDecl: {
  let mk: (~loc: loc=?, str, constructor_arguments) => constructor_declaration;
  let singleton: (~loc: loc=?, str) => constructor_declaration;
  let tuple: (~loc: loc=?, str, list(parsed_type)) => constructor_declaration;
};

module LDecl: {
  let mk: (~loc: loc=?, id, parsed_type, mut_flag) => label_declaration;
};

module Dat: {
  let mk:
    (~loc: loc=?, str, list(parsed_type), data_kind) => data_declaration;
  let variant:
    (~loc: loc=?, str, list(parsed_type), list(constructor_declaration)) =>
    data_declaration;
  let record:
    (~loc: loc=?, str, list(parsed_type), list(label_declaration)) =>
    data_declaration;
};

module Pat: {
  let mk: (~loc: loc=?, pattern_desc) => pattern;
  let any: (~loc: loc=?, unit) => pattern;
  let var: (~loc: loc=?, str) => pattern;
  let tuple: (~loc: loc=?, list(pattern)) => pattern;
  let record:
    (~loc: loc=?, list((option((id, pattern)), Asttypes.closed_flag))) =>
    pattern;
  let list: (~loc: loc=?, list(pattern), option(pattern)) => pattern;
  let constant: (~loc: loc=?, constant) => pattern;
  let constraint_: (~loc: loc=?, pattern, parsed_type) => pattern;
  let construct: (~loc: loc=?, id, list(pattern)) => pattern;
  let or_: (~loc: loc=?, pattern, pattern) => pattern;
  let alias: (~loc: loc=?, pattern, str) => pattern;
};

module Exp: {
  let mk: (~loc: loc=?, expression_desc) => expression;
  let ident: (~loc: loc=?, id) => expression;
  let constant: (~loc: loc=?, constant) => expression;
  let tuple: (~loc: loc=?, list(expression)) => expression;
  let record: (~loc: loc=?, list((id, expression))) => expression;
  let record_get: (~loc: loc=?, expression, id) => expression;
  let record_set: (~loc: loc=?, expression, id, expression) => expression;
  let list:
    (~loc: loc=?, list(expression), option(expression)) => expression;
  let array: (~loc: loc=?, list(expression)) => expression;
  let array_get: (~loc: loc=?, expression, expression) => expression;
  let array_set:
    (~loc: loc=?, expression, expression, expression) => expression;
  let let_:
    (~loc: loc=?, rec_flag, mut_flag, list(value_binding), expression) =>
    expression;
  let match: (~loc: loc=?, expression, list(match_branch)) => expression;
  let prim1: (~loc: loc=?, prim1, expression) => expression;
  let prim2: (~loc: loc=?, prim2, expression, expression) => expression;
  let if_: (~loc: loc=?, expression, expression, expression) => expression;
  let while_: (~loc: loc=?, expression, expression) => expression;
  let constraint_: (~loc: loc=?, expression, parsed_type) => expression;
  let box_assign: (~loc: loc=?, expression, expression) => expression;
  let assign: (~loc: loc=?, expression, expression) => expression;
  let lambda: (~loc: loc=?, list(pattern), expression) => expression;
  let apply: (~loc: loc=?, expression, list(expression)) => expression;
  let block: (~loc: loc=?, list(expression)) => expression;
  let null: (~loc: loc=?, unit) => expression;
  let ignore: expression => expression;
};

module Top: {
  let mk: (~loc: loc=?, toplevel_stmt_desc) => toplevel_stmt;
  let import: (~loc: loc=?, list(import_declaration)) => toplevel_stmt;
  let foreign: (~loc: loc=?, export_flag, value_description) => toplevel_stmt;
  let primitive:
    (~loc: loc=?, export_flag, value_description) => toplevel_stmt;
  let data: (~loc: loc=?, export_flag, data_declaration) => toplevel_stmt;
  let let_:
    (~loc: loc=?, export_flag, rec_flag, mut_flag, list(value_binding)) =>
    toplevel_stmt;
  let expr: (~loc: loc=?, expression) => toplevel_stmt;
  let export: (~loc: loc=?, list(export_declaration)) => toplevel_stmt;
  let export_all: (~loc: loc=?, list(export_except)) => toplevel_stmt;
};

module Val: {
  let mk:
    (
      ~loc: loc=?,
      ~mod_: str,
      ~name: str,
      ~alias: option(str),
      ~typ: parsed_type,
      ~prim: list(string)
    ) =>
    value_description;
};

module Vb: {let mk: (~loc: loc=?, pattern, expression) => value_binding;};

module Mb: {let mk: (~loc: loc=?, pattern, expression) => match_branch;};

module Imp: {
  let mk:
    (~loc: loc=?, list((import_value, option(id))), str) =>
    list(import_declaration);
};

module Ex: {
  let mk:
    (~loc: loc=?, list((str, option(str)))) => list(export_declaration);
};
