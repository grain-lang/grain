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

exception SyntaxError(Location.t, string);

type listitem('a) =
  | ListItem('a)
  | ListSpread('a, Location.t);

type id = loc(Identifier.t);
type str = loc(string);
type loc = Location.t;

module Const: {
  let string: string => constant;
  let char: string => constant;
  let number: number_type => constant;
  let int32: string => constant;
  let int64: string => constant;
  let float32: string => constant;
  let float64: string => constant;
  let wasmi32: string => constant;
  let wasmi64: string => constant;
  let wasmf32: string => constant;
  let wasmf64: string => constant;
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
    (~loc: loc=?, str, list(parsed_type), data_kind, option(parsed_type)) =>
    data_declaration;
  let abstract:
    (~loc: loc=?, str, list(parsed_type), option(parsed_type)) =>
    data_declaration;
  let variant:
    (~loc: loc=?, str, list(parsed_type), list(constructor_declaration)) =>
    data_declaration;
  let record:
    (~loc: loc=?, str, list(parsed_type), list(label_declaration)) =>
    data_declaration;
};

module Except: {
  let mk: (~loc: loc=?, str, constructor_arguments) => type_exception;
  let singleton: (~loc: loc=?, str) => type_exception;
  let tuple: (~loc: loc=?, str, list(parsed_type)) => type_exception;
};

module Pat: {
  let mk: (~loc: loc=?, pattern_desc) => pattern;
  let any: (~loc: loc=?, unit) => pattern;
  let var: (~loc: loc=?, str) => pattern;
  let tuple: (~loc: loc=?, list(pattern)) => pattern;
  let array: (~loc: loc=?, list(pattern)) => pattern;
  let record:
    (~loc: loc=?, list((option((id, pattern)), Asttypes.closed_flag))) =>
    pattern;
  let list: (~loc: loc=?, list(listitem(pattern))) => pattern;
  let constant: (~loc: loc=?, constant) => pattern;
  let constraint_: (~loc: loc=?, pattern, parsed_type) => pattern;
  let construct: (~loc: loc=?, id, list(pattern)) => pattern;
  let or_: (~loc: loc=?, pattern, pattern) => pattern;
  let alias: (~loc: loc=?, pattern, str) => pattern;
};

module Exp: {
  let mk:
    (~loc: loc=?, ~attributes: attributes=?, expression_desc) => expression;
  let ident: (~loc: loc=?, ~attributes: attributes=?, id) => expression;
  let constant:
    (~loc: loc=?, ~attributes: attributes=?, constant) => expression;
  let tuple:
    (~loc: loc=?, ~attributes: attributes=?, list(expression)) => expression;
  let record:
    (~loc: loc=?, ~attributes: attributes=?, list((id, expression))) =>
    expression;
  let record_get:
    (~loc: loc=?, ~attributes: attributes=?, expression, id) => expression;
  let record_set:
    (~loc: loc=?, ~attributes: attributes=?, expression, id, expression) =>
    expression;
  let list:
    (~loc: loc=?, ~attributes: attributes=?, list(listitem(expression))) =>
    expression;
  let array:
    (~loc: loc=?, ~attributes: attributes=?, list(expression)) => expression;
  let array_get:
    (~loc: loc=?, ~attributes: attributes=?, expression, expression) =>
    expression;
  let array_set:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      expression,
      expression,
      expression
    ) =>
    expression;
  let let_:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      rec_flag,
      mut_flag,
      list(value_binding)
    ) =>
    expression;
  let match:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      expression,
      list(match_branch)
    ) =>
    expression;
  let prim0: (~loc: loc=?, ~attributes: attributes=?, prim0) => expression;
  let prim1:
    (~loc: loc=?, ~attributes: attributes=?, prim1, expression) => expression;
  let prim2:
    (~loc: loc=?, ~attributes: attributes=?, prim2, expression, expression) =>
    expression;
  let primn:
    (~loc: loc=?, ~attributes: attributes=?, primn, list(expression)) =>
    expression;
  let if_:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      expression,
      expression,
      expression
    ) =>
    expression;
  let while_:
    (~loc: loc=?, ~attributes: attributes=?, expression, expression) =>
    expression;
  let for_:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      option(expression),
      option(expression),
      option(expression),
      expression
    ) =>
    expression;
  let continue: (~loc: loc=?, ~attributes: attributes=?, unit) => expression;
  let break: (~loc: loc=?, ~attributes: attributes=?, unit) => expression;
  let constraint_:
    (~loc: loc=?, ~attributes: attributes=?, expression, parsed_type) =>
    expression;
  let box_assign:
    (~loc: loc=?, ~attributes: attributes=?, expression, expression) =>
    expression;
  let assign:
    (~loc: loc=?, ~attributes: attributes=?, expression, expression) =>
    expression;
  let lambda:
    (~loc: loc=?, ~attributes: attributes=?, list(pattern), expression) =>
    expression;
  let apply:
    (~loc: loc=?, ~attributes: attributes=?, expression, list(expression)) =>
    expression;
  let binop:
    (~loc: loc=?, ~attributes: attributes=?, expression, list(expression)) =>
    expression;
  let block:
    (~loc: loc=?, ~attributes: attributes=?, list(expression)) => expression;
  let null: (~loc: loc=?, ~attributes: attributes=?, unit) => expression;
  let ignore: expression => expression;
};

module Top: {
  let mk:
    (~loc: loc=?, ~attributes: attributes=?, toplevel_stmt_desc) =>
    toplevel_stmt;
  let import:
    (~loc: loc=?, ~attributes: attributes=?, import_declaration) =>
    toplevel_stmt;
  let foreign:
    (~loc: loc=?, ~attributes: attributes=?, export_flag, value_description) =>
    toplevel_stmt;
  let primitive:
    (~loc: loc=?, ~attributes: attributes=?, export_flag, value_description) =>
    toplevel_stmt;
  let data:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      list((export_flag, data_declaration))
    ) =>
    toplevel_stmt;
  let let_:
    (
      ~loc: loc=?,
      ~attributes: attributes=?,
      export_flag,
      rec_flag,
      mut_flag,
      list(value_binding)
    ) =>
    toplevel_stmt;
  let expr:
    (~loc: loc=?, ~attributes: attributes=?, expression) => toplevel_stmt;
  let grain_exception:
    (~loc: loc=?, ~attributes: attributes=?, export_flag, type_exception) =>
    toplevel_stmt;
  let export:
    (~loc: loc=?, ~attributes: attributes=?, list(export_declaration)) =>
    toplevel_stmt;
  let export_all:
    (~loc: loc=?, ~attributes: attributes=?, list(export_except)) =>
    toplevel_stmt;
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

module Mb: {
  let mk:
    (~loc: loc=?, pattern, expression, option(expression)) => match_branch;
};

module Imp: {
  let mk: (~loc: loc=?, list(import_value), str) => import_declaration;
};

module Ex: {
  let mk:
    (~loc: loc=?, list((str, option(str)))) => list(export_declaration);
};
