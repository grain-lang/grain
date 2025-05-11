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
exception BadEncoding(Location.t);

type location('a) = loc('a);

type id = loc(Identifier.t);
type str = loc(string);
type loc = Location.t;

module Number: {
  let rational: (str, Location.t, str) => number_type;
};

module Constant: {
  let bytes: str => constant;
  let string: str => constant;
  let char: str => constant;
  let number: number_type => constant;
  let int8: str => constant;
  let int16: str => constant;
  let int32: str => constant;
  let int64: str => constant;
  let uint8: str => constant;
  let uint16: str => constant;
  let uint32: str => constant;
  let uint64: str => constant;
  let float32: str => constant;
  let float64: str => constant;
  let wasmi32: str => constant;
  let wasmi64: str => constant;
  let wasmf32: str => constant;
  let wasmf64: str => constant;
  let bigint: str => constant;
  let rational: str => constant;
  let bool: bool => constant;
  let void: constant;
};

module Type: {
  let mk: (~loc: loc, parsed_type_desc) => parsed_type;
  let any: (~loc: loc, unit) => parsed_type;
  let var: (~loc: loc, string) => parsed_type;
  let arrow:
    (~loc: loc, list(parsed_type_argument), parsed_type) => parsed_type;
  let tuple: (~loc: loc, list(parsed_type)) => parsed_type;
  let constr: (~loc: loc, id, list(parsed_type)) => parsed_type;
  let poly: (~loc: loc, list(str), parsed_type) => parsed_type;
  let force_poly: parsed_type => parsed_type;
};

module ConstructorDeclaration: {
  let mk: (~loc: loc, str, constructor_arguments) => constructor_declaration;
  let singleton: (~loc: loc, str) => constructor_declaration;
  let tuple:
    (~loc: loc, str, location(list(parsed_type))) => constructor_declaration;
  let record:
    (~loc: loc, str, location(list(label_declaration))) =>
    constructor_declaration;
};

module LabelDeclaration: {
  let mk: (~loc: loc, id, parsed_type, mut_flag) => label_declaration;
};

module DataDeclaration: {
  let mk:
    (
      ~loc: loc,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      data_kind,
      option(parsed_type)
    ) =>
    data_declaration;
  let abstract:
    (
      ~loc: loc,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      option(parsed_type)
    ) =>
    data_declaration;
  let variant:
    (
      ~loc: loc,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      list(constructor_declaration)
    ) =>
    data_declaration;
  let record:
    (
      ~loc: loc,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      list(label_declaration)
    ) =>
    data_declaration;
};

module Exception: {
  let mk: (~loc: loc, str, constructor_arguments) => type_exception;
  let singleton: (~loc: loc, str) => type_exception;
  let tuple: (~loc: loc, str, location(list(parsed_type))) => type_exception;
  let record:
    (~loc: loc, str, location(list(label_declaration))) => type_exception;
};

module Pattern: {
  let mk: (~loc: loc, pattern_desc) => pattern;
  let any: (~loc: loc, unit) => pattern;
  let var: (~loc: loc, str) => pattern;
  let tuple: (~loc: loc, list(pattern)) => pattern;
  let array: (~loc: loc, list(pattern)) => pattern;
  let record:
    (~loc: loc, list((option((id, pattern)), Asttypes.closed_flag))) =>
    pattern;
  let list: (~loc: loc, list(list_item(pattern))) => pattern;
  let constant: (~loc: loc, constant) => pattern;
  let constraint_: (~loc: loc, pattern, parsed_type) => pattern;
  let construct: (~loc: loc, id, constructor_pattern) => pattern;
  let singleton_construct: (~loc: loc, id) => pattern;
  let tuple_construct: (~loc: loc, id, list(pattern)) => pattern;
  let record_construct:
    (~loc: loc, id, list((option((id, pattern)), Asttypes.closed_flag))) =>
    pattern;
  let or_: (~loc: loc, pattern, pattern) => pattern;
  let alias: (~loc: loc, pattern, str) => pattern;
};

module Expression: {
  let mk:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, expression_desc) =>
    expression;
  let ident:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, id) => expression;
  let constant:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, constant) =>
    expression;
  let tuple:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(expression)
    ) =>
    expression;
  let record:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      option(expression),
      list((id, expression))
    ) =>
    expression;
  let record_fields:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(record_item(expression))
    ) =>
    expression;
  let record_get:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, expression, id) =>
    expression;
  let record_set:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      id,
      expression
    ) =>
    expression;
  let list:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(list_item(expression))
    ) =>
    expression;
  let array:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(expression)
    ) =>
    expression;
  let array_get:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let array_set:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      ~infix_op: expression=?,
      ~lhs_loc: loc,
      expression,
      expression,
      expression
    ) =>
    expression;
  let let_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      rec_flag,
      mut_flag,
      list(value_binding)
    ) =>
    expression;
  let match:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      location(list(match_branch))
    ) =>
    expression;
  let prim0:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, prim0) => expression;
  let prim1:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      prim1,
      expression
    ) =>
    expression;
  let prim2:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      prim2,
      expression,
      expression
    ) =>
    expression;
  let primn:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      primn,
      list(expression)
    ) =>
    expression;
  let if_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      expression,
      option(expression)
    ) =>
    expression;
  let while_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let for_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      option(expression),
      option(expression),
      option(expression),
      expression
    ) =>
    expression;
  let continue:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, unit) => expression;
  let break:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, unit) => expression;
  let return:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      option(expression)
    ) =>
    expression;
  let constraint_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      parsed_type
    ) =>
    expression;
  let use:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, id, use_items) =>
    expression;
  let box_assign:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let assign:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let lambda:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(lambda_argument),
      expression
    ) =>
    expression;
  let apply:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      list(application_argument)
    ) =>
    expression;
  let construct:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      id,
      constructor_expression
    ) =>
    expression;
  let singleton_construct:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      Location.loc(Identifier.t)
    ) =>
    expression;
  let tuple_construct:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      id,
      list(expression)
    ) =>
    expression;
  let record_construct:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      id,
      list(record_item(expression))
    ) =>
    expression;
  let binop:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      expression,
      expression,
      expression
    ) =>
    expression;
  let block:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(expression)
    ) =>
    expression;
  let ignore: expression => expression;
};

module Toplevel: {
  let mk:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      toplevel_stmt_desc
    ) =>
    toplevel_stmt;
  let include_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      include_declaration
    ) =>
    toplevel_stmt;
  let foreign:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      provide_flag,
      value_description
    ) =>
    toplevel_stmt;
  let module_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      provide_flag,
      module_declaration
    ) =>
    toplevel_stmt;
  let primitive:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      provide_flag,
      primitive_description
    ) =>
    toplevel_stmt;
  let data:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list((provide_flag, data_declaration, loc))
    ) =>
    toplevel_stmt;
  let let_:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      provide_flag,
      rec_flag,
      mut_flag,
      list(value_binding)
    ) =>
    toplevel_stmt;
  let expr:
    (~loc: loc, ~core_loc: loc, ~attributes: attributes=?, expression) =>
    toplevel_stmt;
  let grain_exception:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      provide_flag,
      type_exception
    ) =>
    toplevel_stmt;
  let provide:
    (
      ~loc: loc,
      ~core_loc: loc,
      ~attributes: attributes=?,
      list(provide_item)
    ) =>
    toplevel_stmt;
};

module PrimitiveDescription: {
  let mk: (~loc: loc, ~ident: str, ~name: str, unit) => primitive_description;
};

module ValueDescription: {
  let mk:
    (
      ~loc: loc,
      ~mod_: str,
      ~name: str,
      ~alias: option(str),
      ~typ: parsed_type,
      unit
    ) =>
    value_description;
};

module ValueBinding: {
  let mk: (~loc: loc, pattern, expression) => value_binding;
};

module MatchBranch: {
  let mk:
    (~loc: loc, pattern, expression, option(expression)) => match_branch;
};

module IncludeDeclaration: {
  let mk: (~loc: loc, str, str, option(str)) => include_declaration;
};

module TypeArgument: {
  let mk:
    (~loc: loc, Asttypes.argument_label, parsed_type) => parsed_type_argument;
};

module LambdaArgument: {
  let mk: (~loc: loc, pattern, option(expression)) => lambda_argument;
};

module ModuleDeclaration: {
  let mk: (~loc: loc, str, list(toplevel_stmt)) => module_declaration;
};

module Attribute: {
  let mk: (~loc: loc, str, list(str)) => attribute;
};
