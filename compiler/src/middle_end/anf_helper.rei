open Grain_parsing;
open Grain_typed;
open Types;
open Anftree;

type str = loc(string);
type loc = Location.t;
type env = Env.t;
type ident = Ident.t;

module Imm: {
  let mk: (~loc: loc=?, ~env: env=?, imm_expression_desc) => imm_expression;
  let id: (~loc: loc=?, ~env: env=?, ident) => imm_expression;
  let const: (~loc: loc=?, ~env: env=?, constant) => imm_expression;
};

module Comp: {
  let mk: (~loc: loc=?, ~env: env=?, comp_expression_desc) => comp_expression;
  let imm: (~loc: loc=?, ~env: env=?, imm_expression) => comp_expression;
  let int32: (~loc: loc=?, ~env: env=?, int32) => comp_expression;
  let int64: (~loc: loc=?, ~env: env=?, int64) => comp_expression;
  let prim1:
    (~loc: loc=?, ~env: env=?, prim1, imm_expression) => comp_expression;
  let prim2:
    (~loc: loc=?, ~env: env=?, prim2, imm_expression, imm_expression) =>
    comp_expression;
  let box_assign:
    (~loc: loc=?, ~env: env=?, imm_expression, imm_expression) =>
    comp_expression;
  let assign:
    (~loc: loc=?, ~env: env=?, imm_expression, imm_expression) =>
    comp_expression;
  let tuple:
    (~loc: loc=?, ~env: env=?, list(imm_expression)) => comp_expression;
  let array:
    (~loc: loc=?, ~env: env=?, list(imm_expression)) => comp_expression;
  let array_get:
    (~loc: loc=?, ~env: env=?, imm_expression, imm_expression) =>
    comp_expression;
  let array_set:
    (
      ~loc: loc=?,
      ~env: env=?,
      imm_expression,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let record:
    (
      ~loc: loc=?,
      ~env: env=?,
      imm_expression,
      list((str, imm_expression))
    ) =>
    comp_expression;
  let adt:
    (
      ~loc: loc=?,
      ~env: env=?,
      imm_expression,
      imm_expression,
      list(imm_expression)
    ) =>
    comp_expression;
  let tuple_get:
    (~loc: loc=?, ~env: env=?, int32, imm_expression) => comp_expression;
  let tuple_set:
    (~loc: loc=?, ~env: env=?, int32, imm_expression, imm_expression) =>
    comp_expression;
  let adt_get:
    (~loc: loc=?, ~env: env=?, int32, imm_expression) => comp_expression;
  let adt_get_tag:
    (~loc: loc=?, ~env: env=?, imm_expression) => comp_expression;
  let record_get:
    (~loc: loc=?, ~env: env=?, int32, imm_expression) => comp_expression;
  let record_set:
    (~loc: loc=?, ~env: env=?, int32, imm_expression, imm_expression) =>
    comp_expression;
  let if_:
    (
      ~loc: loc=?,
      ~env: env=?,
      imm_expression,
      anf_expression,
      anf_expression
    ) =>
    comp_expression;
  let while_:
    (~loc: loc=?, ~env: env=?, anf_expression, anf_expression) =>
    comp_expression;
  let switch_:
    (
      ~loc: loc=?,
      ~env: env=?,
      imm_expression,
      list((int, anf_expression))
    ) =>
    comp_expression;
  let app:
    (~loc: loc=?, ~env: env=?, imm_expression, list(imm_expression)) =>
    comp_expression;
  let app_builtin:
    (~loc: loc=?, ~env: env=?, string, string, list(imm_expression)) =>
    comp_expression;
  let lambda:
    (~loc: loc=?, ~env: env=?, list(ident), anf_expression) => comp_expression;
  let string: (~loc: loc=?, ~env: env=?, string) => comp_expression;
};

module AExp: {
  let mk: (~loc: loc=?, ~env: env=?, anf_expression_desc) => anf_expression;
  let let_:
    (
      ~loc: loc=?,
      ~env: env=?,
      ~glob: global_flag=?,
      rec_flag,
      list((ident, comp_expression)),
      anf_expression
    ) =>
    anf_expression;
  let seq:
    (~loc: loc=?, ~env: env=?, comp_expression, anf_expression) =>
    anf_expression;
  let comp: (~loc: loc=?, ~env: env=?, comp_expression) => anf_expression;
};

module Imp: {
  let mk: (ident, import_desc, import_shape) => import_spec;
  let grain_value: (ident, string, string, import_shape) => import_spec;
  let wasm_func: (ident, string, string, import_shape) => import_spec;
  let js_func: (ident, string, string, import_shape) => import_spec;
};
