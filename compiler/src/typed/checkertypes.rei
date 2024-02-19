open Grain_parsing;
open Parsetree;
open Types;

/* This variant is used to print improved error messages, and does not affect
      the behavior of the typechecker itself.

      It describes possible explanation for types enforced by a keyword of the
      language; e.g. "if" requires the condition to be of type bool, and the
      then-branch to be of type unit if there is no else branch; "for" requires
      indices to be of type int, and the body to be of type unit.
   */
type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | Loop_conditional
  | Loop_body
  | Assert_condition
  | Sequence_left_hand_side
  | Assign_not_box
  | Assign_not_array
  | Assign_not_array_index;

/* The combination of a type and a "type forcing context". The intent is that it
      describes a type that is "expected" (required) by the context. If unifying
      with such a type fails, then the "explanation" field explains why it was
      required, in order to display a more enlightening error message.
   */
type type_expected = {
  ty: type_expr,
  explanation: option(type_forcing_context),
};

/*
   Saving and outputting type information.
   We keep these function names short, because they have to be
   called each time we create a record of type [Typedtree.expression]
   or [Typedtree.pattern] that will end up in the typed AST.
 */

let re: Typedtree.expression => Typedtree.expression;
let rp: Typedtree.pattern => Typedtree.pattern;

let mk_expected:
  (~explanation: type_forcing_context=?, type_expr) => type_expected;

let type_constant: Asttypes.constant => type_expr;

let constant:
  (Location.t, Parsetree.constant) =>
  result(Asttypes.constant, Location.error);

let constant_or_raise:
  (Env.t, Location.t, Parsetree.constant) => Asttypes.constant;
