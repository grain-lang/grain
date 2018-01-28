open Grain_parsing
open Parsetree
open Types

type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side

type type_expected = {
  ty: type_expr;
  explanation: type_forcing_context option;
}

type error

(*
  Saving and outputting type information.
  We keep these function names short, because they have to be
  called each time we create a record of type [Typedtree.expression]
  or [Typedtree.pattern] that will end up in the typed AST.
*)

val re: Typedtree.expression -> Typedtree.expression
val rp: Typedtree.pattern -> Typedtree.pattern

val mk_expected : ?explanation:type_forcing_context -> type_expr -> type_expected

val type_constant : Asttypes.constant -> type_expr

val constant : Parsetree.constant -> (Asttypes.constant, error) result

val constant_or_raise : Env.t -> Location.t -> Parsetree.constant -> Asttypes.constant
