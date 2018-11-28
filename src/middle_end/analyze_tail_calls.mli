open Anftree
open Grain_typed

val comp_is_tail_recursive : comp_expression -> bool option
val comp_is_tail_call : comp_expression -> bool option
val mark_not_tail_recursive : comp_expression -> unit

val analyze : Analysis_pass.t
