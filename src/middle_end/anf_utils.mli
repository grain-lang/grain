open Grain_typed
open Anftree

val anf_free_vars : anf_expression -> Ident.Set.t
val comp_free_vars : comp_expression -> Ident.Set.t
val imm_free_vars : imm_expression -> Ident.Set.t

val anf_count_vars : anf_expression -> int
val comp_count_vars : comp_expression -> int
