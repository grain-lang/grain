open Anftree
open Grain_typed

val imm_expression_purity : imm_expression -> bool option
val comp_expression_purity : comp_expression -> bool option
val anf_expression_purity : anf_expression -> bool option

val pure_identifiers : anf_program -> bool Ident.tbl

val analyze : Analysis_pass.t
