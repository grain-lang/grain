open Grain_typed;
open Anftree;

type ident_allocation_pair = (Ident.t, Types.allocation_type);

module IdentAllocationSet: Set.S with type elt = ident_allocation_pair;

let anf_free_vars: anf_expression => IdentAllocationSet.t;
let comp_free_vars: comp_expression => IdentAllocationSet.t;
let imm_free_vars: imm_expression => IdentAllocationSet.t;

let anf_count_vars: anf_expression => (int, int, int, int, int);
let comp_count_vars: comp_expression => (int, int, int, int, int);

let clear_locations: anf_program => anf_program;
