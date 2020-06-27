open Grain_typed;
open Anftree;

let anf_free_vars: anf_expression => Ident.Set.t;
let comp_free_vars: comp_expression => Ident.Set.t;
let imm_free_vars: imm_expression => Ident.Set.t;

let anf_count_vars: anf_expression => int;
let comp_count_vars: comp_expression => int;

let clear_locations: anf_program => anf_program;
