open Anftree;
open Grain_typed;

let anf_free_vars: anf_expression => Ident.Set.t;
let comp_free_vars: comp_expression => Ident.Set.t;

let analyze: Analysis_pass.t;
