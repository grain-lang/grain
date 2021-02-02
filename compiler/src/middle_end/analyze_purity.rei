open Anftree;
open Grain_typed;

let comp_expression_purity: comp_expression => option(bool);
let anf_expression_purity: anf_expression => option(bool);

let analyze: Analysis_pass.t;
