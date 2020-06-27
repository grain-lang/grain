open Anftree;
open Grain_typed;

let imm_expression_purity: imm_expression => option(bool);
let comp_expression_purity: comp_expression => option(bool);
let anf_expression_purity: anf_expression => option(bool);

let pure_identifiers: anf_program => Ident.tbl(bool);

let analyze: Analysis_pass.t;
