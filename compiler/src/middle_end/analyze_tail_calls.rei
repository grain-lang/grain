open Anftree;
open Grain_typed;

let comp_is_tail_recursive: comp_expression => option(bool);
let comp_is_tail_call: comp_expression => option(bool);
let mark_not_tail_recursive: comp_expression => unit;

let analyze: Analysis_pass.t;
