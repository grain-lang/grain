/** Optimization pass which optimizes boxes used locally into local mutations. */

let clear_mutable_variables: unit => unit;
let is_mutable_variable: Grain_typed.Ident.t => bool;

let optimize: Optimization_pass.t;
