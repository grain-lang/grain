open Grain_typed;
open Anftree;

type stack_size = {
  stack_size_ptr: int,
  stack_size_i32: int,
  stack_size_i64: int,
  stack_size_f32: int,
  stack_size_f64: int,
};

let anf_count_vars: anf_expression => stack_size;
let comp_count_vars: comp_expression => stack_size;

let clear_locations: anf_program => anf_program;
