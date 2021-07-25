let known_function: Grain_middle_end.Anftree.imm_expression => option(string);
let has_first_class_usage: Grain_middle_end.Anftree.imm_expression => bool;

let preprocess: Grain_middle_end.Anftree.anf_program => unit;
