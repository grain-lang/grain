open Grain_middle_end;

let global_name: Grain_typed.Ident.t => string;
let transl_anf_program: Anftree.anf_program => Mashtree.mash_program;
