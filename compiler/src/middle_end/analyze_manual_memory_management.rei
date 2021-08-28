open Anftree;
open Grain_typed;

let mod_has_manual_memory_management: ref(bool);

let is_manual_memory_management_call: Ident.t => bool;

let analyze: Analysis_pass.t;
