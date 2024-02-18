open Grain_typed;

let grain_type_code_block: string => string;

let grain_code_block: string => string;

let markdown_join: (string, string) => string;

let print_type: (Env.t, Types.type_expr) => string;

let print_type_raw: Types.type_expr => string;

let print_mod_type: Types.module_declaration => string;
