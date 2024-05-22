open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_codegen;

type input_source =
  | InputString(string)
  | InputFile(string);

type compilation_state_desc =
  | Initial(input_source)
  | Parsed(Parsetree.parsed_program)
  | WellFormed(Parsetree.parsed_program)
  | TypeChecked(Typedtree.typed_program)
  | TypedWellFormed(Typedtree.typed_program)
  | Linearized(Anftree.anf_program)
  | Optimized(Anftree.anf_program)
  | Mashed(Mashtree.mash_program)
  | ObjectEmitted;

type compilation_state = {
  cstate_desc: compilation_state_desc,
  cstate_filename: option(string),
  cstate_object_outfile: string,
};

type compilation_action =
  | Continue(compilation_state)
  | Stop;

type error =
  | Cannot_parse_inline_flags(string)
  | Cannot_use_help_or_version;

exception InlineFlagsError(Location.t, error);

let default_wasm_filename: string => string;
let default_object_filename: string => string;

let stop_after_parse: compilation_state => compilation_action;

let stop_after_well_formed: compilation_state => compilation_action;

let stop_after_typed: compilation_state => compilation_action;

let stop_after_typed_well_formed: compilation_state => compilation_action;

let stop_after_anf: compilation_state => compilation_action;

let stop_after_optimization: compilation_state => compilation_action;

let stop_after_mashed: compilation_state => compilation_action;

let stop_after_object_emitted: compilation_state => compilation_action;

let reset_compiler_state: unit => unit;

let compile_wasi_polyfill: unit => unit;

let compile_string:
  (
    ~hook: compilation_state => compilation_action=?,
    ~name: string=?,
    ~outfile: string,
    string
  ) =>
  compilation_state;

let compile_file:
  (
    ~hook: compilation_state => compilation_action=?,
    ~outfile: string,
    string
  ) =>
  compilation_state;
