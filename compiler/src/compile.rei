open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_codegen;
open Grain_linking;

type input_source =
  | InputString(string)
  | InputFile(string);

type compilation_state_desc =
  | Initial(input_source)
  | Parsed(Parsetree.parsed_program)
  | WellFormed(Parsetree.parsed_program)
  | DependenciesCompiled(Parsetree.parsed_program)
  | TypeChecked(Typedtree.typed_program)
  | TypedWellFormed(Typedtree.typed_program)
  | Linearized(Anftree.anf_program)
  | Optimized(Anftree.anf_program)
  | Mashed(Mashtree.mash_program)
  | Compiled(Compmod.compiled_program)
  | ObjectFileEmitted(Compmod.compiled_program)
  | Linked(Compmod.compiled_program)
  | Assembled;

type compilation_state = {
  cstate_desc: compilation_state_desc,
  cstate_filename: option(string),
  cstate_outfile: option(string),
};

type compilation_action =
  | Continue(compilation_state)
  | Stop;

type error =
  | Cannot_parse_inline_flags(string)
  | Cannot_use_help_or_version;

exception InlineFlagsError(Location.t, error);

let default_output_filename: string => string;

let stop_after_parse: compilation_state => compilation_action;

let stop_after_well_formed: compilation_state => compilation_action;

let stop_after_typed: compilation_state => compilation_action;

let stop_after_typed_well_formed: compilation_state => compilation_action;

let stop_after_anf: compilation_state => compilation_action;

let stop_after_optimization: compilation_state => compilation_action;

let stop_after_mashed: compilation_state => compilation_action;

let stop_after_compiled: compilation_state => compilation_action;

let stop_after_object_file_emitted: compilation_state => compilation_action;

let stop_after_linked: compilation_state => compilation_action;

let stop_after_assembled: compilation_state => compilation_action;

let compile_string:
  (
    ~is_root_file: bool=?,
    ~hook: compilation_state => compilation_action=?,
    ~name: string=?,
    ~outfile: string=?,
    ~reset: bool=?,
    string
  ) =>
  compilation_state;

let compile_file:
  (
    ~is_root_file: bool=?,
    ~hook: compilation_state => compilation_action=?,
    ~outfile: string=?,
    ~reset: bool=?,
    string
  ) =>
  compilation_state;
