open Grain_parsing;
open Grain_utils;

let locate_module_file:
  (~loc: Location.t, ~disable_relpath: bool=?, string) => Filepath.t;

let locate_unit_object_file:
  (~path: list(Filepath.t)=?, ~base_dir: Filepath.t=?, string) => Filepath.t;

let resolve_unit:
  (~cache: bool=?, ~base_dir: Filepath.t=?, string) => Filepath.t;

let compile_module_dependency: ref((Filepath.t, Filepath.t) => unit);

let read_file_cmi: Filepath.t => Cmi_format.cmi_infos;

let clear_dependency_graph: unit => unit;

// Patched in by env.re:
let with_preserve_unit:
  ref((~loc: Location.t, string, Filepath.t, unit => unit) => unit);

let current_unit_name: ref(unit => string);

let current_filename: ref(unit => Filepath.t);

let dump_dependency_graph: unit => unit;
