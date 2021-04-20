let locate_module_file:
  (~loc: Grain_parsing.Location.t, list(string), string) => string;

let locate_unit_object_file: (~path: list(string)=?, string) => string;

let resolve_unit:
  (~search_path: list(string)=?, ~cache: bool=?, string) => string;

let compile_module_dependency: ref((string, string) => unit);

let read_file_cmi: string => Cmi_format.cmi_infos;

let clear_dependency_graph: unit => unit;

// Patched in by env.re:
let with_preserve_unit:
  ref((~loc: Grain_parsing.Location.t, string, string, unit => unit) => unit);

let current_unit_name: ref(unit => string);

let current_filename: ref(unit => string);

let dump_dependency_graph: unit => unit;
