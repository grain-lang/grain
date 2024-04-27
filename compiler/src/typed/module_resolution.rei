let get_output_name: string => string;

let locate_module_file:
  (~loc: Grain_parsing.Location.t, ~disable_relpath: bool=?, string) => string;

let locate_unit_object_file:
  (~path: list(string)=?, ~base_dir: string=?, string) => string;

let resolve_unit:
  (
    ~search_path: list(string)=?,
    ~cache: bool=?,
    ~base_dir: string=?,
    string
  ) =>
  string;

let compile_module_dependency: ref((string, string) => unit);

let compile_dependency_graph:
  (~base_file: string, list(Grain_parsing.Location.loc(string))) => unit;

let read_file_cmi: string => Cmi_format.cmi_infos;

let clear_dependency_graph: unit => unit;

// Patched in by env.re:
let with_preserve_unit:
  ref((~loc: Grain_parsing.Location.t, string, string, unit => unit) => unit);

let current_unit_name: ref(unit => string);

let current_filename: ref(unit => string);

let get_dependencies: unit => list(string);

let dump_dependency_graph: unit => unit;
