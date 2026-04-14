let get_object_name: string => string;

let source_artifact_filename: (~ext: string, string) => string;

let source_output_filename: (~ext: string, string) => string;

let locate_object_file: (~loc: Grain_parsing.Location.t, string) => string;

let locate_unit_object_file: (~base_dir: string=?, string) => string;

let resolve_unit: (~cache: bool=?, ~base_dir: string=?, string) => string;

let load_dependency_graph: string => unit;
let load_dependency_graph_from_string: (string, string) => unit;

let read_file_cmi: string => Cmi_format.cmi_infos;

let clear_dependency_graph: unit => unit;

let current_filename: ref(unit => string);

let get_dependencies: unit => list(string);

let get_out_of_date_dependencies: unit => list(string);

let dump_dependency_graph: unit => unit;
