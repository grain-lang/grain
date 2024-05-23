let resolve_unit: (~base_dir: string, string) => option(string);

let locate_object_file:
  (~loc: Grain_parsing.Location.t=?, ~base_dir: string=?, string) => string;

let read_file_cmi: string => Cmi_format.cmi_infos;

let clear_cmi_cache: unit => unit;
