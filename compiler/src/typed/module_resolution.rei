let resolve_unit:
  (~base_dir: Fp.t(Fp.absolute), string) => option(Fp.t(Fp.absolute));

let locate_object_file:
  (
    ~loc: Grain_parsing.Location.t=?,
    ~base_dir: Fp.t(Fp.absolute)=?,
    string
  ) =>
  Fp.t(Fp.absolute);

let read_file_cmi: string => Cmi_format.cmi_infos;

let clear_cmi_cache: unit => unit;
