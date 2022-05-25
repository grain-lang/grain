let locate_module_file:
  (~loc: Grain_parsing.Location.t, ~disable_relpath: bool=?, Fp.firstClass) =>
  Fp.t(Fp.absolute);

let locate_unit_object_file:
  (
    ~path: list(Fp.t(Fp.absolute))=?,
    ~base_dir: Fp.t(Fp.absolute)=?,
    Fp.firstClass
  ) =>
  Fp.t(Fp.absolute);

let resolve_unit:
  (~cache: bool=?, ~base_dir: Fp.t(Fp.absolute)=?, Fp.firstClass) =>
  Fp.t(Fp.absolute);

let compile_module_dependency:
  ref((Fp.t(Fp.absolute), Fp.t(Fp.absolute)) => unit);

let read_file_cmi: Fp.t(Fp.absolute) => Cmi_format.cmi_infos;

let clear_dependency_graph: unit => unit;

// Patched in by env.re:
let with_preserve_unit:
  ref((~loc: Grain_parsing.Location.t, string, string, unit => unit) => unit);

let current_unit_name: ref(unit => string);

let current_filename: ref(unit => Fp.t(Fp.absolute));

let dump_dependency_graph: unit => unit;
