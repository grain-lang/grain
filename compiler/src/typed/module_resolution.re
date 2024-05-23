open Grain_parsing;
open Grain_utils;
open Cmi_format;

type error =
  | No_object_file(option(Location.t), string, option(string));

exception Error(error);

let error = err => raise(Error(err));

type module_location_result =
  | GrainModule(string, option(string)) /* Grain Source file, Compiled object */
  | ObjectFile(string); /* Compiled object */

let file_exists = Fs_access.file_exists;

let cmi_cache = Hashtbl.create(16);
let clear_cmi_cache = () => Hashtbl.clear(cmi_cache);
let read_file_cmi = f => {
  switch (Hashtbl.find_opt(cmi_cache, f)) {
  | Some(cmi) => cmi
  | None =>
    let cmi = Cmi_format.read_cmi(f);
    Hashtbl.add(cmi_cache, f, cmi);
    cmi;
  };
};

/**
  Locates the object on disk.
*/
let find_object = (base_dir, path, unit) => {
  let dirs =
    if (Filepath.String.is_relpath(unit)) {
      [base_dir];
    } else {
      path;
    };

  List.find_map(
    dir => {
      let name = Filepath.String.replace_extension(unit, ".gro");
      let fullname = Filepath.String.concat(dir, name);
      let fullname = Filepath.(to_string(String.derelativize(fullname)));
      if (file_exists(fullname)) {
        Some(fullname);
      } else {
        None;
      };
    },
    dirs,
  );
};

let located_module_cache: Hashtbl.t((string, string), module_location_result) =
  Hashtbl.create(16);
let resolutions: Hashtbl.t((string, string), string) = Hashtbl.create(16);

let log_resolution = (unit_name, dir, resolution) => {
  Hashtbl.add(resolutions, (dir, unit_name), resolution);
};

let resolve_unit = (~base_dir, unit_name) => {
  let path = Config.module_search_path();
  switch (Hashtbl.find_opt(resolutions, (base_dir, unit_name))) {
  | Some(res) => Some(res)
  | _ =>
    let obj = find_object(base_dir, path, unit_name);
    switch (obj) {
    | Some(obj) =>
      log_resolution(unit_name, base_dir, obj);
      Some(obj);
    | None => None
    };
  };
};

let try_resolve_unit = (~loc=?, base_dir, unit) => {
  let resolve = resolve_unit(~base_dir);
  Filepath.String.(
    switch (resolve(unit)) {
    | Some(resolved) => resolved
    | None =>
      if (check_suffix(unit, ".gr")) {
        let no_extension = chop_suffix(unit, ".gr");
        switch (resolve(no_extension)) {
        | exception Not_found => error(No_object_file(loc, unit, None))
        | _ =>
          let name = !is_relpath(unit) ? no_extension : unit;
          // The filepath might have come in as `.gr.gr` so we need to chop again
          let module_name = chop_suffix(no_extension, ".gr");
          error(
            No_object_file(
              loc,
              name,
              Some("did you mean \"" ++ module_name ++ "\"?"),
            ),
          );
        };
      } else {
        switch (resolve(unit ++ ".gr")) {
        | exception Not_found => error(No_object_file(loc, unit, None))
        | _ =>
          error(
            No_object_file(
              loc,
              unit,
              Some("did you mean \"" ++ unit ++ ".gr\"?"),
            ),
          )
        };
      }
    }
  );
};

let locate_object_file = (~loc=?, ~base_dir=Config.project_root^, unit_name) => {
  try_resolve_unit(~loc?, base_dir, unit_name);
};

let () = {
  Fs_access.register_cache_flusher(() => Hashtbl.clear(cmi_cache));
  Fs_access.register_cache_flusher(() => Hashtbl.clear(located_module_cache));
  Fs_access.register_cache_flusher(() => Hashtbl.clear(resolutions));
};

/* Error report */

open Format;

let report_error = ppf =>
  fun
  | No_object_file(_, m, None) =>
    fprintf(ppf, "Missing file for module \"%s\"", m)
  | No_object_file(_, m, Some(msg)) =>
    fprintf(ppf, "Missing file for module \"%s\": %s", m, msg);

let () =
  Location.register_error_of_exn(
    fun
    | Error(No_object_file(Some(loc), _, _) as err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );

let () = Printexc.record_backtrace(true);
