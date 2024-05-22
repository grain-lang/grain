open Grain_parsing;
open Grain_typed;
open Module_resolution;
open Grain_utils;
open Cmi_format;

type error =
  | Missing_module(Location.t, Path.t, Path.t)
  | No_module_file(Location.t, string, option(string));

exception Error(error);

let error = err => raise(Error(err));

type module_location_result =
  | GrainModule(string, option(string)) /* Grain Source file, Compiled object */
  | ObjectFile(string); /* Compiled object */

let last_modified = Fs_access.last_modified;
let file_exists = Fs_access.file_exists;

let file_older = (a, b) => {
  last_modified(a) < last_modified(b);
};

let get_object_name = name => Filepath.String.replace_extension(name, "gro");

let find_ext_in_dir = (dir, name) => {
  let fullname = Filepath.String.concat(dir, name);
  let rec process_ext =
    fun
    | [] => None
    | [ext, ...tl] => {
        let with_ext = Filepath.String.replace_extension(fullname, ext);
        if (file_exists(with_ext)) {
          Some((with_ext, dir, name));
        } else {
          process_ext(tl);
        };
      };
  process_ext;
};

/**
  Locates src file on disk.

  Explicit relative file paths (also referred to as local paths) resolve
  relative to the base_dir. These are paths like "./foo.gr".

  Implicit relative file paths resolve within their associated library prefix.
  These are paths like "somelib/foo".
*/
let find_src = (base_dir, libs, name) => {
  let (base_dir, is_lib) =
    if (Filepath.String.is_relpath(name)) {
      (base_dir, false);
    } else {
      // TODO: Implement this efficiently
      let lib =
        List.find_opt(
          ((prefix, _)) => String.starts_with(~prefix, name),
          libs,
        );
      let dir =
        switch (lib) {
        | Some((prefix, lib_path)) =>
          String.sub(
            lib_path,
            0,
            String.length(lib_path) - String.length(prefix) - 1,
          )
        | None =>
          // Falls back to the stdlib
          List.assoc("stdlib", libs)
        };
      (dir, true);
    };

  let name = Filepath.String.replace_extension(name, ".gr");
  let fullname = Filepath.String.concat(base_dir, name);
  if (file_exists(fullname)) {
    (fullname, base_dir, name, is_lib);
  } else {
    raise(Not_found);
  };
};

/**
  Provides location where the object should be placed. Does not check if the
  object actually exists.

  Local paths resolve within the <target>/build directory. Library paths
  resolve within <target>/lib.
*/
let object_path = (base_dir, libs, name) => {
  let (base_dir, is_lib) =
    if (Filepath.String.is_relpath(name)) {
      (base_dir, false);
    } else {
      // TODO: Implement this efficiently
      let lib =
        List.find_opt(
          ((prefix, _)) => String.starts_with(~prefix, name),
          libs,
        );
      let dir =
        switch (lib) {
        | Some((prefix, lib_path)) =>
          String.sub(
            lib_path,
            0,
            String.length(lib_path) - String.length(prefix) - 1,
          )
        | None =>
          // Falls back to the stdlib libs
          List.assoc("stdlib", libs)
        };
      (dir, true);
    };

  let name = Filepath.String.replace_extension(name, ".gr");
  let fullname = Filepath.String.concat(base_dir, name);
  if (file_exists(fullname)) {
    (fullname, base_dir, name, is_lib);
  } else {
    raise(Not_found);
  };
};

let located_module_cache: Hashtbl.t((string, string), module_location_result) =
  Hashtbl.create(16);
let resolutions: Hashtbl.t((string, string), string) = Hashtbl.create(16);

let log_resolution = (unit_name, dir, basename) => {
  let resolution =
    Filepath.(
      to_string @@ String.derelativize @@ String.concat(dir, basename)
    );
  Hashtbl.add(resolutions, (dir, unit_name), resolution);
  resolution;
};

let resolve_unit = (~base_dir, unit_name) => {
  switch (locate_object_file(~base_dir, unit_name)) {
  | obj => Some(obj)
  | exception _ => None
  };
};

let locate_module = (base_dir, path, unit_name) => {
  switch (Hashtbl.find_opt(located_module_cache, (base_dir, unit_name))) {
  | Some(m) => m
  | None =>
    let (dir, m) =
      switch (find_src(base_dir, path, unit_name)) {
      | (srcpath, dir, basename, is_lib) =>
        // THIS IS WHERE I'M AT
        ignore(log_resolution(unit_name, dir, basename));
        let file = find_ext_in_dir(dir, basename, ["gr"]);
        switch (file) {
        | Some((srcpath, _, _)) => (
            dir,
            GrainModule(srcpath, Some(objpath)),
          )
        | None => (dir, ObjectFile(objpath))
        };
      | exception Not_found =>
        let (srcpath, dir, _, _) =
          find_src(~check_src=true, base_dir, path, unit_name);
        (dir, GrainModule(srcpath, None));
      };
    Hashtbl.add(located_module_cache, (dir, unit_name), m);
    m;
  };
};

let try_locate_module = (base_dir, active_search_path, name, loc) => {
  let locate = locate_module(base_dir, active_search_path);
  Filepath.String.(
    try(locate(name)) {
    | Not_found =>
      if (check_suffix(name, ".gr")) {
        let no_extension = chop_suffix(name, ".gr");
        switch (locate(no_extension)) {
        | exception Not_found => error(No_module_file(loc, name, None))
        | _ =>
          let name = !is_relpath(name) ? no_extension : name;
          // The filepath might have come in as `.gr.gr` so we need to chop again
          let module_name = chop_suffix(no_extension, ".gr");
          error(
            No_module_file(
              loc,
              name,
              Some("did you mean \"" ++ module_name ++ "\"?"),
            ),
          );
        };
      } else {
        switch (locate(name ++ ".gr")) {
        | exception Not_found => error(No_module_file(loc, name, None))
        | _ =>
          error(
            No_module_file(
              loc,
              name,
              Some("did you mean \"" ++ name ++ ".gr\"?"),
            ),
          )
        };
      }
    }
  );
};

type dependency_node = {
  // dn_unit_name is a hashtable because we may have a situation
  // where A depends on B and C, and both B and C depend on D.
  // D will then have two unit names, corresponding to the "view" from B and C.
  dn_unit_name: Hashtbl.t(option(dependency_node), string), // <- node_where_imported: name_of_unit_where_imported
  dn_file_name: string,
  dn_up_to_date: ref(bool), // cached up_to_date check
  dn_latest_resolution: ref(option(module_location_result)),
};

let located_to_object_file_name = (~base=?, located) => {
  let ret =
    switch (located) {
    | GrainModule(srcpath, None) => get_object_name(srcpath)
    | GrainModule(_, Some(outpath))
    | ObjectFile(outpath) => outpath
    };
  Filepath.to_string(Filepath.String.derelativize(~base?, ret));
};

module Dependency_graph =
  Dependency_graph.Make({
    type t = dependency_node;
    let compare = (dn1, dn2) =>
      String.compare(dn1.dn_file_name, dn2.dn_file_name);
    let hash = dn => Hashtbl.hash(dn.dn_file_name);
    let equal = (dn1, dn2) =>
      String.equal(dn1.dn_file_name, dn2.dn_file_name);

    let get_srcname = dn => {
      switch (dn.dn_latest_resolution^) {
      | None => failwith("impossible: get_srcname > No resolution")
      | Some(ObjectFile(_)) =>
        failwith("impossible: get_srcname > No source")
      | Some(GrainModule(srcpath, _)) =>
        Filepath.to_string(Filepath.String.derelativize(srcpath))
      };
    };
    let get_filename = dn => dn.dn_file_name;

    let rec get_dependencies: (t, string => option(t)) => list(t) =
      (dn, lookup) => {
        let base_dir = Filepath.String.dirname(dn.dn_file_name);
        let active_search_path = Config.libraries();
        let located = dn.dn_latest_resolution^;

        let from_srcpath = srcpath => {
          List.map(
            name => {
              let located =
                try_locate_module(
                  base_dir,
                  active_search_path,
                  name.Location.txt,
                  name.Location.loc,
                );
              let out_file_name = located_to_object_file_name(located);
              let existing_dependency = lookup(out_file_name);
              switch (existing_dependency) {
              | Some(ed) =>
                Hashtbl.add(ed.dn_unit_name, Some(dn), name.Location.txt);
                ed;
              | None =>
                let tbl = Hashtbl.create(8);
                Hashtbl.add(tbl, Some(dn), name.Location.txt);
                {
                  dn_unit_name: tbl,
                  dn_file_name: out_file_name,
                  dn_up_to_date: ref(false), // <- needs to be checked
                  dn_latest_resolution: ref(Some(located)),
                };
              };
            },
            Grain_parsing.Driver.scan_for_imports(srcpath),
          );
        };

        // For the moment, from the dependency graph's perspective, we assume that
        // nothing uses --no-pervasives or --no-gc.
        switch (located) {
        | None => failwith("get_dependencies: Should be impossible")
        | Some(ObjectFile(_)) => []
        | Some(GrainModule(srcpath, None)) => from_srcpath(srcpath)
        | Some(GrainModule(srcpath, Some(objpath))) =>
          switch (read_file_cmi(objpath)) {
          | exception (Cmi_format.Error(_)) => from_srcpath(srcpath)
          | cmi =>
            List.map(
              ((name, _)) => {
                let located =
                  try_locate_module(
                    base_dir,
                    active_search_path,
                    name,
                    Location.in_file(dn.dn_file_name),
                  );
                let out_file_name = located_to_object_file_name(located);
                let existing_dependency = lookup(out_file_name);
                switch (existing_dependency) {
                | Some(ed) =>
                  Hashtbl.add(ed.dn_unit_name, Some(dn), name);
                  ed;
                | None =>
                  let tbl = Hashtbl.create(8);
                  Hashtbl.add(tbl, Some(dn), name);
                  {
                    dn_unit_name: tbl,
                    dn_file_name: out_file_name,
                    dn_up_to_date: ref(false), // <- needs to be checked
                    dn_latest_resolution: ref(Some(located)),
                  };
                };
              },
              cmi.cmi_crcs,
            )
          }
        };
      };

    let check_up_to_date: t => unit =
      dn => {
        switch (dn.dn_up_to_date^, dn.dn_latest_resolution^) {
        | (true, _) => ()
        | (false, None)
        | (false, Some(GrainModule(_, None))) =>
          // File isn't compiled, so it's not up-to-date yet.
          dn.dn_up_to_date := false
        | (false, Some(ObjectFile(_))) =>
          // WASM modules are always up-to-date
          dn.dn_up_to_date := true
        | (false, Some(GrainModule(srcpath, Some(objpath)))) =>
          // Compiled file is up-to-date if the srcpath is older than the objpath,
          // all dependencies have expected CRC, and the module was compiled with
          // the current compiler configuration. Otherwise, we need to recompile.
          let config_sum = Cmi_format.config_sum();
          let base_dir = Filepath.String.dirname(srcpath);
          let up_to_date =
            switch (read_file_cmi(objpath)) {
            // Treat corrupted CMI as invalid
            | exception (Cmi_format.Error(_)) => false
            | cmi =>
              config_sum == cmi.cmi_config_sum
              && file_older(srcpath, objpath)
              && List.for_all(
                   ((name, crc)) => {
                     let resolved = resolve_unit(~base_dir, name);
                     let object_name = get_object_name(resolved);
                     Fs_access.file_exists(object_name)
                     && (
                       try(read_file_cmi(object_name).cmi_crc == crc) {
                       | _ => false
                       }
                     );
                   },
                   cmi.cmi_crcs,
                 )
            };
          dn.dn_up_to_date := up_to_date;
        };
      };

    let is_up_to_date = dn => {
      dn.dn_up_to_date^;
    };
  });

let process_dependency = (~loc, ~base_file, unit_name) => {
  let base_dir = Filepath.String.dirname(base_file);
  let path = Config.libraries();
  let located = try_locate_module(base_dir, path, unit_name, loc);
  let object_file = located_to_object_file_name(located);
  let current_dep_node = Dependency_graph.lookup_filename(base_file);
  let existing_dependency = Dependency_graph.lookup_filename(object_file);
  let dn =
    switch (existing_dependency) {
    | Some(ed) =>
      Hashtbl.add(ed.dn_unit_name, current_dep_node, unit_name);
      ed;
    | None =>
      let tbl = Hashtbl.create(8);
      Hashtbl.add(tbl, current_dep_node, unit_name);
      {
        dn_unit_name: tbl,
        dn_file_name: object_file,
        dn_up_to_date: ref(false), // <- needs to be checked
        dn_latest_resolution: ref(Some(located)),
      };
    };
  Dependency_graph.register(dn);
};

let process_dependencies = (~base_file, dependencies) => {
  Location.(
    List.iter(
      ({txt: dependency, loc}) =>
        process_dependency(~loc, ~base_file, dependency),
      dependencies,
    )
  );
};

let load_dependency_graph = base_file => {
  let dependencies = Driver.scan_for_imports(base_file);
  process_dependencies(~base_file, dependencies);
};

let load_dependency_graph_from_string = (name, src) => {
  let dependencies = Driver.scan_string_for_imports(name, src);
  process_dependencies(~base_file=name, dependencies);
};

let clear_dependency_graph = () => {
  Dependency_graph.clear();
};

let get_dependencies = () => {
  Dependency_graph.get_dependencies();
};

let get_out_of_date_dependencies = () => {
  let out_of_date = Dependency_graph.get_out_of_date_dependencies();
  Hashtbl.clear(cmi_cache);
  out_of_date;
};

let () = {
  Fs_access.register_cache_flusher(() => Hashtbl.clear(cmi_cache));
  Fs_access.register_cache_flusher(() => Hashtbl.clear(located_module_cache));
  Fs_access.register_cache_flusher(() => Hashtbl.clear(resolutions));
};

let dump_dependency_graph = Dependency_graph.dump;

/* Error report */

open Format;

let report_error = ppf =>
  fun
  | Missing_module(_, path1, path2) => {
      fprintf(ppf, "@[@[<hov>");
      if (Path.same(path1, path2)) {
        fprintf(ppf, "Internal path@ %s@ is dangling.", Path.name(path1));
      } else {
        fprintf(
          ppf,
          "Internal path@ %s@ expands to@ %s@ which is dangling.",
          Path.name(path1),
          Path.name(path2),
        );
      };
      fprintf(
        ppf,
        "@]@ @[%s@ %s@ %s.@]@]",
        "The compiled interface for module",
        Ident.name(Path.head(path2)),
        "was not found",
      );
    }
  | No_module_file(_, m, None) =>
    fprintf(ppf, "Missing file for module \"%s\"", m)
  | No_module_file(_, m, Some(msg)) =>
    fprintf(ppf, "Missing file for module \"%s\": %s", m, msg);

let () =
  Location.register_error_of_exn(
    fun
    | Error(Missing_module(loc, _, _) as err)
    | Error(No_module_file(loc, _, _) as err) when loc != Location.dummy_loc =>
      Some(Location.error_of_printer(loc, report_error, err))
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );

let () = Printexc.record_backtrace(true);
