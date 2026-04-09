open Grain_parsing;
open Grain_utils;
open Cmi_format;

type error =
  | No_module_file(Location.t, string, option(string))
  | Source_outside_roots(Fp.t(Fp.absolute));

exception Error(error);

let error = err => raise(Error(err));

type origin =
  | Project
  | Stdlib
  | Library(string);

type source_root = {
  origin,
  root: Fp.t(Fp.absolute),
};

let object_search_path = () => Config.include_dirs^;

let source_roots = () => {
  let libs =
    List.map(
      ((name, path)) =>
        {
          origin: Library(name),
          root: path,
        },
      Config.libraries^,
    );
  let stdlib =
    switch (Config.stdlib_dir^) {
    | Some(p) => [
        {
          origin: Stdlib,
          root: p,
        },
      ]
    | None => []
    };
  let project = [
    {
      origin: Project,
      root: Config.project_root^,
    },
  ];
  stdlib @ libs @ project;
};

type module_location_result =
  | GrainModule({
      source: Fp.t(Fp.absolute),
      compiled_object: option(Fp.t(Fp.absolute)),
      origin,
      root: Fp.t(Fp.absolute),
    })
  | ObjectFile({compiled_object: Fp.t(Fp.absolute)});

let current_filename: ref(unit => string) =
  ref(() => failwith("current_filename should be filled in by env.re"));

let last_modified = Fs_access.last_modified;
let file_exists = Fs_access.file_exists;

let file_older = (a, b) => {
  last_modified(a) < last_modified(b);
};

let cmi_cache = Hashtbl.create(16);
let read_file_cmi = f => {
  switch (Hashtbl.find_opt(cmi_cache, f)) {
  | Some(cmi) => cmi
  | None =>
    let cmi = Cmi_format.read_cmi(f);
    Hashtbl.add(cmi_cache, f, cmi);
    cmi;
  };
};

let profile_dir_name = () =>
  switch (Config.profile^) {
  | Config.Debug => "debug"
  | Config.Release => "release"
  };

let source_to_artifact_path =
    (~ext, ~origin, ~root, srcpath: Fp.t(Fp.absolute)): Fp.t(Fp.absolute) => {
  let path =
    Filepath.replace_extension(
      Fp.relativizeExn(~source=root, ~dest=srcpath),
      ext,
    );
  let base = Fp.At.(Config.target_dir^ / profile_dir_name());
  let dir =
    switch (origin) {
    | Project => Fp.At.(base / "build")
    | Stdlib => Fp.At.(base / "deps" / "stdlib")
    | Library(name) => Fp.At.(base / "deps" / name)
    };
  Fp.join(dir, path);
};

let classify_source = (srcpath: Fp.t(Fp.absolute)) => {
  let roots = source_roots();
  let rec find =
    fun
    | [] => None
    | [{origin, root}, ..._] when Filepath.is_under(~root, srcpath) =>
      Some((origin, root))
    | [_, ...rest] => find(rest);
  find(roots);
};

let object_file_for_source = (~origin, ~root, srcpath) => {
  let target_obj =
    source_to_artifact_path(~ext="gro", ~origin, ~root, srcpath);
  let cached =
    file_exists(Fp.toString(target_obj)) ? Some(target_obj) : None;
  GrainModule({
    source: srcpath,
    compiled_object: cached,
    origin,
    root,
  });
};

let find_object_in_path = (path, unit_name) => {
  switch (Fp.relative(unit_name)) {
  | None => None
  | Some(rel) =>
    let rel = Filepath.replace_extension(rel, "gro");
    let rec try_dir = (
      fun
      | [] => None
      | [dir, ...rest] => {
          let candidate = Fp.join(dir, rel);
          if (Filepath.is_under(~root=dir, candidate)
              && file_exists(Fp.toString(candidate))) {
            Some(candidate);
          } else {
            try_dir(rest);
          };
        }
    );
    try_dir(path);
  };
};

let find_source_under_root = (root, unit_name) => {
  switch (Fp.relative(unit_name)) {
  | None => None
  | Some(rel) =>
    let rel = Filepath.replace_extension(rel, "gr");
    let candidate = Fp.join(root, rel);
    if (Filepath.is_under(~root, candidate)
        && file_exists(Fp.toString(candidate))) {
      Some(candidate);
    } else {
      None;
    };
  };
};

module PathTbl = {
  type t('a) = Hashtbl.t(string, 'a);
  let create: int => t('a) = Hashtbl.create;

  let key = (dir: Fp.t(Fp.absolute), unit_name) =>
    Format.sprintf("%s::%s", Fp.toString(dir), unit_name);

  let add = (tbl, dir, unit_name, v) =>
    Hashtbl.add(tbl, key(dir, unit_name), v);

  let find_opt = (tbl, dir, unit_name) =>
    Hashtbl.find_opt(tbl, key(dir, unit_name));
};

let located_module_cache:
  Hashtbl.t(string, PathTbl.t(module_location_result)) =
  Hashtbl.create(16);
let current_located_module_cache = () => {
  switch (Hashtbl.find_opt(located_module_cache, current_filename^())) {
  | Some(v) => v
  | None =>
    let new_table = PathTbl.create(12);
    Hashtbl.add(located_module_cache, current_filename^(), new_table);
    new_table;
  };
};
let to_absolute = path =>
  switch (Fp.testForPath(path)) {
  | Some(Absolute(p)) => p
  | Some(Relative(rel)) => Fp.join(Filepath.get_cwd(), rel)
  | None => failwith("to_absolute: invalid path: " ++ path)
  };

let base_dir_of_current_file = () => {
  Fp.dirName(to_absolute(current_filename^()));
};

let from_source = srcpath =>
  if (file_exists(Fp.toString(srcpath))) {
    switch (classify_source(srcpath)) {
    | Some((origin, root)) => object_file_for_source(~origin, ~root, srcpath)
    | None => error(Source_outside_roots(srcpath))
    };
  } else {
    raise(Not_found);
  };

let resolve_library = unit_name =>
  switch (Filepath.String.first_segment(unit_name)) {
  | Some((first, rest)) when List.mem_assoc(first, Config.libraries^) =>
    let lib_root = List.assoc(first, Config.libraries^);
    Option.map(
      srcpath =>
        object_file_for_source(
          ~origin=Library(first),
          ~root=lib_root,
          srcpath,
        ),
      find_source_under_root(lib_root, rest),
    );
  | _ => None
  };

let resolve_object = unit_name =>
  Option.map(
    objpath => ObjectFile({compiled_object: objpath}),
    find_object_in_path(object_search_path(), unit_name),
  );

let resolve_stdlib = unit_name =>
  switch (Config.stdlib_dir^) {
  | Some(stdlib_root) =>
    Option.map(
      srcpath =>
        object_file_for_source(~origin=Stdlib, ~root=stdlib_root, srcpath),
      find_source_under_root(stdlib_root, unit_name),
    )
  | None => None
  };

let resolve_module_uncached = (base_dir, unit_name) =>
  if (!Filename.is_relative(unit_name)) {
    // Absolute path
    switch (Fp.absoluteCurrentPlatform(unit_name)) {
    | Some(p) => from_source(p)
    | None => raise(Not_found)
    };
  } else if (!Filename.is_implicit(unit_name)) {
    // Relative local path, i.e. "./foo.gr" or "../foo.gr"
    switch (Fp.relative(unit_name)) {
    | None => raise(Not_found)
    | Some(rel) =>
      let candidate = Fp.join(base_dir, rel);
      if (file_exists(Fp.toString(candidate))) {
        from_source(candidate);
      } else {
        raise(Not_found);
      };
    };
  } else {
    // Library path, i.e. "foo/bar"
    switch (resolve_stdlib(unit_name)) {
    | Some(result) => result
    | None =>
      switch (resolve_library(unit_name)) {
      | Some(result) => result
      | None => raise(Not_found)
      }
    };
  };

let locate_module =
    (base_dir: Fp.t(Fp.absolute), unit_name): module_location_result => {
  switch (
    PathTbl.find_opt(current_located_module_cache(), base_dir, unit_name)
  ) {
  | Some(m) => m
  | None =>
    let result = resolve_module_uncached(base_dir, unit_name);
    PathTbl.add(current_located_module_cache(), base_dir, unit_name, result);
    result;
  };
};

let is_explicit_relpath = name =>
  Filename.is_relative(name) && !Filename.is_implicit(name);

let locate_object = (base_dir, unit_name) =>
  try(locate_module(base_dir, unit_name)) {
  | Not_found =>
    let from_relpath =
      if (is_explicit_relpath(unit_name)
          && Filename.check_suffix(unit_name, ".gr")) {
        switch (Fp.relative(unit_name)) {
        | Some(rel) =>
          let obj_path =
            Filepath.replace_extension(Fp.join(base_dir, rel), "gro");
          if (file_exists(Fp.toString(obj_path))) {
            Some(ObjectFile({compiled_object: obj_path}));
          } else {
            None;
          };
        | None => None
        };
      } else {
        None;
      };
    switch (from_relpath) {
    | Some(result) => result
    | None =>
      switch (resolve_object(unit_name)) {
      | Some(result) => result
      | None => raise(Not_found)
      }
    };
  };

let try_locate_module = (base_dir: Fp.t(Fp.absolute), name: string, loc) =>
  try(locate_object(base_dir, name)) {
  | Not_found =>
    if (Filename.check_suffix(name, ".gr")) {
      let no_extension = Filename.chop_suffix(name, ".gr");
      switch (locate_module(base_dir, no_extension)) {
      | exception Not_found => error(No_module_file(loc, name, None))
      | _ =>
        let reported_name = is_explicit_relpath(name) ? name : no_extension;
        let module_name =
          if (Filename.check_suffix(no_extension, ".gr")) {
            Filename.chop_suffix(no_extension, ".gr");
          } else {
            no_extension;
          };
        error(
          No_module_file(
            loc,
            reported_name,
            Some("did you mean \"" ++ module_name ++ "\"?"),
          ),
        );
      };
    } else {
      switch (locate_module(base_dir, name ++ ".gr")) {
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
  };

type dependency_node = {
  dn_unit_name: Hashtbl.t(option(dependency_node), string),
  dn_file_name: string,
  dn_up_to_date: ref(bool),
  dn_latest_resolution: ref(option(module_location_result)),
};

let located_to_object_file_path = located =>
  switch (located) {
  | GrainModule({source: srcpath, compiled_object: _, origin, root}) =>
    source_to_artifact_path(~ext="gro", ~origin, ~root, srcpath)
  | ObjectFile({compiled_object: outpath}) => outpath
  };

let located_to_object_file_name = located =>
  Fp.toString(located_to_object_file_path(located));

let locate_unit_object_file = (~base_dir=?, unit_name) => {
  let base_dir =
    switch (base_dir) {
    | None => base_dir_of_current_file()
    | Some(bd) => to_absolute(bd)
    };
  located_to_object_file_name(locate_object(base_dir, unit_name));
};

let resolve_unit = (~cache as _=true, ~base_dir=?, unit_name) => {
  let base_dir =
    switch (base_dir) {
    | None => base_dir_of_current_file()
    | Some(bd) => to_absolute(bd)
    };
  Fp.toString(
    located_to_object_file_path(locate_object(base_dir, unit_name)),
  );
};

let source_artifact_filename = (~ext, name) => {
  let abs = to_absolute(name);
  switch (classify_source(abs)) {
  | Some((origin, root)) =>
    Fp.toString(source_to_artifact_path(~ext, ~origin, ~root, abs))
  | None => error(Source_outside_roots(abs))
  };
};

let source_output_filename = (~ext, name) => {
  let abs = to_absolute(name);
  switch (classify_source(abs)) {
  | Some(_) =>
    let basename =
      Filepath.String.replace_extension(
        Option.get(Fp.baseName(abs)),
        ext,
      );
    let base = Fp.At.(Config.target_dir^ / profile_dir_name());
    Fp.toString(Fp.append(base, basename));
  | None => error(Source_outside_roots(abs))
  };
};

let get_object_name = name => source_artifact_filename(~ext="gro", name);

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
      | Some(GrainModule({source: srcpath, _})) => Fp.toString(srcpath)
      };
    };
    let get_filename = dn => dn.dn_file_name;

    let rec get_dependencies: (t, string => option(t)) => list(t) =
      (dn, lookup) => {
        let located = dn.dn_latest_resolution^;

        let make_dep_node = (~locate, unit_name, loc) => {
          let located = locate(unit_name, loc);
          let out_file_name = located_to_object_file_name(located);
          let existing_dependency = lookup(out_file_name);
          switch (existing_dependency) {
          | Some(ed) =>
            Hashtbl.add(ed.dn_unit_name, Some(dn), unit_name);
            ed;
          | None =>
            let tbl = Hashtbl.create(8);
            Hashtbl.add(tbl, Some(dn), unit_name);
            {
              dn_unit_name: tbl,
              dn_file_name: out_file_name,
              dn_up_to_date: ref(false),
              dn_latest_resolution: ref(Some(located)),
            };
          };
        };
        let from_srcpath = (~base_dir, srcpath_str) =>
          List.map(
            name =>
              make_dep_node(
                ~locate=try_locate_module(base_dir),
                name.Location.txt,
                name.Location.loc,
              ),
            Grain_parsing.Driver.scan_for_imports(srcpath_str),
          );
        let from_cmi = (~locate, objpath_str) =>
          switch (read_file_cmi(objpath_str)) {
          | exception (Cmi_format.Error(_)) => None
          | cmi =>
            Some(
              List.map(
                ((name, _)) =>
                  make_dep_node(
                    ~locate,
                    name,
                    Location.in_file(dn.dn_file_name),
                  ),
                cmi.cmi_crcs,
              ),
            )
          };

        switch (located) {
        | None => failwith("get_dependencies: Should be impossible")
        | Some(ObjectFile({compiled_object: objpath})) =>
          let base_dir = Fp.dirName(objpath);
          switch (
            from_cmi(
              ~locate=try_locate_module(base_dir),
              Fp.toString(objpath),
            )
          ) {
          | Some(deps) => deps
          | None => []
          };
        | Some(GrainModule({source: srcpath, compiled_object: None, _})) =>
          let base_dir = Fp.dirName(srcpath);
          from_srcpath(~base_dir, Fp.toString(srcpath));
        | Some(
            GrainModule({
              source: srcpath,
              compiled_object: Some(objpath),
              _,
            }),
          ) =>
          let base_dir = Fp.dirName(srcpath);
          switch (
            from_cmi(
              ~locate=try_locate_module(base_dir),
              Fp.toString(objpath),
            )
          ) {
          | Some(deps) => deps
          | None => from_srcpath(~base_dir, Fp.toString(srcpath))
          };
        };
      };

    let check_up_to_date: t => unit =
      dn => {
        switch (dn.dn_up_to_date^, dn.dn_latest_resolution^) {
        | (true, _) => ()
        | (false, None)
        | (false, Some(GrainModule({compiled_object: None, _}))) =>
          dn.dn_up_to_date := false
        | (false, Some(ObjectFile({compiled_object: _}))) =>
          dn.dn_up_to_date := true
        | (
            false,
            Some(
              GrainModule({
                source: srcpath,
                compiled_object: Some(objpath),
                _,
              }),
            ),
          ) =>
          let config_sum = Cmi_format.config_sum();
          let base_dir = Fp.dirName(srcpath);
          let srcpath = Fp.toString(srcpath);
          let objpath = Fp.toString(objpath);
          let up_to_date =
            switch (read_file_cmi(objpath)) {
            | exception (Cmi_format.Error(_)) => false
            | cmi =>
              config_sum == cmi.cmi_config_sum
              && file_older(srcpath, objpath)
              && List.for_all(
                   ((name, crc)) => {
                     switch (locate_module(base_dir, name)) {
                     | exception _ => false
                     | located =>
                       let object_name =
                         Fp.toString(located_to_object_file_path(located));
                       file_exists(object_name)
                       && (
                         try(read_file_cmi(object_name).cmi_crc == crc) {
                         | _ => false
                         }
                       );
                     }
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

let locate_object_file = (~loc, unit_name) => {
  let base_dir = base_dir_of_current_file();
  let located = try_locate_module(base_dir, unit_name, loc);
  located_to_object_file_name(located);
};

let process_dependency = (~loc, ~base_file, unit_name) => {
  let base_dir = Fp.dirName(to_absolute(base_file));
  let located = try_locate_module(base_dir, unit_name, loc);
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
        dn_up_to_date: ref(false),
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
  Fs_access.register_cache_flusher((
    Hashtbl.remove(cmi_cache),
    () => Hashtbl.clear(cmi_cache),
  ));
  Fs_access.register_cache_flusher((
    Hashtbl.remove(located_module_cache),
    () => Hashtbl.clear(located_module_cache),
  ));
};

let dump_dependency_graph = Dependency_graph.dump;

open Format;

let report_error = ppf =>
  fun
  | No_module_file(_, m, None) =>
    fprintf(ppf, "Missing file for module \"%s\"", m)
  | No_module_file(_, m, Some(msg)) =>
    fprintf(ppf, "Missing file for module \"%s\": %s", m, msg)
  | Source_outside_roots(p) => {
      fprintf(
        ppf,
        "Source file %s is not within the project or any configured library.",
        Fp.toString(p),
      );
    };

let () =
  Location.register_error_of_exn(
    fun
    | Error(No_module_file(loc, _, _) as err) when loc != Location.dummy_loc =>
      Some(Location.error_of_printer(loc, report_error, err))
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );

let () = Printexc.record_backtrace(true);
