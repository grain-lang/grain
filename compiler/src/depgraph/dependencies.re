open Grain_parsing;
open Grain_typed;
open Module_resolution;
open Grain_utils;
open Cmi_format;

type error =
  | Missing_module(Location.t, Path.t, Path.t)
  | No_module_file(Location.t, string, option(string))
  | Invalid_src_path(string);

exception Error(error);

let error = err => raise(Error(err));

let object_path = (~project_root, ~target_dir, src_path) => {
  let rel_path =
    switch (Fp.relativize(~source=project_root, ~dest=src_path)) {
    | Ok(fp) => fp
    | Error(_) =>
      raise(Error(Invalid_src_path(Fp.toDebugString(src_path))))
    };
  Fp.join(target_dir, rel_path);
};

let last_modified = f => Fs_access.last_modified(Fp.toString(f));
let file_exists = f => Fs_access.file_exists(Fp.toString(f));

let file_older = (a, b) => {
  last_modified(a) < last_modified(b);
};

let get_object_name = name => Filepath.String.replace_extension(name, "gro");

/**
  Locates src file on disk.
*/
let find_src = (~src_base_dir, name) => {
  let name = Filepath.String.replace_extension(name, ".gr");
  let fullname = Fp.append(src_base_dir, name);
  if (file_exists(fullname)) {
    fullname;
  } else {
    raise(Not_found);
  };
};

let located_src_cache:
  Hashtbl.t((Fp.t(Fp.absolute), string), Fp.t(Fp.absolute)) =
  Hashtbl.create(16);

let locate_src = (~src_base_dir, unit_name) => {
  switch (Hashtbl.find_opt(located_src_cache, (src_base_dir, unit_name))) {
  | Some(m) => m
  | None =>
    let located = find_src(~src_base_dir, unit_name);
    Hashtbl.add(located_src_cache, (src_base_dir, unit_name), located);
    located;
  };
};

let try_locate_src = (~src_base_dir, name, loc) => {
  let locate = locate_src(~src_base_dir);
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
  dn_src_path: Fp.t(Fp.absolute),
  dn_obj_path: Fp.t(Fp.absolute),
  dn_up_to_date: ref(bool) // cached up_to_date check
};

module Dependency_graph =
  Dependency_graph.Make({
    type t = dependency_node;
    let compare = (dn1, dn2) =>
      if (Fp.eq(dn1.dn_src_path, dn2.dn_src_path)) {
        0;
      } else {
        1;
      };
    let hash = dn => Hashtbl.hash(dn.dn_src_path);
    let equal = (dn1, dn2) => Fp.eq(dn1.dn_src_path, dn2.dn_src_path);

    let get_srcname = dn => {
      dn.dn_src_path;
    };
    let get_objname = dn => dn.dn_src_path;

    let rec get_dependencies:
      (
        ~project_root: Fp.t(Fp.absolute),
        ~target_dir: Fp.t(Fp.absolute),
        t,
        Fp.t(Fp.absolute) => option(t)
      ) =>
      list(t) =
      (~project_root, ~target_dir, dn, lookup) => {
        let src_base_dir = Fp.dirName(dn.dn_src_path);

        let from_srcpath = srcpath => {
          List.filter_map(
            unit =>
              if (Filepath.String.is_relpath(unit.Location.txt)) {
                let located_src =
                  try_locate_src(
                    ~src_base_dir,
                    unit.Location.txt,
                    unit.Location.loc,
                  );
                let obj =
                  object_path(~project_root, ~target_dir, located_src);
                let existing_dependency = lookup(obj);
                switch (existing_dependency) {
                | Some(ed) =>
                  Hashtbl.add(ed.dn_unit_name, Some(dn), unit.Location.txt);
                  Some(ed);
                | None =>
                  let tbl = Hashtbl.create(8);
                  Hashtbl.add(tbl, Some(dn), unit.Location.txt);
                  Some({
                    dn_unit_name: tbl,
                    dn_src_path: located_src,
                    dn_obj_path: obj,
                    dn_up_to_date: ref(false) // <- needs to be checked
                  });
                };
              } else {
                None;
              },
            Grain_parsing.Driver.scan_for_imports(Fp.toString(srcpath)),
          );
        };

        if (file_exists(dn.dn_obj_path)) {
          switch (read_file_cmi(Fp.toString(dn.dn_obj_path))) {
          | exception (Cmi_format.Error(_)) => from_srcpath(dn.dn_src_path)
          | cmi =>
            List.filter_map(
              ((unit, _)) =>
                if (Filepath.String.is_relpath(unit)) {
                  let located_src =
                    try_locate_src(
                      ~src_base_dir,
                      unit,
                      Location.in_file(Fp.toString(dn.dn_src_path)),
                    );
                  let obj =
                    object_path(~project_root, ~target_dir, located_src);
                  let existing_dependency = lookup(obj);
                  switch (existing_dependency) {
                  | Some(ed) =>
                    Hashtbl.add(ed.dn_unit_name, Some(dn), unit);
                    Some(ed);
                  | None =>
                    let tbl = Hashtbl.create(8);
                    Hashtbl.add(tbl, Some(dn), unit);
                    Some({
                      dn_unit_name: tbl,
                      dn_src_path: located_src,
                      dn_obj_path: obj,
                      dn_up_to_date: ref(false) // <- needs to be checked
                    });
                  };
                } else {
                  None;
                },
              cmi.cmi_crcs,
            )
          };
        } else {
          from_srcpath(dn.dn_src_path);
        };
      };

    let check_up_to_date: t => unit =
      dn => {
        switch (dn.dn_up_to_date^) {
        | true => ()
        | false when !file_exists(dn.dn_obj_path) =>
          // File isn't compiled, so it's not up-to-date yet.
          dn.dn_up_to_date := false
        | _ =>
          // Compiled file is up-to-date if the srcpath is older than the objpath,
          // all dependencies have expected CRC, and the module was compiled with
          // the current compiler configuration. Otherwise, we need to recompile.
          let config_sum = Cmi_format.config_sum();
          let obj_base_dir = Fp.dirName(dn.dn_obj_path);
          let up_to_date =
            switch (read_file_cmi(Fp.toString(dn.dn_obj_path))) {
            // Treat corrupted CMI as invalid
            | exception (Cmi_format.Error(_)) => false
            | cmi =>
              config_sum == cmi.cmi_config_sum
              && file_older(dn.dn_src_path, dn.dn_obj_path)
              && List.for_all(
                   ((name, crc)) => {
                     let resolved =
                       resolve_unit(~base_dir=obj_base_dir, name);
                     switch (resolved) {
                     | Some(obj) =>
                       file_exists(obj)
                       && (
                         try(read_file_cmi(Fp.toString(obj)).cmi_crc == crc) {
                         | _ => false
                         }
                       )
                     | None => false
                     };
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

let process_dependency =
    (~loc, ~project_root, ~target_dir, ~base_file, unit_name) => {
  let src_base_dir = Fp.dirName(base_file);
  let located = try_locate_src(~src_base_dir, unit_name, loc);
  let object_file = object_path(~project_root, ~target_dir, located);
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
        dn_src_path: base_file,
        dn_obj_path: object_file,
        dn_up_to_date: ref(false) // <- needs to be checked
      };
    };
  Dependency_graph.register(~project_root, ~target_dir, dn);
};

let process_dependencies =
    (~project_root, ~target_dir, ~base_file, dependencies) => {
  Location.(
    List.iter(
      ({txt: dependency, loc}) =>
        process_dependency(
          ~loc,
          ~project_root,
          ~target_dir,
          ~base_file,
          dependency,
        ),
      dependencies,
    )
  );
};

let load_dependency_graph = (~project_root, ~target_dir, base_file) => {
  let dependencies = Driver.scan_for_imports(Fp.toString(base_file));
  process_dependencies(~project_root, ~target_dir, ~base_file, dependencies);
};

let load_dependency_graph_from_string =
    (~project_root, ~target_dir, name, src) => {
  let dependencies = Driver.scan_string_for_imports(name, src);
  process_dependencies(
    ~base_file=Fp.append(project_root, name),
    dependencies,
  );
};

let clear_dependency_graph = () => {
  Dependency_graph.clear();
};

let get_dependencies = () => {
  Dependency_graph.get_dependencies();
};

let get_out_of_date_dependencies = () => {
  let out_of_date = Dependency_graph.get_out_of_date_dependencies();
  clear_cmi_cache();
  out_of_date;
};

let () = {
  Fs_access.register_cache_flusher(() => Hashtbl.clear(located_src_cache));
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
    fprintf(ppf, "Missing file for module \"%s\": %s", m, msg)
  | Invalid_src_path(path) =>
    fprintf(ppf, "Cannot create relative path from %s to project root", path);

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
