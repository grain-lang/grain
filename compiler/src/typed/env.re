/* This is a stripped-down version of OCaml's typing/env.ml module. The original copyright notice is reproduced below: */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Grain_parsing;
open Grain_utils;
open Sexplib.Conv;
open Cmi_format;
open Path;
open Types;

let add_delayed_check_forward = ref(_ => assert(false));

type type_descriptions = (
  list(constructor_description),
  list(label_description),
);
module PathMap = {
  include Map.Make(Path);

  let sexp_of_t = (conv, m) => {
    let sexp_of_path = Path.sexp_of_t;
    open Sexplib;
    let pairs =
      List.map(
        ((key, v)) => Sexp.List([sexp_of_path(key), conv(v)]),
        bindings(m),
      );
    Sexp.List(pairs);
  };

  let t_of_sexp = (conv, sexp) => {
    let path_of_sexp = Path.t_of_sexp;
    Sexplib.Conv.(
      Sexplib.Sexp.(
        switch (sexp) {
        | Atom(str) => of_sexp_error("t_of_sexp: list needed", sexp)
        | List(sexprs) =>
          let fields =
            List.map(
              fun
              | List([key, value]) => (path_of_sexp(key), conv(value))
              | _ => of_sexp_error("t_of_sexp: invalid field", sexp),
              sexprs,
            );
          List.fold_left((acc, (k, v)) => add(k, v, acc), empty, fields);
        }
      )
    );
  };
};

let prefixed_sg = Hashtbl.create(113);

type dependency_chain = list(Location.loc(string));

type error =
  | Illegal_renaming(string, string, string)
  | Inconsistent_import(string, string, string)
  | Need_recursive_types(string, string)
  | Depend_on_unsafe_string_unit(string, string)
  | Missing_module(Location.t, Path.t, Path.t)
  | Unbound_module(Location.t, string)
  | Unbound_label(Location.t, string)
  | No_module_file(string, option(string))
  | Value_not_found_in_module(Location.t, string, string)
  | Illegal_value_name(Location.t, string)
  | Cyclic_dependencies(string, dependency_chain);

exception Error(error);

let error = err => raise(Error(err));

module EnvLazy: {
  type t('a, 'b);

  type log;

  let force: ('a => 'b, t('a, 'b)) => 'b;
  let create: 'a => t('a, 'b);
  let get_arg: t('a, 'b) => option('a);

  /* [force_logged log f t] is equivalent to [force f t] but if [f] returns [None] then
     [t] is recorded in [log]. [backtrack log] will then reset all the recorded [t]s back
     to their original state. */
  let log: unit => log;
  let force_logged:
    (log, 'a => option('b), t('a, option('b))) => option('b);
  let backtrack: log => unit;
} = {
  type t('a, 'b) = ref(eval('a, 'b))

  and eval('a, 'b) =
    | Done('b)
    | Raise(exn)
    | Thunk('a);

  type undo =
    | Nil
    | Cons(t('a, 'b), 'a, undo): undo;

  type log = ref(undo);

  let force = (f, x) =>
    switch (x^) {
    | Done(x) => x
    | Raise(e) => raise(e)
    | Thunk(e) =>
      switch (f(e)) {
      | y =>
        x := Done(y);
        y;
      | exception e =>
        x := Raise(e);
        raise(e);
      }
    };

  let get_arg = x =>
    switch (x^) {
    | Thunk(a) => Some(a)
    | _ => None
    };

  let create = x => ref(Thunk(x));

  let log = () => ref(Nil);

  let force_logged = (log, f, x) =>
    switch (x^) {
    | Done(x) => x
    | Raise(e) => raise(e)
    | Thunk(e) =>
      switch (f(e)) {
      | None =>
        x := Done(None);
        log := [@implicit_arity] Cons(x, e, log^);
        None;
      | Some(_) as y =>
        x := Done(y);
        y;
      | exception e =>
        x := Raise(e);
        raise(e);
      }
    };

  let backtrack = log => {
    let rec loop =
      fun
      | Nil => ()
      | [@implicit_arity] Cons(x, e, rest) => {
          x := Thunk(e);
          loop(rest);
        };

    loop(log^);
  };
};

[@deriving sexp]
type summary =
  | Env_empty
  | Env_value(summary, Ident.t, value_description)
  | Env_type(summary, Ident.t, type_declaration)
  | Env_module(summary, Ident.t, module_declaration)
  | Env_modtype(summary, Ident.t, modtype_declaration)
  | Env_open(summary, Path.t)
  | Env_constraints(summary, PathMap.t(type_declaration))
  | Env_copy_types(summary, list(string));

module TycompTbl = {
  /** This module is used to store components of types (i.e. labels
      and constructors).  We keep a representation of each nested
      "open" and the set of local bindings between each of them. */;

  type t('a) = {
    /** Local bindings since the last open. */
    current: Ident.tbl('a),
    /** Symbolic representation of the last (innermost) open, if any. */
    opened: option(opened('a)),
  }

  and opened('a) = {
    /** Components from the opened module. We keep a list of
        bindings for each name, as in comp_labels and
        comp_constrs. */
    components: Tbl.t(string, list('a)),
    /** A callback to be applied when a component is used from this
        "open".  This is used to detect unused "opens".  The
        arguments are used to detect shadowing. */
    using: option((string, option(('a, 'a))) => unit),
    /** The table before opening the module. */
    next: t('a),
  };

  let empty = {current: Ident.empty, opened: None};

  let add = (id, x, tbl) => {
    ...tbl,
    current: Ident.add(id, x, tbl.current),
  };

  let add_open = (slot, wrap, components, next) => {
    let using =
      switch (slot) {
      | None => None
      | Some(f) => Some((s, x) => f(s, wrap(x)))
      };

    {current: Ident.empty, opened: Some({using, components, next})};
  };

  let rec find_same = (id, tbl) =>
    try(Ident.find_same(id, tbl.current)) {
    | Not_found as exn =>
      switch (tbl.opened) {
      | Some({next, _}) => find_same(id, next)
      | None => raise(exn)
      }
    };

  let nothing = () => ();

  let mk_callback = (rest, name, desc) =>
    fun
    | None => nothing
    | Some(f) => (
        () =>
          switch (rest) {
          | [] => f(name, None)
          | [(hidden, _), ..._] => f(name, Some((desc, hidden)))
          }
      );

  let rec find_all = (name, tbl) =>
    List.map(
      ((_id, desc)) => (desc, nothing),
      Ident.find_all(name, tbl.current),
    )
    @ (
      switch (tbl.opened) {
      | None => []
      | Some({using, next, components}) =>
        let rest = find_all(name, next);
        switch (Tbl.find(name, components)) {
        | exception Not_found => rest
        | opened =>
          List.map(
            desc => (desc, mk_callback(rest, name, desc, using)),
            opened,
          )
          @ rest
        };
      }
    );

  let rec fold_name = (f, tbl, acc) => {
    let acc = Ident.fold_name((_id, d) => f(d), tbl.current, acc);
    switch (tbl.opened) {
    | Some({using: _, next, components}) =>
      acc
      |> Tbl.fold(_name => List.fold_right(desc => f(desc)), components)
      |> fold_name(f, next)
    | None => acc
    };
  };

  let rec local_keys = (tbl, acc) => {
    let acc =
      Ident.fold_all((k, _, accu) => [k, ...accu], tbl.current, acc);
    switch (tbl.opened) {
    | Some(o) => local_keys(o.next, acc)
    | None => acc
    };
  };

  let diff_keys = (is_local, tbl1, tbl2) => {
    let keys2 = local_keys(tbl2, []);
    List.filter(
      id =>
        is_local(find_same(id, tbl2))
        && (
          try(
            {
              ignore(find_same(id, tbl1));
              false;
            }
          ) {
          | Not_found => true
          }
        ),
      keys2,
    );
  };
};

module IdTbl = {
  /** This module is used to store all kinds of components except
      (labels and constructors) in environments.  We keep a
      representation of each nested "open" and the set of local
      bindings between each of them. */;

  type t('a) = {
    /** Local bindings since the last open */
    current: Ident.tbl('a),
    /** Symbolic representation of the last (innermost) open, if any. */
    opened: option(opened('a)),
  }

  and opened('a) = {
    /** The path of the opened module, to be prefixed in front of
        its local names to produce a valid path in the current
        environment. */
    root: Path.t,
    /** Components from the opened module. */
    components: Tbl.t(string, ('a, int)),
    /** A callback to be applied when a component is used from this
        "open".  This is used to detect unused "opens".  The
        arguments are used to detect shadowing. */
    using: option((string, option(('a, 'a))) => unit),
    /** The table before opening the module. */
    next: t('a),
  };

  let empty = {current: Ident.empty, opened: None};

  let add = (id, x, tbl) => {
    ...tbl,
    current: Ident.add(id, x, tbl.current),
  };

  let add_open = (slot, wrap, root, components, next) => {
    let using =
      switch (slot) {
      | None => None
      | Some(f) => Some((s, x) => f(s, wrap(x)))
      };

    {current: Ident.empty, opened: Some({using, root, components, next})};
  };

  let rec find_same = (id, tbl) =>
    try(Ident.find_same(id, tbl.current)) {
    | Not_found as exn =>
      switch (tbl.opened) {
      | Some({next, _}) => find_same(id, next)
      | None => raise(exn)
      }
    };

  let rec find_name = (~mark, name, tbl) =>
    try({
      let (id, desc) = Ident.find_name(name, tbl.current);
      (PIdent(id), desc);
    }) {
    | Not_found as exn =>
      switch (tbl.opened) {
      | Some({using, root, next, components}) =>
        try({
          let (descr, pos) = Tbl.find(name, components);
          let res = ([@implicit_arity] PExternal(root, name, pos), descr);
          if (mark) {
            switch (using) {
            | None => ()
            | Some(f) =>
              let tmp = find_name(~mark=false, name, next);
              try(f(name, Some((snd(tmp), snd(res))))) {
              | Not_found => f(name, None)
              };
            };
          };
          res;
        }) {
        | Not_found => find_name(~mark, name, next)
        }
      | None => raise(exn)
      }
    };

  let rec update = (name, f, tbl) =>
    try({
      let (id, desc) = Ident.find_name(name, tbl.current);
      let new_desc = f(desc);
      {...tbl, current: Ident.add(id, new_desc, tbl.current)};
    }) {
    | Not_found =>
      switch (tbl.opened) {
      | Some({root, using, next, components}) =>
        try({
          let (desc, pos) = Tbl.find(name, components);
          let new_desc = f(desc);
          let components = Tbl.add(name, (new_desc, pos), components);
          {...tbl, opened: Some({root, using, next, components})};
        }) {
        | Not_found =>
          let next = update(name, f, next);
          {...tbl, opened: Some({root, using, next, components})};
        }
      | None => tbl
      }
    };

  let rec find_all = (name, tbl) =>
    List.map(
      ((id, desc)) => (PIdent(id), desc),
      Ident.find_all(name, tbl.current),
    )
    @ (
      switch (tbl.opened) {
      | None => []
      | Some({root, using: _, next, components}) =>
        try({
          let (desc, pos) = Tbl.find(name, components);
          [
            ([@implicit_arity] PExternal(root, name, pos), desc),
            ...find_all(name, next),
          ];
        }) {
        | Not_found => find_all(name, next)
        }
      }
    );

  let rec fold_name = (f, tbl, acc) => {
    let acc =
      Ident.fold_name(
        (id, d) => f(Ident.name(id), (PIdent(id), d)),
        tbl.current,
        acc,
      );
    switch (tbl.opened) {
    | Some({root, using: _, next, components}) =>
      acc
      |> Tbl.fold(
           (name, (desc, pos)) =>
             f(name, ([@implicit_arity] PExternal(root, name, pos), desc)),
           components,
         )
      |> fold_name(f, next)
    | None => acc
    };
  };

  let rec local_keys = (tbl, acc) => {
    let acc =
      Ident.fold_all((k, _, accu) => [k, ...accu], tbl.current, acc);
    switch (tbl.opened) {
    | Some(o) => local_keys(o.next, acc)
    | None => acc
    };
  };

  let rec iter = (f, tbl) => {
    Ident.iter((id, desc) => f(id, (PIdent(id), desc)), tbl.current);
    switch (tbl.opened) {
    | Some({root, using: _, next, components}) =>
      Tbl.iter(
        (s, (x, pos)) =>
          f(
            Ident.hide(Ident.create(s) /* ??? */),
            ([@implicit_arity] PExternal(root, s, pos), x),
          ),
        components,
      );
      iter(f, next);
    | None => ()
    };
  };

  let diff_keys = (tbl1, tbl2) => {
    let keys2 = local_keys(tbl2, []);
    List.filter(
      id =>
        try(
          {
            ignore(find_same(id, tbl1));
            false;
          }
        ) {
        | Not_found => true
        },
      keys2,
    );
  };
};

type comp_tbl('a) = Tbl.t(string, ('a, int));

type t = {
  values: IdTbl.t(value_description),
  types: IdTbl.t((type_declaration, type_descriptions)),
  constructors: TycompTbl.t(constructor_description),
  labels: TycompTbl.t(label_description),
  components: IdTbl.t(module_components),
  modules:
    IdTbl.t(EnvLazy.t((Subst.t, module_declaration), module_declaration)),
  modtypes: IdTbl.t(modtype_declaration),
  local_constraints: PathMap.t(type_declaration),
  summary,
}

and module_component_components = {
  mutable comp_values: comp_tbl(value_description),
  mutable comp_constrs: Tbl.t(string, list(constructor_description)),
  mutable comp_labels: Tbl.t(string, list(label_description)),
  mutable comp_types: comp_tbl((type_declaration, type_descriptions)),
  mutable comp_components: comp_tbl(module_components),
  mutable comp_modules:
    comp_tbl(EnvLazy.t((Subst.t, module_declaration), module_declaration)),
  mutable comp_modtypes: comp_tbl(modtype_declaration),
}

and module_components = {
  loc: Location.t,
  comps:
    EnvLazy.t(
      (t, Subst.t, Path.t, Types.module_type),
      option(module_component_components),
    ),
};

let empty = {
  values: IdTbl.empty,
  types: IdTbl.empty,
  components: IdTbl.empty,
  modules: IdTbl.empty,
  modtypes: IdTbl.empty,
  constructors: TycompTbl.empty,
  labels: TycompTbl.empty,
  local_constraints: PathMap.empty,
  summary: Env_empty,
};

let copy_local = (~from, env) => {
  ...env,
  local_constraints: from.local_constraints,
};

let same_constr = ref((_, _, _) => assert(false));

type can_load_modules =
  | Can_load_modules
  | Cannot_load_modules(EnvLazy.log);

let can_load_modules = ref(Can_load_modules);

let without_cmis = (f, x) => {
  let log = EnvLazy.log();
  let res =
    Misc.(
      protect_refs(
        [[@implicit_arity] R(can_load_modules, Cannot_load_modules(log))],
        () =>
        f(x)
      )
    );

  EnvLazy.backtrack(log);
  res;
};

/* Forward declarations */

let components_of_module' =
  ref(
    (~deprecated as _, ~loc as _, _env, _sub, _path, _mty) => assert(false):
                                                                    (
                                                                    ~deprecated: option(
                                                                    string,
                                                                    ),
                                                                    ~loc: Location.t,
                                                                    t,
                                                                    Subst.t,
                                                                    Path.t,
                                                                    module_type
                                                                    ) =>
                                                                    module_components,
  );
let components_of_module_maker' =
  ref(
    ((_env, _sub, _path, _mty)) => assert(false):
                                                     (
                                                       (
                                                         t,
                                                         Subst.t,
                                                         Path.t,
                                                         module_type,
                                                       )
                                                     ) =>
                                                     option(
                                                       module_component_components,
                                                     ),
  );
let check_modtype_inclusion =
  /* to be filled with Includemod.check_modtype_inclusion */
  ref(
    (~loc as _, _env, _mty1, _path1, _mty2) => assert(false):
                                                                (
                                                                  ~loc: Location.t,
                                                                  t,
                                                                  module_type,
                                                                  Path.t,
                                                                  module_type
                                                                ) =>
                                                                unit,
  );
let strengthen =
  /* to be filled with Mtype.strengthen */
  ref(
    (~aliasable as _, _env, _mty, _path) => assert(false):
                                                             (
                                                               ~aliasable: bool,
                                                               t,
                                                               module_type,
                                                               Path.t
                                                             ) =>
                                                             module_type,
  );

let md = (md_type, md_filepath) => {
  md_type,
  md_filepath,
  md_loc: Location.dummy_loc,
};

let subst_modtype_maker = ((subst, md)) =>
  if (subst === Subst.identity) {
    md;
  } else {
    {...md, md_type: Subst.modtype(subst, md.md_type)};
  };

let get_components_opt = c =>
  switch (can_load_modules^) {
  | Can_load_modules => EnvLazy.force(components_of_module_maker'^, c.comps)
  | Cannot_load_modules(log) =>
    EnvLazy.force_logged(log, components_of_module_maker'^, c.comps)
  };

let empty_structure = {
  comp_values: Tbl.empty,
  comp_constrs: Tbl.empty,
  comp_labels: Tbl.empty,
  comp_types: Tbl.empty,
  comp_components: Tbl.empty,
  comp_modules: Tbl.empty,
  comp_modtypes: Tbl.empty,
};

let get_components = c =>
  switch (get_components_opt(c)) {
  | None => empty_structure
  | Some(c) => c
  };

let current_unit = ref(("", ""));

let set_unit = ((name, source)) => current_unit := (name, source);

let get_unit = () => current_unit^;

/* Persistent structure descriptions */

type pers_flags = Cmi_format.pers_flags = | Rectypes | Opaque | Unsafe_string;

type pers_struct = {
  ps_name: string,
  ps_sig: Lazy.t(signature),
  ps_comps: module_components,
  ps_crcs: list((string, option(Digest.t))),
  ps_filename: string,
  ps_flags: list(pers_flags),
};

let persistent_structures: Hashtbl.t(string, option(pers_struct)) =
  Hashtbl.create(17);

let unit_to_file: Hashtbl.t(string, string) = Hashtbl.create(17);

let compilation_in_progress: Hashtbl.t(string, Location.loc(string)) =
  Hashtbl.create(17); /* (module, dependent) */

/* Consistency between persistent structures */

module Consistbl = Consistbl.Make(Misc.Stdlib.String);

let crc_units = Consistbl.create();

module StringSet =
  Set.Make({
    type t = string;
    let compare = String.compare;
  });

let imported_units = ref(StringSet.empty);

let add_import = s => imported_units := StringSet.add(s, imported_units^);

let imported_opaque_units = ref(StringSet.empty);

let add_imported_opaque = s =>
  imported_opaque_units := StringSet.add(s, imported_opaque_units^);

let clear_imports = () => {
  Consistbl.clear(crc_units);
  imported_units := StringSet.empty;
  imported_opaque_units := StringSet.empty;
};

let check_consistency = ps =>
  try(
    List.iter(
      ((name, crco)) =>
        switch (crco) {
        | None => ()
        | Some(crc) =>
          add_import(name);
          Consistbl.check(crc_units, name, crc, ps.ps_filename);
        },
      ps.ps_crcs,
    )
  ) {
  | [@implicit_arity] Consistbl.Inconsistency(name, source, auth) =>
    error([@implicit_arity] Inconsistent_import(name, auth, source))
  };

/* Reading persistent structures from .cmi files */

let save_pers_struct = (crc, ps) => {
  let modname = ps.ps_name;
  Hashtbl.add(persistent_structures, modname, Some(ps));
  List.iter(
    fun
    | Rectypes => ()
    | Unsafe_string => ()
    | Opaque => add_imported_opaque(modname),
    ps.ps_flags,
  );
  Consistbl.set(crc_units, modname, crc, ps.ps_filename);
  add_import(modname);
};

let get_dependency_chain = (~loc, unit_name) => {
  let rec help = filename =>
    switch (Hashtbl.find_opt(compilation_in_progress, filename)) {
    | None => []
    | Some(dep) when Hashtbl.mem(unit_to_file, dep.txt) => [
        dep,
        ...help(Hashtbl.find(unit_to_file, dep.txt)),
      ]
    | Some(dep) => [dep]
    };

  let nameloc = Location.mkloc(unit_name, loc);
  switch (Hashtbl.find_opt(unit_to_file, unit_name)) {
  | None => [nameloc]
  | Some(fname) => [nameloc, ...help(fname)]
  };
};

let mark_in_progress = (~loc, unit_name, sourcefile) => {
  if (Hashtbl.mem(compilation_in_progress, sourcefile)) {
    error(
      [@implicit_arity]
      Cyclic_dependencies(unit_name, get_dependency_chain(~loc, unit_name)),
    );
  };
  let (stored_name, _) = get_unit();
  Hashtbl.add(
    compilation_in_progress,
    sourcefile,
    Location.mkloc(stored_name, loc),
  );
  Hashtbl.add(unit_to_file, unit_name, sourcefile);
};

let mark_completed = (unit_name, sourcefile) => {
  Hashtbl.remove(compilation_in_progress, sourcefile);
  Hashtbl.remove(unit_to_file, unit_name);
};

type module_location_result =
  | GrainModule(string, option(string)) /* Grain Source file, Compiled object */
  | WasmModule(string); /* Compiled object */

let compile_module_dependency =
  ref((filename, output_file) =>
    failwith("compile_module Should be filled in by compile.ml")
  );

let file_older = (a, b) => {
  let last_modified = f => Unix.(stat(f).st_mtime);
  last_modified(a) < last_modified(b);
};

let get_output_name = name => {
  let name =
    try(Filename.chop_extension(name)) {
    | Invalid_argument(_) => name
    };
  name ++ ".wasm";
};

let get_up_to_date = (~loc, unit_name) =>
  fun
  | WasmModule(path) => path
  | [@implicit_arity] GrainModule(srcpath, Some(objpath))
      when file_older(srcpath, objpath) => objpath
  | [@implicit_arity] GrainModule(srcpath, _) => {
      let srcpath = Grain_utils.Files.derelativize(srcpath);
      /* Note: This is a potential security issue? */
      let outpath = get_output_name(srcpath);
      mark_in_progress(~loc, unit_name, srcpath);
      let saved_unit = get_unit();
      let saved = Ident.save_state();
      compile_module_dependency^(srcpath, outpath);
      Ident.restore_state(saved);
      set_unit(saved_unit);
      mark_completed(unit_name, srcpath);
      outpath;
    };

let find_ext_in_dir = (dir, name) => {
  let file_exists = Sys.file_exists;
  let fullname = Filename.concat(dir, name);
  let rec process_ext =
    fun
    | [] => None
    | [ext, ..._] when file_exists(fullname ++ ext) =>
      Some((fullname ++ ext, dir, name, ext))
    | [_, ...tl] => process_ext(tl);
  process_ext;
};

let find_in_path_uncap = (~exts=[], path, name) => {
  let rec try_dir =
    fun
    | [] => raise(Not_found)
    | [dir, ...rem] =>
      switch (find_ext_in_dir(dir, name, exts)) {
      | Some(path) => path
      | None => try_dir(rem)
      };
  try_dir(path);
};

let locate_module = (path, unit_name) => {
  let grain_src_exts = [".gr"];
  switch (find_in_path_uncap(~exts=[".wasm"], path, unit_name)) {
  | (objpath, dir, basename, ext) =>
    switch (find_ext_in_dir(dir, basename, grain_src_exts)) {
    | Some((srcpath, _, _, _)) =>
      [@implicit_arity] GrainModule(srcpath, Some(objpath))
    | None => WasmModule(objpath)
    }
  | exception Not_found =>
    let (srcpath, _, _, _) =
      find_in_path_uncap(~exts=grain_src_exts, path, unit_name);
    [@implicit_arity] GrainModule(srcpath, None);
  };
};

let locate_module_file = (~loc, path, unit_name) => {
  /* NOTE: We need to take care here to *not* wrap get_up_to_date with this try/with, since
     it will falsely raise No_module_file if a Not_found is raised during the compilation */
  let located =
    try(locate_module(path, unit_name)) {
    | Not_found => error([@implicit_arity] No_module_file(unit_name, None))
    };
  get_up_to_date(~loc, unit_name, located);
};

let resolutions = Hashtbl.create(12);

let resolve_unit = unit_name =>
  try(Hashtbl.find(resolutions, unit_name)) {
  | Not_found =>
    let path = Grain_utils.Config.module_search_path();
    let exts = [".gr", ".wasm"];
    let (_, dir, basename, _) = find_in_path_uncap(~exts, path, unit_name);
    let resolution =
      Grain_utils.Files.derelativize @@ Filename.concat(dir, basename);
    Hashtbl.add(resolutions, unit_name, resolution);
    resolution;
  };

module Persistent_signature = {
  type t = {
    filename: string,
    cmi: Cmi_format.cmi_infos,
  };

  let load =
    ref((~loc=Location.dummy_loc, ~unit_name) =>
      switch (
        locate_module_file(
          ~loc,
          Grain_utils.Config.module_search_path(),
          unit_name,
        )
      ) {
      | filename => Some({filename, cmi: read_cmi(filename)})
      | exception Not_found => None
      }
    );
};

let acknowledge_pers_struct =
    (check, modname, {Persistent_signature.filename, cmi}) => {
  let name = cmi.cmi_name;
  let sign = cmi.cmi_sign;
  let crcs = cmi.cmi_crcs;
  let flags = cmi.cmi_flags;
  let comps =
    components_of_module'^(
      ~deprecated=None,
      ~loc=Location.dummy_loc,
      empty,
      Subst.identity,
      PIdent(Ident.create_persistent(name)),
      TModSignature(sign),
    );

  let ps = {
    ps_name: name,
    ps_sig: lazy(Subst.signature(Subst.identity, sign)),
    ps_comps: comps,
    ps_crcs: crcs,
    ps_filename: filename,
    ps_flags: flags,
  };

  List.iter(
    fun
    | Rectypes =>
      if (! Clflags.recursive_types^) {
        let (unit_name, _) = get_unit();
        error([@implicit_arity] Need_recursive_types(ps.ps_name, unit_name));
      }
    | Unsafe_string =>
      if (Config.safe_string^) {
        let (unit_name, _) = get_unit();
        error(
          [@implicit_arity]
          Depend_on_unsafe_string_unit(ps.ps_name, unit_name),
        );
      }
    | Opaque => add_imported_opaque(modname),
    ps.ps_flags,
  );
  if (check) {
    check_consistency(ps);
  };
  Hashtbl.add(persistent_structures, modname, Some(ps));
  ps;
};

let read_pers_struct = (check, modname, filename) => {
  add_import(modname);
  let cmi = read_cmi(filename);
  acknowledge_pers_struct(
    check,
    modname,
    {Persistent_signature.filename, cmi},
  );
};

let find_pers_struct = (~loc, check, name, filepath) => {
  if (name == "*predef*") {
    raise(Not_found);
  };
  switch (Hashtbl.find(persistent_structures, name)) {
  | Some(ps) => ps
  | None => raise(Not_found)
  | exception Not_found =>
    switch (can_load_modules^) {
    | Cannot_load_modules(_) => raise(Not_found)
    | Can_load_modules =>
      let ps = {
        let filepath =
          try(Option.get(filepath)) {
          | Invalid_argument(_) => failwith("No file path specified")
          };
        switch (Persistent_signature.load^(~loc, ~unit_name=filepath)) {
        | Some(ps) => ps
        | None =>
          Hashtbl.add(persistent_structures, name, None);
          raise(Not_found);
        };
      };

      add_import(name);
      acknowledge_pers_struct(check, name, ps);
    }
  };
};

/* Emits a warning if there is no valid cmi for name */
let check_pers_struct = (~loc, name, filename) =>
  try(ignore(find_pers_struct(~loc, false, name, filename))) {
  | Not_found =>
    let err = [@implicit_arity] No_module_file(name, None);
    error(err);
  | Cmi_format.Error(err) =>
    let msg = Format.asprintf("%a", Cmi_format.report_error, err);
    let err = [@implicit_arity] No_module_file(name, Some(msg));
    error(err);
  | Error(err) =>
    let msg =
      switch (err) {
      | [@implicit_arity] Illegal_renaming(name, ps_name, filename) =>
        Format.asprintf(
          " %a@ contains the compiled interface for @ %s when %s was expected",
          Location.print_filename,
          filename,
          ps_name,
          name,
        )
      | Inconsistent_import(_) => assert(false)
      | [@implicit_arity] Need_recursive_types(name, _) =>
        Format.sprintf("%s uses recursive types", name)
      | [@implicit_arity] Depend_on_unsafe_string_unit(name, _) =>
        Printf.sprintf("%s uses -unsafe-string", name)
      | Unbound_label(_) => assert(false)
      | Unbound_module(_) => assert(false)
      | Missing_module(_) => assert(false)
      | No_module_file(_) => assert(false)
      | Value_not_found_in_module(_) => assert(false)
      | Illegal_value_name(_) => assert(false)
      | Cyclic_dependencies(_) => assert(false)
      };

    let err = [@implicit_arity] No_module_file(name, Some(msg));
    error(err);
  };

let read_pers_struct = (modname, filename) =>
  read_pers_struct(true, modname, filename);

let find_pers_struct = (name, filename) =>
  find_pers_struct(true, name, filename);

let check_pers_struct = (~loc, name, filename) =>
  if (!Hashtbl.mem(persistent_structures, name)) {
    /* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. */
    add_import(name);
    if (Warnings.is_active([@implicit_arity] Warnings.NoCmiFile("", None))) {
      add_delayed_check_forward^(() =>
        check_pers_struct(~loc, name, filename)
      );
    };
  };

let rec find_module_descr = (path, filename, env) =>
  switch (path) {
  | PIdent(id) =>
    try(IdTbl.find_same(id, env.components)) {
    | Not_found =>
      let (_, unit_source) = get_unit();
      if (Ident.persistent(id)
          && !(
               Option.value(~default=Ident.name(id), filename) == unit_source
             )) {
        find_pers_struct(~loc=Location.dummy_loc, Ident.name(id), filename).
          ps_comps;
      } else {
        raise(Not_found);
      };
    }
  | [@implicit_arity] PExternal(m, s, pos) =>
    let c = get_components(find_module_descr(m, filename, env));
    let (descr, _pos) = Tbl.find(s, c.comp_components);
    descr;
  };

let find = (proj1, proj2, path, env) =>
  switch (path) {
  | PIdent(id) => IdTbl.find_same(id, proj1(env))
  | [@implicit_arity] PExternal(m, n, _pos) =>
    let c = get_components(find_module_descr(m, None, env));
    let (data, _pos) = Tbl.find(n, proj2(c));
    data;
  };

let find_value = find(env => env.values, sc => sc.comp_values);

let find_type_full = find(env => env.types, sc => sc.comp_types)

and find_modtype = find(env => env.modtypes, sc => sc.comp_modtypes);

let find_type_full = (path, env) =>
  switch (path) {
  | PIdent(_) =>
    try((PathMap.find(path, env.local_constraints), ([], []))) {
    | Not_found => find_type_full(path, env)
    }
  | _ => find_type_full(path, env)
  };

let find_type = (p, env) => fst(find_type_full(p, env));

let find_type_descrs = (p, env) => snd(find_type_full(p, env));

let find_module = (~alias, path, filename, env) =>
  switch (path) {
  | PIdent(id) =>
    try({
      let data = IdTbl.find_same(id, env.modules);
      EnvLazy.force(subst_modtype_maker, data);
    }) {
    | Not_found =>
      let (_, unit_source) = get_unit();
      if (Ident.persistent(id)
          && !(
               Option.value(~default=Ident.name(id), filename) == unit_source
             )) {
        let ps =
          find_pers_struct(
            ~loc=Location.dummy_loc,
            Ident.name(id),
            filename,
          );
        md(TModSignature(Lazy.force(ps.ps_sig)), filename);
      } else {
        raise(Not_found);
      };
    }
  | [@implicit_arity] PExternal(m, n, _pos) =>
    let c = get_components(find_module_descr(m, filename, env));
    let (data, _pos) = Tbl.find(n, c.comp_modules);
    EnvLazy.force(subst_modtype_maker, data);
  };

let rec normalize_path = (lax, env, path) =>
  switch (path) {
  | [@implicit_arity] PExternal(p, s, pos) =>
    [@implicit_arity] PExternal(normalize_path(lax, env, p), s, pos)
  | _ => path
  };
/* No module aliases, so this is all we need*/

let normalize_path = (oloc, env, path) =>
  try(normalize_path(oloc == None, env, path)) {
  | Not_found =>
    switch (oloc) {
    | None => assert(false)
    | Some(loc) =>
      raise(
        Error(
          [@implicit_arity]
          Missing_module(loc, path, normalize_path(true, env, path)),
        ),
      )
    }
  };

let normalize_path_prefix = (oloc, env, path) =>
  switch (path) {
  | [@implicit_arity] PExternal(p, s, pos) =>
    [@implicit_arity] PExternal(normalize_path(oloc, env, p), s, pos)
  | PIdent(_) => path
  };
/*| PApply _ -> assert false*/

let find_module = find_module(~alias=false);

/* Find the manifest type associated to a type when appropriate:
   - the type should be public */
let find_type_expansion = (path, env) => {
  let decl = find_type(path, env);
  switch (decl.type_manifest) {
  | Some(body) => (
      decl.type_params,
      body,
      Option.map(snd, decl.type_newtype_level),
    )
  /* The manifest type of Private abstract data types without
     private row are still considered unknown to the type system.
     Hence, this case is caught by the following clause that also handles
     purely abstract data types without manifest type definition. */
  | _ => raise(Not_found)
  };
};

/* Find the manifest type information associated to a type, i.e.
   the necessary information for the compiler's type-based optimisations.
   In particular, the manifest type associated to a private abstract type
   is revealed for the sake of compiler's type-based optimisations. */
let find_type_expansion_opt = (path, env) => {
  let decl = find_type(path, env);
  switch (decl.type_manifest) {
  /* The manifest type of Private abstract data types can still get
     an approximation using their manifest type. */
  | Some(body) => (
      decl.type_params,
      body,
      Option.map(snd, decl.type_newtype_level),
    )
  | _ => raise(Not_found)
  };
};

let find_modtype_expansion = (path, env) =>
  switch (find_modtype(path, env).mtd_type) {
  | None => raise(Not_found)
  | Some(mty) => mty
  };

let has_local_constraints = env => !PathMap.is_empty(env.local_constraints);

let copy_types = (l, env) => {
  let f = desc => {
    ...desc,
    val_type: Subst.type_expr(Subst.identity, desc.val_type),
  };
  let values =
    List.fold_left((env, s) => IdTbl.update(s, f, env), env.values, l);
  {
    ...env,
    values,
    summary: [@implicit_arity] Env_copy_types(env.summary, l),
  };
};

/* Currently a no-op */
let mark_value_used = (env, name, loc) => (); /*Printf.eprintf "Marking value %s used\n" name*/
let mark_type_used = (env, name, loc) => ();
let mark_module_used = (env, name, loc) => ();

let rec lookup_module_descr_aux = (~mark, id, env) =>
  Identifier.(
    switch (id) {
    | IdentName(s) =>
      let id = Ident.create_persistent(s);
      (PIdent(id), IdTbl.find_same(id, env.components));
    | [@implicit_arity] IdentExternal(m, n) =>
      let (p, descr) = lookup_module_descr(~mark, m, env);
      /* let (_, pos) = Tbl.find n (get_components descr).comp_components in */
      /* FIXME: Should this have a proper position? */
      ([@implicit_arity] PExternal(p, n, Path.nopos), descr);
    }
  )

and lookup_module_descr = (~mark, id, env) => {
  let (p, comps) as res = lookup_module_descr_aux(~mark, id, env);
  if (mark) {
    mark_module_used(env, Path.last(p), comps.loc);
  };
  res;
}

and lookup_module = (~loc=?, ~load, ~mark, id, filename, env): Path.t =>
  switch (id) {
  | Identifier.IdentName(s) =>
    try({
      let (p, data) = IdTbl.find_name(~mark, s, env.modules);
      let {md_loc, md_type} = EnvLazy.force(subst_modtype_maker, data);
      if (mark) {
        mark_module_used(env, s, md_loc);
      };
      switch (md_type) {
      | TModIdent(Path.PIdent(id)) when Ident.name(id) == "#recmod#" =>
        /* see #5965 */
        failwith("NYI: lookup_module: raise Recmodule")
      | _ => ()
      };
      p;
    }) {
    | Not_found =>
      let (_, unit_source) = get_unit();
      if (Option.value(~default=s, filename) == unit_source) {
        raise(Not_found);
      };
      let p = PIdent(Ident.create_persistent(s));
      let loc = Option.value(~default=Location.dummy_loc, loc);
      // !Grain_utils.Config.transparent_modules &&
      if (!load) {
        check_pers_struct(~loc, s, filename);
      } else {
        ignore(find_pers_struct(~loc, s, filename));
      };
      p;
    }
  | [@implicit_arity] Identifier.IdentExternal(l, s) =>
    let (p, descr) = lookup_module_descr(~mark, l, env);
    let c = get_components(descr);
    let (_data, pos) = Tbl.find(s, c.comp_modules);
    let (comps, _) = Tbl.find(s, c.comp_components);
    if (mark) {
      mark_module_used(env, s, comps.loc);
    };
    let p = [@implicit_arity] PExternal(p, s, pos);
    p;
  };

let lookup_idtbl = (~mark, proj1, proj2, id, env) =>
  Identifier.(
    switch (id) {
    | IdentName(s) => IdTbl.find_name(~mark, s, proj1(env))
    | [@implicit_arity] IdentExternal(m, n) =>
      let (p, desc) = lookup_module_descr(~mark, id, env);
      let (data, pos) = Tbl.find(n, proj2(get_components(desc)));
      let new_path =
        switch (p) {
        | [@implicit_arity] PExternal(path, name, _) =>
          [@implicit_arity] PExternal(path, name, pos)
        | _ => assert(false)
        };
      (new_path, data);
    }
  );

let lookup_tycomptbl = (~mark, proj1, proj2, id, env) =>
  Identifier.(
    switch (id) {
    | IdentName(s) => TycompTbl.find_all(s, proj1(env))
    | [@implicit_arity] IdentExternal(m, n) =>
      let (p, desc) = lookup_module_descr(~mark, id, env);
      let comps =
        try(Tbl.find(n, proj2(get_components(desc)))) {
        | Not_found => []
        };
      List.map(data => (data, () => ()), comps);
    }
  );

let lookup_value = (~mark, id, e) =>
  lookup_idtbl(~mark, x => x.values, x => x.comp_values, id, e);

let lookup_type = (~mark, id, e) =>
  lookup_idtbl(~mark, x => x.types, x => x.comp_types, id, e);

let lookup_modtype = (~mark, id, e) =>
  lookup_idtbl(~mark, env => env.modtypes, sc => sc.comp_modtypes, id, e);

let lookup_all_constructors = (~mark, id, env) =>
  lookup_tycomptbl(~mark, x => x.constructors, x => x.comp_constrs, id, env);

let lookup_constructor = (~mark=true, id, env) =>
  switch (lookup_all_constructors(~mark, id, env)) {
  | [] => raise(Not_found)
  | [(desc, use), ..._] =>
    if (mark) {
      use();
    };
    desc;
  };

let lookup_all_labels = (~mark, lid, env) =>
  lookup_tycomptbl(~mark, x => x.labels, x => x.comp_labels, lid, env);

let lookup_label = (~mark, lid, env) =>
  switch (lookup_all_labels(~mark, lid, env)) {
  | [] => assert(false)
  | [(desc, use), ..._] =>
    use();
    desc;
  };

let mark_type_path = (env, path) =>
  try({
    let decl = find_type(path, env);
    mark_type_used(env, Path.last(path), decl);
  }) {
  | Not_found => ()
  };

let ty_path = t =>
  switch (Btype.repr(t)) {
  | {desc: [@implicit_arity] TTyConstr(path, _, _)} => path
  | _ => assert(false)
  };

let lookup_value = (~mark=true, lid, env) => {
  let (_, desc) as r = lookup_value(~mark, lid, env);
  if (mark) {
    mark_value_used(env, Identifier.last(lid), desc);
  };
  r;
};

let lookup_type = (~mark=true, lid, env) => {
  let (path, (decl, _)) = lookup_type(~mark, lid, env);
  if (mark) {
    mark_type_used(env, Identifier.last(lid), decl);
  };
  path;
};

let lookup_all_constructors = (~mark=true, lid, env) =>
  try({
    let cstrs = lookup_all_constructors(~mark, lid, env);
    let wrap_use = (desc, use, ()) =>
      if (mark) {
        mark_type_path(env, ty_path(desc.cstr_res));
        use();
      };

    List.map(((cstr, use)) => (cstr, wrap_use(cstr, use)), cstrs);
  }) {
  | Not_found
      when
        switch (lid) {
        | Identifier.IdentName(_) => true
        | _ => false
        } =>
    []
  };

let lookup_label = (~mark=true, lid, env) => lookup_label(~mark, lid, env);

let lookup_all_labels = (~mark=true, lid, env) =>
  try({
    let labels = lookup_all_labels(~mark, lid, env);
    let wrap_use = (desc, use, ()) =>
      if (mark) {
        mark_type_path(env, ty_path(desc.lbl_res));
        use();
      };

    List.map(((label, use)) => (label, wrap_use(label, use)), labels);
  }) {
  | Not_found
      when
        switch (lid) {
        | Identifier.IdentName(_) => true
        | _ => false
        } =>
    []
  };

let lookup_module = (~load, ~loc=?, ~mark=true, lid, filename, env) =>
  lookup_module(~load, ~loc?, ~mark, lid, filename, env);

let lookup_modtype = (~loc=?, ~mark=true, lid, env) =>
  lookup_modtype(~mark, lid, env);

/* Iter on an environment (ignoring the body of functors and
   not yet evaluated structures) */

type iter_cont = unit => unit;
let iter_env_cont = ref([]);

let rec scrape_alias_for_visit = (env, mty) =>
  switch (mty) {
  | _ => true
  };

let iter_env = (proj1, proj2, f, env, ()) => {
  IdTbl.iter((id, x) => f(PIdent(id), x), proj1(env));
  let rec iter_components = (path, path', mcomps) => {
    let cont = () => {
      let visit =
        switch (EnvLazy.get_arg(mcomps.comps)) {
        | None => true
        | Some((env, _sub, _path, mty)) => scrape_alias_for_visit(env, mty)
        };

      if (!visit) {
        ();
      } else {
        let comps = get_components(mcomps);
        Tbl.iter(
          (s, (d, n)) =>
            f(
              [@implicit_arity] PExternal(path, s, n),
              ([@implicit_arity] PExternal(path', s, n), d),
            ),
          proj2(comps),
        );
        Tbl.iter(
          (s, (c, n)) =>
            iter_components(
              [@implicit_arity] PExternal(path, s, n),
              [@implicit_arity] PExternal(path', s, n),
              c,
            ),
          comps.comp_components,
        );
      };
    };
    iter_env_cont := [(path, cont), ...iter_env_cont^];
  };

  Hashtbl.iter(
    (s, pso) =>
      switch (pso) {
      | None => ()
      | Some(ps) =>
        let id = PIdent(Ident.create_persistent(s));
        iter_components(id, id, ps.ps_comps);
      },
    persistent_structures,
  );
  IdTbl.iter(
    (id, (path, comps)) => iter_components(PIdent(id), path, comps),
    env.components,
  );
};

let run_iter_cont = l => {
  iter_env_cont := [];
  List.iter(c => c(), l);
  let cont = List.rev(iter_env_cont^);
  iter_env_cont := [];
  cont;
};

let iter_types = f => iter_env(env => env.types, sc => sc.comp_types, f);

let same_types = (env1, env2) =>
  env1.types === env2.types && env1.components === env2.components;

let used_persistent = () => {
  let r = ref(Concr.empty);
  Hashtbl.iter(
    (s, pso) =>
      if (pso !== None) {
        r := Concr.add(s, r^);
      },
    persistent_structures,
  );
  r^;
};

let find_all_comps = (proj, s, (p, mcomps)) => {
  let comps = get_components(mcomps);
  try({
    let (c, n) = Tbl.find(s, proj(comps));
    [([@implicit_arity] PExternal(p, s, n), c)];
  }) {
  | Not_found => []
  };
};

let rec find_shadowed_comps = (path, env) =>
  switch (path) {
  | PIdent(id) => IdTbl.find_all(Ident.name(id), env.components)
  | [@implicit_arity] PExternal(p, s, _) =>
    let l = find_shadowed_comps(p, env);
    let l' = List.map(find_all_comps(comps => comps.comp_components, s), l);
    List.flatten(l');
  };

let find_shadowed = (proj1, proj2, path, env) =>
  switch (path) {
  | PIdent(id) => IdTbl.find_all(Ident.name(id), proj1(env))
  | [@implicit_arity] PExternal(p, s, _) =>
    let l = find_shadowed_comps(p, env);
    let l' = List.map(find_all_comps(proj2, s), l);
    List.flatten(l');
  };

let find_shadowed_types = (path, env) =>
  List.map(
    fst,
    find_shadowed(env => env.types, comps => comps.comp_types, path, env),
  );

let rec scrape_alias = (env, ~path=?, mty) =>
  switch (mty, path) {
  | (TModIdent(p), _)
  | (TModAlias(p), _) =>
    try(scrape_alias(env, find_modtype_expansion(p, env), ~path?)) {
    | Not_found => mty
    }
  | (mty, Some(path)) => strengthen^(~aliasable=true, env, mty, path)
  | _ => mty
  };

let scrape_alias = (env, mty) => scrape_alias(env, mty);

let rec prefix_idents = (root, pos, sub) =>
  fun
  | [] => ([], sub)
  | [[@implicit_arity] TSigValue(id, decl), ...rem] => {
      let p = [@implicit_arity] PExternal(root, Ident.name(id), pos);
      let nextpos =
        switch (decl.val_kind) {
        | TValPrim(_) => pos
        | _ => pos + 1
        };
      let (pl, final_sub) = prefix_idents(root, nextpos, sub, rem);
      ([p, ...pl], final_sub);
    }
  | [[@implicit_arity] TSigType(id, _, _), ...rem] => {
      let p = [@implicit_arity] PExternal(root, Ident.name(id), nopos);
      let (pl, final_sub) =
        prefix_idents(root, pos, Subst.add_type(id, p, sub), rem);
      ([p, ...pl], final_sub);
    }
  | [[@implicit_arity] TSigModule(id, _, _), ...rem] => {
      let p = [@implicit_arity] PExternal(root, Ident.name(id), pos);
      let (pl, final_sub) =
        prefix_idents(root, pos + 1, Subst.add_module(id, p, sub), rem);
      ([p, ...pl], final_sub);
    }
  | [[@implicit_arity] TSigModType(id, _), ...rem] => {
      let p = [@implicit_arity] PExternal(root, Ident.name(id), nopos);
      let (pl, final_sub) =
        prefix_idents(
          root,
          pos,
          Subst.add_modtype(id, TModIdent(p), sub),
          rem,
        );
      ([p, ...pl], final_sub);
    };

let prefix_idents = (root, sub, sg) =>
  if (sub == Subst.identity) {
    let sgs =
      try(Hashtbl.find(prefixed_sg, root)) {
      | Not_found =>
        let sgs = ref([]);
        Hashtbl.add(prefixed_sg, root, sgs);
        sgs;
      };

    try(List.assq(sg, sgs^)) {
    | Not_found =>
      let r = prefix_idents(root, 0, sub, sg);
      sgs := [(sg, r), ...sgs^];
      r;
    };
  } else {
    prefix_idents(root, 0, sub, sg);
  };

let check_value_name = (name, loc) =>
  /* Note: we could also check here general validity of the
     identifier, to protect against bad identifiers forged other
     steps of the pipeline. */
  if (String.length(name) > 0 && name.[0] == '#') {
    for (i in 1 to String.length(name) - 1) {
      if (name.[i] == '#') {
        raise(Error([@implicit_arity] Illegal_value_name(loc, name)));
      };
    };
  };

/* Compute structure descriptions */

let add_to_tbl = (id, decl, tbl) => {
  let decls =
    try(Tbl.find(id, tbl)) {
    | Not_found => []
    };
  Tbl.add(id, [decl, ...decls], tbl);
};

let rec components_of_module =
        (~deprecated: option(string), ~loc, env, sub, path, mty) => {
  loc,
  comps: EnvLazy.create((env, sub, path, mty)),
}

and components_of_module_maker = ((env, sub, path, mty)) =>
  switch (scrape_alias(env, mty)) {
  | TModSignature(sg) =>
    let c = {
      comp_values: Tbl.empty,
      comp_constrs: Tbl.empty,
      comp_labels: Tbl.empty,
      comp_types: Tbl.empty,
      comp_modules: Tbl.empty,
      comp_components: Tbl.empty,
      comp_modtypes: Tbl.empty,
    };
    let (pl, sub) =
      switch (mty, path) {
      | (TModAlias(path), _)
      | (TModIdent(path), _)
      | (TModSignature(_), path) => prefix_idents(path, sub, sg)
      };
    let env = ref(env);
    let pos = ref(0);
    List.iter2(
      (item, path) =>
        switch (item) {
        | [@implicit_arity] TSigValue(id, decl) =>
          let decl' = Subst.value_description(sub, decl);
          let decl' = {...decl', val_fullpath: path};
          c.comp_values =
            Tbl.add(Ident.name(id), (decl', pos^), c.comp_values);
          switch (decl.val_kind) {
          | TValPrim(_) => ()
          | _ => incr(pos)
          };
        | [@implicit_arity] TSigType(id, decl, _) =>
          let decl' = Subst.type_declaration(sub, decl);
          let constructors = Datarepr.constructors_of_type(path, decl');
          let cstrs =
            List.map(snd, Datarepr.constructors_of_type(path, decl'));
          let labels = List.map(snd, Datarepr.labels_of_type(path, decl'));
          List.iter(
            ((id, desc)) => {
              let val_type =
                switch (desc.cstr_args) {
                | [] => desc.cstr_res
                | args =>
                  Btype.newgenty(
                    [@implicit_arity] TTyArrow(args, desc.cstr_res, TComOk),
                  )
                };
              let val_type =
                switch (desc.cstr_existentials) {
                | [] => val_type
                | existentials =>
                  Btype.newgenty(
                    [@implicit_arity] TTyPoly(val_type, existentials),
                  )
                };
              let get_path = name =>
                switch (path) {
                | PIdent(_) => PIdent(Ident.create(name))
                | [@implicit_arity] PExternal(PIdent(mod_), _, level) =>
                  [@implicit_arity] PExternal(PIdent(mod_), name, level)
                | [@implicit_arity] PExternal(PExternal(_), _, _) =>
                  failwith("NYI: Multiple PExternal")
                };
              let val_desc = {
                val_type,
                val_fullpath: get_path(desc.cstr_name),
                val_kind: TValConstructor(desc),
                val_loc: desc.cstr_loc,
                val_mutable: false,
              };
              c.comp_values =
                Tbl.add(Ident.name(id), (val_desc, nopos), c.comp_values);
            },
            constructors,
          );
          c.comp_types =
            Tbl.add(
              Ident.name(id),
              ((decl', (cstrs, labels)), nopos),
              c.comp_types,
            );
          List.iter(
            descr =>
              c.comp_constrs =
                add_to_tbl(descr.cstr_name, descr, c.comp_constrs),
            cstrs,
          );
          List.iter(
            descr =>
              c.comp_labels =
                add_to_tbl(descr.lbl_name, descr, c.comp_labels),
            labels,
          );
          env := store_type_infos(id, decl, env^);
        | [@implicit_arity] TSigModule(id, md, _) =>
          let md' = EnvLazy.create((sub, md));
          c.comp_modules =
            Tbl.add(Ident.name(id), (md', pos^), c.comp_modules);
          let comps =
            components_of_module(
              ~deprecated=None,
              ~loc=md.md_loc,
              env^,
              sub,
              path,
              md.md_type,
            );

          c.comp_components =
            Tbl.add(Ident.name(id), (comps, pos^), c.comp_components);
          env := store_module(~check=false, id, md, env^);
          incr(pos);
        | [@implicit_arity] TSigModType(id, decl) =>
          let decl' = Subst.modtype_declaration(sub, decl);
          c.comp_modtypes =
            Tbl.add(Ident.name(id), (decl', nopos), c.comp_modtypes);
          env := store_modtype(id, decl, env^);
        },
      sg,
      pl,
    );
    Some(c);
  | TModIdent(_) => None
  | TModAlias(_) => None
  }

and store_type = (~check, id, info, env) => {
  /*let loc = info.type_loc in*/
  /*if check then
    check_usage loc id (fun s -> Warnings.Unused_type_declaration s)
      type_declarations;*/
  let path = PIdent(id);
  let constructors = Datarepr.constructors_of_type(path, info);
  let labels = Datarepr.labels_of_type(path, info);
  let descrs = (List.map(snd, constructors), List.map(snd, labels));

  let val_descrs =
    List.map(
      ((id, desc)) => {
        let val_type =
          switch (desc.cstr_args) {
          | [] => desc.cstr_res
          | args =>
            Btype.newgenty(
              [@implicit_arity] TTyArrow(args, desc.cstr_res, TComOk),
            )
          };
        let val_type =
          switch (desc.cstr_existentials) {
          | [] => val_type
          | existentials =>
            Btype.newgenty([@implicit_arity] TTyPoly(val_type, existentials))
          };
        let val_desc = {
          val_type,
          val_fullpath: PIdent(Ident.create(desc.cstr_name)),
          val_kind: TValConstructor(desc),
          val_loc: desc.cstr_loc,
          val_mutable: false,
        };
        (id, val_desc);
      },
      constructors,
    );

  /*if check && not loc.Location.loc_ghost &&
      Warnings.is_active (Warnings.Unused_constructor ("", false, false))
    then begin
      let ty = Ident.name id in
      List.iter
        begin fun (_, {cstr_name = c; _}) ->
          let k = (ty, loc, c) in
          if not (Hashtbl.mem used_constructors k) then
            let used = constructor_usages () in
            Hashtbl.add used_constructors k (add_constructor_usage used)
            if not (ty = "" || ty.[0] = '_')
            then !add_delayed_check_forward
                (fun () ->
                  if not (is_in_signature env) && not used.cu_positive then
                    Location.prerr_warning loc
                      (Warnings.Unused_constructor
                         (c, used.cu_pattern, used.cu_privatize)))
        end
        constructors
      end;*/
  {
    ...env,
    constructors:
      List.fold_right(
        ((id, descr), constrs) => TycompTbl.add(id, descr, constrs),
        constructors,
        env.constructors,
      ),
    labels:
      List.fold_right(
        ((id, descr), labels) => TycompTbl.add(id, descr, labels),
        labels,
        env.labels,
      ),
    types: IdTbl.add(id, (info, descrs), env.types),
    values:
      List.fold_left(
        (acc, (id, val_desc)) => IdTbl.add(id, val_desc, acc),
        env.values,
        val_descrs,
      ),
    summary:
      List.fold_left(
        (acc, (id, val_desc)) =>
          [@implicit_arity] Env_value(acc, id, val_desc),
        [@implicit_arity] Env_type(env.summary, id, info),
        val_descrs,
      ),
  };
}

and store_type_infos = (id, info, env) =>
  /* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. */
  {
    ...env,
    types: IdTbl.add(id, (info, ([], [])), env.types),
    summary: [@implicit_arity] Env_type(env.summary, id, info),
  }

and store_module = (~check, id, md, env) =>
  /*let loc = md.md_loc in
    if check then
      check_usage loc id (fun s -> Warnings.Unused_module s)
        module_declarations;***/
  {
    ...env,
    modules:
      IdTbl.add(id, EnvLazy.create((Subst.identity, md)), env.modules),
    components:
      IdTbl.add(
        id,
        components_of_module(
          ~deprecated=None,
          ~loc=md.md_loc,
          env,
          Subst.identity,
          PIdent(id),
          md.md_type,
        ),
        env.components,
      ),
    summary: [@implicit_arity] Env_module(env.summary, id, md),
  }

and store_modtype = (id, info, env) => {
  ...env,
  modtypes: IdTbl.add(id, info, env.modtypes),
  summary: [@implicit_arity] Env_modtype(env.summary, id, info),
};

let store_value = (id, decl, env) => {
  ...env,
  values: IdTbl.add(id, decl, env.values),
  summary: [@implicit_arity] Env_value(env.summary, id, decl),
};

let _ = {
  components_of_module' := components_of_module;
  components_of_module_maker' := components_of_module_maker;
};

let add_value = (~check=?, id, desc, env) => store_value(id, desc, env);

let add_type = (~check, id, info, env) => store_type(~check, id, info, env);

let add_module_declaration = (~arg=false, ~check, id, md, env) => {
  let env = store_module(~check, id, md, env);
  env;
}

and add_modtype = (id, info, env) => store_modtype(id, info, env);

let add_module = (~arg=?, id, mty, mf, env) =>
  add_module_declaration(~check=false, ~arg?, id, md(mty, mf), env);

let add_constructor = (id, desc, {constructors, _} as e) => {
  ...e,
  constructors: TycompTbl.add(id, desc, constructors),
};

let add_local_type = (path, info, env) => {
  ...env,
  local_constraints: PathMap.add(path, info, env.local_constraints),
};

let add_local_constraint = (path, info, elv, env) =>
  switch (info) {
  | {type_manifest: Some(_), type_newtype_level: Some((lv, _))} =>
    /* elv is the expansion level, lv is the definition level */
    let info = {...info, type_newtype_level: Some((lv, elv))};
    add_local_type(path, info, env);
  | _ => assert(false)
  };

/* Insertion of bindings by name */

let enter = (store_fun, name, data, env) => {
  let id = Ident.create(name);
  (id, store_fun(id, data, env));
};

let enter_value = {
  let store_fun = (id, data, env) =>
    store_value(id, {...data, val_fullpath: Path.PIdent(id)}, env);

  enter(store_fun);
}
and enter_type = enter(store_type(~check=true))
and enter_module_declaration = (~arg=?, id, md, env) =>
  add_module_declaration(~arg?, ~check=true, id, md, env)
/* let (id, env) = enter store_module name md env in
   (id, add_functor_arg ?arg id env) */
and enter_modtype = enter(store_modtype);

let enter_module = (~arg=?, s, mty, env) => {
  let id = Ident.create(s);
  (id, enter_module_declaration(~arg?, id, md(mty, None), env));
};

/* Insertion of all components of a signature */

let add_item = (comp, env) =>
  switch (comp) {
  | [@implicit_arity] TSigValue(id, decl) => add_value(id, decl, env)
  | [@implicit_arity] TSigType(id, decl, _) =>
    add_type(~check=false, id, decl, env)
  | [@implicit_arity] TSigModule(id, md, _) =>
    add_module_declaration(~check=false, id, md, env)
  | [@implicit_arity] TSigModType(id, decl) => add_modtype(id, decl, env)
  };

let rec add_signature = (sg, env) =>
  switch (sg) {
  | [] => env
  | [comp, ...rem] => add_signature(rem, add_item(comp, env))
  };

/* Open a signature path */

let add_components =
    (~filter_modules=?, ~filter_components=?, slot, root, env0, comps) => {
  let add_l = (w, comps, env0) => TycompTbl.add_open(slot, w, comps, env0);

  let add = (w, comps, env0) => IdTbl.add_open(slot, w, root, comps, env0);

  let skipped_modules = ref(StringSet.empty);
  let filter = (tbl, env0_tbl) =>
    switch (filter_modules) {
    | None => tbl
    | Some(f) =>
      Tbl.fold(
        (m, x, acc) =>
          if (f(m)) {
            Tbl.add(m, x, acc);
          } else {
            assert(
              switch (IdTbl.find_name(m, env0_tbl, ~mark=false)) {
              | (_: (_, _)) => false
              | exception _ => true
              },
            );
            skipped_modules := StringSet.add(m, skipped_modules^);
            acc;
          },
        tbl,
        Tbl.empty,
      )
    };

  let filter_and_add = (w, comps, env0) => {
    let comps = filter(comps, env0);
    add(w, comps, env0);
  };

  let filtered_components =
    switch (filter_components) {
    | Some(f) =>
      let filter = tbl => {
        let new_tbl = ref(Tbl.empty);
        Tbl.iter(
          (name, value) =>
            switch (f(name)) {
            | Some(new_name) => new_tbl := Tbl.add(new_name, value, new_tbl^)
            | None => ()
            },
          tbl,
        );
        new_tbl^;
      };

      {
        ...comps,
        comp_constrs: filter(comps.comp_constrs),
        comp_values: filter(comps.comp_values),
        comp_types: filter(comps.comp_types),
        comp_modtypes: filter(comps.comp_modtypes),
        comp_components: filter(comps.comp_components),
      };
    | None => comps
    };

  let constructors =
    add_l(
      x => `Constructor(x),
      filtered_components.comp_constrs,
      env0.constructors,
    );

  let values =
    add(x => `Value(x), filtered_components.comp_values, env0.values);

  let types = add(x => `Type(x), filtered_components.comp_types, env0.types);

  let modtypes =
    add(
      x => `Module_type(x),
      filtered_components.comp_modtypes,
      env0.modtypes,
    );

  let components =
    filter_and_add(
      x => `Component(x),
      filtered_components.comp_components,
      env0.components,
    );

  let modules =
    filter_and_add(
      x => `Module(x),
      filtered_components.comp_modules,
      env0.modules,
    );

  {
    ...env0,
    summary: [@implicit_arity] Env_open(env0.summary, root),
    constructors,
    values,
    types,
    modtypes,
    components,
    modules,
  };
};

let same_filepath = (unit1, unit2) =>
  resolve_unit(unit1) == resolve_unit(unit2);

let check_opened = (mod_: Parsetree.import_declaration, env) => {
  let rec find_open = summary =>
    switch (summary) {
    | Env_empty => None
    | [@implicit_arity]
      Env_module(summary, {name} as id, {md_filepath: Some(filepath)})
        when same_filepath(filepath, mod_.pimp_path.txt) =>
      Some(PIdent(id))
    | [@implicit_arity] Env_module(summary, _, _)
    | [@implicit_arity] Env_value(summary, _, _)
    | [@implicit_arity] Env_type(summary, _, _)
    | [@implicit_arity] Env_modtype(summary, _, _)
    | [@implicit_arity] Env_constraints(summary, _)
    | [@implicit_arity] Env_copy_types(summary, _)
    | [@implicit_arity] Env_open(summary, _) => find_open(summary)
    };

  find_open(env.summary);
};

let add_module_signature =
    (~internal=false, mod_name, mod_: Parsetree.import_declaration, env0) => {
  let name =
    switch (mod_name) {
    | Identifier.IdentName(name) => name
    | Identifier.IdentExternal(_) => failwith("NYI mod identifer is external")
    };

  let mod_alias =
    switch (
      Option.value(~default=Location.mknoloc(mod_name), mod_.pimp_mod_alias).
        txt
    ) {
    | Identifier.IdentName(name) => name
    | Identifier.IdentExternal(_) =>
      failwith("NYI mod alias identifer is external")
    };
  let mod_ident =
    if (internal) {
      Ident.create(name);
    } else {
      Ident.create_persistent(mod_alias);
    };
  let filename = Some(mod_.pimp_path.txt);
  switch (check_opened(mod_, env0)) {
  | Some(path) when !internal =>
    let mod_type = TModAlias(path);
    add_modtype(
      mod_ident,
      {mtd_type: Some(mod_type), mtd_loc: mod_.pimp_loc},
      env0,
    )
    |> add_module(mod_ident, mod_type, filename);
  | Some(_) => env0
  | None =>
    let {ps_sig} = find_pers_struct(name, filename, mod_.pimp_loc);
    let sign = Lazy.force(ps_sig);
    let sign = Translsig.translate_signature(sign);
    let mod_type = TModSignature(sign);
    add_modtype(
      mod_ident,
      {mtd_type: Some(mod_type), mtd_loc: mod_.pimp_loc},
      env0,
    )
    |> add_module(mod_ident, mod_type, filename);
  };
};

let open_signature =
    (~filter_modules=?, ~filter_components=?, slot, root, filepath, env0) => {
  let comps = get_components(find_module_descr(root, filepath, env0));
  Some(
    add_components(
      ~filter_modules?,
      ~filter_components?,
      slot,
      root,
      env0,
      comps,
    ),
  );
};

let open_pers_signature = (name, filepath, env) =>
  switch (
    open_signature(
      None,
      PIdent(Ident.create_persistent(name)),
      filepath,
      env,
    )
  ) {
  | Some(env) => env
  | None => assert(false)
  }; /* Invalid compilation unit */

let open_signature_of_initially_opened_module =
    (~loc=Location.dummy_loc, root, env) => {
  let load_path = Grain_utils.Config.include_dirs^;
  let filter_modules = m =>
    switch (locate_module_file(~loc, load_path, m)) {
    | (_: string) => false
    | exception Not_found => true
    };

  open_signature(None, root, env, ~filter_modules);
};

let check_imports = (found, all, where) =>
  List.iter(
    comp =>
      if (List.mem(comp, found)) {
        ();
      } else {
        let {loc, txt} = comp;
        error(
          [@implicit_arity]
          Value_not_found_in_module(
            loc,
            Identifier.string_of_ident(txt),
            where,
          ),
        );
      },
    all,
  );

let open_signature =
    (
      ~used_slot=ref(false),
      ~toplevel=false,
      root,
      mod_name,
      mod_: Parsetree.import_declaration,
      env,
    ) => {
  let env = add_module_signature(~internal=true, mod_name, mod_, env);
  switch (mod_.pimp_val) {
  | PImportModule => Some(add_module_signature(mod_name, mod_, env))
  | PImportValues(values) =>
    let imported = ref([]);
    let filter_components = name => {
      let value =
        List.find_opt(
          ((val_name, _)) =>
            switch ((val_name: Parsetree.loc(Identifier.t)).txt) {
            | Identifier.IdentName(id_name) => id_name == name
            | Identifier.IdentExternal(_) => failwith("NYI")
            },
          values,
        );
      switch (value) {
      | Some((val_name, val_alias)) =>
        let new_name = Option.value(~default=val_name, val_alias);
        switch (new_name.txt) {
        | Identifier.IdentName(id_name) =>
          imported := [val_name, ...imported^];
          Some(id_name);
        | Identifier.IdentExternal(_) => failwith("NYI")
        };
      | None => None
      };
    };

    let root =
      switch (check_opened(mod_, env)) {
      | Some(path) => path
      | None => assert(false)
      };

    let new_env =
      open_signature(
        ~filter_components,
        None,
        root,
        Some(mod_.pimp_path.txt),
        env,
      );
    check_imports(
      imported^,
      List.map(((value, _)) => value, values),
      mod_.pimp_path.txt,
    );
    new_env;
  | PImportAllExcept(exceptions) =>
    let rejected = ref([]);
    let filter_components = name =>
      if (List.exists(
            id =>
              switch (id.txt) {
              | Identifier.IdentName(id_name) =>
                if (id_name == name) {
                  rejected := [id, ...rejected^];
                  true;
                } else {
                  false;
                }
              | Identifier.IdentExternal(_) => failwith("NYI")
              },
            exceptions,
          )) {
        None;
      } else {
        Some(name);
      };
    let root =
      switch (check_opened(mod_, env)) {
      | Some(path) => path
      | None => assert(false)
      };

    let new_env =
      open_signature(
        ~filter_components,
        None,
        root,
        Some(mod_.pimp_path.txt),
        env,
      );
    check_imports(rejected^, exceptions, mod_.pimp_path.txt);
    new_env;
  };
};

/* Read a signature from a file */
let read_signature = (modname, filename) => {
  let ps = read_pers_struct(modname, filename);
  Lazy.force(ps.ps_sig);
};

/* Return the CRC of the given compilation unit */
let crc_of_unit = (name, filename) => {
  let ps = find_pers_struct(~loc=Location.dummy_loc, name, filename);
  let crco =
    try(List.assoc(name, ps.ps_crcs)) {
    | Not_found => assert(false)
    };

  switch (crco) {
  | None => assert(false)
  | Some(crc) => crc
  };
};

/* Return the list of imported interfaces with their CRCs */

let imports = () =>
  Consistbl.extract(StringSet.elements(imported_units^), crc_units);

/* Returns true if [s] is an imported opaque module */
let is_imported_opaque = s => StringSet.mem(s, imported_opaque_units^);

/* Save a signature to a file */
/*
 let save_signature_with_imports ~deprecated sg modname filename imports =
   (*prerr_endline filename;
   List.iter (fun (name, crc) -> prerr_endline name) imports;*)
   Btype.cleanup_abbrev ();
   Subst.reset_for_saving ();
   let sg = Subst.signature (Subst.for_saving Subst.identity) sg in
   let flags =
     List.concat [
       if !Grain_utils.Config.recursive_types then [Cmi_format.Rectypes] else [];
       (*if !Grain_utils.Config.opaque then [Cmi_format.Opaque] else [];*)
     ]
   in
   try
     let cmi = {
       cmi_name = modname;
       cmi_sign = sg;
       cmi_crcs = imports;
       cmi_flags = flags;
     } in
     let crc =
       output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
     (* Enter signature in persistent table so that imported_unit()
        will also return its crc *)
     let comps =
       components_of_module ~deprecated ~loc:Location.dummy_loc
         empty Subst.identity
         (PIdent(Ident.create_persistent modname)) (TModSignature sg) in
     let ps =
       { ps_name = modname;
         ps_sig = lazy (Subst.signature Subst.identity sg);
         ps_comps = comps;
         ps_crcs = (cmi.cmi_name, Some crc) :: imports;
         ps_filename = filename;
         ps_flags = cmi.cmi_flags;
       } in
     save_pers_struct crc ps;
     cmi
   with exn ->
     remove_file filename;
     raise exn

 let save_signature ~deprecated sg modname filename =
   save_signature_with_imports ~deprecated sg modname filename (imports())
 */

/* Build a module signature */
let build_signature_with_imports =
    (~deprecated=?, sg, modname, filename, imports) => {
  /*prerr_endline filename;
    List.iter (fun (name, crc) -> prerr_endline name) imports;*/
  Btype.cleanup_abbrev();
  Subst.reset_for_saving();
  let sg = Subst.signature(Subst.for_saving(Subst.identity), sg);
  let flags =
    List.concat([
      if (Grain_utils.Config.recursive_types^) {
        [Cmi_format.Rectypes];
      } else {
        [];
      },
      /*if !Grain_utils.Config.opaque then [Cmi_format.Opaque] else [];*/
    ]);

  try({
    let cmi = {
      cmi_name: modname,
      cmi_sign: sg,
      cmi_crcs: imports,
      cmi_flags: flags,
    };
    let full_cmi =
      Cmi_format.build_full_cmi(
        ~name=modname,
        ~sign=sg,
        ~crcs=imports,
        ~flags,
      );
    let crc =
      switch (full_cmi.cmi_crcs) {
      | [(_, Some(crc)), ..._] => crc
      | _ => failwith("Impossible")
      };

    /* Enter signature in persistent table so that imported_unit()
       will also return its crc */
    let comps =
      components_of_module(
        ~deprecated,
        ~loc=Location.dummy_loc,
        empty,
        Subst.identity,
        PIdent(Ident.create_persistent(modname)),
        TModSignature(sg),
      );
    let ps = {
      ps_name: modname,
      ps_sig: lazy(Subst.signature(Subst.identity, sg)),
      ps_comps: comps,
      ps_crcs: full_cmi.cmi_crcs,
      ps_filename: filename,
      ps_flags: cmi.cmi_flags,
    };
    save_pers_struct(crc, ps);
    cmi;
  }) {
  | exn => raise(exn)
  };
};

let build_signature = (~deprecated=?, sg, modname, filename) =>
  build_signature_with_imports(
    ~deprecated?,
    sg,
    modname,
    filename,
    imports(),
  );

/* Folding on environments */

let find_all = (proj1, proj2, f, lid, env, acc) =>
  switch (lid) {
  | None =>
    IdTbl.fold_name(
      (name, (p, data), acc) => f(name, p, data, acc),
      proj1(env),
      acc,
    )
  | Some(l) =>
    let (p, desc) = lookup_module_descr(~mark=true, l, env);
    let c = get_components(desc);
    Tbl.fold(
      (s, (data, pos), acc) =>
        f(s, [@implicit_arity] PExternal(p, s, pos), data, acc),
      proj2(c),
      acc,
    );
  };

let find_all_simple_list = (proj1, proj2, f, lid, env, acc) =>
  switch (lid) {
  | None =>
    TycompTbl.fold_name((data, acc) => f(data, acc), proj1(env), acc)
  | Some(l) =>
    let (_p, desc) = lookup_module_descr(~mark=true, l, env);
    let c = get_components(desc);
    Tbl.fold(
      (_s, comps, acc) =>
        switch (comps) {
        | [] => acc
        | [data, ..._] => f(data, acc)
        },
      proj2(c),
      acc,
    );
  };

let fold_modules = (f, lid, env, acc) =>
  switch (lid) {
  | None =>
    let acc =
      IdTbl.fold_name(
        (name, (p, data), acc) => {
          let data = EnvLazy.force(subst_modtype_maker, data);
          f(name, p, data, acc);
        },
        env.modules,
        acc,
      );

    /* Hashtbl.fold
       (fun name ps acc ->
         match ps with
             None -> acc
           | Some ps ->
             f name (PIdent(Ident.create_persistent name))
                    (md (TModSignature(Lazy.force ps.ps_sig)) None) acc)
       persistent_structures */
    acc;
  | Some(l) =>
    let (p, desc) = lookup_module_descr(~mark=true, l, env);
    let c = get_components(desc);
    Tbl.fold(
      (s, (data, pos), acc) =>
        f(
          s,
          [@implicit_arity] PExternal(p, s, pos),
          EnvLazy.force(subst_modtype_maker, data),
          acc,
        ),
      c.comp_modules,
      acc,
    );
  };

let fold_values = f => find_all(env => env.values, sc => sc.comp_values, f)
and fold_constructors = f =>
  find_all_simple_list(env => env.constructors, sc => sc.comp_constrs, f)
and fold_types = f => find_all(env => env.types, sc => sc.comp_types, f)
and fold_modtypes = f =>
  find_all(env => env.modtypes, sc => sc.comp_modtypes, f);

let (initial_safe_string, initial_unsafe_string) =
  Builtin_types.build_initial_env(add_type(~check=false), empty);

/* Return the environment summary */

let summary = env =>
  if (PathMap.is_empty(env.local_constraints)) {
    env.summary;
  } else {
    [@implicit_arity] Env_constraints(env.summary, env.local_constraints);
  };

let last_env = ref(empty);
let last_reduced_env = ref(empty);

/* Error report */

open Format;

let format_dependency_chain = (ppf, depchain: dependency_chain) => {
  let print_single = ({txt, loc}) => {
    fprintf(ppf, "@,@[<2> %s at ", txt);
    if (loc == Location.dummy_loc) {
      fprintf(ppf, "<unknown>");
    } else {
      fprintf(ppf, "%a", Location.print_compact, loc);
    };
    fprintf(ppf, "@]");
  };

  fprintf(ppf, "@[<v>Dependency Chain:");
  List.iter(print_single, depchain);
  fprintf(ppf, "@]");
};

let report_error = ppf =>
  fun
  | [@implicit_arity] Illegal_renaming(modname, ps_name, filename) =>
    fprintf(
      ppf,
      "Wrong file naming: %a@ contains the compiled interface for @ %s when %s was expected",
      Location.print_filename,
      filename,
      ps_name,
      modname,
    )
  | [@implicit_arity] Inconsistent_import(name, source1, source2) =>
    fprintf(
      ppf,
      "@[<hov>The files %a@ and %a@ make inconsistent assumptions@ over interface %s@]",
      Location.print_filename,
      source1,
      Location.print_filename,
      source2,
      name,
    )
  | [@implicit_arity] Need_recursive_types(import, export) =>
    fprintf(
      ppf,
      "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]",
      export,
      import,
      "The compilation flag -rectypes is required",
    )
  | [@implicit_arity] Depend_on_unsafe_string_unit(import, export) =>
    fprintf(
      ppf,
      "@[<hov>Unit %s imports from %s, compiled with -unsafe-string.@ %s@]",
      export,
      import,
      "This compiler has been configured in strict safe-string mode (-force-safe-string)",
    )
  | [@implicit_arity] Missing_module(_, path1, path2) => {
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
  | [@implicit_arity] Illegal_value_name(_loc, name) =>
    fprintf(ppf, "'%s' is not a valid value identifier.", name)
  | [@implicit_arity] Unbound_label(_, label) =>
    fprintf(
      ppf,
      "Unbound record label %s. Perhaps you need to import its type or write a type definition?",
      label,
    )
  | [@implicit_arity] Unbound_module(_, modname) =>
    fprintf(ppf, "Unbound module %s", modname)
  | [@implicit_arity] No_module_file(m, None) =>
    fprintf(ppf, "Missing file for module %s", m)
  | [@implicit_arity] No_module_file(m, Some(msg)) =>
    fprintf(ppf, "Missing file for module %s: %s", m, msg)
  | [@implicit_arity] Value_not_found_in_module(_, name, path) =>
    fprintf(ppf, "Export '%s' was not found in '%s'", name, path)
  | [@implicit_arity] Cyclic_dependencies(dep, chain) =>
    fprintf(
      ppf,
      "@[<v>@[Found cyclic dependency: %s@]@,%a@]",
      dep,
      format_dependency_chain,
      chain,
    );

let () =
  Location.register_error_of_exn(
    fun
    | Error(
        (
          [@implicit_arity] Missing_module(loc, _, _) |
          [@implicit_arity] Illegal_value_name(loc, _) |
          [@implicit_arity] Value_not_found_in_module(loc, _, _)
        ) as err,
      )
        when loc != Location.dummy_loc =>
      Some(Location.error_of_printer(loc, report_error, err))
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );
