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
  | Depend_on_unsafe_string_unit(string, string)
  | Missing_module(Location.t, Path.t, Path.t)
  | Unbound_module(Location.t, string)
  | Unbound_label(Location.t, string)
  | Unbound_label_with_alt(Location.t, string, string)
  | No_module_file(string, option(string))
  | Value_not_found_in_module(Location.t, string, string)
  | Module_not_found_in_module(Location.t, string, string, option(string))
  | Type_not_found_in_module(Location.t, string, string)
  | Exception_not_found_in_module(Location.t, string, string)
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
        log := Cons(x, e, log^);
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
      | Cons(x, e, rest) => {
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
  | Env_extension(summary, Ident.t, extension_constructor)
  | Env_module(summary, Ident.t, module_declaration)
  | Env_modtype(summary, Ident.t, modtype_declaration)
  | Env_open(summary, Path.t)
  | Env_constraints(summary, PathMap.t(type_declaration));

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
    /** Aliased names in the module. */
    aliases: Tbl.t(string, string),
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

  let add_open = (slot, wrap, root, components, aliases, next) => {
    let using =
      switch (slot) {
      | None => None
      | Some(f) => Some((s, x) => f(s, wrap(x)))
      };

    {
      current: Ident.empty,
      opened: Some({using, root, components, aliases, next}),
    };
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
      | Some({using, root, next, components, aliases}) =>
        try({
          let (descr, pos) = Tbl.find(name, components);
          let aliased_name =
            try(Tbl.find(name, aliases)) {
            | Not_found => name
            };
          let res = (PExternal(root, aliased_name), descr);
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
      | Some({root, using, next, components, aliases}) =>
        try({
          let (desc, pos) = Tbl.find(name, components);
          let new_desc = f(desc);
          let components = Tbl.add(name, (new_desc, pos), components);
          {...tbl, opened: Some({root, using, next, components, aliases})};
        }) {
        | Not_found =>
          let next = update(name, f, next);
          {...tbl, opened: Some({root, using, next, components, aliases})};
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
          [(PExternal(root, name), desc), ...find_all(name, next)];
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
           (name, (desc, pos)) => f(name, (PExternal(root, name), desc)),
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
            (PExternal(root, s), x),
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
      protect_refs([R(can_load_modules, Cannot_load_modules(log))], () =>
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

let md = (md_type, md_filepath, md_loc) => {md_type, md_filepath, md_loc};

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

let current_unit = ref(("", "", Grain_utils.Config.Normal));

let set_unit = unit => current_unit := unit;

let get_unit = () => current_unit^;

let is_runtime_mode = () => {
  switch (current_unit^) {
  | (_, _, Runtime) => true
  | (_, _, Normal) => false
  };
};

/* Persistent structure descriptions */

type pers_flags = Cmi_format.pers_flags = | Opaque | Unsafe_string;

type pers_struct = {
  ps_name: string,
  ps_sig: Lazy.t(signature),
  ps_comps: module_components,
  ps_crcs: list((string, Digest.t)),
  ps_crc: Digest.t,
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

let add_import = s => {
  imported_units := StringSet.add(s, imported_units^);
};

let imported_opaque_units = ref(StringSet.empty);

let add_imported_opaque = s =>
  imported_opaque_units := StringSet.add(s, imported_opaque_units^);

let with_cleared_imports = thunk => {
  let old_imported_units = imported_units^;
  let old_opaque_units = imported_opaque_units^;
  imported_units := StringSet.empty;
  imported_opaque_units := StringSet.empty;
  let ret = thunk();
  imported_units := old_imported_units;
  imported_opaque_units := old_opaque_units;
  ret;
};

let clear_imports = () => {
  Consistbl.clear(crc_units);
  Hashtbl.clear(persistent_structures);
  imported_units := StringSet.empty;
  imported_opaque_units := StringSet.empty;
};

let check_consistency = ps =>
  try(
    List.iter(
      ((name, crc)) => {
        let resolved_file_name =
          Module_resolution.locate_unit_object_file(
            ~base_dir=Filepath.String.dirname(ps.ps_filename),
            name,
          );
        Consistbl.check(crc_units, resolved_file_name, crc, ps.ps_filename);
      },
      ps.ps_crcs,
    )
  ) {
  | Consistbl.Inconsistency(name, source, auth) =>
    error(Inconsistent_import(name, auth, source))
  };

/* Reading persistent structures from .cmi files */

let save_pers_struct = ps => {
  Hashtbl.add(persistent_structures, ps.ps_filename, Some(ps));
  List.iter(
    fun
    | Unsafe_string => ()
    | Opaque => add_imported_opaque(ps.ps_filename),
    ps.ps_flags,
  );
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
      Cyclic_dependencies(unit_name, get_dependency_chain(~loc, unit_name)),
    );
  };
  let (stored_name, _, _) = get_unit();
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

module Persistent_signature = {
  type t = {
    filename: string,
    cmi: Cmi_format.cmi_infos,
  };

  let load =
    ref((~loc=Location.dummy_loc, unit_name) => {
      switch (Module_resolution.locate_module_file(~loc, unit_name)) {
      | filename =>
        let ret = {filename, cmi: Module_resolution.read_file_cmi(filename)};
        Some(ret);
      | exception Not_found => None
      }
    });
};

let acknowledge_pers_struct = (check, {Persistent_signature.filename, cmi}) => {
  let name = cmi.cmi_name;
  let sign = cmi.cmi_sign;
  let crcs = cmi.cmi_crcs;
  let crc = cmi.cmi_crc;
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
    ps_crc: crc,
    ps_filename: filename,
    ps_flags: flags,
  };

  List.iter(
    fun
    | Unsafe_string =>
      if (Config.safe_string^) {
        let (unit_name, _, _) = get_unit();
        error(Depend_on_unsafe_string_unit(ps.ps_name, unit_name));
      }
    | Opaque => add_imported_opaque(filename),
    ps.ps_flags,
  );
  if (check) {
    check_consistency(ps);
  };
  Hashtbl.add(persistent_structures, filename, Some(ps));
  ps;
};

let find_pers_struct = (~loc, check, filepath) => {
  switch (Hashtbl.find(persistent_structures, filepath)) {
  | Some(ps) => ps
  | None => raise(Not_found)
  | exception Not_found =>
    switch (can_load_modules^) {
    | Cannot_load_modules(_) => raise(Not_found)
    | Can_load_modules =>
      let ps = {
        switch (Persistent_signature.load^(~loc, filepath)) {
        | Some(ps) => ps
        | None =>
          Hashtbl.add(persistent_structures, filepath, None);
          raise(Not_found);
        };
      };

      add_import(filepath);
      acknowledge_pers_struct(check, ps);
    }
  };
};

let load_pers_struct = (~loc, filepath) => {
  find_pers_struct(~loc, false, filepath).ps_name;
};

/* Emits a warning if there is no valid cmi for name */
let check_pers_struct = (~loc, name, filename) =>
  try(ignore(find_pers_struct(~loc, false, filename))) {
  | Not_found =>
    let err = No_module_file(name, None);
    error(err);
  | Cmi_format.Error(err) =>
    let msg = Format.asprintf("%a", Cmi_format.report_error, err);
    let err = No_module_file(name, Some(msg));
    error(err);
  | Error(err) =>
    let msg =
      switch (err) {
      | Illegal_renaming(name, ps_name, filename) =>
        Format.asprintf(
          " %a@ contains the compiled interface for @ %s when %s was expected",
          Location.print_filename,
          filename,
          ps_name,
          name,
        )
      | Inconsistent_import(_) => assert(false)
      | Depend_on_unsafe_string_unit(name, _) =>
        Printf.sprintf("%s uses -unsafe-string", name)
      | Unbound_label(_) => assert(false)
      | Unbound_label_with_alt(_) => assert(false)
      | Unbound_module(_) => assert(false)
      | Missing_module(_) => assert(false)
      | No_module_file(_) => assert(false)
      | Value_not_found_in_module(_) => assert(false)
      | Module_not_found_in_module(_) => assert(false)
      | Type_not_found_in_module(_) => assert(false)
      | Exception_not_found_in_module(_) => assert(false)
      | Illegal_value_name(_) => assert(false)
      | Cyclic_dependencies(_) => assert(false)
      };

    let err = No_module_file(name, Some(msg));
    error(err);
  };

let find_pers_struct = filename => find_pers_struct(true, filename);

let check_pers_struct = (~loc, name, filename) =>
  if (!Hashtbl.mem(persistent_structures, filename)) {
    /* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. */
    add_import(filename);
    if (Warnings.is_active(Warnings.NoCmiFile("", None))) {
      add_delayed_check_forward^(() =>
        check_pers_struct(~loc, name, filename)
      );
    };
  };

let rec find_module_descr = (path, filename, env) => {
  switch (path) {
  | PIdent(id) =>
    try(IdTbl.find_same(id, env.components)) {
    | Not_found =>
      let (_, unit_source, _) = get_unit();
      switch (filename) {
      | Some(filename)
          when Ident.persistent(id) && !(filename == unit_source) =>
        find_pers_struct(~loc=Location.dummy_loc, filename).ps_comps
      | _ => raise(Not_found)
      };
    }
  | PExternal(m, s) =>
    let c = get_components(find_module_descr(m, filename, env));
    let (descr, _pos) = Tbl.find(s, c.comp_components);
    descr;
  };
};

let find = (proj1, proj2, path, env) =>
  switch (path) {
  | PIdent(id) => IdTbl.find_same(id, proj1(env))
  | PExternal(m, n) =>
    let c = get_components(find_module_descr(m, None, env));
    let (data, _pos) = Tbl.find(n, proj2(c));
    data;
  };

let find_tycomp = (proj1, proj2, path, env) =>
  switch (path) {
  | PIdent(id) => TycompTbl.find_same(id, proj1(env))
  | PExternal(m, n) =>
    let c = get_components(find_module_descr(m, None, env));
    switch (Tbl.find(n, proj2(c))) {
    | [cstr, ..._] => cstr
    | _ => raise(Not_found)
    };
  };

let find_value = find(env => env.values, sc => sc.comp_values);

let find_constructor =
  find_tycomp(env => env.constructors, sc => sc.comp_constrs);

let find_type_data = find(env => env.types, sc => sc.comp_types)

and find_modtype = find(env => env.modtypes, sc => sc.comp_modtypes);

let type_of_cstr = path =>
  fun
  | {cstr_inlined: Some(decl), _} => {
      let labels = List.map(snd, Datarepr.labels_of_type(path, decl));
      switch (decl.type_kind) {
      | TDataRecord(_) => (decl, ([], labels))
      | _ =>
        failwith(
          "Impossible: inlined record constructor with non-record data type",
        )
      };
    }
  | _ =>
    failwith("Impossible: Env.type_of_cstr called on non-record constructor");

let find_extension_full = (path, env) => {
  switch (path) {
  | PIdent(id) => TycompTbl.find_same(id, env.constructors)
  | PExternal(p, s) =>
    let comps = get_components(find_module_descr(p, None, env));
    let cstrs = Tbl.find(s, comps.comp_constrs);
    List.find(
      cstr =>
        switch (cstr.cstr_tag) {
        | CstrExtension(_) => true
        | _ => false
        },
      cstrs,
    );
  };
};

let rec find_type_full = (path, env) =>
  switch (path) {
  | PIdent(_) =>
    try((PathMap.find(path, env.local_constraints), ([], []))) {
    | Not_found => find_type_data(path, env)
    }
  | PExternal(p, name) =>
    if (name == "#extension#") {
      let cstr = find_extension_full(p, env);
      type_of_cstr(path, cstr);
    } else {
      try({
        let cstr = find_cstr(p, name, env);
        type_of_cstr(path, cstr);
      }) {
      | Not_found => find_type_data(path, env)
      };
    }
  }

and find_cstr = (path, name, env) => {
  let (_, type_descriptions) = find_type_full(path, env);
  let (cstrs, _) = type_descriptions;
  List.find(cstr => cstr.cstr_name == name, cstrs);
};

let find_type = (p, env) => fst(find_type_full(p, env));

let find_type_descrs = (p, env) => snd(find_type_full(p, env));

let find_module = (path, filename, env) =>
  switch (path) {
  | PIdent(id) =>
    try({
      let data = IdTbl.find_same(id, env.modules);
      EnvLazy.force(subst_modtype_maker, data);
    }) {
    | Not_found =>
      let (_, unit_source, _) = get_unit();
      let filename = Option.value(~default=Ident.name(id), filename);
      if (Ident.persistent(id) && !(filename == unit_source)) {
        let ps = find_pers_struct(~loc=Location.dummy_loc, filename);
        md(
          TModSignature(Lazy.force(ps.ps_sig)),
          Some(filename),
          Location.dummy_loc,
        );
      } else {
        raise(Not_found);
      };
    }
  | PExternal(m, n) =>
    let c = get_components(find_module_descr(m, filename, env));
    let (data, _pos) = Tbl.find(n, c.comp_modules);
    EnvLazy.force(subst_modtype_maker, data);
  };

let find_module_chain = (path, env) => {
  let rec find = (path, env) => {
    switch (path) {
    | PIdent(id) =>
      let data = IdTbl.find_same(id, env.modules);
      (
        [EnvLazy.force(subst_modtype_maker, data)],
        IdTbl.find_same(id, env.components),
      );
    | PExternal(m, s) =>
      let (data, components) = find(m, env);
      let c = get_components(components);
      let (decl, _pos) = Tbl.find(s, c.comp_modules);
      let (components, _pos) = Tbl.find(s, c.comp_components);
      ([EnvLazy.force(subst_modtype_maker, decl), ...data], components);
    };
  };
  let (data, _) = find(path, env);
  data;
};

let rec normalize_path = (lax, env, path) =>
  switch (path) {
  | PIdent(id) when lax && Ident.persistent(id) => path
  | PExternal(p, s) =>
    let p' = normalize_path(lax, env, p);
    if (p == p') {
      expand_path(lax, env, path);
    } else {
      expand_path(lax, env, PExternal(p', s));
    };
  | PIdent(_) => expand_path(lax, env, path)
  }
and expand_path = (lax, env, path) =>
  try(
    switch (find_module(path, None, env)) {
    | {md_type: TModAlias(path1)} => normalize_path(lax, env, path1)
    | _ => path
    }
  ) {
  | Not_found
      when
        lax
        || (
          switch (path) {
          | PIdent(id) => !Ident.persistent(id)
          | _ => true
          }
        ) => path
  };

let normalize_path = (oloc, env, path) =>
  try(normalize_path(oloc == None, env, path)) {
  | Not_found =>
    switch (oloc) {
    | None => assert(false)
    | Some(loc) =>
      raise(
        Error(Missing_module(loc, path, normalize_path(true, env, path))),
      )
    }
  };

let normalize_path_prefix = (oloc, env, path) =>
  switch (path) {
  | PExternal(p, s) => PExternal(normalize_path(oloc, env, p), s)
  | PIdent(_) => path
  };
/*| PApply _ -> assert false*/

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

/* Currently a no-op */
let mark_value_used = (env, name, loc) => (); /*Printf.eprintf "Marking value %s used\n" name*/
let mark_type_used = (env, name, loc) => ();
let mark_module_used = (env, name, loc) => ();
let mark_extension_used = (env, ext, loc) => ();

let rec lookup_module_descr_aux = (~mark, id, env) =>
  Identifier.(
    switch (id) {
    | IdentName({txt: s}) => IdTbl.find_name(~mark, s, env.components)
    | IdentExternal(m, {txt: n}) =>
      let (p, descr) = lookup_module_descr(~mark, m, env);
      let (descr, pos) = Tbl.find(n, get_components(descr).comp_components);
      (PExternal(p, n), descr);
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
  | Identifier.IdentName({txt: s}) =>
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
      let (_, unit_source, _) = get_unit();
      if (Option.value(~default=s, filename) == unit_source) {
        raise(Not_found);
      };
      let p = PIdent(Ident.create_persistent(s));
      let loc = Option.value(~default=Location.dummy_loc, loc);
      // !Grain_utils.Config.transparent_modules &&
      if (!load) {
        raise(Not_found);
      } else {
        ignore(find_pers_struct(~loc, Option.get(filename)));
      };
      p;
    }
  | Identifier.IdentExternal(l, {txt: s}) =>
    let (p, descr) = lookup_module_descr(~mark, l, env);
    let c = get_components(descr);
    let (comps, _) = Tbl.find(s, c.comp_components);
    if (mark) {
      mark_module_used(env, s, comps.loc);
    };
    let p = PExternal(p, s);
    p;
  };

let lookup_idtbl = (~mark, proj1, proj2, id, env) =>
  Identifier.(
    switch (id) {
    | IdentName({txt: s}) => IdTbl.find_name(~mark, s, proj1(env))
    | IdentExternal(m, {txt: n}) =>
      let (p, desc) = lookup_module_descr(~mark, m, env);
      let (data, pos) = Tbl.find(n, proj2(get_components(desc)));
      (PExternal(p, n), data);
    }
  );

let lookup_tycomptbl = (~mark, proj1, proj2, id, env) =>
  Identifier.(
    switch (id) {
    | IdentName({txt: s}) => TycompTbl.find_all(s, proj1(env))
    | IdentExternal(m, {txt: n}) =>
      let (p, desc) = lookup_module_descr(~mark, m, env);
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
  | {desc: TTyConstr(path, _, _)} => path
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
          (s, (d, n)) => f(PExternal(path, s), (PExternal(path', s), d)),
          proj2(comps),
        );
        Tbl.iter(
          (s, (c, n)) =>
            iter_components(PExternal(path, s), PExternal(path', s), c),
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
    [(PExternal(p, s), c)];
  }) {
  | Not_found => []
  };
};

let rec find_shadowed_comps = (path, env) =>
  switch (path) {
  | PIdent(id) => IdTbl.find_all(Ident.name(id), env.components)
  | PExternal(p, s) =>
    let l = find_shadowed_comps(p, env);
    let l' = List.map(find_all_comps(comps => comps.comp_components, s), l);
    List.flatten(l');
  };

let find_shadowed = (proj1, proj2, path, env) =>
  switch (path) {
  | PIdent(id) => IdTbl.find_all(Ident.name(id), proj1(env))
  | PExternal(p, s) =>
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
  | [TSigValue(id, decl), ...rem] => {
      let p = PExternal(root, Ident.name(id));
      let nextpos =
        switch (decl.val_kind) {
        | TValPrim(_) => pos
        | _ => pos + 1
        };
      let (pl, final_sub) = prefix_idents(root, nextpos, sub, rem);
      ([p, ...pl], final_sub);
    }
  | [TSigType(id, _, _), ...rem] => {
      let p = PExternal(root, Ident.name(id));
      let (pl, final_sub) =
        prefix_idents(root, pos, Subst.add_type(id, p, sub), rem);
      ([p, ...pl], final_sub);
    }
  | [TSigTypeExt(id, ec, es), ...rem] => {
      let p = PExternal(root, Ident.name(id));
      let (pl, final_sub) =
        prefix_idents(root, pos, Subst.add_type(id, p, sub), rem);
      ([p, ...pl], final_sub);
    }
  | [TSigModule(id, _, _), ...rem] => {
      let p = PExternal(root, Ident.name(id));
      let (pl, final_sub) =
        prefix_idents(root, pos + 1, Subst.add_module(id, p, sub), rem);
      ([p, ...pl], final_sub);
    }
  | [TSigModType(id, _), ...rem] => {
      let p = PExternal(root, Ident.name(id));
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
        raise(Error(Illegal_value_name(loc, name)));
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
        | TSigValue(id, decl) =>
          let decl' = Subst.value_description(sub, decl);
          let decl' = {...decl', val_fullpath: path, val_internalpath: path};
          c.comp_values =
            Tbl.add(Ident.name(id), (decl', pos^), c.comp_values);
          switch (decl.val_kind) {
          | TValPrim(_) => ()
          | _ => incr(pos)
          };
        | TSigType(id, decl, _) =>
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
                    TTyArrow(
                      List.map(arg => (Unlabeled, arg), args),
                      desc.cstr_res,
                      TComOk,
                    ),
                  )
                };
              let val_type =
                switch (desc.cstr_existentials) {
                | [] => val_type
                | existentials =>
                  Btype.newgenty(TTyPoly(val_type, existentials))
                };
              let val_repr =
                switch (desc.cstr_args) {
                | [] => ReprValue(WasmI32)
                | args =>
                  ReprFunction(
                    List.map(_ => WasmI32, args),
                    [WasmI32],
                    Direct({name: Ident.unique_name(id), closure: false}),
                  )
                };
              let get_path = name =>
                switch (path) {
                | PIdent(_) => PIdent(Ident.create(name))
                | PExternal(p, _) => PExternal(p, name)
                };
              let path = get_path(desc.cstr_name);
              let val_desc = {
                val_type,
                val_repr,
                val_internalpath: path,
                val_fullpath: path,
                val_kind: TValConstructor(desc),
                val_loc: desc.cstr_loc,
                val_mutable: false,
                val_global: true,
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
        | TSigTypeExt(id, ext, _) =>
          let ext' = Subst.extension_constructor(sub, ext);
          let desc = Datarepr.extension_descr(path, ext');
          let val_type =
            switch (desc.cstr_args) {
            | [] => desc.cstr_res
            | args =>
              Btype.newgenty(
                TTyArrow(
                  List.map(arg => (Unlabeled, arg), args),
                  desc.cstr_res,
                  TComOk,
                ),
              )
            };
          let val_type =
            switch (desc.cstr_existentials) {
            | [] => val_type
            | existentials => Btype.newgenty(TTyPoly(val_type, existentials))
            };
          let val_repr =
            switch (desc.cstr_args) {
            | [] => ReprValue(WasmI32)
            | args =>
              ReprFunction(
                List.map(_ => WasmI32, args),
                [WasmI32],
                Direct({name: Ident.unique_name(id), closure: false}),
              )
            };
          let get_path = name =>
            switch (path) {
            | PIdent(_) => PIdent(Ident.create(name))
            | PExternal(p, _) => PExternal(p, name)
            };
          let path = get_path(desc.cstr_name);
          let val_desc = {
            val_type,
            val_repr,
            val_internalpath: path,
            val_fullpath: path,
            val_kind: TValConstructor(desc),
            val_loc: desc.cstr_loc,
            val_mutable: false,
            val_global: true,
          };
          c.comp_values =
            Tbl.add(Ident.name(id), (val_desc, nopos), c.comp_values);
          c.comp_constrs = add_to_tbl(Ident.name(id), desc, c.comp_constrs);
        | TSigModule(id, md, _) =>
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
        | TSigModType(id, decl) =>
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
    summary: Env_type(env.summary, id, info),
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
    summary: Env_type(env.summary, id, info),
  }

and store_extension = (~check, id, ext, env) => {
  let cstr = Datarepr.extension_descr(PIdent(id), ext);
  {
    ...env,
    constructors: TycompTbl.add(id, cstr, env.constructors),
    summary: Env_extension(env.summary, id, ext),
  };
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
    summary: Env_module(env.summary, id, md),
  }

and store_modtype = (id, info, env) => {
  ...env,
  modtypes: IdTbl.add(id, info, env.modtypes),
  summary: Env_modtype(env.summary, id, info),
};

let store_value = (id, decl, env) => {
  ...env,
  values: IdTbl.add(id, decl, env.values),
  summary: Env_value(env.summary, id, decl),
};

let _ = {
  components_of_module' := components_of_module;
  components_of_module_maker' := components_of_module_maker;
};

let add_value = (~check=?, id, desc, env) => store_value(id, desc, env);

let add_type = (~check, id, info, env) => store_type(~check, id, info, env)

and add_extension = (~check, id, ext, env) =>
  store_extension(~check, id, ext, env);

let add_module_declaration = (~arg=false, ~check, id, md, env) => {
  store_module(~check, id, md, env);
}

and add_modtype = (id, info, env) => store_modtype(id, info, env);

let add_module = (~arg=?, id, mty, mf, mloc, env) =>
  add_module_declaration(~check=false, ~arg?, id, md(mty, mf, mloc), env);

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

let enter_module = (~arg=?, s, mty, mloc, env) => {
  let id = Ident.create(s);
  (id, enter_module_declaration(~arg?, id, md(mty, None, mloc), env));
};

/* Insertion of all components of a signature */

let add_item = (comp, env) =>
  switch (comp) {
  | TSigValue(id, decl) => add_value(id, decl, env)
  | TSigType(id, decl, _) => add_type(~check=false, id, decl, env)
  | TSigTypeExt(id, ext, _) => add_extension(~check=false, id, ext, env)
  | TSigModule(id, md, _) =>
    add_module_declaration(~check=false, id, md, env)
  | TSigModType(id, decl) => add_modtype(id, decl, env)
  };

let rec add_signature = (sg, env) =>
  switch (sg) {
  | [] => env
  | [comp, ...rem] => add_signature(rem, add_item(comp, env))
  };

/* Open a signature path */

let add_components = (slot, root, env0, ~type_aliases=Tbl.empty, comps) => {
  let add_l = (w, comps, env0) => TycompTbl.add_open(slot, w, comps, env0);

  let add = (w, comps, ~aliases=Tbl.empty, env0) =>
    IdTbl.add_open(slot, w, root, comps, aliases, env0);

  let constructors =
    add_l(x => `Constructor(x), comps.comp_constrs, env0.constructors);

  let labels = add_l(x => `Label(x), comps.comp_labels, env0.labels);

  let values = add(x => `Value(x), comps.comp_values, env0.values);

  let types =
    add(x => `Type(x), comps.comp_types, ~aliases=type_aliases, env0.types);

  let modtypes =
    add(x => `Module_type(x), comps.comp_modtypes, env0.modtypes);

  let components =
    add(x => `Component(x), comps.comp_components, env0.components);

  let modules = add(x => `Module(x), comps.comp_modules, env0.modules);

  {
    summary: Env_open(env0.summary, root),
    local_constraints: env0.local_constraints,
    constructors,
    labels,
    values,
    types,
    modtypes,
    components,
    modules,
  };
};

let same_filepath = (unit1, unit2) =>
  Module_resolution.resolve_unit(unit1)
  == Module_resolution.resolve_unit(unit2);

let check_opened = (mod_: Parsetree.include_declaration, env) => {
  let rec find_open = summary =>
    switch (summary) {
    | Env_empty => None
    | Env_module(summary, {name} as id, {md_filepath: Some(filepath)})
        when same_filepath(filepath, mod_.pinc_path.txt) =>
      Some(PIdent(id))
    | Env_module(summary, _, _)
    | Env_value(summary, _, _)
    | Env_type(summary, _, _)
    | Env_extension(summary, _, _)
    | Env_modtype(summary, _, _)
    | Env_constraints(summary, _)
    | Env_open(summary, _) => find_open(summary)
    };

  find_open(env.summary);
};

let apply_alias = (name, alias) => {
  let old_name = Identifier.string_of_ident(name.txt);
  let new_name =
    switch (alias) {
    | Some(alias) => Identifier.string_of_ident(alias.txt)
    | None => old_name
    };
  (old_name, new_name);
};

let include_module = (mod_name, mod_: Parsetree.include_declaration, env0) => {
  let name =
    switch (mod_name) {
    | Identifier.IdentName(name) => name
    | Identifier.IdentExternal(_) =>
      failwith("Impossible: external mod identifer")
    };

  let mod_ident = Ident.create_persistent(name.txt);
  let filename = Some(mod_.pinc_path.txt);
  switch (check_opened(mod_, env0)) {
  | Some(path) =>
    if (Path.same(path, PIdent(mod_ident))) {
      env0;
    } else {
      let mod_type = TModAlias(path);
      env0 |> add_module(mod_ident, mod_type, filename, mod_.pinc_loc);
    }
  | _ =>
    let {ps_sig} = find_pers_struct(~loc=mod_.pinc_loc, mod_.pinc_path.txt);
    let sign = Lazy.force(ps_sig);
    let sign = Translsig.translate_signature(sign);
    let mod_type = TModSignature(sign);
    add_modtype(
      mod_ident,
      {mtd_type: Some(mod_type), mtd_loc: mod_.pinc_loc},
      env0,
    )
    |> add_module(mod_ident, mod_type, filename, mod_.pinc_loc);
  };
};

let use_partial_signature = (root, items, env0) => {
  let comps = get_components(find_module_descr(root, None, env0));

  let new_comps = {
    comp_values: Tbl.empty,
    comp_constrs: Tbl.empty,
    comp_labels: Tbl.empty,
    comp_types: Tbl.empty,
    comp_components: Tbl.empty,
    comp_modules: Tbl.empty,
    comp_modtypes: Tbl.empty,
  };

  let type_aliases = ref(Tbl.empty);

  let items =
    List.map(
      item => {
        switch (item) {
        | Parsetree.PUseValue({name, alias, loc}) =>
          let (old_name, new_name) = apply_alias(name, alias);
          switch (Tbl.find(old_name, comps.comp_values)) {
          | exception Not_found =>
            error(
              Value_not_found_in_module(name.loc, old_name, Path.name(root)),
            )
          | (descr, pos) as d =>
            new_comps.comp_values =
              Tbl.add(new_name, d, new_comps.comp_values);
            TUseValue({name: new_name, value: descr, loc});
          };
        | PUseModule({name, alias, loc}) =>
          let (old_name, new_name) = apply_alias(name, alias);
          switch (Tbl.find(old_name, comps.comp_modules)) {
          | exception Not_found =>
            let possible_type =
              if (Tbl.mem(old_name, comps.comp_types)) {
                Some(old_name);
              } else {
                None;
              };
            error(
              Module_not_found_in_module(
                name.loc,
                old_name,
                Path.name(root),
                possible_type,
              ),
            );
          | (descr, pos) as d =>
            new_comps.comp_modules =
              Tbl.add(new_name, d, new_comps.comp_modules);
            new_comps.comp_components =
              Tbl.add(
                new_name,
                Tbl.find(old_name, comps.comp_components),
                new_comps.comp_components,
              );
            TUseModule({
              name: new_name,
              declaration: EnvLazy.force(subst_modtype_maker, descr),
              loc,
            });
          };
        | PUseType({name, alias, loc}) =>
          let (old_name, new_name) = apply_alias(name, alias);
          switch (Tbl.find(old_name, comps.comp_types)) {
          | exception Not_found =>
            error(
              Type_not_found_in_module(name.loc, old_name, Path.name(root)),
            )
          | ((decl, (constructors, labels)), _) as descr =>
            new_comps.comp_types =
              Tbl.add(new_name, descr, new_comps.comp_types);
            type_aliases := Tbl.add(new_name, old_name, type_aliases^);
            List.iter(
              ({cstr_name}) => {
                new_comps.comp_constrs =
                  Tbl.add(
                    cstr_name,
                    Tbl.find(cstr_name, comps.comp_constrs),
                    new_comps.comp_constrs,
                  )
              },
              constructors,
            );
            List.iter(
              ({lbl_name}) => {
                new_comps.comp_labels =
                  Tbl.add(
                    lbl_name,
                    Tbl.find(lbl_name, comps.comp_labels),
                    new_comps.comp_labels,
                  )
              },
              labels,
            );
            TUseType({name: new_name, declaration: decl, loc});
          };
        | PUseException({name, alias, loc}) =>
          let (old_name, new_name) = apply_alias(name, alias);
          switch (Tbl.find(old_name, comps.comp_constrs)) {
          | exception Not_found =>
            error(
              Exception_not_found_in_module(
                name.loc,
                old_name,
                Path.name(root),
              ),
            )
          | cstrs =>
            let (ext, cstr_name) =
              List.find_map(
                cstr =>
                  switch (cstr.cstr_tag) {
                  | CstrExtension(_, _, _, ext) =>
                    Some((ext, cstr.cstr_name))
                  | _ => None
                  },
                cstrs,
              )
              |> Option.get;
            new_comps.comp_constrs =
              Tbl.add(
                new_name,
                Tbl.find(cstr_name, comps.comp_constrs),
                new_comps.comp_constrs,
              );
            TUseException({name: new_name, ext, loc});
          };
        }
      },
      items,
    );

  (
    add_components(None, root, env0, new_comps, ~type_aliases=type_aliases^),
    items,
  );
};

let use_full_signature = (root, env0) => {
  let comps = get_components(find_module_descr(root, None, env0));
  add_components(None, root, env0, comps);
};

let use_full_signature_of_initially_included_module = (root, env) => {
  use_full_signature(root, env);
};

/* Return the CRC of the given compilation unit */
let crc_of_unit = filename => {
  find_pers_struct(~loc=Location.dummy_loc, filename).ps_crc;
};

/* Return the list of imported interfaces with their CRCs */

let imports = () => {
  let imported_units = StringSet.elements(imported_units^);
  let resolved_units =
    List.map(
      unit => Module_resolution.locate_unit_object_file(unit),
      imported_units,
    );
  List.map2(
    (unit, resolved_unit) => (unit, crc_of_unit(resolved_unit)),
    imported_units,
    resolved_units,
  );
};

/* Returns true if [s] is an imported opaque module */
let is_imported_opaque = s => StringSet.mem(s, imported_opaque_units^);

/* Build a module signature */
let build_signature_with_imports =
    (~deprecated=?, sg, modname, filename, imports, type_metadata) => {
  Btype.cleanup_abbrev();
  let sg =
    Subst.with_reset_state(() =>
      Subst.signature(Subst.for_cmi(Subst.identity), sg)
    );

  let flags = [];
  let crc = Cmi_format.build_crc(~name=modname, sg);

  let cmi = {
    cmi_name: modname,
    cmi_sign: sg,
    cmi_crcs: imports,
    cmi_crc: crc,
    cmi_flags: flags,
    cmi_type_metadata: type_metadata,
    cmi_config_sum: Cmi_format.config_sum(),
  };

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
    ps_crcs: cmi.cmi_crcs,
    ps_crc: cmi.cmi_crc,
    ps_filename: Module_resolution.get_output_name(filename),
    ps_flags: cmi.cmi_flags,
  };

  save_pers_struct(ps);

  cmi;
};

let build_signature = (~deprecated=?, sg, modname, filename, type_metadata) =>
  build_signature_with_imports(
    ~deprecated?,
    sg,
    modname,
    filename,
    imports(),
    type_metadata,
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
      (s, (data, pos), acc) => f(s, PExternal(p, s), data, acc),
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
          PExternal(p, s),
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
and fold_labels = f =>
  find_all_simple_list(env => env.labels, sc => sc.comp_labels, f)
and fold_types = f => find_all(env => env.types, sc => sc.comp_types, f)
and fold_modtypes = f =>
  find_all(env => env.modtypes, sc => sc.comp_modtypes, f);

let initial_env =
  Builtin_types.initial_env(
    add_type(~check=false),
    add_extension(~check=false),
    empty,
  );

/* Return the environment summary */

let summary = env =>
  if (PathMap.is_empty(env.local_constraints)) {
    env.summary;
  } else {
    Env_constraints(env.summary, env.local_constraints);
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
  | Illegal_renaming(modname, ps_name, filename) =>
    fprintf(
      ppf,
      "Wrong file naming: %a@ contains the compiled interface for @ %s when %s was expected",
      Location.print_filename,
      filename,
      ps_name,
      modname,
    )
  | Inconsistent_import(name, source1, source2) =>
    fprintf(
      ppf,
      "@[<hov>The files %a@ and %a@ make inconsistent assumptions@ over interface %s@]",
      Location.print_filename,
      source1,
      Location.print_filename,
      source2,
      name,
    )
  | Depend_on_unsafe_string_unit(import, export) =>
    fprintf(
      ppf,
      "@[<hov>Unit %s imports from %s, compiled with -unsafe-string.@ %s@]",
      export,
      import,
      "This compiler has been configured in strict safe-string mode (-force-safe-string)",
    )
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
  | Illegal_value_name(_loc, name) =>
    fprintf(ppf, "\"%s\" is not a valid value identifier.", name)
  | Unbound_label(_, label) =>
    fprintf(
      ppf,
      "Unbound record label %s. Perhaps you need to import its type or write a type definition?",
      label,
    )
  | Unbound_label_with_alt(_, label, alt) =>
    fprintf(
      ppf,
      "Unbound record label %s. However, this label exists on record constructor %s, which is incompatible with this record type.",
      label,
      alt,
    )
  | Unbound_module(_, modname) => fprintf(ppf, "Unbound module %s", modname)
  | No_module_file(m, None) =>
    fprintf(ppf, "Missing file for module \"%s\"", m)
  | No_module_file(m, Some(msg)) =>
    fprintf(ppf, "Missing file for module \"%s\": %s", m, msg)
  | Value_not_found_in_module(_, name, path) =>
    fprintf(ppf, "Unbound value %s in module %s", name, path)
  | Module_not_found_in_module(_, name, path, None) =>
    fprintf(ppf, "Unbound module %s in module %s", name, path)
  | Module_not_found_in_module(_, name, path, Some(ty)) =>
    fprintf(
      ppf,
      "Unbound module %s in module %s. Did you mean `type %s`?",
      name,
      path,
      ty,
    )
  | Type_not_found_in_module(_, name, path) =>
    fprintf(ppf, "Unbound type %s in module %s", name, path)
  | Exception_not_found_in_module(_, name, path) =>
    fprintf(ppf, "Unbound exception %s in module %s", name, path)
  | Cyclic_dependencies(dep, chain) =>
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
          Missing_module(loc, _, _) | Illegal_value_name(loc, _) |
          Value_not_found_in_module(loc, _, _) |
          Module_not_found_in_module(loc, _, _, _) |
          Type_not_found_in_module(loc, _, _) |
          Unbound_module(loc, _) |
          Unbound_label(loc, _)
        ) as err,
      )
        when loc != Location.dummy_loc =>
      Some(Location.error_of_printer(loc, report_error, err))
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );

let () = {
  Module_resolution.with_preserve_unit :=
    (
      (~loc, unit_name, srcpath, thunk) => {
        mark_in_progress(~loc, unit_name, srcpath);
        let saved_unit = get_unit();
        let saved = Ident.save_state();
        let cleanup = () => {
          Ident.restore_state(saved);
          set_unit(saved_unit);
          mark_completed(unit_name, srcpath);
        };
        try({
          let ret = with_cleared_imports(thunk);
          cleanup();
          ret;
        }) {
        | e =>
          cleanup();
          raise(e);
        };
      }
    );
  Module_resolution.current_unit_name :=
    (
      () => {
        let (uname, _, _) = get_unit();
        uname;
      }
    );
  Module_resolution.current_filename :=
    (
      () => {
        let (_, source, _) = get_unit();
        source;
      }
    );
};
