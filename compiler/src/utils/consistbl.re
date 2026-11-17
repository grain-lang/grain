/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Consistency tables: for checking consistency of module CRCs */

open Misc;

type filepath = string;

module Make =
       (
         Module_name: {
           type t;
           module Set: Set.S with type elt = t;
           module Map: Map.S with type key = t;
           module Tbl: Hashtbl.S with type key = t;
           let compare: (t, t) => int;
         },
       ) => {
  type t = Module_name.Tbl.t((Digest.t, filepath));

  let create = () => Module_name.Tbl.create(13);

  let clear = Module_name.Tbl.clear;

  let with_cleared = (tbl, thunk) => {
    let orig_contents = Module_name.Tbl.to_seq(tbl);
    Module_name.Tbl.clear(tbl);
    try({
      let res = thunk();
      Module_name.Tbl.clear(tbl);
      Module_name.Tbl.add_seq(tbl, orig_contents);
      res;
    }) {
    | e =>
      Module_name.Tbl.clear(tbl);
      Module_name.Tbl.add_seq(tbl, orig_contents);
      raise(e);
    };
  };

  exception Inconsistency(Module_name.t, filepath, filepath);

  exception Not_available(Module_name.t);

  let check = (tbl, name, crc, source) =>
    switch (Module_name.Tbl.find_opt(tbl, name)) {
    | Some((old_crc, old_source)) =>
      if (crc != old_crc) {
        raise(Inconsistency(name, source, old_source));
      }
    | None => Module_name.Tbl.add(tbl, name, (crc, source))
    };

  let check_noadd = (tbl, name, crc, source) =>
    switch (Module_name.Tbl.find_opt(tbl, name)) {
    | Some((old_crc, old_source)) =>
      if (crc != old_crc) {
        raise(Inconsistency(name, source, old_source));
      }
    | None => raise(Not_available(name))
    };

  let lookup_opt = (tbl, name) => {
    Module_name.Tbl.find_opt(tbl, name);
  };

  let set = (tbl, name, crc, source) =>
    Module_name.Tbl.add(tbl, name, (crc, source));

  let source = (tbl, name) =>
    switch (Module_name.Tbl.find_opt(tbl, name)) {
    | Some((_, filepath)) => Some(filepath)
    | None => None
    };

  let extract = (l, tbl) => {
    let l = List.sort_uniq(Module_name.compare, l);
    List.fold_left(
      (assc, name) =>
        switch (Module_name.Tbl.find_opt(tbl, name)) {
        | Some((crc, _)) => [(name, Some(crc)), ...assc]
        | None => [(name, None), ...assc]
        },
      [],
      l,
    );
  };

  let extract_map = (mod_names, tbl) =>
    Module_name.Set.fold(
      (name, result) =>
        switch (Module_name.Tbl.find_opt(tbl, name)) {
        | Some((crc, _)) => Module_name.Map.add(name, Some(crc), result)
        | None => Module_name.Map.add(name, None, result)
        },
      mod_names,
      Module_name.Map.empty,
    );

  let filter = (p, tbl) => {
    let to_remove = ref([]);
    Module_name.Tbl.iter(
      (name, _) =>
        if (!p(name)) {
          to_remove := [name, ...to_remove^];
        },
      tbl,
    );
    List.iter(
      name =>
        while (Module_name.Tbl.mem(tbl, name)) {
          Module_name.Tbl.remove(tbl, name);
        },
      to_remove^,
    );
  };
};
