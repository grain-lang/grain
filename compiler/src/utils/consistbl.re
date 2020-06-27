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

  exception Inconsistency(Module_name.t, filepath, filepath);

  exception Not_available(Module_name.t);

  let check = (tbl, name, crc, source) =>
    try({
      let (old_crc, old_source) = Module_name.Tbl.find(tbl, name);
      if (crc != old_crc) {
        raise([@implicit_arity] Inconsistency(name, source, old_source));
      };
    }) {
    | Not_found => Module_name.Tbl.add(tbl, name, (crc, source))
    };

  let check_noadd = (tbl, name, crc, source) =>
    try({
      let (old_crc, old_source) = Module_name.Tbl.find(tbl, name);
      if (crc != old_crc) {
        raise([@implicit_arity] Inconsistency(name, source, old_source));
      };
    }) {
    | Not_found => raise(Not_available(name))
    };

  let set = (tbl, name, crc, source) =>
    Module_name.Tbl.add(tbl, name, (crc, source));

  let source = (tbl, name) => snd(Module_name.Tbl.find(tbl, name));

  let extract = (l, tbl) => {
    let l = List.sort_uniq(Module_name.compare, l);
    List.fold_left(
      (assc, name) =>
        try({
          let (crc, _) = Module_name.Tbl.find(tbl, name);
          [(name, Some(crc)), ...assc];
        }) {
        | Not_found => [(name, None), ...assc]
        },
      [],
      l,
    );
  };

  let extract_map = (mod_names, tbl) =>
    Module_name.Set.fold(
      (name, result) =>
        try({
          let (crc, _) = Module_name.Tbl.find(tbl, name);
          Module_name.Map.add(name, Some(crc), result);
        }) {
        | Not_found => Module_name.Map.add(name, None, result)
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
