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

/** Consistency tables: for checking consistency of module CRCs
  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.
*/;

open Misc;

type filepath = string;

module Make:
  (
    Module_name: {
      type t;
      module Set: Set.S with type elt = t;
      module Map: Map.S with type key = t;
      module Tbl: Hashtbl.S with type key = t;
      let compare: (t, t) => int;
    },
  ) =>
   {
    type t;

    let create: unit => t;

    let clear: t => unit;

    let with_cleared: (t, unit => 'a) => 'a;

    let check: (t, Module_name.t, Digest.t, filepath) => unit;
    /* [check tbl name crc source]
       checks consistency of ([name], [crc]) with infos previously
       stored in [tbl].  If no CRC was previously associated with
       [name], record ([name], [crc]) in [tbl].
       [source] is the name of the file from which the information
       comes from.  This is used for error reporting. */

    let check_noadd: (t, Module_name.t, Digest.t, filepath) => unit;
    /* Same as [check], but raise [Not_available] if no CRC was previously
       associated with [name]. */

    let lookup_opt: (t, Module_name.t) => option((Digest.t, filepath));

    let set: (t, Module_name.t, Digest.t, filepath) => unit;
    /* [set tbl name crc source] forcefully associates [name] with
       [crc] in [tbl], even if [name] already had a different CRC
       associated with [name] in [tbl]. */

    let source: (t, Module_name.t) => filepath;
    /* [source tbl name] returns the file name associated with [name]
       if the latter has an associated CRC in [tbl].
       Raise [Not_found] otherwise. */

    let extract:
      (list(Module_name.t), t) => list((Module_name.t, option(Digest.t)));
    /* [extract tbl names] returns an associative list mapping each string
       in [names] to the CRC associated with it in [tbl]. If no CRC is
       associated with a name then it is mapped to [None]. */

    let extract_map:
      (Module_name.Set.t, t) => Module_name.Map.t(option(Digest.t));
    /* Like [extract] but with a more sophisticated type. */

    let filter: (Module_name.t => bool, t) => unit;
    /* [filter pred tbl] removes from [tbl] table all (name, CRC) pairs
       such that [pred name] is [false]. */

    exception Inconsistency(Module_name.t, filepath, filepath);
    /* Raised by [check] when a CRC mismatch is detected.
       First string is the name of the compilation unit.
       Second string is the source that caused the inconsistency.
       Third string is the source that set the CRC. */

    exception Not_available(Module_name.t);
    /* Raised by [check_noadd] when a name doesn't have an associated
       CRC. */
  };
