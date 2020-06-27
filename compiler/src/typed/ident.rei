/* This file is taken from OCaml. */
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

/* Identifiers (unique names) */

[@deriving (sexp, yojson)]
type t = {
  stamp: int,
  name: string,
  mutable flags: int,
};
type saved_state;

include Identifiable.S with type t := t;
/* Notes:
      - [equal] compares identifiers by name
      - [compare x y] is 0 if [same x y] is true.
      - [compare] compares identifiers by binding location
   */

/** Whether stamps should be disabled when serializing to s-expressions */

let disable_stamps: ref(bool);

/** Whether stamps should be disabled when serializing to s-expressions */

let save_state: unit => saved_state;
let restore_state: saved_state => unit;
let create: string => t;
let create_persistent: string => t;
let create_predef_exn: string => t;
let rename: t => t;
let name: t => string;
let unique_name: t => string;
let unique_toplevel_name: t => string;
let persistent: t => bool;
let same: (t, t) => bool;
/* Compare identifiers by binding location.
   Two identifiers are the same either if they are both
   non-persistent and have been created by the same call to
   [new], or if they are both persistent and have the same
   name. */
let compare: (t, t) => int;
let hide: t => t;
/* Return an identifier with same name as the given identifier,
   but stamp different from any stamp returned by new.
   When put in a 'a tbl, this identifier can only be looked
   up by name. */

let make_global: t => unit;
let global: t => bool;
let is_predef_exn: t => bool;

let binding_time: t => int;
let current_time: unit => int;
let set_current_time: int => unit;
let reinit: unit => unit;

type tbl('a);
/* Association tables from identifiers to type 'a. */

let empty: tbl('a);
let add: (t, 'a, tbl('a)) => tbl('a);
let find_same_opt: (t, tbl('a)) => option('a);
let find_same: (t, tbl('a)) => 'a;
let find_name_opt: (string, tbl('a)) => option((t, 'a));
let find_name: (string, tbl('a)) => (t, 'a);
let find_all: (string, tbl('a)) => list((t, 'a));
let fold_name: ((t, 'a, 'b) => 'b, tbl('a), 'b) => 'b;
let fold_all: ((t, 'a, 'b) => 'b, tbl('a), 'b) => 'b;
let iter: ((t, 'a) => unit, tbl('a)) => unit;

/* Idents for sharing keys */

let make_key_generator: (unit, t) => t;
