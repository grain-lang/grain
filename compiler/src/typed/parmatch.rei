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

/** Detection of partial matches and unused match cases. */;

open Grain_parsing;
open Asttypes;
open Typedtree;
open Types;

/** aka. "Tpat_any" or "_"  */

let omega: pattern;

/** [List.init (fun _ -> omega)] */

let omegas: int => list(pattern);

/** [List.map (fun _ -> omega)] */

let omega_list: list('a) => list(pattern);

/** Keep only the "head" of a pattern: all arguments are replaced by [omega], so
    are variables. */

let normalize_pat: pattern => pattern;

/** [const_compare c1 c2] compares the actual values represented by [c1] and
    [c2], while simply using [Pervasives.compare] would compare the
    representations.

    cf. MPR#5758 */

let const_compare: (constant, constant) => int;

/** [le_pat p q]  means: forall V,  V matches q implies V matches p */

let le_pat: (pattern, pattern) => bool;

/** [le_pats (p1 .. pm) (q1 .. qn)] means: forall i <= m, [le_pat pi qi] */

let le_pats: (list(pattern), list(pattern)) => bool;

/** Exported compatibility functor, abstracted over constructor equality */

module Compat:
  (
    Constr: {
      let equal:
        (Types.constructor_description, Types.constructor_description) => bool;
    },
  ) =>
   {
    let compat: (pattern, pattern) => bool;
    let compats: (list(pattern), list(pattern)) => bool;
  };

exception Empty;

/** [lub p q] is a pattern that matches all values matched by [p] and [q].
    May raise [Empty], when [p] and [q] are not compatible. */

let lub: (pattern, pattern) => pattern;

/** [lubs [p1; ...; pn] [q1; ...; qk]], where [n < k], is
    [[lub p1 q1; ...; lub pk qk]].  */

let lubs: (list(pattern), list(pattern)) => list(pattern);

let get_mins: (('a, 'a) => bool, list('a)) => list('a);

/** Those two functions recombine one pattern and its arguments:
    For instance:
      (_,_)::p1::p2::rem -> (p1, p2)::rem
    The second one will replace mutable arguments by '_'
*/

let set_args: (pattern, list(pattern)) => list(pattern);
let set_args_erase_mutable: (pattern, list(pattern)) => list(pattern);

let pat_of_constr: (pattern, constructor_description) => pattern;
let complete_constrs:
  (pattern, list(constructor_tag)) => list(constructor_description);
let ppat_of_type:
  (Env.t, type_expr) =>
  (Parsetree.pattern, Hashtbl.t(string, constructor_description));

let pressure_variants: (Env.t, list(pattern)) => unit;
let check_partial:
  (
    (Hashtbl.t(string, constructor_description), Parsetree.pattern) =>
    option(pattern),
    Location.t,
    list(match_branch)
  ) =>
  partial;
let check_unused:
  (
    (bool, Hashtbl.t(string, constructor_description), Parsetree.pattern) =>
    option(pattern),
    list(match_branch)
  ) =>
  unit;

/* Irrefutability tests */
let irrefutable: pattern => bool;

/** An inactive pattern is a pattern, matching against which can be duplicated, erased or
    delayed without change in observable behavior of the program.  Patterns containing
    (lazy _) subpatterns or reads of mutable fields are active. */

let inactive: (~partial: partial, pattern) => bool;

/* Ambiguous bindings */
let check_ambiguous_bindings: list(match_branch) => unit;
