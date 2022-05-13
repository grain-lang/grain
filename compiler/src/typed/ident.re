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

open Sexplib.Conv;
open Format;

let disable_stamps = ref(false);
let stamps_disabled = _ => disable_stamps^;

[@deriving (sexp, yojson)]
type t = {
  [@sexp_drop_if stamps_disabled]
  stamp: int,
  name: string,
  [@default 0] [@sexp_drop_default (==)]
  mutable flags: int,
};

type saved_state = {
  currentstamp: int,
  reinit_level: int,
};

let global_flag = 1;
let predef_exn_flag = 2;

/* A stamp of 0 denotes a persistent identifier */

let currentstamp = ref(0);

let create = s => {
  incr(currentstamp);
  {name: s, stamp: currentstamp^, flags: 0};
};

let create_predef_exn = s => {
  incr(currentstamp);
  {name: s, stamp: currentstamp^, flags: predef_exn_flag};
};

let create_persistent = s => {name: s, stamp: 0, flags: global_flag};

let rename = i => {
  incr(currentstamp);
  {...i, stamp: currentstamp^};
};

let name = i => i.name;

let unique_name = i => i.name ++ "_" ++ string_of_int(i.stamp);

let unique_toplevel_name = i => i.name ++ "/" ++ string_of_int(i.stamp);

let persistent = i => i.stamp == 0;

let equal = (i1, i2) => i1.name == i2.name;

let same = (i1, i2) => i1 == i2;
/* Possibly more efficient version (with a real compiler, at least):
   if i1.stamp <> 0
   then i1.stamp = i2.stamp
   else i2.stamp = 0 && i1.name = i2.name */

let compare = (i1, i2) => Stdlib.compare(i1, i2);

let binding_time = i => i.stamp;

let current_time = () => currentstamp^;
let set_current_time = t => currentstamp := max(currentstamp^, t);

let reinit_level = ref(-1);

let save_state = () => {
  currentstamp: currentstamp^,
  reinit_level: reinit_level^,
};

let restore_state = s => {
  currentstamp := s.currentstamp;
  reinit_level := s.reinit_level;
};

let reinit = () =>
  if (reinit_level^ < 0) {
    reinit_level := currentstamp^;
  } else {
    currentstamp := reinit_level^;
  };

let setup = () => {
  // Identifiers below 1000 are used for Grain builtins
  currentstamp := 999;
  reinit_level := (-1);
};

let hide = i => {...i, stamp: (-1)};

let make_global = i => i.flags = i.flags lor global_flag;

let global = i => i.flags land global_flag != 0;

let is_predef_exn = i => i.flags land predef_exn_flag != 0;

let print = (ppf, i) =>
  switch (i.stamp) {
  | 0 => fprintf(ppf, "%s!", i.name)
  | (-1) => fprintf(ppf, "%s#", i.name)
  | n =>
    fprintf(
      ppf,
      "%s/%i%s",
      i.name,
      n,
      if (global(i)) {
        "g";
      } else {
        "";
      },
    )
  };

type tbl('a) =
  | Empty
  | Node(tbl('a), data('a), tbl('a), int)

and data('a) = {
  ident: t,
  data: 'a,
  previous: option(data('a)),
};

let empty = Empty;

/* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 */

let mknode = (l, d, r) => {
  let hl =
    switch (l) {
    | Empty => 0
    | Node(_, _, _, h) => h
    }
  and hr =
    switch (r) {
    | Empty => 0
    | Node(_, _, _, h) => h
    };

  Node(
    l,
    d,
    r,
    if (hl >= hr) {
      hl + 1;
    } else {
      hr + 1;
    },
  );
};

let balance = (l, d, r) => {
  let hl =
    switch (l) {
    | Empty => 0
    | Node(_, _, _, h) => h
    }
  and hr =
    switch (r) {
    | Empty => 0
    | Node(_, _, _, h) => h
    };
  if (hl > hr + 1) {
    switch (l) {
    | Node(ll, ld, lr, _)
        when
          (
            switch (ll) {
            | Empty => 0
            | Node(_, _, _, h) => h
            }
          )
          >= (
               switch (lr) {
               | Empty => 0
               | Node(_, _, _, h) => h
               }
             ) =>
      mknode(ll, ld, mknode(lr, d, r))
    | Node(ll, ld, Node(lrl, lrd, lrr, _), _) =>
      mknode(mknode(ll, ld, lrl), lrd, mknode(lrr, d, r))
    | _ => assert(false)
    };
  } else if (hr > hl + 1) {
    switch (r) {
    | Node(rl, rd, rr, _)
        when
          (
            switch (rr) {
            | Empty => 0
            | Node(_, _, _, h) => h
            }
          )
          >= (
               switch (rl) {
               | Empty => 0
               | Node(_, _, _, h) => h
               }
             ) =>
      mknode(mknode(l, d, rl), rd, rr)
    | Node(Node(rll, rld, rlr, _), rd, rr, _) =>
      mknode(mknode(l, d, rll), rld, mknode(rlr, rd, rr))
    | _ => assert(false)
    };
  } else {
    mknode(l, d, r);
  };
};

let rec add = (id, data) =>
  fun
  | Empty => Node(Empty, {ident: id, data, previous: None}, Empty, 1)
  | Node(l, k, r, h) => {
      let c = compare(id.name, k.ident.name);
      if (c == 0) {
        Node(l, {ident: id, data, previous: Some(k)}, r, h);
      } else if (c < 0) {
        balance(add(id, data, l), k, r);
      } else {
        balance(l, k, add(id, data, r));
      };
    };

let rec find_stamp = s =>
  fun
  | None => raise(Not_found)
  | Some(k) =>
    if (k.ident.stamp == s) {
      k.data;
    } else {
      find_stamp(s, k.previous);
    };

let find_stamp_opt = (s, o) =>
  try(Some(find_stamp(s, o))) {
  | Not_found => None
  };

let rec find_same = id =>
  fun
  | Empty => raise(Not_found)
  | Node(l, k, r, _) => {
      let c = compare(id.name, k.ident.name);
      if (c == 0) {
        if (id.stamp == k.ident.stamp) {
          k.data;
        } else {
          find_stamp(id.stamp, k.previous);
        };
      } else {
        find_same(
          id,
          if (c < 0) {
            l;
          } else {
            r;
          },
        );
      };
    };

let find_same_opt = (id, tbl) =>
  try(Some(find_same(id, tbl))) {
  | Not_found => None
  };

let rec find_name = name =>
  fun
  | Empty => raise(Not_found)
  | Node(l, k, r, _) => {
      let c = compare(name, k.ident.name);
      if (c == 0) {
        (k.ident, k.data);
      } else {
        find_name(
          name,
          if (c < 0) {
            l;
          } else {
            r;
          },
        );
      };
    };

let find_name_opt = (name, tbl) =>
  try(Some(find_name(name, tbl))) {
  | Not_found => None
  };

let rec get_all =
  fun
  | None => []
  | Some(k) => [(k.ident, k.data), ...get_all(k.previous)];

let rec find_all = name =>
  fun
  | Empty => []
  | Node(l, k, r, _) => {
      let c = compare(name, k.ident.name);
      if (c == 0) {
        [(k.ident, k.data), ...get_all(k.previous)];
      } else {
        find_all(
          name,
          if (c < 0) {
            l;
          } else {
            r;
          },
        );
      };
    };

let rec fold_aux = (f, stack, accu) =>
  fun
  | Empty =>
    switch (stack) {
    | [] => accu
    | [a, ...l] => fold_aux(f, l, accu, a)
    }
  | Node(l, k, r, _) => fold_aux(f, [l, ...stack], f(k, accu), r);

let fold_name = (f, tbl, accu) =>
  fold_aux(k => f(k.ident, k.data), [], accu, tbl);

let rec fold_data = (f, d, accu) =>
  switch (d) {
  | None => accu
  | Some(k) => f(k.ident, k.data, fold_data(f, k.previous, accu))
  };

let fold_all = (f, tbl, accu) =>
  fold_aux(k => fold_data(f, Some(k)), [], accu, tbl);

/* let keys tbl = fold_name (fun k _ accu -> k::accu) tbl [] */

let rec iter = f =>
  fun
  | Empty => ()
  | Node(l, k, r, _) => {
      iter(f, l);
      f(k.ident, k.data);
      iter(f, r);
    };

/* Idents for sharing keys */

/* They should be 'totally fresh' -> neg numbers */
let key_name = "";

let make_key_generator = () => {
  let c = ref(1);
  id => {
    let stamp = c^;
    decr(c);
    {...id, name: key_name, stamp};
  };
};

let compare = (x, y) => {
  let c = x.stamp - y.stamp;
  if (c != 0) {
    c;
  } else {
    let c = compare(x.name, y.name);
    if (c != 0) {
      c;
    } else {
      compare(x.flags, y.flags);
    };
  };
};

let output = (oc, id) => output_string(oc, unique_name(id));
let hash = i => Char.code(i.name.[0]) lxor i.stamp;

let original_equal = equal;
include Identifiable.Make({
  type nonrec t = t;
  let compare = compare;
  let output = output;
  let print = print;
  let hash = hash;
  let equal = same;
});
let equal = original_equal;
