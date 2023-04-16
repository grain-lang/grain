/* Stripped down version of typing/btype.ml from the OCaml compiler. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Basic operations on core types */

open Misc;
open Asttypes;
open Types;

/**** Sets, maps and hashtables of types ****/

module TypeSet = Set.Make(TypeOps);
module TypeMap = Map.Make(TypeOps);
module TypeHash = Hashtbl.Make(TypeOps);

/**** Forward declarations ****/

let print_raw =
  ref(_ => assert(false): (Format.formatter, type_expr) => unit);

/**** Type level management ****/

let generic_level = 100000000;

/* Used to mark a type during a traversal. */
let lowest_level = 0;
let pivot_level = 2 * lowest_level - 1;
/* pivot_level - lowest_level < lowest_level */

/**** Some type creators ****/

let new_id = ref(-1);

let newty2 = (level, desc) => {
  incr(new_id);
  {desc, level, id: new_id^};
};
let newgenty = desc => newty2(generic_level, desc);
let newgenvar = (~name=?, ()) => newgenty(TTyVar(name));

/**** Check some types ****/

let is_Tvar =
  fun
  | {desc: TTyVar(_)} => true
  | _ => false;
let is_Tunivar =
  fun
  | {desc: TTyUniVar(_)} => true
  | _ => false;
let is_Tconstr =
  fun
  | {desc: TTyConstr(_)} => true
  | _ => false;

let dummy_method = "*dummy method*";

/**** Definitions for backtracking ****/

type change =
  | Ctype(type_expr, type_desc)
  | Ccompress(type_expr, type_desc, type_desc)
  | Clevel(type_expr, int)
  | Cname(
      ref(option((Path.t, list(type_expr)))),
      option((Path.t, list(type_expr))),
    )
  | Ccommu(ref(commutable), commutable)
  | Cuniv(ref(option(type_expr)), option(type_expr))
  | Ctypeset(ref(TypeSet.t), TypeSet.t);

type changes =
  | Change(change, ref(changes))
  | Unchanged
  | Invalid;

let trail = Weak.create(1);

let log_change = ch =>
  switch (Weak.get(trail, 0)) {
  | None => ()
  | Some(r) =>
    let r' = ref(Unchanged);
    r := Change(ch, r');
    Weak.set(trail, 0, Some(r'));
  };

/**** Representative of a type ****/

let rec repr_link = (compress, t, d) =>
  fun
  | {desc: TTyLink(t') as d'} => repr_link(true, t, d', t')
  | t' => {
      if (compress) {
        log_change(Ccompress(t, t.desc, d));
        t.desc = d;
      };
      t';
    };

let repr = t =>
  switch (t.desc) {
  | TTyLink(t') as d => repr_link(false, t, d, t')
  | _ => t
  };

let rec commu_repr =
  fun
  | TComLink(r) when r^ != TComUnknown => commu_repr(r^)
  | c => c;

/**********************************/
/*  Utilities for type traversal  */
/**********************************/

let iter_type_expr = (f, ty) =>
  switch (ty.desc) {
  | TTyVar(_) => ()
  | TTyArrow(args, ret, _) =>
    List.iter(((_, arg)) => f(arg), args);
    f(ret);
  | TTyTuple(ts) => List.iter(f, ts)
  | TTyRecord(ts) => List.iter(((_, t)) => f(t), ts)
  | TTyConstr(_, args, _) => List.iter(f, args)
  | TTyLink(ty) => f(ty)
  | TTySubst(ty) => f(ty)
  | TTyUniVar(_) => ()
  | TTyPoly(t, l) =>
    f(t);
    List.iter(f, l);
  };

let rec iter_abbrev = f =>
  fun
  | TMemNil => ()
  | TMemCons(_, ty, ty', rem) => {
      f(ty);
      f(ty');
      iter_abbrev(f, rem);
    }
  | TMemLink(rem) => iter_abbrev(f, rem^);

type type_iterators = {
  it_signature: (type_iterators, signature) => unit,
  it_signature_item: (type_iterators, signature_item) => unit,
  it_value_description: (type_iterators, value_description) => unit,
  it_type_declaration: (type_iterators, type_declaration) => unit,
  it_extension_constructor: (type_iterators, extension_constructor) => unit,
  it_module_declaration: (type_iterators, module_declaration) => unit,
  it_modtype_declaration: (type_iterators, modtype_declaration) => unit,
  it_module_type: (type_iterators, module_type) => unit,
  it_type_kind: (type_iterators, type_kind) => unit,
  it_do_type_expr: (type_iterators, type_expr) => unit,
  it_type_expr: (type_iterators, type_expr) => unit,
  it_path: Path.t => unit,
};

let iter_type_expr_cstr_args = f =>
  fun
  | TConstrTuple(tl) => List.iter(f, tl)
  | TConstrRecord(rfs) => List.iter(rf => f(rf.rf_type), rfs)
  | TConstrSingleton => ();

let map_type_expr_cstr_args = f =>
  fun
  | TConstrSingleton => TConstrSingleton
  | TConstrTuple(tl) => TConstrTuple(List.map(f, tl))
  | TConstrRecord(rfs) =>
    TConstrRecord(List.map(rf => {...rf, rf_type: f(rf.rf_type)}, rfs));

let iter_type_expr_kind = f =>
  fun
  | TDataOpen
  | TDataAbstract => ()
  | TDataVariant(cstrs) =>
    List.iter(cd => iter_type_expr_cstr_args(f, cd.cd_args), cstrs)
  | TDataRecord(fields) => List.iter(({rf_type}) => f(rf_type), fields);

let type_iterators = {
  let it_signature = it => List.iter(it.it_signature_item(it))
  and it_signature_item = it =>
    fun
    | TSigValue(_, vd) => it.it_value_description(it, vd)
    | TSigType(_, td, _) => it.it_type_declaration(it, td)
    | TSigTypeExt(_, td, _) => it.it_extension_constructor(it, td)
    | TSigModule(_, md, _) => it.it_module_declaration(it, md)
    | TSigModType(_, mtd) => it.it_modtype_declaration(it, mtd)
  and it_value_description = (it, vd) => it.it_type_expr(it, vd.val_type)
  and it_type_declaration = (it, td) => {
    List.iter(it.it_type_expr(it), td.type_params);
    Option.iter(it.it_type_expr(it), td.type_manifest);
    it.it_type_kind(it, td.type_kind);
  }
  and it_extension_constructor = (it, td) => {
    it.it_path(td.ext_type_path);
    List.iter(it.it_type_expr(it), td.ext_type_params);
    iter_type_expr_cstr_args(it.it_type_expr(it), td.ext_args);
  }
  and it_module_type = it =>
    fun
    | TModIdent(p) => it.it_path(p)
    | TModAlias(p) => it.it_path(p)
    | TModSignature(sg) => it.it_signature(it, sg)
  and it_type_kind = (it, kind) =>
    iter_type_expr_kind(it.it_type_expr(it), kind)
  and it_module_declaration = (it, md) => it.it_module_type(it, md.md_type)
  and it_modtype_declaration = (it, mtd) =>
    Option.iter(it.it_module_type(it), mtd.mtd_type)
  and it_do_type_expr = (it, ty) => {
    iter_type_expr(it.it_type_expr(it), ty);
    switch (ty.desc) {
    | TTyConstr(p, _, _) => it.it_path(p)
    | _ => ()
    };
  }
  and it_path = _p => ();

  {
    it_signature,
    it_signature_item,
    it_path,
    it_type_expr: it_do_type_expr,
    it_do_type_expr,
    it_type_kind,
    it_module_type,
    it_type_declaration,
    it_value_description,
    it_extension_constructor,
    it_module_declaration,
    it_modtype_declaration,
  };
};

let copy_commu = c =>
  if (commu_repr(c) == TComOk) {
    TComOk;
  } else {
    TComLink(ref(TComUnknown));
  };

/* Since univars may be used as row variables, we need to do some
   encoding during substitution */
let rec norm_univar = ty =>
  switch (ty.desc) {
  | TTyLink(ty) => norm_univar(ty)
  | TTyUniVar(_)
  | TTySubst(_) => ty
  | TTyTuple([ty, ..._]) => norm_univar(ty)
  | TTyRecord(_)
  | TTyVar(_)
  | TTyArrow(_)
  | TTyTuple(_)
  | TTyConstr(_)
  | TTyPoly(_) => assert(false)
  };

let rec copy_type_desc = (~keep_names=false, f) =>
  fun
  | TTyVar(_) as ty =>
    if (keep_names) {
      ty;
    } else {
      TTyVar(None);
    }
  | TTyArrow(tyl, ret, c) =>
    TTyArrow(
      List.map(((l, arg)) => (l, f(arg)), tyl),
      f(ret),
      copy_commu(c),
    )
  | TTyTuple(l) => TTyTuple(List.map(f, l))
  | TTyRecord(l) =>
    TTyRecord(List.map(((name, arg)) => (name, f(arg)), l))
  | TTyConstr(p, l, _) => TTyConstr(p, List.map(f, l), ref(TMemNil))
  | TTyUniVar(_) as ty => ty
  | TTySubst(_) => assert(false)
  | TTyLink(ty) => copy_type_desc(f, ty.desc)
  | TTyPoly(ty, tyl) => {
      let tyl = List.map(x => norm_univar(f(x)), tyl);
      TTyPoly(f(ty), tyl);
    };

/* Utilities for copying */

let saved_desc = ref([]);
/* Saved association of generic nodes with their description. */

let save_desc = (ty, desc) => saved_desc := [(ty, desc), ...saved_desc^];

let saved_kinds = ref([]); /* duplicated kind variables */
let new_kinds = ref([]); /* new kind variables */

/* Restored type descriptions. */
let cleanup_types = () => {
  List.iter(((ty, desc)) => ty.desc = desc, saved_desc^);
  List.iter(r => r := None, saved_kinds^);
  saved_desc := [];
  saved_kinds := [];
  new_kinds := [];
};

/* Mark a type. */
let rec mark_type = ty => {
  let ty = repr(ty);
  if (ty.level >= lowest_level) {
    ty.level = pivot_level - ty.level;
    iter_type_expr(mark_type, ty);
  };
};

let mark_type_node = ty => {
  let ty = repr(ty);
  if (ty.level >= lowest_level) {
    ty.level = pivot_level - ty.level;
  };
};

let mark_type_params = ty => iter_type_expr(mark_type, ty);

let type_iterators = {
  let it_type_expr = (it, ty) => {
    let ty = repr(ty);
    if (ty.level >= lowest_level) {
      mark_type_node(ty);
      it.it_do_type_expr(it, ty);
    };
  };

  {...type_iterators, it_type_expr};
};

/* Remove marks from a type. */
let rec unmark_type = ty => {
  let ty = repr(ty);
  if (ty.level < lowest_level) {
    ty.level = pivot_level - ty.level;
    iter_type_expr(unmark_type, ty);
  };
};

let unmark_iterators = {
  let it_type_expr = (_it, ty) => unmark_type(ty);
  {...type_iterators, it_type_expr};
};

let unmark_type_decl = decl =>
  unmark_iterators.it_type_declaration(unmark_iterators, decl);

/*******************************************/
/*  Memorization of abbreviation expansion */
/*******************************************/

/* Search whether the expansion has been memorized. */

let rec find_expans = (priv, p1) =>
  fun
  | TMemNil => None
  | TMemCons(p2, _ty0, ty, _) when Path.same(p1, p2) => Some(ty)
  | TMemCons(_, _, _, rem) => find_expans(priv, p1, rem)
  | TMemLink({contents: rem}) => find_expans(priv, p1, rem);

let memo = ref([]);
/* Contains the list of saved abbreviation expansions. */

let cleanup_abbrev = () => {
  /* Remove all memorized abbreviation expansions. */
  List.iter(abbr => abbr := TMemNil, memo^);
  memo := [];
};

let memorize_abbrev = (mem, priv, path, v, v') => {
  /* Memorize the expansion of an abbreviation. */
  mem := TMemCons(path, v, v', mem^);
  /* check_expans [] v; */
  memo := [mem, ...memo^];
};

let rec forget_abbrev_rec = (mem, path) =>
  switch (mem) {
  | TMemNil => assert(false)
  | TMemCons(path', _, _, rem) when Path.same(path, path') => rem
  | TMemCons(path', v, v', rem) =>
    TMemCons(path', v, v', forget_abbrev_rec(rem, path))
  | TMemLink(mem') =>
    mem' := forget_abbrev_rec(mem'^, path);
    raise(Exit);
  };

let forget_abbrev = (mem, path) =>
  try(mem := forget_abbrev_rec(mem^, path)) {
  | Exit => ()
  };

/**********************************/
/*  Utilities for labels          */
/**********************************/

let is_optional =
  fun
  | Default(_) => true
  | _ => false;

let label_equal = (l1, l2) => {
  switch (l1, l2) {
  | (Unlabeled, Unlabeled) => true
  | (Labeled({txt: name1}), Labeled({txt: name2}))
  | (Default({txt: name1}), Default({txt: name2})) when name1 == name2 =>
    true
  | _ => false
  };
};

let same_label_name = (l1, l2) =>
  switch (l1, l2) {
  | (Unlabeled, Unlabeled) => true
  | (
      Labeled({txt: name1}) | Default({txt: name1}),
      Labeled({txt: name2}) | Default({txt: name2}),
    )
      when name1 == name2 =>
    true
  | _ => false
  };

let label_name =
  fun
  | Unlabeled => ""
  | Labeled(s)
  | Default(s) => s.txt;

let qualified_label_name =
  fun
  | Unlabeled => ""
  | Labeled(s) => s.txt
  | Default(s) => "?" ++ s.txt;

let rec extract_label_aux = (hd, l) =>
  fun
  | [] => raise(Not_found)
  | [(l', t) as p, ...ls] =>
    if (label_name(l') == l) {
      (l', t, List.rev(hd), ls);
    } else {
      extract_label_aux([p, ...hd], l, ls);
    };

let extract_label = (l, ls) => extract_label_aux([], l, ls);

/**********************************/
/*  Utilities for backtracking    */
/**********************************/

let undo_change =
  fun
  | Ctype(ty, desc) => ty.desc = desc
  | Ccompress(ty, desc, _) => ty.desc = desc
  | Clevel(ty, level) => ty.level = level
  | Cname(r, v) => r := v
  | Ccommu(r, v) => r := v
  | Cuniv(r, v) => r := v
  | Ctypeset(r, v) => r := v;

type snapshot = (ref(changes), int);
let last_snapshot = ref(0);

let log_type = ty =>
  if (ty.id <= last_snapshot^) {
    log_change(Ctype(ty, ty.desc));
  };
let link_type = (ty, ty') => {
  log_type(ty);
  let desc = ty.desc;
  ty.desc = TTyLink(ty');
  /* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). */
  switch (desc, ty'.desc) {
  | (TTyVar(name), TTyVar(name')) =>
    switch (name, name') {
    | (Some(_), None) =>
      log_type(ty');
      ty'.desc = TTyVar(name);
    | (None, Some(_)) => ()
    | (Some(_), Some(_)) =>
      if (ty.level < ty'.level) {
        log_type(ty');
        ty'.desc = TTyVar(name);
      }
    | (None, None) => ()
    }
  | _ => ()
  };
};
/* ; assert (check_memorized_abbrevs ()) */
/*  ; check_expans [] ty' */
let set_level = (ty, level) => {
  if (ty.id <= last_snapshot^) {
    log_change(Clevel(ty, ty.level));
  };
  ty.level = level;
};
let set_univar = (rty, ty) => {
  log_change(Cuniv(rty, rty^));
  rty := Some(ty);
};
let set_name = (nm, v) => {
  log_change(Cname(nm, nm^));
  nm := v;
};
let set_commu = (rc, c) => {
  log_change(Ccommu(rc, rc^));
  rc := c;
};
let set_typeset = (rs, s) => {
  log_change(Ctypeset(rs, rs^));
  rs := s;
};

let snapshot = () => {
  let old = last_snapshot^;
  last_snapshot := new_id^;
  switch (Weak.get(trail, 0)) {
  | Some(r) => (r, old)
  | None =>
    let r = ref(Unchanged);
    Weak.set(trail, 0, Some(r));
    (r, old);
  };
};

let rec rev_log = accu =>
  fun
  | Unchanged => accu
  | Invalid => assert(false)
  | Change(ch, next) => {
      let d = next^;
      next := Invalid;
      rev_log([ch, ...accu], d);
    };

let backtrack = ((changes, old)) =>
  switch (changes^) {
  | Unchanged => last_snapshot := old
  | Invalid => failwith("Btype.backtrack")
  | Change(_) as change =>
    cleanup_abbrev();
    let backlog = rev_log([], change);
    List.iter(undo_change, backlog);
    changes := Unchanged;
    last_snapshot := old;
    Weak.set(trail, 0, Some(changes));
  };

let rec rev_compress_log = (log, r) =>
  switch (r^) {
  | Unchanged
  | Invalid => log
  | Change(Ccompress(_), next) => rev_compress_log([r, ...log], next)
  | Change(_, next) => rev_compress_log(log, next)
  };

let undo_compress = ((changes, _old)) =>
  switch (changes^) {
  | Unchanged
  | Invalid => ()
  | Change(_) =>
    let log = rev_compress_log([], changes);
    List.iter(
      r =>
        switch (r^) {
        | Change(Ccompress(ty, desc, d), next) when ty.desc === d =>
          ty.desc = desc;
          r := next^;
        | _ => ()
        },
      log,
    );
  };
