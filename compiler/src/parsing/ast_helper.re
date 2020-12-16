/* This file is mostly copied from OCaml's parsing/ast_helper.ml.
   The original copyright notice is reproduced below. */
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

open Parsetree;

type id = loc(Identifier.t);
type str = loc(string);
type loc = Location.t;

let default_loc_src = ref(() => Location.dummy_loc);

let with_default_loc_src = (ls, f) => {
  let old = default_loc_src^;
  default_loc_src := ls;
  try({
    let r = f();
    default_loc_src := old;
    r;
  }) {
  | exn =>
    default_loc_src := old;
    raise(exn);
  };
};

let with_default_loc = l => with_default_loc_src(() => l);

let ident_empty = {txt: Identifier.IdentName("[]"), loc: default_loc_src^()};
let ident_cons = {
  txt: Identifier.IdentName("[...]"),
  loc: default_loc_src^(),
};

module Const = {
  let string = s => PConstString(s);
  let char = c => PConstChar(c);
  let number = i => PConstNumber(i);
  let int32 = i => PConstInt32(i);
  let int64 = i => PConstInt64(i);
  let float32 = f => PConstFloat32(f);
  let float64 = f => PConstFloat64(f);
  let bool = b => PConstBool(b);
  let void = PConstVoid;
};

module Typ = {
  let mk = (~loc=?, d) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {ptyp_desc: d, ptyp_loc: loc};
  };
  let any = (~loc=?, ()) => mk(~loc?, PTyAny);
  let var = (~loc=?, a) => mk(~loc?, PTyVar(a));
  let arrow = (~loc=?, a, b) => mk(~loc?, PTyArrow(a, b));
  let tuple = (~loc=?, a) => mk(~loc?, PTyTuple(a));
  let constr = (~loc=?, a, b) => mk(~loc?, PTyConstr(a, b));
  let poly = (~loc=?, a, b) => mk(~loc?, PTyPoly(a, b));

  let force_poly = t =>
    switch (t.ptyp_desc) {
    | PTyPoly(_) => t
    | _ => poly(~loc=t.ptyp_loc, [], t)
    };
};

module CDecl = {
  let mk = (~loc=?, n, a) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pcd_name: n, pcd_args: a, pcd_loc: loc};
  };
  let singleton = (~loc=?, n) => mk(~loc?, n, PConstrSingleton);
  let tuple = (~loc=?, n, a) => mk(~loc?, n, PConstrTuple(a));
};

module LDecl = {
  let mk = (~loc=?, n, t, m) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pld_name: n, pld_type: t, pld_mutable: m, pld_loc: loc};
  };
};

module Dat = {
  let mk = (~loc=?, n, t, k) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pdata_name: n, pdata_params: t, pdata_kind: k, pdata_loc: loc};
  };
  let variant = (~loc=?, n, t, cdl) => mk(~loc?, n, t, PDataVariant(cdl));
  let record = (~loc=?, n, t, ldl) => mk(~loc?, n, t, PDataRecord(ldl));
};

module Except = {
  let mk = (~loc=?, n, t) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    let ext = {pext_name: n, pext_kind: PExtDecl(t), pext_loc: loc};
    {ptyexn_constructor: ext, ptyexn_loc: loc};
  };
  let singleton = (~loc=?, n) => mk(~loc?, n, PConstrSingleton);
  let tuple = (~loc=?, n, args) => mk(~loc?, n, PConstrTuple(args));
};

module Pat = {
  let mk = (~loc=?, d) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {ppat_desc: d, ppat_loc: loc};
  };
  let any = (~loc=?, ()) => mk(~loc?, PPatAny);
  let var = (~loc=?, a) => mk(~loc?, PPatVar(a));
  let tuple = (~loc=?, a) => mk(~loc?, PPatTuple(a));
  let record = (~loc=?, a) => {
    let (patterns, closed) =
      List.fold_right(
        ((pat_opt, closed), (pats, closed_acc)) =>
          (
            Option.fold(~some=pat => [pat, ...pats], ~none=pats, pat_opt),
            if (closed_acc == Asttypes.Open) {
              Asttypes.Open;
            } else {
              closed;
            },
          ),
        a,
        ([], Asttypes.Closed),
      );
    mk(~loc?, PPatRecord(patterns, closed));
  };
  let constant = (~loc=?, a) => mk(~loc?, PPatConstant(a));
  let constraint_ = (~loc=?, a, b) => mk(~loc?, PPatConstraint(a, b));
  let construct = (~loc=?, a, b) => mk(~loc?, PPatConstruct(a, b));
  let list = (~loc=?, a, r) => {
    let base = Option.value(~default=construct(ident_empty, []), r);
    List.fold_right(
      (pat, acc) => construct(ident_cons, [pat, acc]),
      a,
      base,
    );
  };
  let or_ = (~loc=?, a, b) => mk(~loc?, PPatOr(a, b));
  let alias = (~loc=?, a, b) => mk(~loc?, PPatAlias(a, b));
};

module Exp = {
  let mk = (~loc=?, d) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pexp_desc: d, pexp_loc: loc};
  };
  let ident = (~loc=?, a) => mk(~loc?, PExpId(a));
  let constant = (~loc=?, a) => mk(~loc?, PExpConstant(a));
  let tuple = (~loc=?, a) => mk(~loc?, PExpTuple(a));
  let record = (~loc=?, a) => mk(~loc?, PExpRecord(a));
  let record_get = (~loc=?, a, b) => mk(~loc?, PExpRecordGet(a, b));
  let record_set = (~loc=?, a, b, c) => mk(~loc?, PExpRecordSet(a, b, c));
  let array = (~loc=?, a) => mk(~loc?, PExpArray(a));
  let array_get = (~loc=?, a, b) => mk(~loc?, PExpArrayGet(a, b));
  let array_set = (~loc=?, a, b, c) => mk(~loc?, PExpArraySet(a, b, c));
  let let_ = (~loc=?, a, b, c, d) => mk(~loc?, PExpLet(a, b, c, d));
  let match = (~loc=?, a, b) => mk(~loc?, PExpMatch(a, b));
  let prim1 = (~loc=?, a, b) => mk(~loc?, PExpPrim1(a, b));
  let prim2 = (~loc=?, a, b, c) => mk(~loc?, PExpPrim2(a, b, c));
  let if_ = (~loc=?, a, b, c) => mk(~loc?, PExpIf(a, b, c));
  let while_ = (~loc=?, a, b) => mk(~loc?, PExpWhile(a, b));
  let constraint_ = (~loc=?, a, b) => mk(~loc?, PExpConstraint(a, b));
  let box_assign = (~loc=?, a, b) => mk(~loc?, PExpBoxAssign(a, b));
  let assign = (~loc=?, a, b) => mk(~loc?, PExpAssign(a, b));
  let lambda = (~loc=?, a, b) => mk(~loc?, PExpLambda(a, b));
  let apply = (~loc=?, a, b) => mk(~loc?, PExpApp(a, b));
  let block = (~loc=?, a) => mk(~loc?, PExpBlock(a));
  let list = (~loc=?, a, base) => {
    let empty = ident(~loc?, ident_empty);
    let cons = ident(ident_cons);
    let base = Option.value(~default=empty, base);
    List.fold_right((expr, acc) => apply(cons, [expr, acc]), a, base);
  };
  let null = (~loc=?, ()) => mk(~loc?, PExpNull);

  let ignore = e =>
    switch (e.pexp_desc) {
    | PExpLet(_) => e
    | _ => prim1(~loc=e.pexp_loc, Ignore, e)
    };
};

module Top = {
  let mk = (~loc=?, d) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {ptop_desc: d, ptop_loc: loc};
  };
  let import = (~loc=?, i) => mk(~loc?, PTopImport(i));
  let foreign = (~loc=?, e, d) => mk(~loc?, PTopForeign(e, d));
  let primitive = (~loc=?, e, d) => mk(~loc?, PTopPrimitive(e, d));
  let data = (~loc=?, e, d) => mk(~loc?, PTopData(e, d));
  let let_ = (~loc=?, e, r, m, vb) => mk(~loc?, PTopLet(e, r, m, vb));
  let expr = (~loc=?, e) => mk(~loc?, PTopExpr(e));
  let grain_exception = (~loc=?, e, ext) =>
    mk(~loc?, PTopException(e, ext));
  let export = (~loc=?, e) => mk(~loc?, PTopExport(e));
  let export_all = (~loc=?, e) => mk(~loc?, PTopExportAll(e));
};

module Val = {
  let mk = (~loc=?, ~mod_, ~name, ~alias, ~typ, ~prim) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {
      pval_mod: mod_,
      pval_name: name,
      pval_name_alias: alias,
      pval_type: typ,
      pval_prim: prim,
      pval_loc: loc,
    };
  };
};

module Vb = {
  let mk = (~loc=?, p, e) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pvb_pat: p, pvb_expr: e, pvb_loc: loc};
  };
};

module Mb = {
  let mk = (~loc=?, p, e, g) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pmb_pat: p, pmb_body: e, pmb_guard: g, pmb_loc: loc};
  };
};

module Imp = {
  let mk = (~loc=?, shapes, path) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    {pimp_val: shapes, pimp_path: path, pimp_loc: loc};
  };
};

module Ex = {
  let mk = (~loc=?, exports) => {
    let loc =
      switch (loc) {
      | None => default_loc_src^()
      | Some(l) => l
      };
    List.map(
      ((name, alias)) => {
        let desc = {pex_name: name, pex_alias: alias, pex_loc: loc};
        let r = Str.regexp("^[A-Z]");
        if (Str.string_match(r, name.txt, 0)) {
          ExportData(desc);
        } else {
          ExportValue(desc);
        };
      },
      exports,
    );
  };
};
