/* See copyright in ast_iterator.mli */
open Parsetree;

type iterator = {
  constant: (iterator, constant) => unit,
  expr: (iterator, expression) => unit,
  pat: (iterator, pattern) => unit,
  typ: (iterator, parsed_type) => unit,
  data: (iterator, data_declaration) => unit,
  constructor: (iterator, constructor_declaration) => unit,
  label: (iterator, label_declaration) => unit,
  location: (iterator, Location.t) => unit,
  import: (iterator, import_declaration) => unit,
  export: (iterator, list(export_declaration)) => unit,
  export_all: (iterator, list(export_except)) => unit,
  value_binding: (iterator, value_binding) => unit,
  match_branch: (iterator, match_branch) => unit,
  value_description: (iterator, value_description) => unit,
  grain_exception: (iterator, type_exception) => unit,
  toplevel: (iterator, toplevel_stmt) => unit,
};

let iter_loc = (sub, {loc, txt}) => sub.location(sub, loc);
let iter_opt = (sub, opt) => Option.iter(iter_loc(sub), opt);

module Cnst = {
  let iter = (sub, c) => ();
};

module E = {
  let iter = (sub, {pexp_desc: desc, pexp_attributes: attrs, pexp_loc: loc}) => {
    sub.location(sub, loc);
    List.iter(
      ((attr, args)) => {
        iter_loc(sub, attr);
        List.iter(iter_loc(sub), args);
      },
      attrs,
    );
    switch (desc) {
    | PExpId(i) => iter_loc(sub, i)
    | PExpConstant(c) => sub.constant(sub, c)
    | PExpTuple(es) => List.iter(sub.expr(sub), es)
    | PExpArray(es) => List.iter(sub.expr(sub), es)
    | PExpArrayGet(a, i) =>
      sub.expr(sub, a);
      sub.expr(sub, i);
    | PExpArraySet(a, i, arg) =>
      sub.expr(sub, a);
      sub.expr(sub, i);
      sub.expr(sub, arg);
    | PExpRecord(es) =>
      List.iter(
        ((name, exp)) => {
          iter_loc(sub, name);
          sub.expr(sub, exp);
        },
        es,
      )
    | PExpRecordGet(e, f) =>
      sub.expr(sub, e);
      iter_loc(sub, f);
    | PExpRecordSet(e, f, v) =>
      sub.expr(sub, e);
      iter_loc(sub, f);
      sub.expr(sub, v);
    | PExpLet(r, m, vbs) => List.iter(sub.value_binding(sub), vbs)
    | PExpMatch(e, mbs) =>
      sub.expr(sub, e);
      List.iter(sub.match_branch(sub), mbs);
    | PExpPrim0(p0) => ()
    | PExpPrim1(p1, e) => sub.expr(sub, e)
    | PExpPrim2(p2, e1, e2) =>
      sub.expr(sub, e1);
      sub.expr(sub, e2);
    | PExpPrimN(p, es) => List.iter(sub.expr(sub), es)
    | PExpBoxAssign(a, b) =>
      sub.expr(sub, a);
      sub.expr(sub, b);
    | PExpAssign(a, b) =>
      sub.expr(sub, a);
      sub.expr(sub, b);
    | PExpIf(c, t, f) =>
      sub.expr(sub, c);
      sub.expr(sub, t);
      sub.expr(sub, f);
    | PExpWhile(c, b) =>
      sub.expr(sub, c);
      sub.expr(sub, b);
    | PExpFor(i, c, inc, b) =>
      Option.iter(sub.expr(sub), i);
      Option.iter(sub.expr(sub), c);
      Option.iter(sub.expr(sub), inc);
      sub.expr(sub, b);
    | PExpContinue
    | PExpBreak => ()
    | PExpConstraint(e, t) =>
      sub.expr(sub, e);
      sub.typ(sub, t);
    | PExpLambda(pl, e) =>
      List.iter(sub.pat(sub), pl);
      sub.expr(sub, e);
    | PExpApp(e, el) =>
      sub.expr(sub, e);
      List.iter(sub.expr(sub), el);
    | PExpBlock(el) => List.iter(sub.expr(sub), el)
    | PExpNull => ()
    };
  };
};

module P = {
  let iter = (sub, {ppat_desc: desc, ppat_loc: loc}) => {
    sub.location(sub, loc);
    switch (desc) {
    | PPatAny => ()
    | PPatVar(sl) => iter_loc(sub, sl)
    | PPatTuple(pl) => List.iter(sub.pat(sub), pl)
    | PPatArray(pl) => List.iter(sub.pat(sub), pl)
    | PPatRecord(fs, _) =>
      List.iter(
        ((id, pat)) => {
          iter_loc(sub, id);
          sub.pat(sub, pat);
        },
        fs,
      )
    | PPatConstant(c) => sub.constant(sub, c)
    | PPatConstraint(p, pt) =>
      sub.pat(sub, p);
      sub.typ(sub, pt);
    | PPatConstruct(id, pl) =>
      iter_loc(sub, id);
      List.iter(sub.pat(sub), pl);
    | PPatOr(p1, p2) =>
      sub.pat(sub, p1);
      sub.pat(sub, p2);
    | PPatAlias(p, id) =>
      sub.pat(sub, p);
      iter_loc(sub, id);
    };
  };
};

module C = {
  let iter = (sub, {pcd_name: name, pcd_args: args, pcd_loc: loc}) => {
    sub.location(sub, loc);
    iter_loc(sub, name);
    switch (args) {
    | PConstrTuple(ptl) => List.iter(sub.typ(sub), ptl)
    | PConstrSingleton => ()
    };
  };
};

module L = {
  let iter = (sub, {pld_name: name, pld_type: typ, pld_loc: loc}) => {
    sub.location(sub, loc);
    iter_loc(sub, name);
    sub.typ(sub, typ);
  };
};

module D = {
  let iter =
      (
        sub,
        {
          pdata_name: name,
          pdata_params: args,
          pdata_kind: kind,
          pdata_loc: loc,
        },
      ) => {
    sub.location(sub, loc);
    iter_loc(sub, name);
    List.iter(sub.typ(sub), args);
    switch (kind) {
    | PDataAbstract => ()
    | PDataVariant(cdl) => List.iter(sub.constructor(sub), cdl)
    | PDataRecord(ldl) => List.iter(sub.label(sub), ldl)
    };
  };
};

module Except = {
  let iter = (sub, {ptyexn_constructor: ext, ptyexn_loc: loc}) => {
    sub.location(sub, loc);
    let {pext_name: n, pext_kind: k, pext_loc: loc} = ext;
    iter_loc(sub, n);
    sub.location(sub, loc);
    switch (k) {
    | PExtDecl(args) =>
      switch (args) {
      | PConstrTuple(ptl) => List.iter(sub.typ(sub), ptl)
      | PConstrSingleton => ()
      }
    | PExtRebind(id) => iter_loc(sub, id)
    };
  };
};

module T = {
  let iter = (sub, {ptyp_desc: desc, ptyp_loc: loc}) => {
    sub.location(sub, loc);
    switch (desc) {
    | PTyAny => ()
    | PTyVar(v) => ()
    | PTyArrow(args, ret) =>
      List.iter(sub.typ(sub), args);
      sub.typ(sub, ret);
    | PTyTuple(ts) => List.iter(sub.typ(sub), ts)
    | PTyConstr(name, ts) =>
      iter_loc(sub, name);
      List.iter(sub.typ(sub), ts);
    | PTyPoly(args, t) =>
      List.iter(iter_loc(sub), args);
      sub.typ(sub, t);
    };
  };
};

module V = {
  let iter = (sub, {pvb_pat: pat, pvb_expr: expr, pvb_loc: loc}) => {
    sub.pat(sub, pat);
    sub.expr(sub, expr);
    sub.location(sub, loc);
  };
};

module MB = {
  let iter = (sub, {pmb_pat: pat, pmb_body: expr, pmb_loc: loc}) => {
    sub.pat(sub, pat);
    sub.expr(sub, expr);
    sub.location(sub, loc);
  };
};

module I = {
  let iter_shape = (sub, ival) => {
    switch (ival) {
    | PImportValues(values) =>
      List.iter(
        ((name, alias)) => {
          iter_loc(sub, name);
          iter_opt(sub, alias);
        },
        values,
      )
    | PImportAllExcept(values) => List.iter(iter_loc(sub), values)
    | PImportModule(id) => iter_loc(sub, id)
    };
  };
  let iter = (sub, {pimp_val, pimp_path, pimp_loc}) => {
    List.iter(iter_shape(sub), pimp_val);
    iter_loc(sub, pimp_path);
    sub.location(sub, pimp_loc);
  };
};

module Ex = {
  let iter = (sub, exports) =>
    List.iter(
      export =>
        switch (export) {
        | ExportData({pex_loc: loc})
        | ExportValue({pex_loc: loc}) => sub.location(sub, loc)
        },
      exports,
    );
  let iter_export_all = (sub, excepts) =>
    List.iter(
      except =>
        switch (except) {
        | ExportExceptData(name)
        | ExportExceptValue(name) => iter_loc(sub, name)
        },
      excepts,
    );
};

module ExD = {
  let iter_export_data_all = (sub, excepts) =>
    List.iter(iter_loc(sub), excepts);
};

module VD = {
  let iter = (sub, {pval_mod: vmod, pval_name: vname, pval_loc: loc}) => {
    sub.location(sub, loc);
    iter_loc(sub, vmod);
    iter_loc(sub, vname);
  };
};

module TL = {
  let iter = (sub, {ptop_desc: desc, ptop_attributes: attrs, ptop_loc: loc}) => {
    sub.location(sub, loc);
    List.iter(
      ((attr, args)) => {
        iter_loc(sub, attr);
        List.iter(iter_loc(sub), args);
      },
      attrs,
    );
    switch (desc) {
    | PTopImport(id) => sub.import(sub, id)
    | PTopExport(ex) => sub.export(sub, ex)
    | PTopExportAll(ex) => sub.export_all(sub, ex)
    | PTopForeign(e, vd) => sub.value_description(sub, vd)
    | PTopPrimitive(e, vd) => sub.value_description(sub, vd)
    | PTopData(dd) => List.iter(((_, d)) => sub.data(sub, d), dd)
    | PTopLet(e, r, m, vb) => List.iter(sub.value_binding(sub), vb)
    | PTopExpr(e) => sub.expr(sub, e)
    | PTopException(e, d) => sub.grain_exception(sub, d)
    };
  };
};

let default_iterator = {
  constant: Cnst.iter,
  expr: E.iter,
  pat: P.iter,
  typ: T.iter,
  data: D.iter,
  constructor: C.iter,
  label: L.iter,
  location: (_, x) => (),
  import: I.iter,
  export: Ex.iter,
  export_all: Ex.iter_export_all,
  value_binding: V.iter,
  match_branch: MB.iter,
  value_description: VD.iter,
  grain_exception: Except.iter,
  toplevel: TL.iter,
};
