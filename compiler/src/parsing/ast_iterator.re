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
  include_: (iterator, include_declaration) => unit,
  provide: (iterator, list(provide_item)) => unit,
  value_binding: (iterator, value_binding) => unit,
  match_branch: (iterator, match_branch) => unit,
  value_description: (iterator, value_description) => unit,
  grain_exception: (iterator, type_exception) => unit,
  toplevel: (iterator, toplevel_stmt) => unit,
};

let iter_loc = (sub, {loc, txt}) => sub.location(sub, loc);
let iter_opt = (sub, opt) => Option.iter(iter_loc(sub), opt);
let iter_ident = (sub, id) => {
  open Identifier;
  iter_loc(sub, id);
  let rec iter = id => {
    switch (id) {
    | IdentName(name) => iter_loc(sub, name)
    | IdentExternal(id, name) =>
      iter(id);
      iter_loc(sub, name);
    };
  };
  iter(id.txt);
};
let iter_expressions = (sub, es) => {
  List.iter(sub.expr(sub), es);
};
let iter_patterns = (sub, ps) => {
  List.iter(sub.pat(sub), ps);
};
let iter_record_fields = (sub, es) => {
  List.iter(
    ((name, exp)) => {
      iter_loc(sub, name);
      sub.expr(sub, exp);
    },
    es,
  );
};

let iter_record_pattern = (sub, fs) => {
  List.iter(
    ((id, pat)) => {
      iter_loc(sub, id);
      sub.pat(sub, pat);
    },
    fs,
  );
};

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
    | PExpId(i) => iter_ident(sub, i)
    | PExpConstant(c) => sub.constant(sub, c)
    | PExpTuple(es) => iter_expressions(sub, es)
    | PExpArray(es) => iter_expressions(sub, es)
    | PExpArrayGet(a, i) =>
      sub.expr(sub, a);
      sub.expr(sub, i);
    | PExpArraySet(a, i, arg) =>
      sub.expr(sub, a);
      sub.expr(sub, i);
      sub.expr(sub, arg);
    | PExpRecord(b, es) =>
      Option.iter(sub.expr(sub), b);
      iter_record_fields(sub, es);
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
    | PExpPrimN(p, es) => iter_expressions(sub, es)
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
    | PExpReturn(e) => Option.iter(sub.expr(sub), e)
    | PExpConstraint(e, t) =>
      sub.expr(sub, e);
      sub.typ(sub, t);
    | PExpUse(i, u) =>
      iter_loc(sub, i);
      switch (u) {
      | PUseItems(items) =>
        List.iter(
          item => {
            switch (item) {
            | PUseType({name, alias, loc})
            | PUseModule({name, alias, loc})
            | PUseValue({name, alias, loc}) =>
              iter_ident(sub, name);
              Option.iter(iter_ident(sub), alias);
              sub.location(sub, loc);
            }
          },
          items,
        )
      | PUseAll => ()
      };
    | PExpLambda(pl, e) =>
      iter_patterns(sub, pl);
      sub.expr(sub, e);
    | PExpApp(e, el) =>
      sub.expr(sub, e);
      iter_expressions(sub, el);
    | PExpConstruct(c, e) =>
      iter_ident(sub, c);
      switch (e) {
      | PExpConstrTuple(el) => iter_expressions(sub, el)
      | PExpConstrRecord(es) => iter_record_fields(sub, es)
      };
    | PExpBlock(el) => iter_expressions(sub, el)
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
    | PPatTuple(pl) => iter_patterns(sub, pl)
    | PPatArray(pl) => iter_patterns(sub, pl)
    | PPatRecord(fs, _) => iter_record_pattern(sub, fs)
    | PPatConstant(c) => sub.constant(sub, c)
    | PPatConstraint(p, pt) =>
      sub.pat(sub, p);
      sub.typ(sub, pt);
    | PPatConstruct(id, p) =>
      iter_ident(sub, id);
      switch (p) {
      | PPatConstrTuple(pl) => iter_patterns(sub, pl)
      | PPatConstrRecord(fs, _) => iter_record_pattern(sub, fs)
      };
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
    | PConstrRecord(ldl) => List.iter(sub.label(sub), ldl)
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
          pdata_manifest: manifest,
          pdata_kind: kind,
          pdata_loc: loc,
        },
      ) => {
    sub.location(sub, loc);
    iter_loc(sub, name);
    List.iter(sub.typ(sub), args);
    Option.iter(sub.typ(sub), manifest);
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
      | PConstrRecord(ldl) => List.iter(sub.label(sub), ldl)
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
      iter_ident(sub, name);
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
  let iter = (sub, {pinc_alias, pinc_path, pinc_loc}) => {
    Option.iter(iter_loc(sub), pinc_alias);
    iter_loc(sub, pinc_path);
    sub.location(sub, pinc_loc);
  };
};

module Pr = {
  let iter = (sub, items) =>
    List.iter(
      item => {
        switch (item) {
        | PProvideType({name, alias, loc})
        | PProvideModule({name, alias, loc})
        | PProvideValue({name, alias, loc}) =>
          iter_ident(sub, name);
          Option.iter(iter_ident(sub), alias);
          sub.location(sub, loc);
        }
      },
      items,
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
    | PTopInclude(id) => sub.include_(sub, id)
    | PTopProvide(ex) => sub.provide(sub, ex)
    | PTopForeign(e, vd) => sub.value_description(sub, vd)
    | PTopPrimitive(e, vd) => sub.value_description(sub, vd)
    | PTopData(dd) => List.iter(((_, d)) => sub.data(sub, d), dd)
    | PTopLet(e, r, m, vb) => List.iter(sub.value_binding(sub), vb)
    | PTopModule(e, d) => List.iter(sub.toplevel(sub), d.pmod_stmts)
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
  include_: I.iter,
  provide: Pr.iter,
  value_binding: V.iter,
  match_branch: MB.iter,
  value_description: VD.iter,
  grain_exception: Except.iter,
  toplevel: TL.iter,
};
