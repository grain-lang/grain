/* See copyright in ast_mapper.mli */
open Parsetree;
open Ast_helper;

type mapper = {
  constant: (mapper, constant) => constant,
  expr: (mapper, expression) => expression,
  pat: (mapper, pattern) => pattern,
  typ: (mapper, parsed_type) => parsed_type,
  data: (mapper, data_declaration) => data_declaration,
  constructor: (mapper, constructor_declaration) => constructor_declaration,
  label: (mapper, label_declaration) => label_declaration,
  location: (mapper, Location.t) => Location.t,
  import: (mapper, list(import_declaration)) => list(import_declaration),
  export: (mapper, list(export_declaration)) => list(export_declaration),
  export_all: (mapper, list(export_except)) => list(export_except),
  value_binding: (mapper, value_binding) => value_binding,
  match_branch: (mapper, match_branch) => match_branch,
  value_description: (mapper, value_description) => value_description,
  toplevel: (mapper, toplevel_stmt) => toplevel_stmt,
};

let map_loc = (sub, {loc, txt}) => {loc: sub.location(sub, loc), txt};
let map_opt = (sub, loc_opt) => Option.map(map_loc(sub), loc_opt);

module Cnst = {
  let map = (sub, c) => c;
};

module E = {
  let map = (sub, {pexp_desc: desc, pexp_loc: loc}) => {
    open Exp;
    let loc = sub.location(sub, loc);
    switch (desc) {
    | PExpId(i) => ident(~loc, map_loc(sub, i))
    | PExpConstant(c) => constant(~loc, sub.constant(sub, c))
    | PExpTuple(es) => tuple(~loc, List.map(sub.expr(sub), es))
    | PExpArray(es) => array(~loc, List.map(sub.expr(sub), es))
    | [@implicit_arity] PExpArrayGet(a, i) =>
      array_get(~loc, sub.expr(sub, a), sub.expr(sub, i))
    | [@implicit_arity] PExpArraySet(a, i, arg) =>
      array_set(
        ~loc,
        sub.expr(sub, a),
        sub.expr(sub, i),
        sub.expr(sub, arg),
      )
    | PExpRecord(es) =>
      record(
        ~loc,
        List.map(
          ((name, expr)) => (map_loc(sub, name), sub.expr(sub, expr)),
          es,
        ),
      )
    | [@implicit_arity] PExpRecordGet(e, f) =>
      record_get(~loc, sub.expr(sub, e), map_loc(sub, f))
    | [@implicit_arity] PExpRecordSet(e, f, v) =>
      record_set(~loc, sub.expr(sub, e), map_loc(sub, f), sub.expr(sub, v))
    | [@implicit_arity] PExpLet(r, m, vbs, e) =>
      let_(
        ~loc,
        r,
        m,
        List.map(sub.value_binding(sub), vbs),
        sub.expr(sub, e),
      )
    | [@implicit_arity] PExpMatch(e, mbs) =>
      match(~loc, sub.expr(sub, e), List.map(sub.match_branch(sub), mbs))
    | [@implicit_arity] PExpPrim1(p1, e) =>
      prim1(~loc, p1, sub.expr(sub, e))
    | [@implicit_arity] PExpPrim2(p2, e1, e2) =>
      prim2(~loc, p2, sub.expr(sub, e1), sub.expr(sub, e2))
    | [@implicit_arity] PExpBoxAssign(e1, e2) =>
      box_assign(~loc, sub.expr(sub, e1), sub.expr(sub, e2))
    | [@implicit_arity] PExpAssign(e1, e2) =>
      assign(~loc, sub.expr(sub, e1), sub.expr(sub, e2))
    | [@implicit_arity] PExpIf(c, t, f) =>
      if_(~loc, sub.expr(sub, c), sub.expr(sub, t), sub.expr(sub, f))
    | [@implicit_arity] PExpWhile(c, e) =>
      while_(~loc, sub.expr(sub, c), sub.expr(sub, e))
    | [@implicit_arity] PExpLambda(pl, e) =>
      lambda(~loc, List.map(sub.pat(sub), pl), sub.expr(sub, e))
    | [@implicit_arity] PExpApp(e, el) =>
      apply(~loc, sub.expr(sub, e), List.map(sub.expr(sub), el))
    | PExpBlock(el) => block(~loc, List.map(sub.expr(sub), el))
    | PExpNull => null(~loc, ())
    | [@implicit_arity] PExpConstraint(e, t) =>
      constraint_(~loc, sub.expr(sub, e), sub.typ(sub, t))
    };
  };
};

module P = {
  let map = (sub, {ppat_desc: desc, ppat_loc: loc}) => {
    open Pat;
    let loc = sub.location(sub, loc);
    switch (desc) {
    | PPatAny => any(~loc, ())
    | PPatVar(sl) => var(~loc, map_loc(sub, sl))
    | PPatTuple(pl) => tuple(~loc, List.map(sub.pat(sub), pl))
    | [@implicit_arity] PPatRecord(fs, c) =>
      record(
        ~loc,
        List.map(
          ((id, pat)) =>
            (Some((map_loc(sub, id), sub.pat(sub, pat))), c),
          fs,
        ),
      )
    | PPatConstant(c) => constant(~loc, sub.constant(sub, c))
    | [@implicit_arity] PPatConstraint(p, pt) =>
      constraint_(~loc, sub.pat(sub, p), sub.typ(sub, pt))
    | [@implicit_arity] PPatConstruct(id, pl) =>
      construct(~loc, map_loc(sub, id), List.map(sub.pat(sub), pl))
    | [@implicit_arity] PPatOr(p1, p2) =>
      or_(~loc, sub.pat(sub, p1), sub.pat(sub, p2))
    | [@implicit_arity] PPatAlias(p, id) =>
      alias(~loc, sub.pat(sub, p), map_loc(sub, id))
    };
  };
};

module C = {
  let map = (sub, {pcd_name: name, pcd_args: args, pcd_loc: loc}) => {
    open CDecl;
    let loc = sub.location(sub, loc);
    let sname = map_loc(sub, name);
    switch (args) {
    | PConstrTuple(ptl) => tuple(~loc, sname, List.map(sub.typ(sub), ptl))
    | PConstrSingleton => singleton(~loc, sname)
    };
  };
};

module L = {
  let map =
      (sub, {pld_name: name, pld_type: typ, pld_mutable: mut, pld_loc: loc}) => {
    open LDecl;
    let loc = sub.location(sub, loc);
    let sname = map_loc(sub, name);
    mk(~loc, sname, sub.typ(sub, typ), mut);
  };
};

module D = {
  let map =
      (
        sub,
        {
          pdata_name: name,
          pdata_params: args,
          pdata_kind: kind,
          pdata_loc: loc,
        },
      ) => {
    open Dat;
    let loc = sub.location(sub, loc);
    let sname = map_loc(sub, name);
    let sargs = List.map(sub.typ(sub), args);
    switch (kind) {
    | PDataVariant(cdl) =>
      variant(~loc, sname, sargs, List.map(sub.constructor(sub), cdl))
    | PDataRecord(ldl) =>
      record(~loc, sname, sargs, List.map(sub.label(sub), ldl))
    };
  };
};

module T = {
  let map = (sub, {ptyp_desc: desc, ptyp_loc: loc}) => {
    open Typ;
    let loc = sub.location(sub, loc);
    switch (desc) {
    | PTyAny => any(~loc, ())
    | PTyVar(v) => var(~loc, v)
    | [@implicit_arity] PTyArrow(args, ret) =>
      arrow(~loc, List.map(sub.typ(sub), args), sub.typ(sub, ret))
    | PTyTuple(ts) => tuple(~loc, List.map(sub.typ(sub), ts))
    | [@implicit_arity] PTyConstr(name, ts) =>
      constr(~loc, map_loc(sub, name), List.map(sub.typ(sub), ts))
    | [@implicit_arity] PTyPoly(vars, t) =>
      poly(~loc, List.map(map_loc(sub), vars), sub.typ(sub, t))
    };
  };
};

module V = {
  let map = (sub, {pvb_pat: pat, pvb_expr: expr, pvb_loc: loc}) => {
    pvb_pat: sub.pat(sub, pat),
    pvb_expr: sub.expr(sub, expr),
    pvb_loc: sub.location(sub, loc),
  };
};

module MB = {
  let map = (sub, {pmb_pat: pat, pmb_body: expr, pmb_loc: loc}) => {
    pmb_pat: sub.pat(sub, pat),
    pmb_body: sub.expr(sub, expr),
    pmb_loc: sub.location(sub, loc),
  };
};

module I = {
  let map_one =
      (
        sub,
        {
          pimp_mod_alias: alias,
          pimp_path: path,
          pimp_val: ival,
          pimp_loc: loc,
        },
      ) => {
    open Imp;
    let loc = sub.location(sub, loc);
    let ival =
      switch (ival) {
      | PImportValues(values) =>
        PImportValues(
          List.map(
            ((name, alias)) => (map_loc(sub, name), map_opt(sub, alias)),
            values,
          ),
        )
      | PImportAllExcept(values) =>
        PImportAllExcept(List.map(map_loc(sub), values))
      | PImportModule => ival
      };
    {
      pimp_mod_alias: map_opt(sub, alias),
      pimp_path: map_loc(sub, path),
      pimp_val: ival,
      pimp_loc: loc,
    };
  };
  let map = (sub, imports) => List.map(map_one(sub), imports);
};

module Ex = {
  let map = (sub, exports) => {
    let process_desc = ({pex_name, pex_alias, pex_loc}) => {
      let pex_name = map_loc(sub, pex_name);
      let pex_alias =
        switch (pex_alias) {
        | Some(alias) => Some(map_loc(sub, alias))
        | None => None
        };
      let pex_loc = sub.location(sub, pex_loc);
      {pex_name, pex_alias, pex_loc};
    };
    List.map(
      export =>
        switch (export) {
        | ExportData(desc) => ExportData(process_desc(desc))
        | ExportValue(desc) => ExportValue(process_desc(desc))
        },
      exports,
    );
  };
  let map_export_all = (sub, excepts) =>
    List.map(
      except =>
        switch (except) {
        | ExportExceptData(name) => ExportExceptData(map_loc(sub, name))
        | ExportExceptValue(name) => ExportExceptValue(map_loc(sub, name))
        },
      excepts,
    );
};

module VD = {
  let map = (sub, {pval_mod: vmod, pval_name: vname, pval_loc: loc} as d) => {
    let pval_loc = sub.location(sub, loc);
    let pval_mod = map_loc(sub, vmod);
    let pval_name = map_loc(sub, vname);
    {...d, pval_name, pval_mod, pval_loc};
  };
};

module TL = {
  let map = (sub, {ptop_desc: desc, ptop_loc: loc}) => {
    open Top;
    let loc = sub.location(sub, loc);
    switch (desc) {
    | PTopImport(decls) => Top.import(~loc, sub.import(sub, decls))
    | [@implicit_arity] PTopForeign(e, d) =>
      Top.foreign(~loc, e, sub.value_description(sub, d))
    | [@implicit_arity] PTopPrimitive(e, d) =>
      Top.primitive(~loc, e, sub.value_description(sub, d))
    | [@implicit_arity] PTopData(e, dd) =>
      Top.data(~loc, e, sub.data(sub, dd))
    | [@implicit_arity] PTopLet(e, r, m, vb) =>
      Top.let_(~loc, e, r, m, List.map(sub.value_binding(sub), vb))
    | PTopExpr(e) => Top.expr(~loc, sub.expr(sub, e))
    | PTopExport(ex) => Top.export(~loc, sub.export(sub, ex))
    | PTopExportAll(ex) => Top.export_all(~loc, sub.export_all(sub, ex))
    };
  };
};

let default_mapper = {
  constant: Cnst.map,
  expr: E.map,
  pat: P.map,
  typ: T.map,
  data: D.map,
  constructor: C.map,
  label: L.map,
  location: (_, x) => x,
  import: I.map,
  export: Ex.map,
  export_all: Ex.map_export_all,
  value_binding: V.map,
  match_branch: MB.map,
  value_description: VD.map,
  toplevel: TL.map,
};
