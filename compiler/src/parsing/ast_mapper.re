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
  include_: (mapper, include_declaration) => include_declaration,
  provide: (mapper, list(provide_item)) => list(provide_item),
  value_binding: (mapper, value_binding) => value_binding,
  match_branch: (mapper, match_branch) => match_branch,
  primitive_description:
    (mapper, primitive_description) => primitive_description,
  value_description: (mapper, value_description) => value_description,
  grain_exception: (mapper, type_exception) => type_exception,
  toplevel: (mapper, toplevel_stmt) => toplevel_stmt,
};

let map_loc = (sub, {loc, txt}) => {loc: sub.location(sub, loc), txt};
let map_opt = (sub, loc_opt) => Option.map(map_loc(sub), loc_opt);
let map_identifier = (sub, {loc, txt}) => {
  open Identifier;
  let rec map_ident = id =>
    switch (id) {
    | IdentName(n) => IdentName(map_loc(sub, n))
    | IdentExternal(mod_, n) =>
      IdentExternal(map_ident(mod_), map_loc(sub, n))
    };
  {loc: sub.location(sub, loc), txt: map_ident(txt)};
};
let map_record_fields = (sub, es) => {
  List.map(
    ((name, expr)) => (map_loc(sub, name), sub.expr(sub, expr)),
    es,
  );
};

module Cnst = {
  let map = (sub, c) => c;
};

module E = {
  let map =
      (
        sub,
        {
          pexp_desc: desc,
          pexp_attributes: attrs,
          pexp_loc: loc,
          pexp_core_loc: core_loc,
        },
      ) => {
    open Expression;
    let loc = sub.location(sub, loc);
    let core_loc = sub.location(sub, core_loc);
    let attributes =
      List.map(
        ({Asttypes.attr_name, attr_args, attr_loc}) =>
          {
            Asttypes.attr_name: map_loc(sub, attr_name),
            attr_args: List.map(map_loc(sub), attr_args),
            attr_loc: sub.location(sub, attr_loc),
          },
        attrs,
      );
    switch (desc) {
    | PExpId(i) =>
      ident(~loc, ~core_loc, ~attributes, map_identifier(sub, i))
    | PExpConstant(c) =>
      constant(~loc, ~core_loc, ~attributes, sub.constant(sub, c))
    | PExpTuple(es) =>
      tuple(~loc, ~core_loc, ~attributes, List.map(sub.expr(sub), es))
    | PExpList(es) =>
      list(
        ~loc,
        ~core_loc,
        ~attributes,
        List.map(
          item => {
            switch (item) {
            | ListItem(e) => ListItem(sub.expr(sub, e))
            | ListSpread(e, loc) =>
              ListSpread(sub.expr(sub, e), sub.location(sub, loc))
            }
          },
          es,
        ),
      )
    | PExpArray(es) =>
      array(~loc, ~core_loc, ~attributes, List.map(sub.expr(sub), es))
    | PExpArrayGet(a, i) =>
      array_get(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, a),
        sub.expr(sub, i),
      )
    | PExpArraySet({lhs_loc, array, index, value, infix_op}) =>
      array_set(
        ~loc,
        ~core_loc,
        ~attributes,
        ~infix_op=?Option.map(sub.expr(sub), infix_op),
        ~lhs_loc=sub.location(sub, lhs_loc),
        sub.expr(sub, array),
        sub.expr(sub, index),
        sub.expr(sub, value),
      )
    | PExpRecord(b, es) =>
      record(
        ~loc,
        ~core_loc,
        ~attributes,
        Option.map(sub.expr(sub), b),
        map_record_fields(sub, es),
      )
    | PExpRecordGet(e, f) =>
      record_get(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e),
        map_loc(sub, f),
      )
    | PExpRecordSet(e, f, v) =>
      record_set(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e),
        map_loc(sub, f),
        sub.expr(sub, v),
      )
    | PExpLet(r, m, vbs) =>
      let_(
        ~loc,
        ~core_loc,
        ~attributes,
        r,
        m,
        List.map(sub.value_binding(sub), vbs),
      )
    | PExpMatch(e, mbs) =>
      match(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e),
        {
          txt: List.map(sub.match_branch(sub), mbs.txt),
          loc: sub.location(sub, mbs.loc),
        },
      )
    | PExpPrim0(p0) => prim0(~loc, ~core_loc, ~attributes, p0)
    | PExpPrim1(p1, e) =>
      prim1(~loc, ~core_loc, ~attributes, p1, sub.expr(sub, e))
    | PExpPrim2(p2, e1, e2) =>
      prim2(
        ~loc,
        ~core_loc,
        ~attributes,
        p2,
        sub.expr(sub, e1),
        sub.expr(sub, e2),
      )
    | PExpPrimN(p, es) =>
      primn(~loc, ~core_loc, ~attributes, p, List.map(sub.expr(sub), es))
    | PExpBoxAssign(e1, e2) =>
      box_assign(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e1),
        sub.expr(sub, e2),
      )
    | PExpAssign(e1, e2) =>
      assign(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e1),
        sub.expr(sub, e2),
      )
    | PExpIf(c, t, f) =>
      if_(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, c),
        sub.expr(sub, t),
        Option.map(sub.expr(sub), f),
      )
    | PExpWhile(c, e) =>
      while_(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, c),
        sub.expr(sub, e),
      )
    | PExpFor(i, c, inc, e) =>
      for_(
        ~loc,
        ~core_loc,
        ~attributes,
        Option.map(sub.expr(sub), i),
        Option.map(sub.expr(sub), c),
        Option.map(sub.expr(sub), inc),
        sub.expr(sub, e),
      )
    | PExpContinue => continue(~loc, ~core_loc, ~attributes, ())
    | PExpBreak => break(~loc, ~core_loc, ~attributes, ())
    | PExpReturn(e) =>
      return(~loc, ~core_loc, ~attributes, Option.map(sub.expr(sub), e))
    | PExpLambda(pl, e) =>
      lambda(
        ~loc,
        ~core_loc,
        ~attributes,
        List.map(
          arg =>
            {
              pla_label: arg.pla_label,
              pla_pattern: sub.pat(sub, arg.pla_pattern),
              pla_default: Option.map(sub.expr(sub), arg.pla_default),
              pla_loc: sub.location(sub, arg.pla_loc),
            },
          pl,
        ),
        sub.expr(sub, e),
      )
    | PExpApp(e, el) =>
      apply(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e),
        List.map(
          arg =>
            {
              paa_label: arg.paa_label,
              paa_expr: sub.expr(sub, arg.paa_expr),
              paa_loc: sub.location(sub, arg.paa_loc),
            },
          el,
        ),
      )
    | PExpConstruct(id, e) =>
      construct(
        ~loc,
        ~core_loc,
        ~attributes,
        map_identifier(sub, id),
        switch (e) {
        | PExpConstrSingleton => PExpConstrSingleton
        | PExpConstrTuple(el) =>
          PExpConstrTuple(List.map(sub.expr(sub), el))
        | PExpConstrRecord(es) =>
          PExpConstrRecord(map_record_fields(sub, es))
        },
      )
    | PExpBlock(el) =>
      block(~loc, ~core_loc, ~attributes, List.map(sub.expr(sub), el))
    | PExpConstraint(e, t) =>
      constraint_(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.expr(sub, e),
        sub.typ(sub, t),
      )
    | PExpUse(id, u) =>
      let u =
        switch (u) {
        | PUseItems(items) =>
          PUseItems(
            List.map(
              item => {
                switch (item) {
                | PUseType({name, alias, loc}) =>
                  PUseType({
                    name: map_identifier(sub, name),
                    alias: Option.map(map_identifier(sub), alias),
                    loc: sub.location(sub, loc),
                  })
                | PUseException({name, alias, loc}) =>
                  PUseException({
                    name: map_identifier(sub, name),
                    alias: Option.map(map_identifier(sub), alias),
                    loc: sub.location(sub, loc),
                  })
                | PUseModule({name, alias, loc}) =>
                  PUseModule({
                    name: map_identifier(sub, name),
                    alias: Option.map(map_identifier(sub), alias),
                    loc: sub.location(sub, loc),
                  })
                | PUseValue({name, alias, loc}) =>
                  PUseValue({
                    name: map_identifier(sub, name),
                    alias: Option.map(map_identifier(sub), alias),
                    loc: sub.location(sub, loc),
                  })
                }
              },
              items,
            ),
          )
        | PUseAll => PUseAll
        };
      use(~loc, ~core_loc, ~attributes, map_identifier(sub, id), u);
    };
  };
};

module P = {
  let map = (sub, {ppat_desc: desc, ppat_loc: loc}) => {
    open Pattern;
    let loc = sub.location(sub, loc);
    switch (desc) {
    | PPatAny => any(~loc, ())
    | PPatVar(sl) => var(~loc, map_loc(sub, sl))
    | PPatTuple(pl) => tuple(~loc, List.map(sub.pat(sub), pl))
    | PPatList(pl) =>
      list(
        ~loc,
        List.map(
          item => {
            switch (item) {
            | ListItem(p) => ListItem(sub.pat(sub, p))
            | ListSpread(p, loc) =>
              ListSpread(sub.pat(sub, p), sub.location(sub, loc))
            }
          },
          pl,
        ),
      )
    | PPatArray(pl) => array(~loc, List.map(sub.pat(sub), pl))
    | PPatRecord(fs, c) =>
      record(
        ~loc,
        List.map(
          ((id, pat)) =>
            (Some((map_loc(sub, id), sub.pat(sub, pat))), c),
          fs,
        ),
      )
    | PPatConstant(c) => constant(~loc, sub.constant(sub, c))
    | PPatConstraint(p, pt) =>
      constraint_(~loc, sub.pat(sub, p), sub.typ(sub, pt))
    | PPatConstruct(id, p) =>
      construct(
        ~loc,
        map_identifier(sub, id),
        switch (p) {
        | PPatConstrTuple(pl) => PPatConstrTuple(List.map(sub.pat(sub), pl))
        | PPatConstrRecord(fs, c) =>
          PPatConstrRecord(
            List.map(
              ((id, pat)) => (map_loc(sub, id), sub.pat(sub, pat)),
              fs,
            ),
            c,
          )
        | PPatConstrSingleton => PPatConstrSingleton
        },
      )
    | PPatOr(p1, p2) => or_(~loc, sub.pat(sub, p1), sub.pat(sub, p2))
    | PPatAlias(p, id) => alias(~loc, sub.pat(sub, p), map_loc(sub, id))
    };
  };
};

module C = {
  let map = (sub, {pcd_name: name, pcd_args: args, pcd_loc: loc}) => {
    open ConstructorDeclaration;
    let loc = sub.location(sub, loc);
    let sname = map_loc(sub, name);
    switch (args) {
    | PConstrTuple(ptl) =>
      tuple(
        ~loc,
        sname,
        {
          txt: List.map(sub.typ(sub), ptl.txt),
          loc: sub.location(sub, ptl.loc),
        },
      )
    | PConstrRecord(ldl) =>
      record(
        ~loc,
        sname,
        {
          txt: List.map(sub.label(sub), ldl.txt),
          loc: sub.location(sub, ldl.loc),
        },
      )
    | PConstrSingleton => singleton(~loc, sname)
    };
  };
};

module L = {
  let map =
      (sub, {pld_name: name, pld_type: typ, pld_mutable: mut, pld_loc: loc}) => {
    open LabelDeclaration;
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
          pdata_rec: rec_flag,
          pdata_name: name,
          pdata_params: args,
          pdata_kind: kind,
          pdata_manifest: man,
          pdata_loc: loc,
        },
      ) => {
    open DataDeclaration;
    let loc = sub.location(sub, loc);
    let sname = map_loc(sub, name);
    let sargs = List.map(sub.typ(sub), args);
    let sman = Option.map(sub.typ(sub), man);
    switch (kind) {
    | PDataAbstract => abstract(~loc, ~rec_flag, sname, sargs, sman)
    | PDataVariant(cdl) =>
      variant(
        ~loc,
        ~rec_flag,
        sname,
        sargs,
        List.map(sub.constructor(sub), cdl),
      )
    | PDataRecord(ldl) =>
      record(~loc, ~rec_flag, sname, sargs, List.map(sub.label(sub), ldl))
    };
  };
};

module Exc = {
  let map = (sub, {ptyexn_constructor: ext, ptyexn_loc: loc}) => {
    open Exception;
    let cloc = sub.location(sub, loc);
    let {pext_name: n, pext_kind: k, pext_loc: loc} = ext;
    let name = map_loc(sub, n);
    let loc = sub.location(sub, loc);
    let k =
      switch (k) {
      | PExtDecl(args) =>
        PExtDecl(
          switch (args) {
          | PConstrTuple(ptl) =>
            PConstrTuple({
              txt: List.map(sub.typ(sub), ptl.txt),
              loc: sub.location(sub, ptl.loc),
            })
          | PConstrRecord(ldl) =>
            PConstrRecord({
              txt: List.map(sub.label(sub), ldl.txt),
              loc: sub.location(sub, ldl.loc),
            })
          | PConstrSingleton => PConstrSingleton
          },
        )
      | PExtRebind(id) => PExtRebind(map_loc(sub, id))
      };
    let ext = {pext_name: name, pext_kind: k, pext_loc: loc};
    {ptyexn_constructor: ext, ptyexn_loc: cloc};
  };
};

module T = {
  let map = (sub, {ptyp_desc: desc, ptyp_loc: loc}) => {
    open Type;
    let loc = sub.location(sub, loc);
    switch (desc) {
    | PTyAny => any(~loc, ())
    | PTyVar(v) => var(~loc, v)
    | PTyArrow(args, ret) =>
      arrow(
        ~loc,
        List.map(
          arg =>
            {
              ptyp_arg_label: arg.ptyp_arg_label,
              ptyp_arg_type: sub.typ(sub, arg.ptyp_arg_type),
              ptyp_arg_loc: sub.location(sub, arg.ptyp_arg_loc),
            },
          args,
        ),
        sub.typ(sub, ret),
      )
    | PTyTuple(ts) => tuple(~loc, List.map(sub.typ(sub), ts))
    | PTyConstr(name, ts) =>
      constr(~loc, map_identifier(sub, name), List.map(sub.typ(sub), ts))
    | PTyPoly(vars, t) =>
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
  let map =
      (sub, {pmb_pat: pat, pmb_body: expr, pmb_guard: guard, pmb_loc: loc}) => {
    pmb_pat: sub.pat(sub, pat),
    pmb_body: sub.expr(sub, expr),
    pmb_guard: Option.map(sub.expr(sub), guard),
    pmb_loc: sub.location(sub, loc),
  };
};

module I = {
  let map = (sub, {pinc_module, pinc_alias, pinc_path, pinc_loc}) => {
    {
      pinc_path: map_loc(sub, pinc_path),
      pinc_module: map_loc(sub, pinc_module),
      pinc_alias: Option.map(map_loc(sub), pinc_alias),
      pinc_loc: sub.location(sub, pinc_loc),
    };
  };
};

module Pr = {
  let map = (sub, items) => {
    List.map(
      item => {
        switch (item) {
        | PProvideType({name, alias, loc}) =>
          PProvideType({
            name: map_identifier(sub, name),
            alias: Option.map(map_identifier(sub), alias),
            loc: sub.location(sub, loc),
          })
        | PProvideException({name, alias, loc}) =>
          PProvideException({
            name: map_identifier(sub, name),
            alias: Option.map(map_identifier(sub), alias),
            loc: sub.location(sub, loc),
          })
        | PProvideModule({name, alias, loc}) =>
          PProvideModule({
            name: map_identifier(sub, name),
            alias: Option.map(map_identifier(sub), alias),
            loc: sub.location(sub, loc),
          })
        | PProvideValue({name, alias, loc}) =>
          PProvideValue({
            name: map_identifier(sub, name),
            alias: Option.map(map_identifier(sub), alias),
            loc: sub.location(sub, loc),
          })
        }
      },
      items,
    );
  };
};

module PD = {
  let map = (sub, {pprim_ident: ident, pprim_name: name, pprim_loc: loc}) => {
    let pprim_loc = sub.location(sub, loc);
    let pprim_ident = map_loc(sub, ident);
    let pprim_name = map_loc(sub, name);
    {pprim_ident, pprim_name, pprim_loc};
  };
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
  let map =
      (
        sub,
        {
          ptop_desc: desc,
          ptop_attributes: attrs,
          ptop_loc: loc,
          ptop_core_loc: core_loc,
        },
      ) => {
    open Toplevel;
    let loc = sub.location(sub, loc);
    let core_loc = sub.location(sub, core_loc);
    let attributes =
      List.map(
        ({Asttypes.attr_name, attr_args, attr_loc}) =>
          {
            Asttypes.attr_name: map_loc(sub, attr_name),
            attr_args: List.map(map_loc(sub), attr_args),
            attr_loc: sub.location(sub, attr_loc),
          },
        attrs,
      );
    switch (desc) {
    | PTopInclude(decls) =>
      Toplevel.include_(
        ~loc,
        ~core_loc,
        ~attributes,
        sub.include_(sub, decls),
      )
    | PTopForeign(e, d) =>
      Toplevel.foreign(
        ~loc,
        ~core_loc,
        ~attributes,
        e,
        sub.value_description(sub, d),
      )
    | PTopPrimitive(e, d) =>
      Toplevel.primitive(
        ~loc,
        ~core_loc,
        ~attributes,
        e,
        sub.primitive_description(sub, d),
      )
    | PTopData(dd) =>
      Toplevel.data(
        ~loc,
        ~core_loc,
        ~attributes,
        List.map(
          ((e, d, l)) => (e, sub.data(sub, d), sub.location(sub, l)),
          dd,
        ),
      )
    | PTopLet(e, r, m, vb) =>
      Toplevel.let_(
        ~loc,
        ~core_loc,
        ~attributes,
        e,
        r,
        m,
        List.map(sub.value_binding(sub), vb),
      )
    | PTopModule(e, d) =>
      Toplevel.module_(
        ~loc,
        ~core_loc,
        ~attributes,
        e,
        {...d, pmod_stmts: List.map(sub.toplevel(sub), d.pmod_stmts)},
      )
    | PTopExpr(e) =>
      Toplevel.expr(~loc, ~core_loc, ~attributes, sub.expr(sub, e))
    | PTopException(e, d) =>
      Toplevel.grain_exception(
        ~loc,
        ~core_loc,
        ~attributes,
        e,
        sub.grain_exception(sub, d),
      )
    | PTopProvide(ex) =>
      Toplevel.provide(~loc, ~core_loc, ~attributes, sub.provide(sub, ex))
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
  include_: I.map,
  provide: Pr.map,
  value_binding: V.map,
  match_branch: MB.map,
  primitive_description: PD.map,
  value_description: VD.map,
  grain_exception: Exc.map,
  toplevel: TL.map,
};
