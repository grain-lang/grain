open Parsetree;

type hooks = {
  enter_location: Location.t => unit,
  leave_location: Location.t => unit,
  enter_attribute: attribute => unit,
  leave_attribute: attribute => unit,
  enter_parsed_program: parsed_program => unit,
  leave_parsed_program: parsed_program => unit,
  enter_include: include_declaration => unit,
  leave_include: include_declaration => unit,
  enter_provide: list(provide_item) => unit,
  leave_provide: list(provide_item) => unit,
  enter_foreign: (provide_flag, value_description) => unit,
  leave_foreign: (provide_flag, value_description) => unit,
  enter_primitive: (provide_flag, primitive_description) => unit,
  leave_primitive: (provide_flag, primitive_description) => unit,
  enter_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit,
  leave_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit,
  enter_module: (provide_flag, module_declaration) => unit,
  leave_module: (provide_flag, module_declaration) => unit,
  enter_pattern: pattern => unit,
  leave_pattern: pattern => unit,
  enter_expression: expression => unit,
  leave_expression: expression => unit,
  enter_type: parsed_type => unit,
  leave_type: parsed_type => unit,
  enter_toplevel_stmt: toplevel_stmt => unit,
  leave_toplevel_stmt: toplevel_stmt => unit,
  enter_constant: constant => unit,
  leave_constant: constant => unit,
  enter_let: (rec_flag, mut_flag, list(value_binding)) => unit,
  leave_let: (rec_flag, mut_flag, list(value_binding)) => unit,
  enter_value_binding: value_binding => unit,
  leave_value_binding: value_binding => unit,
  enter_data_declarations:
    list((provide_flag, data_declaration, Location.t)) => unit,
  leave_data_declarations:
    list((provide_flag, data_declaration, Location.t)) => unit,
  enter_data_declaration: data_declaration => unit,
  leave_data_declaration: data_declaration => unit,
};

let iter_constant = (hooks, c) => {
  hooks.enter_constant(c);
  hooks.leave_constant(c);
};

let iter_location = (hooks, loc) => {
  hooks.enter_location(loc);
  hooks.leave_location(loc);
};

// Just a helper to extract the Location.t from a loc('a)
let iter_loc = (hooks, {loc}) => {
  iter_location(hooks, loc);
};

let iter_ident = (hooks, id) => {
  open Identifier;
  iter_loc(hooks, id);
  let rec iter = id => {
    switch (id) {
    | IdentName(name) => iter_loc(hooks, name)
    | IdentExternal(id, name) =>
      iter(id);
      iter_loc(hooks, name);
    };
  };
  iter(id.txt);
};

let iter_attribute =
    (hooks, {Asttypes.attr_name, attr_args, attr_loc} as attr) => {
  hooks.enter_attribute(attr);
  iter_loc(hooks, attr_name);
  List.iter(iter_loc(hooks), attr_args);
  iter_location(hooks, attr_loc);
  hooks.leave_attribute(attr);
};

let iter_attributes = (hooks, attrs) => {
  List.iter(iter_attribute(hooks), attrs);
};

let rec iter_parsed_program = (hooks, {statements} as program) => {
  hooks.enter_parsed_program(program);
  iter_attributes(hooks, program.attributes);
  iter_toplevel_stmts(hooks, statements);
  hooks.leave_parsed_program(program);
}

and iter_toplevel_stmts = (hooks, stmts) => {
  List.iter(iter_toplevel_stmt(hooks), stmts);
}

and iter_toplevel_stmt =
    (
      hooks,
      {
        ptop_desc: desc,
        ptop_attributes: attrs,
        ptop_loc: loc,
        ptop_core_loc: core_loc,
      } as top,
    ) => {
  hooks.enter_toplevel_stmt(top);
  iter_location(hooks, loc);
  iter_location(hooks, core_loc);
  iter_attributes(hooks, attrs);
  switch (desc) {
  | PTopInclude(id) => iter_include(hooks, id)
  | PTopProvide(ex) => iter_provide(hooks, ex)
  | PTopForeign(p, vd) => iter_foreign(hooks, p, vd)
  | PTopPrimitive(p, vd) => iter_primitive(hooks, p, vd)
  | PTopData(dds) => iter_data_declarations(hooks, dds)
  | PTopLet(p, r, m, vbs) => iter_top_let(hooks, p, r, m, vbs)
  | PTopModule(p, d) => iter_module(hooks, p, d)
  | PTopExpr(e) => iter_expression(hooks, e)
  | PTopException(e, d) => iter_exception(hooks, d)
  };
  hooks.leave_toplevel_stmt(top);
}

and iter_include = (hooks, {pinc_alias, pinc_path, pinc_loc} as id) => {
  hooks.enter_include(id);
  Option.iter(iter_loc(hooks), pinc_alias);
  iter_loc(hooks, pinc_path);
  iter_location(hooks, pinc_loc);
  hooks.leave_include(id);
}

and iter_provide = (hooks, items) => {
  hooks.enter_provide(items);
  List.iter(
    item => {
      switch (item) {
      | PProvideType({name, alias, loc})
      | PProvideException({name, alias, loc})
      | PProvideModule({name, alias, loc})
      | PProvideValue({name, alias, loc}) =>
        iter_ident(hooks, name);
        Option.iter(iter_ident(hooks), alias);
        iter_location(hooks, loc);
      }
    },
    items,
  );
  hooks.leave_provide(items);
}

and iter_foreign = (hooks, p, vd) => {
  hooks.enter_foreign(p, vd);
  iter_value_description(hooks, vd);
  hooks.leave_foreign(p, vd);
}

and iter_primitive = (hooks, p, pd) => {
  hooks.enter_primitive(p, pd);
  iter_primitive_description(hooks, pd);
  hooks.leave_primitive(p, pd);
}

and iter_value_description =
    (hooks, {pval_mod: vmod, pval_name: vname, pval_loc: loc}) => {
  iter_location(hooks, loc);
  iter_loc(hooks, vmod);
  iter_loc(hooks, vname);
}

and iter_primitive_description =
    (hooks, {pprim_ident: ident, pprim_name: name, pprim_loc: loc}) => {
  iter_location(hooks, loc);
  iter_loc(hooks, ident);
  iter_loc(hooks, name);
}

and iter_data_declarations = (hooks, dds) => {
  hooks.enter_data_declarations(dds);
  List.iter(
    ((_, d, l)) => {
      iter_data_declaration(hooks, d);
      iter_location(hooks, l);
    },
    dds,
  );
  hooks.leave_data_declarations(dds);
}

and iter_data_declaration =
    (
      hooks,
      {
        pdata_name: name,
        pdata_params: args,
        pdata_manifest: manifest,
        pdata_kind: kind,
        pdata_loc: loc,
      } as d,
    ) => {
  hooks.enter_data_declaration(d);
  iter_location(hooks, loc);
  iter_loc(hooks, name);
  List.iter(iter_type(hooks), args);
  Option.iter(iter_type(hooks), manifest);
  switch (kind) {
  | PDataAbstract => ()
  | PDataVariant(cdl) => List.iter(iter_constructor(hooks), cdl)
  | PDataRecord(ldl) => List.iter(iter_label(hooks), ldl)
  };
  hooks.leave_data_declaration(d);
}

and iter_top_let = (hooks, p, r, m, vbs) => {
  hooks.enter_top_let(p, r, m, vbs);
  iter_value_bindings(hooks, vbs);
  hooks.leave_top_let(p, r, m, vbs);
}

and iter_module = (hooks, p, d) => {
  hooks.enter_module(p, d);
  List.iter(iter_toplevel_stmt(hooks), d.pmod_stmts);
  hooks.leave_module(p, d);
}

and iter_value_bindings = (hooks, vbs) => {
  List.iter(iter_value_binding(hooks), vbs);
}

and iter_value_binding =
    (hooks, {pvb_pat: pat, pvb_expr: expr, pvb_loc: loc} as vb) => {
  hooks.enter_value_binding(vb);
  iter_pattern(hooks, pat);
  iter_expression(hooks, expr);
  iter_location(hooks, loc);
  hooks.leave_value_binding(vb);
}

and iter_expressions = (hooks, es) => {
  List.iter(iter_expression(hooks), es);
}

and iter_expression =
    (
      hooks,
      {
        pexp_desc: desc,
        pexp_attributes: attrs,
        pexp_loc: loc,
        pexp_core_loc: core_loc,
      } as expr,
    ) => {
  hooks.enter_expression(expr);
  iter_location(hooks, loc);
  iter_location(hooks, core_loc);
  iter_attributes(hooks, attrs);
  switch (desc) {
  | PExpId(i) => iter_ident(hooks, i)
  | PExpConstant(c) => iter_constant(hooks, c)
  | PExpTuple(es) => iter_expressions(hooks, es)
  | PExpList(es) =>
    List.iter(
      item => {
        switch (item) {
        | ListItem(e) => iter_expression(hooks, e)
        | ListSpread(e, loc) =>
          iter_expression(hooks, e);
          iter_location(hooks, loc);
        }
      },
      es,
    )
  | PExpArray(es) => iter_expressions(hooks, es)
  | PExpArrayGet(a, i) =>
    iter_expression(hooks, a);
    iter_expression(hooks, i);
  | PExpArraySet({array, index, value, infix_op}) =>
    iter_expression(hooks, array);
    iter_expression(hooks, index);
    iter_expression(hooks, value);
    Option.iter(iter_expression(hooks), infix_op);
  | PExpRecord(b, es) =>
    Option.iter(iter_expression(hooks), b);
    iter_record_fields(hooks, es);
  | PExpRecordGet(e, f) =>
    iter_expression(hooks, e);
    iter_loc(hooks, f);
  | PExpRecordSet(e, f, v) =>
    iter_expression(hooks, e);
    iter_loc(hooks, f);
    iter_expression(hooks, v);
  | PExpLet(r, m, vbs) => iter_let(hooks, r, m, vbs)
  | PExpMatch(e, mbs) =>
    iter_expression(hooks, e);
    iter_loc(hooks, mbs);
    List.iter(iter_match_branch(hooks), mbs.txt);
  | PExpPrim0(p0) => ()
  | PExpPrim1(p1, e) => iter_expression(hooks, e)
  | PExpPrim2(p2, e1, e2) =>
    iter_expression(hooks, e1);
    iter_expression(hooks, e2);
  | PExpPrimN(p, es) => iter_expressions(hooks, es)
  | PExpBoxAssign(a, b) =>
    iter_expression(hooks, a);
    iter_expression(hooks, b);
  | PExpAssign(a, b) =>
    iter_expression(hooks, a);
    iter_expression(hooks, b);
  | PExpIf(c, t, f) =>
    iter_expression(hooks, c);
    iter_expression(hooks, t);
    Option.iter(iter_expression(hooks), f);
  | PExpWhile(c, b) =>
    iter_expression(hooks, c);
    iter_expression(hooks, b);
  | PExpFor(i, c, inc, b) =>
    Option.iter(iter_expression(hooks), i);
    Option.iter(iter_expression(hooks), c);
    Option.iter(iter_expression(hooks), inc);
    iter_expression(hooks, b);
  | PExpContinue
  | PExpBreak => ()
  | PExpReturn(e) => Option.iter(iter_expression(hooks), e)
  | PExpConstraint(e, t) =>
    iter_expression(hooks, e);
    iter_type(hooks, t);
  | PExpUse(i, u) =>
    iter_loc(hooks, i);
    switch (u) {
    | PUseItems(items) =>
      List.iter(
        item => {
          switch (item) {
          | PUseType({name, alias, loc})
          | PUseException({name, alias, loc})
          | PUseModule({name, alias, loc})
          | PUseValue({name, alias, loc}) =>
            iter_ident(hooks, name);
            Option.iter(iter_ident(hooks), alias);
            iter_location(hooks, loc);
          }
        },
        items,
      )
    | PUseAll => ()
    };
  | PExpLambda(pl, e) =>
    List.iter(
      arg => {
        iter_pattern(hooks, arg.pla_pattern);
        Option.iter(iter_expression(hooks), arg.pla_default);
        iter_location(hooks, arg.pla_loc);
      },
      pl,
    );
    iter_expression(hooks, e);
  | PExpApp(e, el) =>
    iter_expression(hooks, e);
    List.iter(
      arg => {
        iter_expression(hooks, arg.paa_expr);
        iter_location(hooks, arg.paa_loc);
      },
      el,
    );
  | PExpConstruct(c, e) =>
    iter_ident(hooks, c);
    switch (e) {
    | PExpConstrTuple(el) => iter_expressions(hooks, el)
    | PExpConstrRecord(es) => iter_record_fields(hooks, es)
    | PExpConstrSingleton => ()
    };
  | PExpBlock(el) => iter_expressions(hooks, el)
  };
  hooks.leave_expression(expr);
}

and iter_record_fields = (hooks, es) => {
  List.iter(
    ((name, exp)) => {
      iter_loc(hooks, name);
      iter_expression(hooks, exp);
    },
    es,
  );
}

and iter_let = (hooks, r, m, vbs) => {
  hooks.enter_let(r, m, vbs);
  iter_value_bindings(hooks, vbs);
  hooks.leave_let(r, m, vbs);
}

and iter_match_branch = (hooks, {pmb_pat: pat, pmb_body: expr, pmb_loc: loc}) => {
  iter_pattern(hooks, pat);
  iter_expression(hooks, expr);
  iter_location(hooks, loc);
}

and iter_exception = (hooks, {ptyexn_constructor: ext, ptyexn_loc: loc}) => {
  iter_location(hooks, loc);
  let {pext_name: n, pext_kind: k, pext_loc: loc} = ext;
  iter_loc(hooks, n);
  iter_location(hooks, loc);
  switch (k) {
  | PExtDecl(args) =>
    switch (args) {
    | PConstrTuple(ptl) =>
      iter_loc(hooks, ptl);
      List.iter(iter_type(hooks), ptl.txt);
    | PConstrRecord(ldl) =>
      iter_loc(hooks, ldl);
      List.iter(iter_label(hooks), ldl.txt);
    | PConstrSingleton => ()
    }
  | PExtRebind(id) => iter_loc(hooks, id)
  };
}

and iter_type = (hooks, {ptyp_desc: desc, ptyp_loc: loc} as typ) => {
  hooks.enter_type(typ);
  iter_location(hooks, loc);
  switch (desc) {
  | PTyAny => ()
  | PTyVar(v) => ()
  | PTyArrow(args, ret) =>
    List.iter(
      arg => {
        iter_type(hooks, arg.ptyp_arg_type);
        iter_location(hooks, arg.ptyp_arg_loc);
      },
      args,
    );
    iter_type(hooks, ret);
  | PTyTuple(ts) => List.iter(iter_type(hooks), ts)
  | PTyConstr(name, ts) =>
    iter_ident(hooks, name);
    List.iter(iter_type(hooks), ts);
  | PTyPoly(args, t) =>
    List.iter(iter_loc(hooks), args);
    iter_type(hooks, t);
  };
  hooks.leave_type(typ);
}

and iter_constructor =
    (hooks, {pcd_name: name, pcd_args: args, pcd_loc: loc}) => {
  iter_location(hooks, loc);
  iter_loc(hooks, name);
  switch (args) {
  | PConstrTuple(ptl) =>
    iter_loc(hooks, ptl);
    List.iter(iter_type(hooks), ptl.txt);
  | PConstrRecord(ldl) =>
    iter_loc(hooks, ldl);
    List.iter(iter_label(hooks), ldl.txt);
  | PConstrSingleton => ()
  };
}

and iter_label = (hooks, {pld_name: name, pld_type: typ, pld_loc: loc}) => {
  iter_location(hooks, loc);
  iter_loc(hooks, name);
  iter_type(hooks, typ);
}

and iter_patterns = (hooks, ps) => {
  List.iter(iter_pattern(hooks), ps);
}

and iter_pattern = (hooks, {ppat_desc: desc, ppat_loc: loc} as pat) => {
  hooks.enter_pattern(pat);
  iter_location(hooks, loc);
  switch (desc) {
  | PPatAny => ()
  | PPatVar(sl) => iter_loc(hooks, sl)
  | PPatTuple(pl) => iter_patterns(hooks, pl)
  | PPatList(pl) =>
    List.iter(
      item => {
        switch (item) {
        | ListItem(p) => iter_pattern(hooks, p)
        | ListSpread(p, loc) =>
          iter_pattern(hooks, p);
          iter_location(hooks, loc);
        }
      },
      pl,
    )
  | PPatArray(pl) => iter_patterns(hooks, pl)
  | PPatRecord(fs, _) => iter_record_patterns(hooks, fs)
  | PPatConstant(c) => iter_constant(hooks, c)
  | PPatConstraint(p, pt) =>
    iter_pattern(hooks, p);
    iter_type(hooks, pt);
  | PPatConstruct(id, p) =>
    iter_ident(hooks, id);
    switch (p) {
    | PPatConstrSingleton => ()
    | PPatConstrTuple(pl) => iter_patterns(hooks, pl)
    | PPatConstrRecord(fs, _) => iter_record_patterns(hooks, fs)
    };
  | PPatOr(p1, p2) =>
    iter_pattern(hooks, p1);
    iter_pattern(hooks, p2);
  | PPatAlias(p, id) =>
    iter_pattern(hooks, p);
    iter_loc(hooks, id);
  };
  hooks.leave_pattern(pat);
}

and iter_record_patterns = (hooks, fs) => {
  List.iter(
    ((id, pat)) => {
      iter_loc(hooks, id);
      iter_pattern(hooks, pat);
    },
    fs,
  );
};

let default_hooks = {
  enter_location: _ => (),
  leave_location: _ => (),

  enter_attribute: _ => (),
  leave_attribute: _ => (),

  enter_parsed_program: _ => (),
  leave_parsed_program: _ => (),

  enter_include: _ => (),
  leave_include: _ => (),

  enter_provide: _ => (),
  leave_provide: _ => (),

  enter_foreign: (_, _) => (),
  leave_foreign: (_, _) => (),

  enter_primitive: (_, _) => (),
  leave_primitive: (_, _) => (),

  enter_top_let: (_, _, _, _) => (),
  leave_top_let: (_, _, _, _) => (),

  enter_module: (_, _) => (),
  leave_module: (_, _) => (),

  enter_pattern: _ => (),
  leave_pattern: _ => (),

  enter_expression: _ => (),
  leave_expression: _ => (),

  enter_type: _ => (),
  leave_type: _ => (),

  enter_toplevel_stmt: _ => (),
  leave_toplevel_stmt: _ => (),

  enter_constant: _ => (),
  leave_constant: _ => (),

  enter_let: (_, _, _) => (),
  leave_let: (_, _, _) => (),

  enter_value_binding: _ => (),
  leave_value_binding: _ => (),

  leave_data_declarations: _ => (),
  enter_data_declarations: _ => (),

  enter_data_declaration: _ => (),
  leave_data_declaration: _ => (),
};
