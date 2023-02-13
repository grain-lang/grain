/* See copyright in ast_iterator.mli */
open Parsetree;

module type IteratorArgument = {
  let enter_location: Location.t => unit;
  let leave_location: Location.t => unit;

  let enter_attribute: attribute => unit;
  let leave_attribute: attribute => unit;

  let enter_parsed_program: parsed_program => unit;
  let leave_parsed_program: parsed_program => unit;

  let enter_include: include_declaration => unit;
  let leave_include: include_declaration => unit;

  let enter_provide: list(provide_item) => unit;
  let leave_provide: list(provide_item) => unit;

  let enter_foreign: (provide_flag, value_description) => unit;
  let leave_foreign: (provide_flag, value_description) => unit;

  let enter_primitive: (provide_flag, value_description) => unit;
  let leave_primitive: (provide_flag, value_description) => unit;

  let enter_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit;
  let leave_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit;

  let enter_module: (provide_flag, module_declaration) => unit;
  let leave_module: (provide_flag, module_declaration) => unit;

  let enter_pattern: pattern => unit;
  let leave_pattern: pattern => unit;

  let enter_expression: expression => unit;
  let leave_expression: expression => unit;

  let enter_type: parsed_type => unit;
  let leave_type: parsed_type => unit;

  let enter_toplevel_stmt: toplevel_stmt => unit;
  let leave_toplevel_stmt: toplevel_stmt => unit;

  let enter_constant: constant => unit;
  let leave_constant: constant => unit;

  let enter_let: (rec_flag, mut_flag, list(value_binding)) => unit;
  let leave_let: (rec_flag, mut_flag, list(value_binding)) => unit;

  let enter_value_binding: value_binding => unit;
  let leave_value_binding: value_binding => unit;

  let enter_data_declarations:
    list((provide_flag, data_declaration)) => unit;
  let leave_data_declarations:
    list((provide_flag, data_declaration)) => unit;

  let enter_data_declaration: data_declaration => unit;
  let leave_data_declaration: data_declaration => unit;
};

module type Iterator = {
  let iter_parsed_program: parsed_program => unit;
  let iter_toplevel_stmt: toplevel_stmt => unit;
  //  let iter_expression: expression => unit;
  //  let iter_pattern: pattern => unit;
};

module MakeIterator = (Iter: IteratorArgument) : Iterator => {
  let iter_constant = c => {
    Iter.enter_constant(c);
    Iter.leave_constant(c);
  };

  let iter_location = loc => {
    Iter.enter_location(loc);
    Iter.leave_location(loc);
  };

  // Just a helper to extract the Location.t from a loc('a)
  let iter_loc = ({loc}) => {
    iter_location(loc);
  };

  let iter_ident = id => {
    open Identifier;
    iter_loc(id);
    let rec iter = id => {
      switch (id) {
      | IdentName(name) => iter_loc(name)
      | IdentExternal(id, name) =>
        iter(id);
        iter_loc(name);
      };
    };
    iter(id.txt);
  };

  let iter_attributes = attrs => {
    List.iter(
      ((attr_name, attr_args) as attr) => {
        Iter.enter_attribute(attr);
        iter_loc(attr_name);
        List.iter(iter_loc, attr_args);
        Iter.leave_attribute(attr);
      },
      attrs,
    );
  };

  let rec iter_parsed_program = ({statements} as program) => {
    Iter.enter_parsed_program(program);
    iter_toplevel_stmts(statements);
    Iter.leave_parsed_program(program);
  }

  and iter_toplevel_stmts = stmts => {
    List.iter(iter_toplevel_stmt, stmts);
  }

  and iter_toplevel_stmt =
      ({ptop_desc: desc, ptop_attributes: attrs, ptop_loc: loc} as top) => {
    Iter.enter_toplevel_stmt(top);
    iter_location(loc);
    iter_attributes(attrs);
    switch (desc) {
    | PTopInclude(id) =>
      Iter.enter_include(id);
      iter_include(id);
      Iter.leave_include(id);
    | PTopProvide(ex) =>
      Iter.enter_provide(ex);
      iter_provide(ex);
      Iter.leave_provide(ex);
    | PTopForeign(p, vd) =>
      Iter.enter_foreign(p, vd);
      iter_value_description(vd);
      Iter.leave_foreign(p, vd);
    | PTopPrimitive(p, vd) =>
      Iter.enter_primitive(p, vd);
      iter_value_description(vd);
      Iter.leave_primitive(p, vd);
    | PTopData(dds) =>
      Iter.enter_data_declarations(dds);
      iter_data_declarations(dds);
      Iter.leave_data_declarations(dds);
    | PTopLet(p, r, m, vbs) =>
      Iter.enter_top_let(p, r, m, vbs);
      iter_value_bindings(vbs);
      Iter.leave_top_let(p, r, m, vbs);
    | PTopModule(p, d) =>
      Iter.enter_module(p, d);
      List.iter(iter_toplevel_stmt, d.pmod_stmts);
      Iter.leave_module(p, d);
    | PTopExpr(e) => iter_expression(e)
    | PTopException(e, d) => iter_exception(d)
    };
    Iter.leave_toplevel_stmt(top);
  }

  and iter_include = ({pinc_alias, pinc_path, pinc_loc}) => {
    Option.iter(iter_loc, pinc_alias);
    iter_loc(pinc_path);
    iter_location(pinc_loc);
  }

  and iter_provide = items => {
    List.iter(
      item => {
        switch (item) {
        | PProvideType({name, alias, loc})
        | PProvideModule({name, alias, loc})
        | PProvideValue({name, alias, loc}) =>
          iter_ident(name);
          Option.iter(iter_ident, alias);
          iter_location(loc);
        }
      },
      items,
    );
  }

  and iter_value_description =
      ({pval_mod: vmod, pval_name: vname, pval_loc: loc}) => {
    iter_location(loc);
    iter_loc(vmod);
    iter_loc(vname);
  }

  and iter_data_declarations = dds => {
    List.iter(((_, d)) => iter_data_declaration(d), dds);
  }

  and iter_data_declaration =
      (
        {
          pdata_name: name,
          pdata_params: args,
          pdata_manifest: manifest,
          pdata_kind: kind,
          pdata_loc: loc,
        } as d,
      ) => {
    iter_location(loc);
    iter_loc(name);
    Iter.enter_data_declaration(d);
    List.iter(iter_type, args);
    Option.iter(iter_type, manifest);
    switch (kind) {
    | PDataAbstract => ()
    | PDataVariant(cdl) => List.iter(iter_constructor, cdl)
    | PDataRecord(ldl) => List.iter(iter_label, ldl)
    };
    Iter.leave_data_declaration(d);
  }

  and iter_value_bindings = vbs => {
    List.iter(iter_value_binding, vbs);
  }

  and iter_value_binding =
      ({pvb_pat: pat, pvb_expr: expr, pvb_loc: loc} as vb) => {
    Iter.enter_value_binding(vb);
    iter_pattern(pat);
    iter_expression(expr);
    iter_location(loc);
    Iter.leave_value_binding(vb);
  }

  and iter_expressions = es => {
    List.iter(iter_expression, es);
  }

  and iter_expression =
      ({pexp_desc: desc, pexp_attributes: attrs, pexp_loc: loc} as expr) => {
    Iter.enter_expression(expr);
    iter_location(loc);
    iter_attributes(attrs);
    switch (desc) {
    | PExpId(i) => iter_ident(i)
    | PExpConstant(c) => iter_constant(c)
    | PExpTuple(es) => iter_expressions(es)
    | PExpArray(es) => iter_expressions(es)
    | PExpArrayGet(a, i) =>
      iter_expression(a);
      iter_expression(i);
    | PExpArraySet(a, i, arg) =>
      iter_expression(a);
      iter_expression(i);
      iter_expression(arg);
    | PExpRecord(b, es) =>
      Option.iter(iter_expression, b);
      iter_record_fields(es);
    | PExpRecordGet(e, f) =>
      iter_expression(e);
      iter_loc(f);
    | PExpRecordSet(e, f, v) =>
      iter_expression(e);
      iter_loc(f);
      iter_expression(v);
    | PExpLet(r, m, vbs) =>
      Iter.enter_let(r, m, vbs);
      iter_value_bindings(vbs);
      Iter.leave_let(r, m, vbs);
    | PExpMatch(e, mbs) =>
      iter_expression(e);
      List.iter(iter_match_branch, mbs);
    | PExpPrim0(p0) => ()
    | PExpPrim1(p1, e) => iter_expression(e)
    | PExpPrim2(p2, e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | PExpPrimN(p, es) => iter_expressions(es)
    | PExpBoxAssign(a, b) =>
      iter_expression(a);
      iter_expression(b);
    | PExpAssign(a, b) =>
      iter_expression(a);
      iter_expression(b);
    | PExpIf(c, t, f) =>
      iter_expression(c);
      iter_expression(t);
      iter_expression(f);
    | PExpWhile(c, b) =>
      iter_expression(c);
      iter_expression(b);
    | PExpFor(i, c, inc, b) =>
      Option.iter(iter_expression, i);
      Option.iter(iter_expression, c);
      Option.iter(iter_expression, inc);
      iter_expression(b);
    | PExpContinue
    | PExpBreak => ()
    | PExpReturn(e) => Option.iter(iter_expression, e)
    | PExpConstraint(e, t) =>
      iter_expression(e);
      iter_type(t);
    | PExpUse(i, u) =>
      iter_loc(i);
      switch (u) {
      | PUseItems(items) =>
        List.iter(
          item => {
            switch (item) {
            | PUseType({name, alias, loc})
            | PUseModule({name, alias, loc})
            | PUseValue({name, alias, loc}) =>
              iter_ident(name);
              Option.iter(iter_ident, alias);
              iter_location(loc);
            }
          },
          items,
        )
      | PUseAll => ()
      };
    | PExpLambda(pl, e) =>
      iter_patterns(pl);
      iter_expression(e);
    | PExpApp(e, el) =>
      iter_expression(e);
      iter_expressions(el);
    | PExpConstruct(c, e) =>
      iter_ident(c);
      switch (e) {
      | PExpConstrTuple(el) => iter_expressions(el)
      | PExpConstrRecord(es) => iter_record_fields(es)
      };
    | PExpBlock(el) => iter_expressions(el)
    | PExpNull => ()
    };
    Iter.leave_expression(expr);
  }

  and iter_record_fields = es => {
    List.iter(
      ((name, exp)) => {
        iter_loc(name);
        iter_expression(exp);
      },
      es,
    );
  }

  and iter_match_branch = ({pmb_pat: pat, pmb_body: expr, pmb_loc: loc}) => {
    iter_pattern(pat);
    iter_expression(expr);
    iter_location(loc);
  }

  and iter_exception = ({ptyexn_constructor: ext, ptyexn_loc: loc}) => {
    iter_location(loc);
    let {pext_name: n, pext_kind: k, pext_loc: loc} = ext;
    iter_loc(n);
    iter_location(loc);
    switch (k) {
    | PExtDecl(args) =>
      switch (args) {
      | PConstrTuple(ptl) => List.iter(iter_type, ptl)
      | PConstrRecord(ldl) => List.iter(iter_label, ldl)
      | PConstrSingleton => ()
      }
    | PExtRebind(id) => iter_loc(id)
    };
  }

  and iter_type = ({ptyp_desc: desc, ptyp_loc: loc} as typ) => {
    Iter.enter_type(typ);
    iter_location(loc);
    switch (desc) {
    | PTyAny => ()
    | PTyVar(v) => ()
    | PTyArrow(args, ret) =>
      List.iter(iter_type, args);
      iter_type(ret);
    | PTyTuple(ts) => List.iter(iter_type, ts)
    | PTyConstr(name, ts) =>
      iter_ident(name);
      List.iter(iter_type, ts);
    | PTyPoly(args, t) =>
      List.iter(iter_loc, args);
      iter_type(t);
    };
    Iter.leave_type(typ);
  }

  and iter_constructor = ({pcd_name: name, pcd_args: args, pcd_loc: loc}) => {
    iter_location(loc);
    iter_loc(name);
    switch (args) {
    | PConstrTuple(ptl) => List.iter(iter_type, ptl)
    | PConstrRecord(ldl) => List.iter(iter_label, ldl)
    | PConstrSingleton => ()
    };
  }

  and iter_label = ({pld_name: name, pld_type: typ, pld_loc: loc}) => {
    iter_location(loc);
    iter_loc(name);
    iter_type(typ);
  }

  and iter_patterns = ps => {
    List.iter(iter_pattern, ps);
  }

  and iter_pattern = ({ppat_desc: desc, ppat_loc: loc} as pat) => {
    Iter.enter_pattern(pat);
    iter_location(loc);
    switch (desc) {
    | PPatAny => ()
    | PPatVar(sl) => iter_loc(sl)
    | PPatTuple(pl) => iter_patterns(pl)
    | PPatArray(pl) => iter_patterns(pl)
    | PPatRecord(fs, _) => iter_record_patterns(fs)
    | PPatConstant(c) => iter_constant(c)
    | PPatConstraint(p, pt) =>
      iter_pattern(p);
      iter_type(pt);
    | PPatConstruct(id, p) =>
      iter_ident(id);
      switch (p) {
      | PPatConstrTuple(pl) => iter_patterns(pl)
      | PPatConstrRecord(fs, _) => iter_record_patterns(fs)
      };
    | PPatOr(p1, p2) =>
      iter_pattern(p1);
      iter_pattern(p2);
    | PPatAlias(p, id) =>
      iter_pattern(p);
      iter_loc(id);
    };
    Iter.leave_pattern(pat);
  }

  and iter_record_patterns = fs => {
    List.iter(
      ((id, pat)) => {
        iter_loc(id);
        iter_pattern(pat);
      },
      fs,
    );
  };
};

module DefaultIteratorArgument: IteratorArgument = {
  let enter_location = _ => ();
  let leave_location = _ => ();

  let enter_attribute = _ => ();
  let leave_attribute = _ => ();

  let enter_parsed_program = _ => ();
  let leave_parsed_program = _ => ();

  let enter_include = _ => ();
  let leave_include = _ => ();

  let enter_provide = _ => ();
  let leave_provide = _ => ();

  let enter_foreign = (_, _) => ();
  let leave_foreign = (_, _) => ();

  let enter_primitive = (_, _) => ();
  let leave_primitive = (_, _) => ();

  let enter_top_let = (_, _, _, _) => ();
  let leave_top_let = (_, _, _, _) => ();

  let enter_module = (_, _) => ();
  let leave_module = (_, _) => ();

  let enter_pattern = _ => ();
  let leave_pattern = _ => ();

  let enter_expression = _ => ();
  let leave_expression = _ => ();

  let enter_type = _ => ();
  let leave_type = _ => ();

  let enter_toplevel_stmt = _ => ();
  let leave_toplevel_stmt = _ => ();

  let enter_constant = _ => ();
  let leave_constant = _ => ();

  let enter_let = (_, _, _) => ();
  let leave_let = (_, _, _) => ();

  let enter_value_binding = _ => ();
  let leave_value_binding = _ => ();

  let leave_data_declarations = _ => ();
  let enter_data_declarations = _ => ();

  let enter_data_declaration = _ => ();
  let leave_data_declaration = _ => ();
};
