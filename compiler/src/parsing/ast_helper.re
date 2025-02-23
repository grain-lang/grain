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

exception SyntaxError(Location.t, string);
exception BadEncoding(Location.t);

type location('a) = loc('a);

type id = loc(Identifier.t);
type str = loc(string);
type loc = Location.t;

let record_pattern_info = record_pats =>
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
    record_pats,
    ([], Asttypes.Closed),
  );

// This normalizes STRING items in the parsetree that aren't constants so we don't need
// to scatter them throughout the rest of the compiler.
let normalize_string = (~loc, item) => {
  switch (Grain_utils.Literals.conv_string(item.txt)) {
  | Ok(i) => {loc: item.loc, txt: i}
  | Error(msg) => raise(SyntaxError(loc, msg))
  };
};

module Number = {
  let rational = (numerator, slash, denominator) => {
    PConstNumberRational({numerator, slash, denominator});
  };
};

module Constant = {
  let bytes = b => PConstBytes(b);
  let string = s => PConstString(s);
  let char = c => PConstChar(c);
  let number = i => PConstNumber(i);
  let int8 = i => PConstInt8(i);
  let int16 = i => PConstInt16(i);
  let int32 = i => PConstInt32(i);
  let int64 = i => PConstInt64(i);
  let uint8 = i => PConstUint8(i);
  let uint16 = i => PConstUint16(i);
  let uint32 = i => PConstUint32(i);
  let uint64 = i => PConstUint64(i);
  let float32 = f => PConstFloat32(f);
  let float64 = f => PConstFloat64(f);
  let wasmi32 = i => PConstWasmI32(i);
  let wasmi64 = i => PConstWasmI64(i);
  let wasmf32 = f => PConstWasmF32(f);
  let wasmf64 = f => PConstWasmF64(f);
  let bigint = i => PConstBigInt(i);
  let rational = r => PConstRational(r);
  let bool = b => PConstBool(b);
  let void = PConstVoid;
};

module Type = {
  let mk = (~loc, d) => {
    {ptyp_desc: d, ptyp_loc: loc};
  };
  let any = (~loc, ()) => mk(~loc, PTyAny);
  let var = (~loc, a) => mk(~loc, PTyVar(a));
  let arrow = (~loc, a, b) => mk(~loc, PTyArrow(a, b));
  let tuple = (~loc, a) => mk(~loc, PTyTuple(a));
  let constr = (~loc, a, b) => mk(~loc, PTyConstr(a, b));
  let poly = (~loc, a, b) => mk(~loc, PTyPoly(a, b));

  let force_poly = t =>
    switch (t.ptyp_desc) {
    | PTyPoly(_) => t
    | _ => poly(~loc=t.ptyp_loc, [], t)
    };
};

module ConstructorDeclaration = {
  let mk = (~loc, n, a) => {
    {pcd_name: n, pcd_args: a, pcd_loc: loc};
  };
  let singleton = (~loc, n) => mk(~loc, n, PConstrSingleton);
  let tuple = (~loc, n, a) => mk(~loc, n, PConstrTuple(a));
  let record = (~loc, n, a) => {
    List.iter(
      ld =>
        if (ld.pld_mutable == Mutable) {
          raise(
            SyntaxError(
              ld.pld_loc,
              "An inline record constructor cannot have mutable fields.",
            ),
          );
        },
      a.txt,
    );
    mk(~loc, n, PConstrRecord(a));
  };
};

module LabelDeclaration = {
  let mk = (~loc, n, t, m) => {
    {pld_name: n, pld_type: t, pld_mutable: m, pld_loc: loc};
  };
};

module DataDeclaration = {
  let mk = (~loc, ~rec_flag=Nonrecursive, n, t, k, m) => {
    {
      pdata_rec: rec_flag,
      pdata_name: n,
      pdata_params: t,
      pdata_kind: k,
      pdata_manifest: m,
      pdata_loc: loc,
    };
  };
  let abstract = (~loc, ~rec_flag=?, n, t, m) =>
    mk(~loc, ~rec_flag?, n, t, PDataAbstract, m);
  let variant = (~loc, ~rec_flag=?, n, t, cdl) =>
    mk(~loc, ~rec_flag?, n, t, PDataVariant(cdl), None);
  let record = (~loc, ~rec_flag=?, n, t, ldl) =>
    mk(~loc, ~rec_flag?, n, t, PDataRecord(ldl), None);
};

module Exception = {
  let mk = (~loc, n, t) => {
    let ext = {pext_name: n, pext_kind: PExtDecl(t), pext_loc: loc};
    {ptyexn_constructor: ext, ptyexn_loc: loc};
  };
  let singleton = (~loc, n) => mk(~loc, n, PConstrSingleton);
  let tuple = (~loc, n, args) => mk(~loc, n, PConstrTuple(args));
  let record = (~loc, n, args) => {
    List.iter(
      ld =>
        if (ld.pld_mutable == Mutable) {
          raise(
            SyntaxError(
              ld.pld_loc,
              "A record exception constructor cannot have mutable fields.",
            ),
          );
        },
      args.txt,
    );
    mk(~loc, n, PConstrRecord(args));
  };
};

module Pattern = {
  let mk = (~loc, d) => {
    {ppat_desc: d, ppat_loc: loc};
  };
  let any = (~loc, ()) => mk(~loc, PPatAny);
  let var = (~loc, a) => mk(~loc, PPatVar(a));
  let tuple = (~loc, a) => mk(~loc, PPatTuple(a));
  let array = (~loc, a) => mk(~loc, PPatArray(a));
  let record = (~loc, a) => {
    let (patterns, closed) = record_pattern_info(a);
    mk(~loc, PPatRecord(patterns, closed));
  };
  let constant = (~loc, a) => mk(~loc, PPatConstant(a));
  let constraint_ = (~loc, a, b) => mk(~loc, PPatConstraint(a, b));
  let construct = (~loc, a, b) => mk(~loc, PPatConstruct(a, b));
  let singleton_construct = (~loc, a) =>
    construct(~loc, a, PPatConstrSingleton);
  let tuple_construct = (~loc, a, b) =>
    construct(~loc, a, PPatConstrTuple(b));
  let record_construct = (~loc, a, b) => {
    let (patterns, closed) = record_pattern_info(b);
    construct(~loc, a, PPatConstrRecord(patterns, closed));
  };
  let list = (~loc, a) => {
    mk(~loc, PPatList(a));
  };
  let or_ = (~loc, a, b) => mk(~loc, PPatOr(a, b));
  let alias = (~loc, a, b) => mk(~loc, PPatAlias(a, b));
};

module Expression = {
  let mk = (~loc, ~core_loc, ~attributes=?, d) => {
    let attributes = Option.value(~default=[], attributes);
    {
      pexp_desc: d,
      pexp_attributes: attributes,
      pexp_loc: loc,
      pexp_core_loc: core_loc,
    };
  };
  let ident = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpId(a));
  let constant = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpConstant(a));
  let tuple = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpTuple(a));
  let record = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpRecord(a, b));
  let record_fields = (~loc, ~core_loc, ~attributes=?, a) =>
    switch (a) {
    | [] => failwith("Impossible: empty record field list")
    | [base, ...rest] =>
      let (spread_base, record_items) =
        switch (base) {
        | RecordItem(id, expr) => (None, [(id, expr)])
        | RecordSpread(expr, _) => (Some(expr), [])
        };
      let record_items =
        List.fold_left(
          (acc, expr) => {
            switch (expr) {
            | RecordItem(id, expr) => [(id, expr), ...acc]
            | RecordSpread(_, loc) =>
              switch (spread_base) {
              | None =>
                raise(
                  SyntaxError(
                    loc,
                    "A record spread can only appear at the beginning of a record expression.",
                  ),
                )
              | Some(_) =>
                raise(
                  SyntaxError(
                    loc,
                    "A record expression may only contain one record spread.",
                  ),
                )
              }
            }
          },
          record_items,
          rest,
        );
      let record_items = List.rev(record_items);
      record(~loc, ~core_loc, ~attributes?, spread_base, record_items);
    };
  let record_get = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpRecordGet(a, b));
  let record_set = (~loc, ~core_loc, ~attributes=?, a, b, c) =>
    mk(~loc, ~core_loc, ~attributes?, PExpRecordSet(a, b, c));
  let array = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpArray(a));
  let array_get = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpArrayGet(a, b));
  let array_set =
      (
        ~loc,
        ~core_loc,
        ~attributes=?,
        ~infix_op=?,
        ~lhs_loc,
        array,
        index,
        value,
      ) =>
    mk(
      ~loc,
      ~core_loc,
      ~attributes?,
      PExpArraySet({lhs_loc, array, index, value, infix_op}),
    );
  let let_ = (~loc, ~core_loc, ~attributes=?, a, b, c) =>
    mk(~loc, ~core_loc, ~attributes?, PExpLet(a, b, c));
  let match = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpMatch(a, b));
  let prim0 = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpPrim0(a));
  let prim1 = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpPrim1(a, b));
  let prim2 = (~loc, ~core_loc, ~attributes=?, a, b, c) =>
    mk(~loc, ~core_loc, ~attributes?, PExpPrim2(a, b, c));
  let primn = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpPrimN(a, b));
  let if_ = (~loc, ~core_loc, ~attributes=?, a, b, c) =>
    mk(~loc, ~core_loc, ~attributes?, PExpIf(a, b, c));
  let while_ = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpWhile(a, b));
  let for_ = (~loc, ~core_loc, ~attributes=?, a, b, c, d) =>
    mk(~loc, ~core_loc, ~attributes?, PExpFor(a, b, c, d));
  let continue = (~loc, ~core_loc, ~attributes=?, ()) =>
    mk(~loc, ~core_loc, ~attributes?, PExpContinue);
  let break = (~loc, ~core_loc, ~attributes=?, ()) =>
    mk(~loc, ~core_loc, ~attributes?, PExpBreak);
  let return = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpReturn(a));
  let constraint_ = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpConstraint(a, b));
  let use = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpUse(a, b));
  let box_assign = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpBoxAssign(a, b));
  let assign = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpAssign(a, b));
  let lambda = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpLambda(a, b));
  let apply = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpApp(a, b));
  let construct = (~loc, ~core_loc, ~attributes=?, a, b) =>
    mk(~loc, ~core_loc, ~attributes?, PExpConstruct(a, b));
  let singleton_construct = (~loc, ~core_loc, ~attributes=?, a) =>
    construct(~loc, ~core_loc, ~attributes?, a, PExpConstrSingleton);
  let tuple_construct = (~loc, ~core_loc, ~attributes=?, a, b) =>
    construct(~loc, ~core_loc, ~attributes?, a, PExpConstrTuple(b));
  let record_construct = (~loc, ~core_loc, ~attributes=?, a, b) => {
    let record_items =
      List.map(
        expr => {
          switch (expr) {
          | RecordItem(id, expr) => (id, expr)
          | RecordSpread(_, loc) =>
            raise(
              SyntaxError(
                loc,
                "A record spread cannot appear in an inline record constructor expression.",
              ),
            )
          }
        },
        b,
      );
    construct(
      ~loc,
      ~core_loc,
      ~attributes?,
      a,
      PExpConstrRecord(record_items),
    );
  };
  // It's difficult to parse rational numbers while division exists (in the
  // parser state where you've read NUMBER_INT and you're looking ahead at /,
  // you've got a shift/reduce conflict between reducing const -> NUMBER_INT
  // and shifting /, and if you choose to reduce you can't parse a rational,
  // and if you choose to shift then 1 / foo would always be a syntax error
  // because the parser would expect a number). It's easier to just parse it
  // as division and have this action decide that it's actually a rational.
  let binop = (~loc, ~core_loc, ~attributes=?, f, a, b) => {
    // Locations of nested binops are difficult to compute in the parser so we
    // just set the location manually here
    let loc =
      Location.(
        switch (a, b) {
        | ({pexp_loc: {loc_start}}, {pexp_loc: {loc_end}}) => {
            ...loc,
            loc_start,
            loc_end,
          }
        }
      );
    switch (f, a, b) {
    | (
        {pexp_desc: PExpId({txt: IdentName({txt: "/", loc: slash_loc})})},
        {
          pexp_desc: PExpConstant(PConstNumber(PConstNumberInt(numerator))),
        },
        {
          pexp_desc:
            PExpConstant(PConstNumber(PConstNumberInt(denominator))),
        },
      ) =>
      constant(
        ~loc,
        ~core_loc,
        ~attributes?,
        Constant.number(Number.rational(numerator, slash_loc, denominator)),
      )
    | _ =>
      mk(
        ~loc,
        ~core_loc,
        ~attributes?,
        PExpApp(
          f,
          [
            {paa_label: Unlabeled, paa_expr: a, paa_loc: a.pexp_loc},
            {paa_label: Unlabeled, paa_expr: b, paa_loc: b.pexp_loc},
          ],
        ),
      )
    };
  };
  let block = (~loc, ~core_loc, ~attributes=?, a) =>
    mk(~loc, ~core_loc, ~attributes?, PExpBlock(a));
  let list = (~loc, ~core_loc, ~attributes=?, a) => {
    mk(~loc, ~core_loc, ~attributes?, PExpList(a));
  };

  let ignore = e =>
    switch (e.pexp_desc) {
    | PExpLet(_) => e
    | _ =>
      prim1(
        ~loc=e.pexp_loc,
        ~core_loc=e.pexp_core_loc,
        ~attributes=e.pexp_attributes,
        Ignore,
        e,
      )
    };
};

module Toplevel = {
  let mk = (~loc, ~core_loc, ~attributes=?, d) => {
    let attributes = Option.value(~default=[], attributes);
    {
      ptop_desc: d,
      ptop_attributes: attributes,
      ptop_loc: loc,
      ptop_core_loc: core_loc,
    };
  };
  let include_ = (~loc, ~core_loc, ~attributes=?, i) =>
    mk(~loc, ~core_loc, ~attributes?, PTopInclude(i));
  let foreign = (~loc, ~core_loc, ~attributes=?, e, d) =>
    mk(~loc, ~core_loc, ~attributes?, PTopForeign(e, d));
  let module_ = (~loc, ~core_loc, ~attributes=?, e, m) =>
    mk(~loc, ~core_loc, ~attributes?, PTopModule(e, m));
  let primitive = (~loc, ~core_loc, ~attributes=?, e, d) =>
    mk(~loc, ~core_loc, ~attributes?, PTopPrimitive(e, d));
  let data = (~loc, ~core_loc, ~attributes=?, elts) =>
    mk(~loc, ~core_loc, ~attributes?, PTopData(elts));
  let let_ = (~loc, ~core_loc, ~attributes=?, e, r, m, vb) =>
    mk(~loc, ~core_loc, ~attributes?, PTopLet(e, r, m, vb));
  let expr = (~loc, ~core_loc, ~attributes=?, e) =>
    mk(~loc, ~core_loc, ~attributes?, PTopExpr(e));
  let grain_exception = (~loc, ~core_loc, ~attributes=?, e, ext) =>
    mk(~loc, ~core_loc, ~attributes?, PTopException(e, ext));
  let provide = (~loc, ~core_loc, ~attributes=?, e) =>
    mk(~loc, ~core_loc, ~attributes?, PTopProvide(e));
};

module PrimitiveDescription = {
  let mk = (~loc, ~ident, ~name, ()) => {
    {
      pprim_ident: ident,
      pprim_name: normalize_string(~loc, name),
      pprim_loc: loc,
    };
  };
};

module ValueDescription = {
  let mk = (~loc, ~mod_, ~name, ~alias, ~typ, ()) => {
    {
      pval_mod: normalize_string(~loc, mod_),
      pval_name: name,
      pval_name_alias: alias,
      pval_type: typ,
      pval_loc: loc,
    };
  };
};

module ValueBinding = {
  let mk = (~loc, p, e) => {
    {pvb_pat: p, pvb_expr: e, pvb_loc: loc};
  };
};

module MatchBranch = {
  let mk = (~loc, p, e, g) => {
    {pmb_pat: p, pmb_body: e, pmb_guard: g, pmb_loc: loc};
  };
};

module IncludeDeclaration = {
  let mk = (~loc, path, module_, alias) => {
    let path = normalize_string(~loc, path);
    let filename =
      if (!Grain_utils.Filepath.String.is_relpath(path.txt)) {
        path.txt ++ ".gr";
      } else {
        path.txt;
      };
    let path = {txt: filename, loc: path.loc};
    {pinc_alias: alias, pinc_module: module_, pinc_path: path, pinc_loc: loc};
  };
};

module TypeArgument = {
  let mk = (~loc, label, typ) => {
    {ptyp_arg_label: label, ptyp_arg_type: typ, ptyp_arg_loc: loc};
  };
};

module LambdaArgument = {
  let mk = (~loc, pattern, default) => {
    open Asttypes;
    let label =
      switch (pattern.ppat_desc) {
      | PPatVar(name)
      | PPatAlias({ppat_desc: PPatVar(name)}, _)
      | PPatAlias(_, name)
      | PPatConstraint(
          {
            ppat_desc:
              PPatVar(name) | PPatAlias({ppat_desc: PPatVar(name)}, _) |
              PPatAlias(_, name),
          },
          _,
        ) =>
        Some(name)
      | _ => None
      };
    let pla_label =
      switch (label, default) {
      | (Some(name), Some(_)) => Default(name)
      | (Some(name), None) => Labeled(name)
      | (None, None) => Unlabeled
      | (None, Some(_)) =>
        raise(SyntaxError(loc, "Default arguments must be named."))
      };
    let pla_pattern = pattern;
    let pla_default = default;
    {pla_label, pla_default, pla_pattern, pla_loc: loc};
  };
};

module ModuleDeclaration = {
  let mk = (~loc, name, stmts) => {
    {pmod_name: name, pmod_stmts: stmts, pmod_loc: loc};
  };
};

module Attribute = {
  let mk = (~loc, attr_name, attr_args) => {
    {
      Asttypes.attr_name,
      attr_args: List.map(normalize_string(~loc), attr_args),
      attr_loc: loc,
    };
  };
};
