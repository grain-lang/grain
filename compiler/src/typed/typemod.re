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

open Grain_parsing;
open Misc;
open Longident;
open Path;
open Asttypes;
open Parsetree;
open Types;
open Format;

module String = Misc.Stdlib.String;

type error =
  | Cannot_apply(module_type)
  | Not_included(list(Includemod.error))
  | Cannot_eliminate_dependency(module_type)
  | Signature_expected
  | Structure_expected(module_type)
  | With_no_component(Identifier.t)
  | With_mismatch(Identifier.t, list(Includemod.error))
  | With_makes_applicative_functor_ill_typed(
      Identifier.t,
      Path.t,
      list(Includemod.error),
    )
  | With_changes_module_alias(Identifier.t, Ident.t, Path.t)
  | With_cannot_remove_constrained_type
  | Repeated_name(string, string)
  | Non_generalizable(type_expr)
  | Non_generalizable_module(module_type)
  | Implementation_is_required(string)
  | Interface_not_compiled(string)
  | Not_allowed_in_functor_body
  | Not_a_packed_module(type_expr)
  | Incomplete_packed_module(type_expr)
  | Scoping_pack(Identifier.t, type_expr)
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias(Path.t);

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

open Typedtree;

let extract_sig = (env, loc, mty) =>
  switch (Env.scrape_alias(env, mty)) {
  | TModSignature(sg) => sg
  | TModAlias(path) => raise(Error(loc, env, Cannot_scrape_alias(path)))
  | _ => raise(Error(loc, env, Signature_expected))
  };

let extract_sig_open = (env, loc, mty) =>
  switch (Env.scrape_alias(env, mty)) {
  | TModSignature(sg) => sg
  | TModAlias(path) => raise(Error(loc, env, Cannot_scrape_alias(path)))
  | mty => raise(Error(loc, env, Structure_expected(mty)))
  };

/* Compute the environment after opening a module */

let type_open_ = (~used_slot=?, ~toplevel=?, env, mod_) => {
  let filepath = Some(mod_.pimp_path.txt);
  let mod_name =
    switch (
      List.find_map(
        fun
        | PImportModule(id) => Some(id)
        | _ => None,
        mod_.pimp_val,
      )
    ) {
    | Some({txt: IdentName(name)}) => name
    | Some(_) => failwith("multilevel mod name")
    | None =>
      "%" ++ Grain_utils.Files.filename_to_module_name(mod_.pimp_path.txt)
    };
  let mod_name = Identifier.IdentName(mod_name);
  let path =
    Typetexp.lookup_module(
      ~load=true,
      env,
      mod_.pimp_loc,
      mod_name,
      filepath,
    );
  switch (
    Env.open_signature(~used_slot?, ~toplevel?, path, mod_name, mod_, env)
  ) {
  | Some(env) => (path, env)
  | None =>
    let md = Env.find_module(path, filepath, env);
    ignore(extract_sig_open(env, mod_.pimp_loc, md.md_type));
    assert(false);
  };
};

let type_initially_opened_module = (env, module_name, module_path) => {
  let loc = Location.in_file("compiler internals");
  let lid = {Asttypes.loc, txt: Identifier.IdentName(module_name)};
  let path =
    Typetexp.lookup_module(~load=true, env, lid.loc, lid.txt, module_path);
  switch (
    Env.open_signature_of_initially_opened_module(path, module_path, env)
  ) {
  | Some(env) => (path, env)
  | None =>
    let md = Env.find_module(path, module_path, env);
    ignore(extract_sig_open(env, lid.loc, md.md_type));
    assert(false);
  };
};

let type_open = (~toplevel=?, env, sod) => {
  let (path, newenv) =
    /*Builtin_attributes.warning_scope sod.popen_attributes
      (fun () ->*/
    type_open_(~toplevel?, env, sod);
  /* )*/

  let od = {
    /*open_override = sod.popen_override;*/
    timp_path: path,
    /*open_attributes = sod.popen_attributes;*/
    timp_loc: sod.pimp_loc,
  };

  (path, newenv, od);
};

/* Simplify multiple specifications of a value or an extension in a signature.
   (Other signature components, e.g. types, modules, etc, are checked for
   name uniqueness.)  If multiple specifications with the same name,
   keep only the last (rightmost) one. */

let simplify_signature = sg => {
  let rec aux =
    fun
    | [] => ([], String.Set.empty, String.Set.empty)
    | [TSigValue(id, _descr) as component, ...sg] => {
        let (sg, val_names, ext_names) as k = aux(sg);
        let name = Ident.name(id);
        if (String.Set.mem(name, val_names)) {
          k;
        } else {
          ([component, ...sg], String.Set.add(name, val_names), ext_names);
        };
      }
    | [TSigTypeExt(id, _descr, _stat) as component, ...sg] => {
        let (sg, val_names, ext_names) as k = aux(sg);
        let name = Ident.name(id);
        if (String.Set.mem(name, ext_names)) {
          k;
        } else {
          ([component, ...sg], val_names, String.Set.add(name, ext_names));
        };
      }
    | [component, ...sg] => {
        let (sg, val_names, ext_names) = aux(sg);
        ([component, ...sg], val_names, ext_names);
      };

  let (sg, _, _) = aux(sg);
  sg;
};

/* Add recursion flags on declarations arising from a mutually recursive
   block. */

let map_rec = (fn, decls, rem) =>
  switch (decls) {
  | [] => rem
  | [d1, ...dl] => [fn(TRecFirst, d1), ...map_end(fn(TRecNext), dl, rem)]
  };

let map_rec_type = (~rec_flag, fn, decls, rem) =>
  switch (decls) {
  | [] => rem
  | [d1, ...dl] =>
    let first =
      switch (rec_flag) {
      | Recursive => TRecFirst
      | Nonrecursive => TRecNot
      };
    [fn(first, d1), ...map_end(fn(TRecNext), dl, rem)];
  };

let rec map2_end = (fn, lst, others, rem) => {
  switch((lst, others)) {
  | ([], _) => rem
  | (_, []) => failwith("impossible: map2_end")
  | ([d1, ...dl], [o1, ...ol]) => [fn(d1, o1), ...map2_end(fn, dl, ol, rem)]
  }
};

let map2_rec_type = (~rec_flag, fn, decls, others, rem) =>
  switch ((decls, others)) {
  | ([], _) => rem
  | (_, []) => failwith("impossible: map2_rec_type")
  | ([d1, ...dl], [o1, ...ol]) =>
    let first =
      switch (rec_flag) {
      | Recursive => TRecFirst
      | Nonrecursive => TRecNot
      };

    [fn(first, d1, o1), ...map2_end(fn(TRecNext), dl, ol, rem)];
  };

let rec map_rec_type_with_row_types = (~rec_flag, fn, decls, rem) =>
  switch (decls) {
  | [] => rem
  | [d1, ...dl] =>
    /*if Btype.is_row_name (Ident.name d1.typ_id) then
        fn Trec_not d1 :: map_rec_type_with_row_types ~rec_flag fn dl rem
      else*/
    map_rec_type(~rec_flag, fn, decls, rem)
  };

let rec map2_rec_type_with_row_types = (~rec_flag, fn, decls, others, rem) =>
  switch (decls) {
  | [] => rem
  | [d1, ...dl] =>
    /*if Btype.is_row_name (Ident.name d1.typ_id) then
        fn Trec_not d1 :: map_rec_type_with_row_types ~rec_flag fn dl rem
      else*/
    map2_rec_type(~rec_flag, fn, decls, others, rem)
  };

/* Auxiliaries for checking uniqueness of names in signatures and structures */

module StringSet =
  Set.Make({
    type t = string;
    let compare = (x: t, y) => String.compare(x, y);
  });

let check = (cl, loc, set_ref, name) =>
  if (StringSet.mem(name, set_ref^)) {
    raise(Error(loc, Env.empty, Repeated_name(cl, name)));
  } else {
    set_ref := StringSet.add(name, set_ref^);
  };

type names = {
  types: ref(StringSet.t),
  modules: ref(StringSet.t),
  modtypes: ref(StringSet.t),
  typexts: ref(StringSet.t),
};

let new_names = () => {
  types: ref(StringSet.empty),
  modules: ref(StringSet.empty),
  modtypes: ref(StringSet.empty),
  typexts: ref(StringSet.empty),
};

let check_name = (check, names, name) => check(names, name.loc, name.txt);
let check_type = (names, loc, s) => check("type", loc, names.types, s);
let check_module = (names, loc, s) => check("module", loc, names.modules, s);
let check_modtype = (names, loc, s) =>
  check("module type", loc, names.modtypes, s);

let check_sig_item = (names, loc) =>
  fun
  | TSigType(id, _, _) => check_type(names, loc, Ident.name(id))
  | TSigModule(id, _, _) => check_module(names, loc, Ident.name(id))
  | TSigModType(id, _) => check_modtype(names, loc, Ident.name(id))
  | _ => ();

/* Check that all core type schemes in a structure are closed */

let rec closed_modtype = env =>
  fun
  | TModIdent(_) => true
  | TModAlias(_) => true
  | TModSignature(sg) => {
      let env = Env.add_signature(sg, env);
      List.for_all(closed_signature_item(env), sg);
    }
/*| Mty_functor(id, param, body) ->
  let env = Env.add_module ~arg:true id (Btype.default_mty param) env in
  closed_modtype env body*/

and closed_signature_item = env =>
  fun
  | TSigValue(_id, desc) => Ctype.closed_schema(env, desc.val_type)
  | TSigModule(_id, md, _) => closed_modtype(env, md.md_type)
  | _ => true;

let check_nongen_scheme = (env, sig_item) =>
  switch (sig_item) {
  | TSigValue(_id, vd) =>
    if (!Ctype.closed_schema(env, vd.val_type)) {
      raise(Error(vd.val_loc, env, Non_generalizable(vd.val_type)));
    }
  | TSigModule(_id, md, _) =>
    if (!closed_modtype(env, md.md_type)) {
      raise(Error(md.md_loc, env, Non_generalizable_module(md.md_type)));
    }
  | _ => ()
  };

let check_nongen_schemes = (env, sg) =>
  List.iter(check_nongen_scheme(env), sg);

/* Normalize types in a signature */

let rec normalize_modtype = (env, modty) =>
  switch (modty) {
  | TModIdent(_)
  | TModAlias(_) => modty
  | TModSignature(sg) => TModSignature(normalize_signature(env, sg))
  }
/*| Mty_functor(_id, _param, body) -> normalize_modtype env body*/

and normalize_signature = env => List.map(normalize_signature_item(env))

and normalize_signature_item = (env, item) =>
  switch (item) {
  | TSigValue(id, desc) =>
    Ctype.normalize_type(env, desc.val_type);
    let desc = {
      ...desc,
      val_repr: Type_utils.repr_of_type(env, desc.val_type),
    };
    TSigValue(id, desc);
  | TSigModule(id, md, rs) =>
    TSigModule(id, {...md, md_type: normalize_modtype(env, md.md_type)}, rs)
  | _ => item
  };

let enrich_type_decls = (anchor, decls, oldenv, newenv) =>
  switch (anchor) {
  | None => newenv
  | Some(p) =>
    List.fold_left(
      (e, info) => {
        let id = info.data_id;
        let info' = {
          let p = PExternal(p, Ident.name(id), nopos);
          let decl = info.data_type;
          switch (decl.type_manifest) {
          | Some(_) => decl
          | None =>
            try({
              let orig_decl = Env.find_type(p, oldenv);
              if (orig_decl.type_arity != decl.type_arity) {
                decl;
              } else {
                {
                  ...decl,
                  type_manifest:
                    Some(
                      Btype.newgenty(
                        TTyConstr(p, decl.type_params, ref(TMemNil)),
                      ),
                    ),
                };
              };
            }) {
            | Not_found => decl
            }
          };
        };

        Env.add_type(~check=true, id, info', e);
      },
      oldenv,
      decls,
    )
  };

let type_module = (~toplevel=false, funct_body, anchor, env, sstr /*scope*/) => {
  let export_all = ref((false, [], []));
  List.iter(
    ({ptop_desc}) =>
      /* Take the last export *; after well-formedness there should only be one */
      switch (ptop_desc) {
      | PTopExportAll(excepts) =>
        export_all := (true, [], []);
        List.iter(
          except =>
            switch (except) {
            | ExportExceptData(name) =>
              let (_, values, datas) = export_all^;
              export_all := (true, values, [name, ...datas]);
            | ExportExceptValue(name) =>
              let (_, values, datas) = export_all^;
              export_all := (true, [name, ...values], datas);
            },
          excepts,
        );
      | _ => ()
      },
    sstr.Parsetree.statements,
  );

  let string_needs_export = (str: Grain_parsing.Parsetree.loc(string)) => {
    let (flag, excepts, _) = export_all^;
    flag && (!) @@ List.exists(({txt}) => txt == str.txt, excepts);
  };

  let ident_needs_export = (id: Ident.t) => {
    let (flag, excepts, _) = export_all^;
    flag && (!) @@ List.exists(except_id => id.name == except_id.txt, excepts);
  };

  let data_needs_export = (str: Grain_parsing.Parsetree.loc(string)) => {
    let (flag, _, excepts) = export_all^;
    flag && (!) @@ List.exists(({txt}) => txt == str.txt, excepts);
  };

  let process_foreign = (env, e, d, attributes, loc) => {
    let (desc, newenv) = Typedecl.transl_value_decl(env, loc, d);
    let e =
      if (string_needs_export(d.pval_name)) {
        Exported;
      } else {
        e;
      };
    let signature =
      switch (e) {
      | Exported => Some(TSigValue(desc.tvd_id, desc.tvd_val))
      | Nonexported => None
      };
    let foreign = {
      ttop_desc: TTopForeign(e, desc),
      ttop_loc: loc,
      ttop_env: env,
      ttop_attributes: attributes,
    };
    (newenv, signature, foreign);
  };

  let process_primitive = (env, e, d, attributes, loc) => {
    let (desc, newenv) = Typedecl.transl_value_decl(env, loc, d);
    let e =
      if (string_needs_export(d.pval_name)) {
        Exported;
      } else {
        e;
      };
    let signature =
      switch (e) {
      | Exported => Some(TSigValue(desc.tvd_id, desc.tvd_val))
      | Nonexported => None
      };
    let (defs, newenv) = Translprim.transl_prim(newenv, desc);
    let prim = {
      ttop_desc: TTopLet(e, Nonrecursive, Immutable, defs),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: attributes,
    };
    (newenv, signature, prim);
  };

  let process_import = (env, import, attributes, loc) => {
    let (_path, newenv, od) = type_open(env, import);
    (
      newenv,
      {
        ttop_desc: TTopImport(od),
        ttop_attributes: attributes,
        ttop_loc: loc,
        ttop_env: env,
      },
    );
  };

  let process_datas = (env, data_decls, attributes, loc) => {
    // e, datas
    let (decls, newenv) = Typedecl.transl_data_decl(env, Recursive, List.map(((_, d)) => d, data_decls));
    let ty_decl =
      map2_rec_type_with_row_types(
        ~rec_flag=Recursive,
        (rs, info, e) => {
          let e =
            if (data_needs_export(info.data_name)) {
              Exported;
            } else {
              e;
            };
          switch (e) {
          | Exported => TSigType(info.data_id, info.data_type, rs)
          | Nonexported =>
            TSigType(
              info.data_id,
              {...info.data_type, type_kind: TDataAbstract},
              rs,
            )
          };
        },
        decls,
        List.map(((e, _)) => e, data_decls),
        [],
      );
    let statement = {
      ttop_desc: TTopData(decls),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: attributes,
    };
    let newenv = enrich_type_decls(anchor, decls, env, newenv);
    (newenv, ty_decl, statement);
  };

  let process_let =
      (env, export_flag, rec_flag, mut_flag, binds, attributes, loc) => {
    Ctype.init_def(Ident.current_time());
    let scope = None;
    let (defs, newenv) =
      Typecore.type_binding(env, rec_flag, mut_flag, binds, scope);
    let () =
      if (rec_flag == Recursive) {
        Typecore.check_recursive_bindings(env, defs);
      };

    let some_exported = ref(false);
    let signatures =
      List.fold_right(
        (id, sigs) =>
          if (ident_needs_export(id) || export_flag == Exported) {
            some_exported := true;
            [TSigValue(id, Env.find_value(PIdent(id), newenv)), ...sigs];
          } else {
            sigs;
          },
        let_bound_idents(defs),
        [],
      );
    let export_flag =
      if (some_exported^) {
        Exported;
      } else {
        export_flag;
      };
    let stmt = {
      ttop_desc: TTopLet(export_flag, rec_flag, mut_flag, defs),
      ttop_loc: loc,
      ttop_env: env,
      ttop_attributes: attributes,
    };
    (newenv, signatures, [stmt]);
  };

  let process_expr = (env, expr, attributes, loc) => {
    let expr = Typecore.type_statement_expr(env, expr);
    {
      ttop_desc: TTopExpr(expr),
      ttop_attributes: attributes,
      ttop_loc: loc,
      ttop_env: env,
    };
  };

  let process_exception = (env, export_flag, ext, attributes, loc) => {
    let (ext, newenv) = Typedecl.transl_exception(env, ext);
    let stmt = {
      ttop_desc: TTopException(export_flag, ext),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: attributes,
    };
    let export_flag =
      if (string_needs_export(ext.ext_name)) {
        Exported;
      } else {
        export_flag;
      };
    let sign =
      switch (export_flag) {
      | Exported =>
        Some(TSigTypeExt(ext.ext_id, ext.ext_type, TExtException))
      | Nonexported => None
      };
    (newenv, sign, stmt);
  };

  let process_export_value = (env, exports, loc) => {
    let bindings =
      List.map(
        ({pex_name: name, pex_alias: alias, pex_loc: loc}) => {
          let exported_name =
            switch (alias) {
            | Some(alias) => alias.txt
            | None => name.txt
            };
          let name = {txt: Identifier.IdentName(name.txt), loc};
          let bind_name = {txt: exported_name, loc};
          {
            pvb_pat: {
              ppat_desc: PPatVar(bind_name),
              ppat_loc: loc,
            },
            pvb_expr: {
              pexp_loc: loc,
              pexp_desc: PExpId(name),
              pexp_attributes: [],
            },
            pvb_loc: loc,
          };
        },
        exports,
      );
    process_let(env, Exported, Nonrecursive, Immutable, bindings, loc);
  };

  let type_export_aliases = ref([]);
  let process_export_data = (env, exports, loc) => {
    let process_one = (rs, {pex_name: name, pex_alias: alias, pex_loc: loc}) => {
      let type_id = Env.lookup_type(IdentName(name.txt), env);
      switch (alias) {
      | Some(alias) =>
        type_export_aliases :=
          [
            (type_id, PIdent(Ident.create(alias.txt))),
            ...type_export_aliases^,
          ]
      | None => ()
      };
      let type_ = Env.find_type(type_id, env);
      TSigType(Path.head(type_id), type_, rs);
    };
    if (List.length(exports) > 1) {
      [
        process_one(TRecFirst, List.hd(exports)),
        ...List.map(process_one(TRecNext), List.tl(exports)),
      ];
    } else {
      List.map(process_one(TRecNot), exports);
    };
  };

  let process_export = (env, exports, attributes, loc) => {
    let (values, datas) =
      List.fold_right(
        (export, (values, datas)) =>
          switch (export) {
          | ExportValue(desc) => ([desc, ...values], datas)
          | ExportData(desc) => (values, [desc, ...datas])
          },
        exports,
        ([], []),
      );
    let data_sigs =
      if (List.length(datas) > 0) {
        process_export_data(env, datas, loc);
      } else {
        [];
      };
    let (env, sigs, stmts) =
      if (List.length(values) > 0) {
        process_export_value(env, values, attributes, loc);
      } else {
        (env, [], []);
      };
    (env, data_sigs @ sigs, stmts);
  };

  let (final_env, signatures, statements) =
    List.fold_left(
      (
        (env, signatures, statements),
        {ptop_desc, ptop_attributes: attributes, ptop_loc: loc},
      ) =>
        switch (ptop_desc) {
        | PTopImport(i) =>
          let (new_env, stmt) = process_import(env, i, attributes, loc);
          (new_env, signatures, [stmt, ...statements]);
        | PTopExport(ex) =>
          let (new_env, sigs, stmts) =
            process_export(env, ex, attributes, loc);
          (new_env, List.rev(sigs) @ signatures, stmts @ statements);
        | PTopForeign(e, d) =>
          let (new_env, signature, statement) =
            process_foreign(env, e, d, attributes, loc);
          let signatures =
            switch (signature) {
            | Some(s) => [s, ...signatures]
            | None => signatures
            };
          (new_env, signatures, [statement, ...statements]);
        | PTopPrimitive(e, d) =>
          let (new_env, signature, statement) =
            process_primitive(env, e, d, attributes, loc);
          let signatures =
            switch (signature) {
            | Some(s) => [s, ...signatures]
            | None => signatures
            };
          (new_env, signatures, [statement, ...statements]);
        | PTopData(data_decls) =>
          let (new_env, sigs, statement) =
            process_datas(env, data_decls, attributes, loc);
          (
            new_env,
            List.rev(sigs) @ signatures,
            [statement, ...statements],
          );
        | PTopLet(e, r, m, vb) =>
          let (new_env, sigs, stmts) =
            process_let(env, e, r, m, vb, attributes, loc);
          (new_env, List.rev(sigs) @ signatures, stmts @ statements);
        | PTopExpr(e) =>
          let statement = process_expr(env, e, attributes, loc);
          (env, signatures, [statement, ...statements]);
        | PTopException(e, d) =>
          let (new_env, signature, statement) =
            process_exception(env, e, d.ptyexn_constructor, attributes, loc);
          let signatures =
            switch (signature) {
            | Some(s) => [s, ...signatures]
            | None => signatures
            };
          (new_env, signatures, [statement, ...statements]);
        | PTopExportAll(_) => (env, signatures, statements)
        },
      (env, [], []),
      sstr.Parsetree.statements,
    );
  let (signatures, statements) = (
    List.rev(signatures),
    List.rev(statements),
  );

  let resolve_type_alias = signature => {
    let rec get_alias = (aliases, id) =>
      switch (aliases) {
      | [(name, alias), ...rest] =>
        if (Path.same(name, id)) {
          Some((name, alias));
        } else {
          get_alias(rest, id);
        }
      | [] => None
      };
    let rec resolve_type_expr = ({desc} as expr) =>
      switch (desc) {
      | TTyVar(_)
      | TTyUniVar(_) => expr
      | TTyLink(link) => {...expr, desc: TTyLink(resolve_type_expr(link))}
      | TTySubst(sub) => {...expr, desc: TTySubst(resolve_type_expr(sub))}
      | TTyArrow(args, result, c) => {
          ...expr,
          desc:
            TTyArrow(
              List.map(resolve_type_expr, args),
              resolve_type_expr(result),
              c,
            ),
        }
      | TTyTuple(elts) => {
          ...expr,
          desc: TTyTuple(List.map(resolve_type_expr, elts)),
        }
      | TTyRecord(elts) => {
          ...expr,
          desc:
            TTyRecord(
              List.map(
                ((name, elt)) => (name, resolve_type_expr(elt)),
                elts,
              ),
            ),
        }
      | TTyPoly(one, all) => {
          ...expr,
          desc:
            TTyPoly(
              resolve_type_expr(one),
              List.map(resolve_type_expr, all),
            ),
        }
      | TTyConstr(id, args, a) =>
        let name =
          snd @@
          Option.value(~default=(id, id)) @@
          get_alias(type_export_aliases^, id);
        {
          ...expr,
          desc: TTyConstr(name, List.map(resolve_type_expr, args), a),
        };
      };

    let resolve_type_decl = ({type_params, type_manifest, type_kind} as decl) =>
      switch (type_kind) {
      | TDataVariant(cdecls) => {
          ...decl,
          type_params: List.map(resolve_type_expr, type_params),
          type_manifest:
            switch (type_manifest) {
            | Some(expr) => Some(resolve_type_expr(expr))
            | None => None
            },
          type_kind:
            TDataVariant(
              List.map(
                ({cd_args, cd_res} as cdecl: Types.constructor_declaration) =>
                  {
                    ...cdecl,
                    cd_res:
                      switch (cd_res) {
                      | Some(expr) => Some(resolve_type_expr(expr))
                      | None => None
                      },
                    cd_args:
                      switch (cd_args) {
                      | TConstrSingleton => cd_args
                      | TConstrTuple(exprs) =>
                        TConstrTuple(List.map(resolve_type_expr, exprs))
                      },
                  },
                cdecls,
              ),
            ),
        }
      | _ => decl
      };
    switch (signature) {
    | TSigType(id, decl, rs) =>
      switch (get_alias(type_export_aliases^, PIdent(id))) {
      | None => TSigType(id, resolve_type_decl(decl), rs)
      | Some((name, alias)) =>
        TSigType(Path.head(alias), resolve_type_decl(decl), rs)
      }
    | TSigValue(id, {val_type, val_kind} as vd) =>
      let val_kind =
        switch (val_kind) {
        | TValConstructor({cstr_res, cstr_existentials, cstr_args} as cd) =>
          TValConstructor({
            ...cd,
            cstr_res: resolve_type_expr(cstr_res),
            cstr_existentials: List.map(resolve_type_expr, cstr_existentials),
            cstr_args: List.map(resolve_type_expr, cstr_args),
          })
        | _ => val_kind
        };

      TSigValue(
        id,
        {...vd, val_kind, val_type: resolve_type_expr(val_type)},
      );
    | _ => signature
    };
  };

  let signatures = List.map(resolve_type_alias, signatures);

  let run = () => {
    /* TODO: Is it really safe to drop the import statements here? */
    let stritems = (statements, final_env);
    (stritems, signatures, final_env);
  };

  run();
};

let type_module = type_module(false, None);

let lookup_implicit_module_spec = m =>
  switch (m) {
  | Grain_utils.Config.Pervasives_mod => ("Pervasives", "pervasives", true)
  | Grain_utils.Config.Gc_mod => ("GC", "runtime/gc", false)
  };

let get_current_implicit_modules = () =>
  List.map(
    lookup_implicit_module_spec,
    Grain_utils.Config.get_implicit_opens(),
  );

let open_implicit_module = (m, env, in_env) => {
  open Asttypes;
  let loc = Location.dummy_loc;
  let (modname, filename, _) = m;
  let values =
    if (in_env) {
      [
        PImportModule(Location.mknoloc(Identifier.IdentName(modname))),
        PImportAllExcept([]),
      ];
    } else {
      [];
    };
  let (_path, newenv) =
    type_open_(
      env,
      {
        pimp_path: {
          loc,
          txt: filename,
        },
        pimp_val: values,
        pimp_loc: loc,
      },
    );
  newenv;
};

let initial_env = () => {
  Ident.reinit();
  let initial = Env.initial_env;
  let env = initial;
  let (unit_name, source, mode) = Env.get_unit();
  List.fold_left(
    (env, m) => {
      let (modname, _, in_env) = m;
      if (unit_name != modname) {
        open_implicit_module(m, env, in_env);
      } else {
        env;
      };
    },
    env,
    get_current_implicit_modules(),
  );
};

let get_compilation_mode = () => {
  switch (Grain_utils.Config.compilation_mode^) {
  | Some("runtime") => Env.Runtime
  | _ => Env.Normal
  };
};

let type_implementation = prog => {
  let sourcefile = prog.prog_loc.loc_start.pos_fname;
  /* TODO: Do we maybe need a fallback here? */
  let modulename = Grain_utils.Files.filename_to_module_name(sourcefile);
  Env.set_unit((modulename, sourcefile, get_compilation_mode()));
  let initenv = initial_env();
  let (stritems, sg, finalenv) = type_module(initenv, prog);
  let (statements, env) = stritems;
  let simple_sg = simplify_signature(sg);
  let filename = sourcefile; /* TODO: I think this is okay */
  let coercion =
    Includemod.compunit(
      initenv,
      ~mark=Includemod.Mark_positive,
      sourcefile,
      sg,
      "(inferred signature)",
      simple_sg,
    );

  check_nongen_schemes(finalenv, simple_sg);
  let normalized_sig = normalize_signature(finalenv, simple_sg);
  let signature = Env.build_signature(normalized_sig, modulename, filename);
  ignore(coercion);
  {statements, env, signature};
};

/* Error report */

open Printtyp;

let report_error = ppf =>
  fun
  | Cannot_apply(mty) =>
    fprintf(
      ppf,
      "@[This module is not a functor; it has type@ %a@]",
      modtype,
      mty,
    )
  | Not_included(errs) =>
    fprintf(
      ppf,
      "@[<v>Signature mismatch:@ %a@]",
      Includemod.report_error,
      errs,
    )
  | Cannot_eliminate_dependency(mty) =>
    fprintf(
      ppf,
      "@[This functor has type@ %a@ The parameter cannot be eliminated in the result type.@  Please bind the argument to a module identifier.@]",
      modtype,
      mty,
    )
  | Signature_expected => fprintf(ppf, "This module type is not a signature")
  | Structure_expected(mty) =>
    fprintf(
      ppf,
      "@[This module is not a structure; it has type@ %a",
      modtype,
      mty,
    )
  | With_no_component(lid) =>
    fprintf(
      ppf,
      "@[The signature constrained by `with' has no component named %a@]",
      identifier,
      lid,
    )
  | With_mismatch(lid, explanation) =>
    fprintf(
      ppf,
      "@[<v>@[In this `with' constraint, the new definition of %a@ does not match its original definition@ in the constrained signature:@]@ %a@]",
      identifier,
      lid,
      Includemod.report_error,
      explanation,
    )
  | With_makes_applicative_functor_ill_typed(lid, path, explanation) =>
    fprintf(
      ppf,
      "@[<v>@[This `with' constraint on %a makes the applicative functor @ type %s ill-typed in the constrained signature:@]@ %a@]",
      identifier,
      lid,
      Path.name(path),
      Includemod.report_error,
      explanation,
    )
  | With_changes_module_alias(lid, id, path) =>
    fprintf(
      ppf,
      "@[<v>@[This `with' constraint on %a changes %s, which is aliased @ in the constrained signature (as %s)@].@]",
      identifier,
      lid,
      Path.name(path),
      Ident.name(id),
    )
  | With_cannot_remove_constrained_type =>
    fprintf(
      ppf,
      "@[<v>Destructive substitutions are not supported for constrained @ types (other than when replacing a type constructor with @ a type constructor with the same arguments).@]",
    )
  | Repeated_name(kind, name) =>
    fprintf(
      ppf,
      "@[Multiple definition of the %s name %s.@ Names must be unique in a given structure or signature.@]",
      kind,
      name,
    )
  | Non_generalizable(typ) =>
    fprintf(
      ppf,
      "@[The type of this expression,@ %a,@ contains type variables that cannot be generalized@]",
      type_scheme,
      typ,
    )
  | Non_generalizable_module(mty) =>
    fprintf(
      ppf,
      "@[The type of this module,@ %a,@ contains type variables that cannot be generalized@]",
      modtype,
      mty,
    )
  | Implementation_is_required(intf_name) =>
    fprintf(
      ppf,
      "@[The interface %a@ declares values, not just types.@ An implementation must be provided.@]",
      Location.print_filename,
      intf_name,
    )
  | Interface_not_compiled(intf_name) =>
    fprintf(
      ppf,
      "@[Could not find the .cmi file for interface@ %a.@]",
      Location.print_filename,
      intf_name,
    )
  | Not_allowed_in_functor_body =>
    fprintf(
      ppf,
      "@[This expression creates fresh types.@ %s@]",
      "It is not allowed inside applicative functors.",
    )
  | Not_a_packed_module(ty) =>
    fprintf(
      ppf,
      "This expression is not a packed module. It has type@ %a",
      type_expr,
      ty,
    )
  | Incomplete_packed_module(ty) =>
    fprintf(
      ppf,
      "The type of this packed module contains variables:@ %a",
      type_expr,
      ty,
    )
  | Scoping_pack(lid, ty) => {
      fprintf(
        ppf,
        "The type %a in this module cannot be exported.@ ",
        identifier,
        lid,
      );
      fprintf(
        ppf,
        "Its type contains local dependencies:@ %a",
        type_expr,
        ty,
      );
    }
  | Recursive_module_require_explicit_type =>
    fprintf(ppf, "Recursive modules require an explicit module type.")
  | Apply_generative =>
    fprintf(ppf, "This is a generative functor. It can only be applied to ()")
  | Cannot_scrape_alias(p) =>
    fprintf(ppf, "This is an alias for module %a, which is missing", path, p);

let report_error = (env, ppf, err) =>
  Printtyp.wrap_printing_env(~error=true, env, () => report_error(ppf, err));

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, env, err) =>
      Some(Location.error_of_printer(loc, report_error(env), err))
    | Error_forward(err) => Some(err)
    | _ => None,
  );
