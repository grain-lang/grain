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
  | Include_module_name_mismatch(string, string)
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
  | Cannot_scrape_alias(Path.t)
  | Nonrecursive_type_with_recursion(Identifier.t);

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

let include_module = (env, sod) => {
  let include_path = sod.pinc_path.txt;
  let mod_name = Env.load_pers_struct(~loc=sod.pinc_loc, include_path);
  if (mod_name != sod.pinc_module.txt) {
    raise(
      Error(
        sod.pinc_module.loc,
        env,
        Include_module_name_mismatch(sod.pinc_module.txt, mod_name),
      ),
    );
  };
  let mod_name =
    switch (sod.pinc_alias) {
    | Some({txt: alias}) => alias
    | None => mod_name
    };
  let mod_name =
    Identifier.IdentName(Location.mkloc(mod_name, sod.pinc_loc));
  let path =
    Typetexp.lookup_module(
      ~load=true,
      env,
      sod.pinc_loc,
      mod_name,
      Some(include_path),
    );
  let newenv = Env.include_module(mod_name, sod, env);

  let od = {tinc_path: path, tinc_loc: sod.pinc_loc};

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
  switch (lst, others) {
  | ([], _) => rem
  | (_, []) => failwith("impossible: map2_end")
  | ([d1, ...dl], [o1, ...ol]) => [
      fn(d1, o1),
      ...map2_end(fn, dl, ol, rem),
    ]
  };
};

let map2_rec_type = (~rec_flag, fn, decls, others, rem) =>
  switch (decls, others) {
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
  | [d1, ...dl] => map2_rec_type(~rec_flag, fn, decls, others, rem)
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
  | TSigModule(id, md, rs) when Option.is_some(md.md_filepath) =>
    // imported modules should remain intact
    TSigModule(id, md, rs)
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
          let p = PExternal(p, Ident.name(id));
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

let rec type_module = (~toplevel=false, anchor, env, statements) => {
  let process_foreign = (env, e, d, attributes, loc) => {
    let (desc, newenv) = Typedecl.transl_value_decl(env, loc, d);
    let signature =
      switch (e) {
      | Provided => Some(TSigValue(desc.tvd_id, desc.tvd_val))
      | NotProvided => None
      | Abstract => failwith("Impossible: abstract foreign")
      };
    let foreign = {
      ttop_desc: TTopForeign(desc),
      ttop_loc: loc,
      ttop_env: env,
      ttop_attributes: Typetexp.type_attributes(attributes),
    };
    (newenv, signature, foreign);
  };

  let process_primitive = (env, e, d, attributes, loc) => {
    let (defs, id, desc, newenv) = Translprim.transl_prim(env, d);
    let signature =
      switch (e) {
      | Provided => Some(TSigValue(id, desc))
      | NotProvided => None
      | Abstract => failwith("Impossible: abstract primitive")
      };
    let prim = {
      ttop_desc: TTopLet(Nonrecursive, Immutable, defs),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: Typetexp.type_attributes(attributes),
    };
    (newenv, signature, prim);
  };

  let process_include = (env, import, attributes, loc) => {
    let (_path, newenv, od) = include_module(env, import);
    (
      newenv,
      {
        ttop_desc: TTopInclude(od),
        ttop_attributes: Typetexp.type_attributes(attributes),
        ttop_loc: loc,
        ttop_env: env,
      },
    );
  };

  let process_datas = (env, data_decls, attributes, loc) => {
    // A well-formedness check would have detected issues with improper use of
    // `rec` on mutually-recursive types
    let rec_flag =
      switch (data_decls) {
      | [(_, {pdata_rec: Recursive}, _), ..._] => Recursive
      | _ => Nonrecursive
      };
    let (decls, newenv) =
      try(Typedecl.transl_data_decl(env, rec_flag, data_decls)) {
      | Typetexp.Error(_, _, Typetexp.Unbound_type_constructor(_)) =>
        // Hack to detect if `rec` should be included on this type: if it does
        // not raise an exception in `Recursive` mode but does otherwise,
        // suggest that `rec` be used
        switch (Typedecl.transl_data_decl(env, Recursive, data_decls)) {
        | exception exn => raise(exn)
        | _ =>
          let (_, {pdata_name: type_name}, _) = List.hd(data_decls);
          raise(
            Error(
              loc,
              env,
              Nonrecursive_type_with_recursion(IdentName(type_name)),
            ),
          );
        }
      };
    let ty_decls =
      map2_rec_type_with_row_types(
        ~rec_flag,
        (rs, info, e) => {
          switch (e) {
          | NotProvided => None
          | Provided => Some(TSigType(info.data_id, info.data_type, rs))
          | Abstract =>
            Some(
              TSigType(
                info.data_id,
                {
                  ...info.data_type,
                  type_kind: TDataAbstract,
                  // Removing the manifest hides the type implementation
                  // of this type from the consuming module
                  type_manifest: None,
                },
                rs,
              ),
            )
          }
        },
        decls,
        List.map(((e, _, _)) => e, data_decls),
        [],
      );
    let ty_decls = List.filter_map(decl => decl, ty_decls);
    let statement = {
      ttop_desc: TTopData(decls),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: Typetexp.type_attributes(attributes),
    };
    let newenv = enrich_type_decls(anchor, decls, env, newenv);
    (newenv, ty_decls, statement);
  };

  let process_module = (env, provide_flag, desc, attributes, loc) => {
    // The env returned here is the environment _within_ the module
    let (statements, signature, inner_env) =
      type_module(None, env, desc.pmod_stmts);
    let signature = normalize_signature(inner_env, signature);
    let mod_name = Ident.create(desc.pmod_name.txt);
    let mod_type = TModSignature(signature);
    let newenv = Env.add_module(mod_name, mod_type, None, loc, env);
    let mod_decl = Env.find_module(PIdent(mod_name), None, newenv);
    let signature =
      switch (provide_flag) {
      | Provided => Some(TSigModule(mod_name, mod_decl, TRecNot))
      | NotProvided => None
      | Abstract => failwith("Impossible: abstract module")
      };
    let statement = {
      ttop_desc:
        TTopModule({
          tmod_id: mod_name,
          tmod_decl: mod_decl,
          tmod_statements: statements,
          tmod_provided: provide_flag,
          tmod_loc: loc,
        }),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: Typetexp.type_attributes(attributes),
    };
    (newenv, signature, statement);
  };

  let rec process_let =
          (env, provide_flag, rec_flag, mut_flag, binds, attributes, loc) => {
    Ctype.init_def(Ident.current_time());
    let scope = None;
    let (defs, newenv) =
      Typecore.type_binding(env, rec_flag, mut_flag, binds, scope);
    let () =
      if (rec_flag == Recursive) {
        Typecore.check_recursive_bindings(env, defs);
      };

    let attributes = Typetexp.type_attributes(attributes);

    let idents = let_bound_idents(defs);

    let some_exported = ref(false);
    let signatures =
      List.fold_right(
        (id, sigs) =>
          if (provide_flag == Provided) {
            some_exported := true;
            [TSigValue(id, Env.find_value(PIdent(id), newenv)), ...sigs];
          } else {
            sigs;
          },
        idents,
        [],
      );
    let stmt = {
      ttop_desc: TTopLet(rec_flag, mut_flag, defs),
      ttop_loc: loc,
      ttop_env: env,
      ttop_attributes: attributes,
    };

    switch (
      List.find_opt(
        fun
        | {txt: External_name(_)} => true
        | _ => false,
        attributes,
      )
    ) {
    | Some({txt: External_name(name)}) =>
      let export =
        PProvideValue({
          name:
            Location.mknoloc(
              Identifier.IdentName(
                Location.mknoloc(Ident.name(List.hd(idents))),
              ),
            ),
          alias: Some(Location.mknoloc(Identifier.IdentName(name))),
          loc: name.loc,
        });
      let (newsignatures, stmts) =
        process_provide_value(newenv, [export], []);
      (newenv, signatures @ newsignatures, [stmt] @ stmts);
    | _ => (newenv, signatures, [stmt])
    };
  }

  and process_provide_value = (env, exports, attributes) => {
    let (sigs, values) =
      List.split @@
      List.map(
        item => {
          switch (item) {
          | PProvideValue({name: {txt: IdentName(name)}, alias, loc}) =>
            let id =
              switch (alias) {
              | Some({txt: IdentName(alias)}) => Ident.create(alias.txt)
              | Some(_) => failwith("Impossible: invalid alias")
              | None => Ident.create(name.txt)
              };
            let name = Identifier.IdentName(name);
            let (p, {val_fullpath} as desc) =
              Typetexp.find_value(env, loc, name);
            (
              TSigValue(id, desc),
              {tex_id: id, tex_path: val_fullpath, tex_loc: loc},
            );
          | _ => failwith("Impossible: non-value provide")
          }
        },
        exports,
      );
    (
      sigs,
      [
        {
          ttop_desc: TTopProvide(values),
          ttop_loc: Location.dummy_loc,
          ttop_env: env,
          ttop_attributes: Typetexp.type_attributes(attributes),
        },
      ],
    );
  };

  let process_expr = (env, expr, attributes, loc) => {
    let expr = Typecore.type_statement_expr(env, expr);
    (
      expr.exp_env,
      {
        ttop_desc: TTopExpr(expr),
        ttop_attributes: Typetexp.type_attributes(attributes),
        ttop_loc: loc,
        ttop_env: env,
      },
    );
  };

  let process_exception = (env, provide_flag, ext, attributes, loc) => {
    let (ext, newenv) = Typedecl.transl_exception(env, ext);
    let stmt = {
      ttop_desc: TTopException(ext),
      ttop_loc: loc,
      ttop_env: newenv,
      ttop_attributes: Typetexp.type_attributes(attributes),
    };
    let sign =
      switch (provide_flag) {
      | Provided =>
        Some(TSigTypeExt(ext.ext_id, ext.ext_type, TExtException))
      | NotProvided => None
      | Abstract => failwith("Impossible: abstract exception")
      };
    (newenv, sign, stmt);
  };

  let type_export_aliases = ref([]);

  let process_provide = (env, items, attributes, loc) => {
    List.fold_right(
      (item, (sigs, stmts)) =>
        switch (item) {
        | PProvideValue({name: {txt: IdentName(name)}, alias, loc}) =>
          let id =
            switch (alias) {
            | Some({txt: IdentName(alias)}) => Ident.create(alias.txt)
            | Some(_) => failwith("Impossible: invalid alias")
            | None => Ident.create(name.txt)
            };
          let name = Identifier.IdentName(name);
          let (p, {val_fullpath} as desc) =
            Typetexp.find_value(env, loc, name);
          (
            [TSigValue(id, desc), ...sigs],
            [
              {
                ttop_desc:
                  TTopProvide([
                    {tex_id: id, tex_path: val_fullpath, tex_loc: loc},
                  ]),
                ttop_loc: loc,
                ttop_env: env,
                ttop_attributes: Typetexp.type_attributes(attributes),
              },
              ...stmts,
            ],
          );
        | PProvideType({name: {txt: IdentName(name)}, alias, loc}) =>
          let (type_path, type_) =
            Typetexp.find_type(env, loc, IdentName(name));
          let id =
            switch (alias) {
            | Some({txt: IdentName(alias)}) =>
              let id = Ident.create(alias.txt);
              type_export_aliases :=
                [(type_path, PIdent(id)), ...type_export_aliases^];
              id;
            | Some(_) => failwith("Impossible: invalid alias")
            | None => Ident.create(name.txt)
            };
          let type_ =
            switch (type_path) {
            | PExternal(_) => {
                ...type_,
                type_manifest:
                  Some(Ctype.newconstr(type_path, type_.type_params)),
              }
            | PIdent(_) => type_
            };
          ([TSigType(id, type_, TRecNot), ...sigs], stmts);
        | PProvideException({name: {txt: IdentName(name)}, alias, loc}) =>
          let ext = Typetexp.find_exception(env, loc, IdentName(name));
          let id =
            switch (alias) {
            | Some({txt: IdentName(alias)}) => Ident.create(alias.txt)
            | Some(_) => failwith("Impossible: invalid alias")
            | None => Ident.create(name.txt)
            };
          ([TSigTypeExt(id, ext, TExtException), ...sigs], stmts);
        | PProvideModule({name: {txt: IdentName(name)}, alias, loc}) =>
          let (mod_path, mod_decl) =
            Typetexp.find_module(env, loc, IdentName(name));
          let create_path = (mod_path, id) => {
            Path.PExternal(mod_path, Ident.name(id));
          };
          let provided_values = ref([]);
          let rec process_module_items = (mod_path, signature) => {
            List.map(
              item => {
                switch (item) {
                | TSigValue(id, {val_internalpath, val_loc} as vd) =>
                  let path = create_path(mod_path, id);
                  provided_values :=
                    [
                      {tex_id: id, tex_path: path, tex_loc: val_loc},
                      ...provided_values^,
                    ];
                  TSigValue(id, {...vd, val_internalpath: path});
                | TSigModule(
                    id,
                    {md_type: TModSignature(signature)} as md,
                    rs,
                  ) =>
                  let signature =
                    process_module_items(
                      create_path(mod_path, id),
                      signature,
                    );
                  TSigModule(
                    id,
                    {...md, md_type: TModSignature(signature)},
                    rs,
                  );
                | TSigModule(_)
                | TSigType(_)
                | TSigTypeExt(_)
                | TSigModType(_) => item
                }
              },
              signature,
            );
          };
          let mod_decl =
            switch (mod_decl.md_type) {
            | TModSignature(signature) => {
                ...mod_decl,
                md_type:
                  TModSignature(process_module_items(mod_path, signature)),
              }
            | _ => mod_decl
            };
          let sig_ =
            switch (alias) {
            | Some({txt: IdentName(alias)}) =>
              TSigModule(Ident.create(alias.txt), mod_decl, TRecNot)
            | Some(_) => failwith("Impossible: invalid alias")
            | None => TSigModule(Ident.create(name.txt), mod_decl, TRecNot)
            };
          (
            [sig_, ...sigs],
            [
              {
                ttop_desc: TTopProvide(provided_values^),
                ttop_loc: loc,
                ttop_env: env,
                ttop_attributes: Typetexp.type_attributes(attributes),
              },
              ...stmts,
            ],
          );
        | _ => failwith("Impossible: non-value provide")
        },
      items,
      ([], []),
    );
  };

  let (final_env, signatures, statements) =
    List.fold_left(
      (
        (env, signatures, statements),
        {ptop_desc, ptop_attributes: attributes, ptop_loc: loc},
      ) =>
        switch (ptop_desc) {
        | PTopInclude(i) =>
          let (new_env, stmt) = process_include(env, i, attributes, loc);
          (new_env, signatures, [stmt, ...statements]);
        | PTopProvide(items) =>
          let (sigs, stmts) = process_provide(env, items, attributes, loc);
          (env, List.rev(sigs) @ signatures, List.rev(stmts) @ statements);
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
        | PTopModule(e, d) =>
          let (new_env, signature, statement) =
            process_module(env, e, d, attributes, loc);
          let signatures =
            switch (signature) {
            | Some(s) => [s, ...signatures]
            | None => signatures
            };
          (new_env, signatures, [statement, ...statements]);
        | PTopLet(e, r, m, vb) =>
          let (new_env, sigs, stmts) =
            process_let(env, e, r, m, vb, attributes, loc);
          (new_env, List.rev(sigs) @ signatures, stmts @ statements);
        | PTopExpr(e) =>
          let (new_env, statement) = process_expr(env, e, attributes, loc);
          (new_env, signatures, [statement, ...statements]);
        | PTopException(e, d) =>
          let (new_env, signature, statement) =
            process_exception(env, e, d.ptyexn_constructor, attributes, loc);
          let signatures =
            switch (signature) {
            | Some(s) => [s, ...signatures]
            | None => signatures
            };
          (new_env, signatures, [statement, ...statements]);
        },
      (env, [], []),
      statements,
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
              List.map(((l, arg)) => (l, resolve_type_expr(arg)), args),
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
                      | TConstrRecord(rfs) =>
                        TConstrRecord(
                          List.map(
                            rf =>
                              {
                                ...rf,
                                Types.rf_type:
                                  resolve_type_expr(rf.Types.rf_type),
                              },
                            rfs,
                          ),
                        )
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
        TSigType(
          Ident.create(Path.last(alias)),
          resolve_type_decl(decl),
          rs,
        )
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

  (statements, signatures, final_env);
};

let type_module = type_module(None);

let register_implicit_modules = modules => {
  List.iter(
    m => {
      let filepath =
        switch (m) {
        | Grain_utils.Config.Pervasives_mod => "pervasives.gr"
        | Grain_utils.Config.Gc_mod => "runtime/gc.gr"
        };
      Env.add_import(filepath);
    },
    modules,
  );
};

let lookup_implicit_module_spec = m =>
  switch (m) {
  | Grain_utils.Config.Pervasives_mod => Some(("Pervasives", "pervasives.gr"))
  | Grain_utils.Config.Gc_mod => None
  };

let get_current_used_implicit_modules = implicit_opens =>
  List.filter_map(lookup_implicit_module_spec, implicit_opens);

let use_implicit_module = (m, env) => {
  open Asttypes;
  let loc = Location.dummy_loc;
  let (modname, filename) = m;
  let filepath = Some(filename);
  let ident = Identifier.IdentName(Location.mknoloc(modname));
  let path = Typetexp.lookup_module(~load=true, env, loc, ident, filepath);
  let include_desc = {
    pinc_path: Location.mknoloc(filename),
    pinc_module: Location.mknoloc(modname),
    pinc_alias: None,
    pinc_loc: loc,
  };
  let env = Env.include_module(ident, include_desc, env);
  Env.use_full_signature_of_initially_included_module(path, env);
};

let initial_env = () => {
  Ident.reinit();
  let initial = Env.initial_env;
  let env = initial;
  let implicit_modules = Grain_utils.Config.get_implicit_opens();
  register_implicit_modules(implicit_modules);
  let (unit_name, source, mode) = Env.get_unit();
  List.fold_left(
    (env, m) => {
      let (modname, _) = m;
      if (unit_name != modname) {
        use_implicit_module(m, env);
      } else {
        env;
      };
    },
    env,
    get_current_used_implicit_modules(implicit_modules),
  );
};

let type_implementation = prog => {
  let sourcefile = prog.prog_loc.loc_start.pos_fname;
  let module_name = prog.module_name.txt;
  Env.set_unit((
    module_name,
    sourcefile,
    Grain_utils.Config.compilation_mode^,
  ));
  let initenv = initial_env();
  let (statements, sg, finalenv) = type_module(initenv, prog.statements);
  let simple_sg = simplify_signature(sg);
  let filename = sourcefile; // TODO(#1396): Don't use filepath as filename

  check_nongen_schemes(finalenv, simple_sg);
  let normalized_sig = normalize_signature(finalenv, simple_sg);
  // Use placeholder for now; will be populated later in compilation
  let type_metadata =
    Cmi_format.{ctm_metadata: "", ctm_exceptions: "", ctm_offsets_tbl: []};
  let signature =
    Env.build_signature(normalized_sig, module_name, filename, type_metadata);
  {
    module_name: prog.module_name,
    statements,
    env: finalenv,
    signature,
    comments: prog.comments,
  };
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
  | Include_module_name_mismatch(provided_name, actual_name) =>
    fprintf(
      ppf,
      "This statement includes module %s, but the file at the path defines module %s. Did you mean `include %s as %s`?",
      provided_name,
      actual_name,
      actual_name,
      provided_name,
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
    fprintf(ppf, "This is an alias for module %a, which is missing", path, p)
  | Nonrecursive_type_with_recursion(lid) =>
    fprintf(
      ppf,
      "Unbound type constructor %a. Are you missing the `rec` keyword on this type?",
      identifier,
      lid,
    );

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
