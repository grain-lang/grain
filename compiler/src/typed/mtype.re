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

/* Operations on module types */

open Grain_parsing;
open Asttypes;
open Path;
open Types;

let rec scrape = (env, mty) =>
  switch (mty) {
  | TModIdent(p) =>
    try(scrape(env, Env.find_modtype_expansion(p, env))) {
    | Not_found => mty
    }
  | _ => mty
  };

let freshen = mty => Subst.modtype(Subst.identity, mty);

let rec strengthen = (~aliasable, env, mty, p) =>
  switch (scrape(env, mty)) {
  /*| TModSignature sg ->
        TModSignature(strengthen_sig ~aliasable env sg p 0)
    | Mty_functor(param, arg, res)
      when !Clflags.applicative_functors && Ident.name param <> "*" ->
        Mty_functor(param, arg,
          strengthen ~aliasable:false env res (Papply(p, PIdent param)))*/
  | mty => mty
  }

and strengthen_sig = (~aliasable, env, sg, p, pos) =>
  switch (sg) {
  | [] => []
  | [[@implicit_arity] TSigValue(_, desc) as sigelt, ...rem] =>
    let nextpos =
      switch (desc.val_kind) {
      | TValPrim(_) => pos
      | _ => pos + 1
      };

    [sigelt, ...strengthen_sig(~aliasable, env, rem, p, nextpos)];
  /*| TSigType(id, {type_kind=TDataAbstract}, _) ::
    (TSigType(id', {type_private=Private}, _) :: _ as rem)
    when Ident.name id = Ident.name id' ^ "#row" ->
      strengthen_sig ~aliasable env rem p pos*/
  | [[@implicit_arity] TSigType(id, decl, rs), ...rem] =>
    let newdecl =
      switch (decl.type_manifest, /*decl.type_private,*/ decl.type_kind) {
      | (Some(_), /*Public,*/ _) => decl
      /*| Some _, Private, (Type_record _ | Type_variant _) -> decl*/
      | _ =>
        let manif =
          Some(
            Btype.newgenty(
              [@implicit_arity]
              TTyConstr(
                [@implicit_arity] PExternal(p, Ident.name(id), nopos),
                decl.type_params,
                ref(TMemNil),
              ),
            ),
          );
        if (decl.type_kind == TDataAbstract) {
          {...decl, /*type_private = Public;*/ type_manifest: manif};
        } else {
          {...decl, type_manifest: manif};
        };
      };

    [
      [@implicit_arity] TSigType(id, newdecl, rs),
      ...strengthen_sig(~aliasable, env, rem, p, pos),
    ];
  /*| (Sig_typext _ as sigelt) :: rem ->
    sigelt :: strengthen_sig ~aliasable env rem p (pos+1)*/
  | [[@implicit_arity] TSigModule(id, md, rs), ...rem] =>
    let str =
      strengthen_decl(
        ~aliasable,
        env,
        md,
        [@implicit_arity] PExternal(p, Ident.name(id), pos),
      );

    [
      [@implicit_arity] TSigModule(id, str, rs),
      ...strengthen_sig(
           ~aliasable,
           Env.add_module_declaration(~check=false, id, md, env),
           rem,
           p,
           pos + 1,
         ),
    ];
  /* Need to add the module in case it defines manifest module types */
  | [[@implicit_arity] TSigModType(id, decl), ...rem] =>
    let newdecl =
      switch (decl.mtd_type) {
      | None => {
          ...decl,
          mtd_type:
            Some(
              TModIdent(
                [@implicit_arity] PExternal(p, Ident.name(id), nopos),
              ),
            ),
        }
      | Some(_) => decl
      };

    [
      [@implicit_arity] TSigModType(id, newdecl),
      ...strengthen_sig(
           ~aliasable,
           Env.add_modtype(id, decl, env),
           rem,
           p,
           pos,
         ),
    ];
  }
/* Need to add the module type in case it is manifest */
/*| (Sig_class _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p (pos+1)
  | (Sig_class_type _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p pos*/

and strengthen_decl = (~aliasable, env, md, p) =>
  switch (md.md_type) {
  | TModAlias(_) => md
  | _ when aliasable => {...md, md_type: TModAlias(p)}
  | mty => {...md, md_type: strengthen(~aliasable, env, mty, p)}
  };

let () = Env.strengthen := strengthen;

let scrape_for_type_of = (env, mty) => {
  let rec loop = (env, path, mty) =>
    switch (mty, path) {
    | (TModAlias(path), _) =>
      try({
        let md = Env.find_module(path, None, env);
        loop(env, Some(path), md.md_type);
      }) {
      | Not_found => mty
      }
    | (mty, Some(path)) => strengthen(~aliasable=false, env, mty, path)
    | _ => mty
    };

  loop(env, None, mty);
};

/* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. */

type variance =
  | Co
  | Contra
  | Strict;

let nondep_supertype = (env, mid, mty) => {
  let rec nondep_mty = (env, va, mty) =>
    switch (mty) {
    | TModIdent(p) =>
      if (Path.isfree(mid, p)) {
        nondep_mty(env, va, Env.find_modtype_expansion(p, env));
      } else {
        mty;
      }
    | TModAlias(p) =>
      if (Path.isfree(mid, p)) {
        nondep_mty(env, va, Env.find_module(p, None, env).md_type);
      } else {
        mty;
      }
    | TModSignature(sg) => TModSignature(nondep_sig(env, va, sg))
    }
  /*| Mty_functor(param, arg, res) ->
    let var_inv =
      match va with Co -> Contra | Contra -> Co | Strict -> Strict in
    Mty_functor(param, Option.map (nondep_mty env var_inv) arg,
                nondep_mty
                  (Env.add_module ~arg:true param
                     (Btype.default_mty arg) env) va res)*/

  and nondep_sig = (env, va) =>
    fun
    | [] => []
    | [item, ...rem] => {
        let rem' = nondep_sig(env, va, rem);
        switch (item) {
        | [@implicit_arity] TSigValue(id, d) => [
            [@implicit_arity]
            TSigValue(
              id,
              {...d, val_type: Ctype.nondep_type(env, mid, d.val_type)},
            ),
            ...rem',
          ]
        | [@implicit_arity] TSigType(id, d, rs) => [
            [@implicit_arity]
            TSigType(
              id,
              Ctype.nondep_type_decl(env, mid, id, va == Co, d),
              rs,
            ),
            ...rem',
          ]
        /*| Sig_typext(id, ext, es) ->
          Sig_typext(id, Ctype.nondep_extension_constructor env mid ext, es)
          :: rem'*/
        | [@implicit_arity] TSigModule(id, md, rs) => [
            [@implicit_arity]
            TSigModule(
              id,
              {...md, md_type: nondep_mty(env, va, md.md_type)},
              rs,
            ),
            ...rem',
          ]
        | [@implicit_arity] TSigModType(id, d) =>
          try([
            [@implicit_arity] TSigModType(id, nondep_modtype_decl(env, d)),
            ...rem',
          ]) {
          | Not_found =>
            switch (va) {
            | Co => [
                [@implicit_arity]
                TSigModType(
                  id,
                  {
                    mtd_type: None,
                    mtd_loc: Location.dummy_loc /*mtd_attributes=[]*/,
                  },
                ),
                ...rem',
              ]
            | _ => raise(Not_found)
            }
          }
        };
      }
  /*| Sig_class(id, d, rs) ->
        Sig_class(id, Ctype.nondep_class_declaration env mid d, rs)
        :: rem'
    | Sig_class_type(id, d, rs) ->
        Sig_class_type(id, Ctype.nondep_cltype_declaration env mid d, rs)
        :: rem'*/

  and nondep_modtype_decl = (env, mtd) => {
    ...mtd,
    mtd_type: Option.map(nondep_mty(env, Strict), mtd.mtd_type),
  };

  nondep_mty(env, Co, mty);
};

let enrich_typedecl = (env, p, id, decl) =>
  switch (decl.type_manifest) {
  | Some(_) => decl
  | None =>
    try({
      let orig_decl = Env.find_type(p, env);
      if (decl.type_arity != orig_decl.type_arity) {
        decl;
      } else {
        let orig_ty =
          Ctype.reify_univars(
            Btype.newgenty(
              [@implicit_arity]
              TTyConstr(p, orig_decl.type_params, ref(TMemNil)),
            ),
          );

        let new_ty =
          Ctype.reify_univars(
            Btype.newgenty(
              [@implicit_arity]
              TTyConstr(PIdent(id), decl.type_params, ref(TMemNil)),
            ),
          );

        let env = Env.add_type(~check=false, id, decl, env);
        Ctype.mcomp(env, orig_ty, new_ty);
        let orig_ty =
          Btype.newgenty(
            [@implicit_arity] TTyConstr(p, decl.type_params, ref(TMemNil)),
          );

        {...decl, type_manifest: Some(orig_ty)};
      };
    }) {
    | Not_found
    | Ctype.Unify(_) =>
      /* - Not_found: type which was not present in the signature, so we don't
         have anything to do.
         - Unify: the current declaration is not compatible with the one we
         got from the signature. We should just fail now, but then, we could
         also have failed if the arities of the two decls were different,
         which we didn't. */
      decl
    }
  };

let rec enrich_modtype = (env, p, mty) =>
  switch (mty) {
  | TModSignature(sg) => TModSignature(List.map(enrich_item(env, p), sg))
  | _ => mty
  }

and enrich_item = (env, p) =>
  fun
  | [@implicit_arity] TSigType(id, decl, rs) =>
    [@implicit_arity]
    TSigType(
      id,
      enrich_typedecl(
        env,
        [@implicit_arity] PExternal(p, Ident.name(id), nopos),
        id,
        decl,
      ),
      rs,
    )
  | [@implicit_arity] TSigModule(id, md, rs) =>
    [@implicit_arity]
    TSigModule(
      id,
      {
        ...md,
        md_type:
          enrich_modtype(
            env,
            [@implicit_arity] PExternal(p, Ident.name(id), nopos),
            md.md_type,
          ),
      },
      rs,
    )
  | item => item;

let rec type_paths = (env, p, mty) =>
  switch (scrape(env, mty)) {
  | TModIdent(_) => []
  | TModAlias(_) => []
  | TModSignature(sg) => type_paths_sig(env, p, 0, sg)
  }
/*| Mty_functor _ -> []*/

and type_paths_sig = (env, p, pos, sg) =>
  switch (sg) {
  | [] => []
  | [[@implicit_arity] TSigValue(_id, decl), ...rem] =>
    let pos' =
      switch (decl.val_kind) {
      | TValPrim(_) => pos
      | _ => pos + 1
      };
    type_paths_sig(env, p, pos', rem);
  | [[@implicit_arity] TSigType(id, _decl, _), ...rem] => [
      [@implicit_arity] PExternal(p, Ident.name(id), nopos),
      ...type_paths_sig(env, p, pos, rem),
    ]
  | [[@implicit_arity] TSigModule(id, md, _), ...rem] =>
    type_paths(
      env,
      [@implicit_arity] PExternal(p, Ident.name(id), pos),
      md.md_type,
    )
    @ type_paths_sig(
        Env.add_module_declaration(~check=false, id, md, env),
        p,
        pos + 1,
        rem,
      )
  | [[@implicit_arity] TSigModType(id, decl), ...rem] =>
    type_paths_sig(Env.add_modtype(id, decl, env), p, pos, rem)
  };
/*| (Sig_typext _ | Sig_class _) :: rem ->
  type_paths_sig env p (pos+1) rem*/
/*| (Sig_class_type _) :: rem ->
  type_paths_sig env p pos rem*/

let rec no_code_needed = (env, mty) =>
  switch (scrape(env, mty)) {
  | TModIdent(_) => false
  | TModSignature(sg) => no_code_needed_sig(env, sg)
  /*| Mty_functor(_, _, _) -> false*/
  /*| Mty_alias(Mta_absent, _) -> true*/
  | TModAlias(_) => false
  }

and no_code_needed_sig = (env, sg) =>
  switch (sg) {
  | [] => true
  | [[@implicit_arity] TSigValue(_id, decl), ...rem] =>
    switch (decl.val_kind) {
    | TValPrim(_) => no_code_needed_sig(env, rem)
    | _ => false
    }
  | [[@implicit_arity] TSigModule(id, md, _), ...rem] =>
    no_code_needed(env, md.md_type)
    && no_code_needed_sig(
         Env.add_module_declaration(~check=false, id, md, env),
         rem,
       )
  | [TSigType(_) | TSigModType(_) /*| Sig_class_type _*/, ...rem] =>
    no_code_needed_sig(env, rem)
  };
/*| (Sig_typext _ | Sig_class _) :: _ ->
  false*/

/* Check whether a module type may return types */

let rec contains_type = env =>
  fun
  | TModIdent(path) =>
    try(
      switch (Env.find_modtype(path, env).mtd_type) {
      | None => raise(Exit) /* PR#6427 */
      | Some(mty) => contains_type(env, mty)
      }
    ) {
    | Not_found => raise(Exit)
    }
  | TModSignature(sg) => contains_type_sig(env, sg)
  /*| Mty_functor (_, _, body) ->
    contains_type env body*/
  | TModAlias(_) => ()

and contains_type_sig = env => List.iter(contains_type_item(env))

and contains_type_item = env =>
  fun
  /*TSigType (_,({type_manifest = None} |
    {type_kind = TDataAbstract; type_private = Private}),_)*/
  | TSigModType(_) =>
    /* We consider that extension constructors with an inlined
       record create a type (the inlined record), even though
       it would be technically safe to ignore that considering
       the current constraints which guarantee that this type
       is kept local to expressions.  */
    /*| Sig_typext (_, {ext_args = Cstr_record _}, _)*/
    raise(Exit)
  | [@implicit_arity] TSigModule(_, {md_type: mty}, _) =>
    contains_type(env, mty)
  | TSigValue(_)
  | TSigType(_) =>
    /*| Sig_typext _*/
    /*| Sig_class _*/
    /*| Sig_class_type _*/
    ();

let contains_type = (env, mty) =>
  try(
    {
      contains_type(env, mty);
      false;
    }
  ) {
  | Exit => true
  };

/* Remove module aliases from a signature */

module PathSet = Set.Make(Path);
module PathMap = Map.Make(Path);

let rec get_prefixes =
  fun
  | PIdent(_) => PathSet.empty
  | [@implicit_arity] PExternal(p, _, _) =>
    /*| Papply (p, _)*/
    PathSet.add(p, get_prefixes(p));

let rec get_arg_paths =
  fun
  | PIdent(_) => PathSet.empty
  | [@implicit_arity] PExternal(p, _, _) => get_arg_paths(p);
/*| Papply (p1, p2) ->
  PathSet.add p2
    (PathSet.union (get_prefixes p2)
       (PathSet.union (get_arg_paths p1) (get_arg_paths p2)))*/

let rec rollback_path = (subst, p) =>
  try(PIdent(PathMap.find(p, subst))) {
  | Not_found =>
    switch (p) {
    | PIdent(_) /*| Papply _*/ => p
    | [@implicit_arity] PExternal(p1, s, n) =>
      let p1' = rollback_path(subst, p1);
      if (Path.same(p1, p1')) {
        p;
      } else {
        rollback_path(subst, [@implicit_arity] PExternal(p1', s, n));
      };
    }
  };

let rec collect_ids = (subst, bindings, p) =>
  switch (rollback_path(subst, p)) {
  | PIdent(id) =>
    let ids =
      try(collect_ids(subst, bindings, Ident.find_same(id, bindings))) {
      | Not_found => Ident.Set.empty
      };

    Ident.Set.add(id, ids);
  | _ => Ident.Set.empty
  };

let collect_arg_paths = mty => {
  open Btype;
  let paths = ref(PathSet.empty)
  and subst = ref(PathMap.empty)
  and bindings = ref(Ident.empty);
  /* let rt = Ident.create "Root" in
     and prefix = ref (Path.PIdent rt) in */
  let it_path = p => paths := PathSet.union(get_arg_paths(p), paths^)
  and it_signature_item = (it, si) => {
    type_iterators.it_signature_item(it, si);
    switch (si) {
    | [@implicit_arity] TSigModule(id, {md_type: TModAlias(p)}, _) =>
      bindings := Ident.add(id, p, bindings^)
    | [@implicit_arity] TSigModule(id, {md_type: TModSignature(sg)}, _) =>
      List.iter(
        fun
        | [@implicit_arity] TSigModule(id', _, _) =>
          subst :=
            PathMap.add(
              [@implicit_arity] PExternal(PIdent(id), Ident.name(id'), -1),
              id',
              subst^,
            )
        | _ => (),
        sg,
      )
    | _ => ()
    };
  };

  let it = {...type_iterators, it_path, it_signature_item};
  it.it_module_type(it, mty);
  it.it_module_type(unmark_iterators, mty);
  PathSet.fold(
    p => Ident.Set.union(collect_ids(subst^, bindings^, p)),
    paths^,
    Ident.Set.empty,
  );
};

let rec remove_aliases_mty = (env, excl, mty) =>
  switch (mty) {
  | TModSignature(sg) => TModSignature(remove_aliases_sig(env, excl, sg))
  | TModAlias(_) =>
    let mty' = Env.scrape_alias(env, mty);
    if (mty' == mty) {
      mty;
    } else {
      remove_aliases_mty(env, excl, mty');
    };
  | mty => mty
  }

and remove_aliases_sig = (env, excl, sg) =>
  switch (sg) {
  | [] => []
  | [[@implicit_arity] TSigModule(id, md, rs), ...rem] =>
    let mty =
      switch (md.md_type) {
      | TModAlias(_) when Ident.Set.mem(id, excl) => md.md_type
      | mty => remove_aliases_mty(env, excl, mty)
      };

    [
      [@implicit_arity] TSigModule(id, {...md, md_type: mty}, rs),
      ...remove_aliases_sig(Env.add_module(id, mty, None, env), excl, rem),
    ];
  | [[@implicit_arity] TSigModType(id, mtd), ...rem] => [
      [@implicit_arity] TSigModType(id, mtd),
      ...remove_aliases_sig(Env.add_modtype(id, mtd, env), excl, rem),
    ]
  | [it, ...rem] => [it, ...remove_aliases_sig(env, excl, rem)]
  };

let scrape_for_type_of = (~remove_aliases, env, mty) =>
  if (remove_aliases) {
    let excl = collect_arg_paths(mty);
    remove_aliases_mty(env, excl, mty);
  } else {
    scrape_for_type_of(env, mty);
  };

/* Lower non-generalizable type variables */

let lower_nongen = (nglev, mty) => {
  open Btype;
  let it_type_expr = (it, ty) => {
    let ty = repr(ty);
    switch (ty) {
    | {desc: TTyVar(_), level} =>
      if (level < generic_level && level > nglev) {
        set_level(ty, nglev);
      }
    | _ => type_iterators.it_type_expr(it, ty)
    };
  };

  let it = {...type_iterators, it_type_expr};
  it.it_module_type(it, mty);
  it.it_module_type(unmark_iterators, mty);
};
