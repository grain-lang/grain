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

/* Inclusion checks for the module language */

open Grain_parsing;
open Grain_utils;
open Misc;
open Path;
open Typedtree;
open Types;

type symptom =
  | Missing_field(Ident.t, Location.t, string) /* kind */
  | Value_descriptions(Ident.t, value_description, value_description)
  | Type_declarations(
      Ident.t,
      type_declaration,
      type_declaration,
      list(Includecore.type_mismatch),
    )
  | Extension_constructors(
      Ident.t,
      extension_constructor,
      extension_constructor,
    )
  | Module_types(module_type, module_type)
  | Modtype_infos(Ident.t, modtype_declaration, modtype_declaration)
  | Modtype_permutation
  | Interface_mismatch(string, string)
  /*| Class_type_declarations of
        Ident.t * class_type_declaration * class_type_declaration *
        Ctype.class_match_failure list
    | Class_declarations of
        Ident.t * class_declaration * class_declaration *
        Ctype.class_match_failure list*/
  | Unbound_modtype_path(Path.t)
  | Unbound_module_path(Path.t)
  | Invalid_module_alias(Path.t);

/* see cmi */
type module_coercion = option(unit);

type pos =
  | Module(Ident.t)
  | Modtype(Ident.t)
  | Arg(Ident.t)
  | Body(Ident.t);
type error = (list(pos), Env.t, symptom);

exception Error(list(error));

type mark =
  | Mark_both
  | Mark_positive
  | Mark_negative
  | Mark_neither;

let negate_mark =
  fun
  | Mark_both => Mark_both
  | Mark_positive => Mark_negative
  | Mark_negative => Mark_positive
  | Mark_neither => Mark_neither;

let mark_positive =
  fun
  | Mark_both
  | Mark_positive => true
  | Mark_negative
  | Mark_neither => false;

/* All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. */

/* Inclusion between value descriptions */

let value_descriptions = (~loc, env, ~mark, cxt, subst, id, vd1, vd2) => {
  Cmt_format.record_value_dependency(vd1, vd2);
  /*if mark_positive mark then
    Env.mark_value_used (Ident.name id) vd1;*/
  None;
};
/*let vd2 = Subst.value_description subst vd2 in
  try
    Includecore.value_descriptions ~loc env (Ident.name id) vd1 vd2
  with Includecore.Dont_match ->
    raise(Error[cxt, env, Value_descriptions(id, vd1, vd2)])*/

/* Inclusion between type declarations */

let type_declarations =
    (~loc, env, ~mark, ~old_env=env, cxt, subst, id, decl1, decl2) => {
  let mark = mark_positive(mark);
  /*if mark then
    Env.mark_type_used (Ident.name id) decl1;*/
  let decl2 = Subst.type_declaration(subst, decl2);
  let err =
    Includecore.type_declarations(
      ~loc,
      env,
      ~mark,
      Ident.name(id),
      decl1,
      id,
      decl2,
    );

  if (err != []) {
    raise(
      Error([(cxt, old_env, Type_declarations(id, decl1, decl2, err))]),
    );
  };
};

/* Inclusion between extension constructors */

let extension_constructors = (~loc, env, ~mark, cxt, subst, id, ext1, ext2) => {
  let mark = mark_positive(mark);
  // TODO(#324): Enable or remove
  // let ext2 = Subst.extension_constructor(subst, ext2);
  if (Includecore.extension_constructors(~loc, env, ~mark, id, ext1, ext2)) {
    ();
  } else {
    raise(Error([(cxt, env, Extension_constructors(id, ext1, ext2))]));
  };
};

/* Inclusion between class declarations */

/*let class_type_declarations ~loc ~old_env env cxt subst id decl1 decl2 =
  let decl2 = Subst.cltype_declaration subst decl2 in
  match Includeclass.class_type_declarations ~loc env decl1 decl2 with
    []     -> ()
  | reason ->
    raise(Error[cxt, old_env,
                Class_type_declarations(id, decl1, decl2, reason)])*/

/*let class_declarations ~old_env env cxt subst id decl1 decl2 =
  let decl2 = Subst.class_declaration subst decl2 in
  match Includeclass.class_declarations env decl1 decl2 with
    []     -> ()
  | reason ->
    raise(Error[cxt, old_env, Class_declarations(id, decl1, decl2, reason)])*/

/* Expand a module type identifier when possible */

exception Dont_match;

let may_expand_module_path = (env, path) =>
  try(
    {
      ignore(Env.find_modtype_expansion(path, env));
      true;
    }
  ) {
  | Not_found => false
  };

let expand_module_path = (env, cxt, path) =>
  try(Env.find_modtype_expansion(path, env)) {
  | Not_found => raise(Error([(cxt, env, Unbound_modtype_path(path))]))
  };

let expand_module_alias = (env, cxt, path) =>
  try(Env.find_module(path, None, env).md_type) {
  | Not_found => raise(Error([(cxt, env, Unbound_module_path(path))]))
  };

/*
 let rec normalize_module_path env cxt path =
   match expand_module_alias env cxt path with
     Mty_alias path' -> normalize_module_path env cxt path'
   | _ -> path
 */

/* Extract name, kind and ident from a signature item */

type field_desc =
  | Field_value(string)
  | Field_type(string)
  | Field_typext(string)
  | Field_module(string)
  | Field_modtype(string)
  | Field_class(string)
  | Field_classtype(string);

let kind_of_field_desc =
  fun
  | Field_value(_) => "value"
  | Field_type(_) => "type"
  | Field_typext(_) => "extension constructor"
  | Field_module(_) => "module"
  | Field_modtype(_) => "module type"
  | Field_class(_) => "class"
  | Field_classtype(_) => "class type";

let item_ident_name =
  fun
  | TSigValue(id, d) => (id, d.val_loc, Field_value(Ident.name(id)))
  | TSigType(id, d, _) => (id, d.type_loc, Field_type(Ident.name(id)))
  | TSigTypeExt(id, d, _) => (id, d.ext_loc, Field_typext(Ident.name(id)))
  | TSigModule(id, d, _) => (id, d.md_loc, Field_module(Ident.name(id)))
  | TSigModType(id, d) => (id, d.mtd_loc, Field_modtype(Ident.name(id)));
/*  | Sig_class(id, d, _) -> (id, d.cty_loc, Field_class(Ident.name id))*/
/*  | Sig_class_type(id, d, _) -> (id, d.clty_loc, Field_classtype(Ident.name id))*/

let is_runtime_component =
  fun
  | TSigValue(_, {val_kind: TValPrim(_)})
  | TSigType(_, _, _)
  | TSigModType(_, _) =>
    /*| Sig_class_type(_,_,_)*/
    false
  | TSigValue(_, _)
  | TSigTypeExt(_, _, _)
  | TSigModule(_, _, _) =>
    /*| Sig_class(_, _,_)*/
    true;

/* Print a coercion */

let rec print_list = (pr, ppf) =>
  fun
  | [] => ()
  | [a] => pr(ppf, a)
  | [a, ...l] => {
      pr(ppf, a);
      Format.fprintf(ppf, ";@ ");
      print_list(pr, ppf, l);
    };
let print_list = (pr, ppf, l) =>
  Format.fprintf(ppf, "[@[%a@]]", print_list(pr), l);

/*let rec print_coercion ppf c =
    let pr fmt = Format.fprintf ppf fmt in
    match c with
      Tcoerce_none -> pr "id"
    | Tcoerce_structure (fl, nl) ->
      pr "@[<2>struct@ %a@ %a@]"
        (print_list print_coercion2) fl
        (print_list print_coercion3) nl
    | Tcoerce_functor (inp, out) ->
      pr "@[<2>functor@ (%a)@ (%a)@]"
        print_coercion inp
        print_coercion out
    | Tcoerce_primitive {pc_desc; pc_env = _; pc_type}  ->
      pr "prim %s@ (%a)" pc_desc.Primitive.prim_name
        Printtyp.raw_type_expr pc_type
    | Tcoerce_alias (p, c) ->
      pr "@[<2>alias %a@ (%a)@]"
        Printtyp.path p
        print_coercion c
  and print_coercion2 ppf (n, c) =
    Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c
  and print_coercion3 ppf (i, n, c) =
    Format.fprintf ppf "@[%s, %d,@ %a@]"
      (Ident.unique_name i) n print_coercion c*/

/* Simplify a structure coercion */

let simplify_structure_coercion = (cc, id_pos_list) => None;
/*let rec is_identity_coercion pos = function
    | [] ->
      true
    | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure (cc, id_pos_list)*/

/* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. */

let rec modtypes = (~loc, env, ~mark, cxt, subst, mty1, mty2) =>
  try(try_modtypes(~loc, env, ~mark, cxt, subst, mty1, mty2)) {
  | Dont_match =>
    raise(
      Error([(cxt, env, Module_types(mty1, Subst.modtype(subst, mty2)))]),
    )
  | Error(reasons) /* as err*/ =>
    switch (mty1, mty2) {
    /*  Mty_alias _, _
        | _, Mty_alias _ -> raise err*/
    | _ =>
      raise(
        Error([
          (cxt, env, Module_types(mty1, Subst.modtype(subst, mty2))),
          ...reasons,
        ]),
      )
    }
  }

and try_modtypes = (~loc, env, ~mark, cxt, subst, mty1, mty2) =>
  switch (mty1, mty2) {
  | (TModAlias(p1), TModAlias(p2)) =>
    if (!Path.same(p1, p2)) {
      let p1 = Env.normalize_path(None, env, p1);
      let p2 = Env.normalize_path(None, env, Subst.module_path(subst, p2));
      if (!Path.same(p1, p2)) {
        ignore(raise(Dont_match));
      };
    };
    None;
  | (TModAlias(p1), _) =>
    let p1 =
      try(Env.normalize_path(Some(Location.dummy_loc), env, p1)) {
      | Env.Error(Env.Missing_module(_, _, path)) =>
        raise(Error([(cxt, env, Unbound_module_path(path))]))
      };
    let mty1 =
      Mtype.strengthen(
        ~aliasable=true,
        env,
        expand_module_alias(env, cxt, p1),
        p1,
      );
    let cc = modtypes(~loc, env, ~mark, cxt, subst, mty1, mty2);
    cc;
  | (TModIdent(p1), _) when may_expand_module_path(env, p1) =>
    try_modtypes(
      ~loc,
      env,
      ~mark,
      cxt,
      subst,
      expand_module_path(env, cxt, p1),
      mty2,
    )
  | (_, TModIdent(_)) =>
    try_modtypes2(~loc, env, ~mark, cxt, mty1, Subst.modtype(subst, mty2))
  /*| (Mty_signature sig1, Mty_signature sig2) ->
    signatures ~loc env ~mark cxt subst sig1 sig2*/
  /*| (Mty_functor(param1, None, res1), Mty_functor(_param2, None, res2)) ->
      begin
        match modtypes ~loc env ~mark (Body param1::cxt) subst res1 res2 with
        | Tcoerce_none -> Tcoerce_none
        | cc -> Tcoerce_functor (Tcoerce_none, cc)
      end
    | (Mty_functor(param1, Some arg1, res1),
       Mty_functor(param2, Some arg2, res2)) ->
      let arg2' = Subst.modtype subst arg2 in
      let cc_arg =
        modtypes ~loc env ~mark:(negate_mark mark)
          (Arg param1::cxt) Subst.identity arg2' arg1
      in
      let cc_res =
        modtypes ~loc (Env.add_module param1 arg2' env) ~mark
          (Body param1::cxt) (Subst.add_module param2 (Pident param1) subst)
          res1 res2
      in
      begin match (cc_arg, cc_res) with
          (Tcoerce_none, Tcoerce_none) -> Tcoerce_none
        | _ -> Tcoerce_functor(cc_arg, cc_res)
      end*/
  | (_, _) => raise(Dont_match)
  }

and try_modtypes2 = (~loc, env, ~mark, cxt, mty1, mty2) =>
  /* mty2 is an identifier */
  switch (mty1, mty2) {
  | (TModIdent(p1), TModIdent(p2))
      when
        Path.same(
          Env.normalize_path_prefix(None, env, p1),
          Env.normalize_path_prefix(None, env, p2),
        ) =>
    None /*Tcoerce_none*/
  | (_, TModIdent(p2)) when may_expand_module_path(env, p2) =>
    try_modtypes(
      ~loc,
      env,
      ~mark,
      cxt,
      Subst.identity,
      mty1,
      expand_module_path(env, cxt, p2),
    )
  | (_, _) => raise(Dont_match)
  }

/* Inclusion between signatures */

and signatures = (~loc, env, ~mark, cxt, subst, sig1, sig2) => {
  /* Environment used to check inclusion of components */
  let new_env = Env.add_signature(sig1, env) /*(Env.in_signature true env)*/;
  /* Keep ids for module aliases */
  let (id_pos_list, _) =
    List.fold_left(
      ((l, pos)) =>
        fun
        | TSigModule(id, _, _) => (
            [(id, pos, None /*Tcoerce_none*/), ...l],
            pos + 1,
          )
        | item => (
            l,
            if (is_runtime_component(item)) {
              pos + 1;
            } else {
              pos;
            },
          ),
      ([], 0),
      sig1,
    );
  /* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component */
  let rec build_component_table = (pos, tbl) =>
    fun
    | [] => (pos, tbl)
    | [item, ...rem] => {
        let (id, _loc, name) = item_ident_name(item);
        let nextpos =
          if (is_runtime_component(item)) {
            pos + 1;
          } else {
            pos;
          };
        build_component_table(
          nextpos,
          Tbl.add(name, (id, item, pos), tbl),
          rem,
        );
      };
  let (len1, comps1) = build_component_table(0, Tbl.empty, sig1);
  let len2 =
    List.fold_left(
      (n, i) =>
        if (is_runtime_component(i)) {
          n + 1;
        } else {
          n;
        },
      0,
      sig2,
    );

  /* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. */
  let rec pair_components = (subst, paired, unpaired) =>
    fun
    | [] =>
      switch (unpaired) {
      | [] =>
        let cc =
          signature_components(
            ~loc,
            env,
            ~mark,
            new_env,
            cxt,
            subst,
            List.rev(paired),
          );

        if (len1 == len2) {
          /* see PR#5098 */
          simplify_structure_coercion(cc, id_pos_list);
        } else {
          None;
        }; /*Tcoerce_structure (cc, id_pos_list)*/
      | _ => raise(Error(unpaired))
      }
    | [item2, ...rem] => {
        let (id2, loc, name2) = item_ident_name(item2);
        let (name2, report) =
          switch (item2, name2) {
          /*Sig_type (_, {type_manifest=None}, _), Field_type s
            when Btype.is_row_name s ->
            (* Do not report in case of failure,
               as the main type will generate an error *)
            Field_type (String.sub s 0 (String.length s - 4)), false*/
          | _ => (name2, true)
          };

        try({
          let (id1, item1, pos1) = Tbl.find(name2, comps1);
          let new_subst =
            switch (item2) {
            | TSigType(_) => Subst.add_type(id2, PIdent(id1), subst)
            | TSigModule(_) => Subst.add_module(id2, PIdent(id1), subst)
            | TSigModType(_) =>
              Subst.add_modtype(id2, TModIdent(PIdent(id1)), subst)
            | TSigValue(_)
            | TSigTypeExt(_) =>
              /*| Sig_class _ | Sig_class_type _*/
              subst
            };

          pair_components(
            new_subst,
            [(item1, item2, pos1), ...paired],
            unpaired,
            rem,
          );
        }) {
        | Not_found =>
          let unpaired =
            if (report) {
              [
                (
                  cxt,
                  env,
                  Missing_field(id2, loc, kind_of_field_desc(name2)),
                ),
                ...unpaired,
              ];
            } else {
              unpaired;
            };
          pair_components(subst, paired, unpaired, rem);
        };
      };
  /* Do the pairing and checking, and return the final coercion */
  pair_components(subst, [], [], sig2);
}

/* Inclusion between signature components */

and signature_components = (~loc, old_env, ~mark, env, cxt, subst, paired) => {
  let comps_rec = rem =>
    signature_components(~loc, old_env, ~mark, env, cxt, subst, rem);

  switch (paired) {
  | [] => []
  | [(TSigValue(id1, valdecl1), TSigValue(_id2, valdecl2), pos), ...rem] =>
    /*let cc =
        value_descriptions ~loc env ~mark cxt subst id1 valdecl1 valdecl2
      in*/
    switch (valdecl2.val_kind) {
    | TValPrim(_) => comps_rec(rem)
    | _ => [(pos, None /*cc*/), ...comps_rec(rem)]
    }
  | [(TSigType(id1, tydecl1, _), TSigType(_id2, tydecl2, _), _pos), ...rem] =>
    type_declarations(
      ~loc,
      ~old_env,
      env,
      ~mark,
      cxt,
      subst,
      id1,
      tydecl1,
      tydecl2,
    );
    comps_rec(rem);
  | [(TSigTypeExt(id1, ext1, _), TSigTypeExt(_id2, ext2, _), pos), ...rem] =>
    extension_constructors(~loc, env, ~mark, cxt, subst, id1, ext1, ext2);
    [(pos, None), ...comps_rec(rem)];
  | [(TSigModule(id1, mty1, _), TSigModule(_id2, mty2, _), pos), ...rem] =>
    let cc =
      module_declarations(~loc, env, ~mark, cxt, subst, id1, mty1, mty2);
    [(pos, cc), ...comps_rec(rem)];
  | [(TSigModType(id1, info1), TSigModType(_id2, info2), _pos), ...rem] =>
    modtype_infos(~loc, env, ~mark, cxt, subst, id1, info1, info2);
    comps_rec(rem);
  /*| (Sig_class(id1, decl1, _), Sig_class(_id2, decl2, _), pos) :: rem ->
    class_declarations ~old_env env cxt subst id1 decl1 decl2;
    (pos, Tcoerce_none) :: comps_rec rem*/
  /*| (Sig_class_type(id1, info1, _),
     Sig_class_type(_id2, info2, _), _pos) :: rem ->
    class_type_declarations ~loc ~old_env env cxt subst id1 info1 info2;
    comps_rec rem*/
  | _ => assert(false)
  };
}

and module_declarations = (~loc, env, ~mark, cxt, subst, id1, md1, md2) => {
  /*Builtin_attributes.check_deprecated_inclusion
    ~def:md1.md_loc
    ~use:md2.md_loc
    loc
    md1.md_attributes md2.md_attributes
    (Ident.name id1);*/
  let p1 = PIdent(id1);
  /*if mark_positive mark then
    Env.mark_module_used (Ident.name id1) md1.md_loc;*/
  modtypes(
    ~loc,
    env,
    ~mark,
    [Module(id1), ...cxt],
    subst,
    Mtype.strengthen(~aliasable=true, env, md1.md_type, p1),
    md2.md_type,
  );
}

/* Inclusion between module type specifications */

and modtype_infos = (~loc, env, ~mark, cxt, subst, id, info1, info2) => {
  /*Builtin_attributes.check_deprecated_inclusion
    ~def:info1.mtd_loc
    ~use:info2.mtd_loc
    loc
    info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);*/
  let info2 = Subst.modtype_declaration(subst, info2);
  let cxt' = [Modtype(id), ...cxt];
  try(
    switch (info1.mtd_type, info2.mtd_type) {
    | (None, None) => ()
    | (Some(_), None) => ()
    | (Some(mty1), Some(mty2)) =>
      check_modtype_equiv(~loc, env, ~mark, cxt', mty1, mty2)
    | (None, Some(mty2)) =>
      check_modtype_equiv(
        ~loc,
        env,
        ~mark,
        cxt',
        TModIdent(PIdent(id)),
        mty2,
      )
    }
  ) {
  | Error(reasons) =>
    raise(Error([(cxt, env, Modtype_infos(id, info1, info2)), ...reasons]))
  };
}

and check_modtype_equiv = (~loc, env, ~mark, cxt, mty1, mty2) =>
  switch (
    modtypes(~loc, env, ~mark, cxt, Subst.identity, mty1, mty2),
    modtypes(
      ~loc,
      env,
      ~mark=negate_mark(mark),
      cxt,
      Subst.identity,
      mty2,
      mty1,
    ),
  ) {
  | (None, None) =>
    /*(Tcoerce_none, Tcoerce_none)*/

    ()
  | (_c1, _c2) =>
    /* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
       print_coercion _c1 print_coercion _c2; */
    raise(Error([(cxt, env, Modtype_permutation)]))
  };

/* Simplified inclusion check between module types (for Env) */

let can_alias = (env, path) => true;
/* Elided, since no_apply is always true and is_functor_arg is always false */
/*let rec no_apply = function
    | PIdent _ -> true
    | PExternal(p, _, _) -> no_apply p
    (*| Papply _ -> false*)
  in
  no_apply path && not (Env.is_functor_arg path env)*/

let check_modtype_inclusion = (~loc, env, mty1, path1, mty2) => {
  let aliasable = can_alias(env, path1);
  ignore(
    modtypes(
      ~loc,
      env,
      ~mark=Mark_both,
      [],
      Subst.identity,
      Mtype.strengthen(~aliasable, env, mty1, path1),
      mty2,
    ),
  );
};

let () =
  Env.check_modtype_inclusion :=
    (
      (~loc, a, b, c, d) =>
        try((check_modtype_inclusion(~loc, a, b, c, d): unit)) {
        | Error(_) => raise(Not_found)
        }
    );

/* Check that an implementation of a compilation unit meets its
   interface. */

let compunit =
    (env, ~mark=Mark_both, impl_name, impl_sig, intf_name, intf_sig) =>
  try(
    signatures(
      ~loc=Location.in_file(impl_name),
      env,
      ~mark,
      [],
      Subst.identity,
      impl_sig,
      intf_sig,
    )
  ) {
  | Error(reasons) =>
    raise(
      Error([
        ([], Env.empty, Interface_mismatch(impl_name, intf_name)),
        ...reasons,
      ]),
    )
  };

/* Hide the context and substitution parameters to the outside world */

let modtypes = (~loc, env, ~mark=Mark_both, mty1, mty2) =>
  modtypes(~loc, env, ~mark, [], Subst.identity, mty1, mty2);
let signatures = (env, ~mark=Mark_both, sig1, sig2) =>
  signatures(
    ~loc=Location.dummy_loc,
    env,
    ~mark,
    [],
    Subst.identity,
    sig1,
    sig2,
  );
let type_declarations = (~loc, env, ~mark=Mark_both, id, decl1, decl2) =>
  type_declarations(~loc, env, ~mark, [], Subst.identity, id, decl1, decl2);

/*
 let modtypes env m1 m2 =
   let c = modtypes env m1 m2 in
   Format.eprintf "@[<2>modtypes@ %a@ %a =@ %a@]@."
     Printtyp.modtype m1 Printtyp.modtype m2
     print_coercion c;
   c
 */

/* Error report */

open Format;
open Printtyp;

let show_loc = (msg, ppf, loc) => {
  let pos = loc.Location.loc_start;
  if (List.mem(pos.Lexing.pos_fname, ["", "_none_", "//toplevel//"])) {
    ();
  } else {
    fprintf(ppf, "@\n@[<2>%a:@ %s@]", Location.print_loc, loc, msg);
  };
};

let show_locs = (ppf, (loc1, loc2)) => {
  show_loc("Expected declaration", ppf, loc2);
  show_loc("Actual declaration", ppf, loc1);
};

let include_err = ppf =>
  fun
  | Missing_field(id, loc, kind) => {
      fprintf(
        ppf,
        "The %s `%a' is required but not provided",
        kind,
        ident,
        id,
      );
      show_loc("Expected declaration", ppf, loc);
    }
  | Value_descriptions(id, d1, d2) => {
      fprintf(
        ppf,
        "@[<hv 2>Values do not match:@ %a@;<1 -2>is not included in@ %a@]",
        value_description(id),
        d1,
        value_description(id),
        d2,
      );
      show_locs(ppf, (d1.val_loc, d2.val_loc));
    }
  | Type_declarations(id, d1, d2, errs) =>
    fprintf(
      ppf,
      "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a@]",
      "Type declarations do not match",
      type_declaration(id),
      d1,
      "is not included in",
      type_declaration(id),
      d2,
      show_locs,
      (d1.type_loc, d2.type_loc),
      Includecore.report_type_mismatch(
        "the first",
        "the second",
        "declaration",
      ),
      errs,
    )
  | Extension_constructors(id, x1, x2) => {
      fprintf(
        ppf,
        "@[<hv 2>Extension declarations do not match:@ %a@;<1 -2>is not included in@ %a@]",
        extension_constructor(id),
        x1,
        extension_constructor(id),
        x2,
      );
      show_locs(ppf, (x1.ext_loc, x2.ext_loc));
    }
  | Module_types(mty1, mty2) =>
    fprintf(
      ppf,
      "@[<hv 2>Modules do not match:@ %a@;<1 -2>is not included in@ %a@]",
      modtype,
      mty1,
      modtype,
      mty2,
    )
  | Modtype_infos(id, d1, d2) =>
    fprintf(
      ppf,
      "@[<hv 2>Module type declarations do not match:@ %a@;<1 -2>does not match@ %a@]",
      modtype_declaration(id),
      d1,
      modtype_declaration(id),
      d2,
    )
  | Modtype_permutation =>
    fprintf(ppf, "Illegal permutation of structure fields")
  | Interface_mismatch(impl_name, intf_name) =>
    fprintf(
      ppf,
      "@[The implementation %s@ does not match the interface %s:",
      impl_name,
      intf_name,
    )
  /*| Class_type_declarations(id, d1, d2, reason) ->
      fprintf ppf
        "@[<hv 2>Class type declarations do not match:@ \
         %a@;<1 -2>does not match@ %a@]@ %a"
        (Printtyp.cltype_declaration id) d1
        (Printtyp.cltype_declaration id) d2
        Includeclass.report_error reason
    | Class_declarations(id, d1, d2, reason) ->
      fprintf ppf
        "@[<hv 2>Class declarations do not match:@ \
         %a@;<1 -2>does not match@ %a@]@ %a"
        (Printtyp.class_declaration id) d1
        (Printtyp.class_declaration id) d2
        Includeclass.report_error reason*/
  | Unbound_modtype_path(path) =>
    fprintf(ppf, "Unbound module type %a", Printtyp.path, path)
  | Unbound_module_path(path) =>
    fprintf(ppf, "Unbound module %a", Printtyp.path, path)
  | Invalid_module_alias(path) =>
    fprintf(ppf, "Module %a cannot be aliased", Printtyp.path, path);

let rec context = ppf =>
  fun
  | [Module(id), ...rem] =>
    fprintf(ppf, "@[<2>module %a%a@]", ident, id, args, rem)
  | [Modtype(id), ...rem] =>
    fprintf(ppf, "@[<2>module type %a =@ %a@]", ident, id, context_mty, rem)
  | [Body(x), ...rem] =>
    fprintf(ppf, "functor (%s) ->@ %a", argname(x), context_mty, rem)
  | [Arg(x), ...rem] =>
    fprintf(ppf, "functor (%a : %a) -> ...", ident, x, context_mty, rem)
  | [] => fprintf(ppf, "<here>")
and context_mty = ppf =>
  fun
  | [Module(_) | Modtype(_), ..._] as rem =>
    fprintf(ppf, "@[<2>sig@ %a@;<1 -2>end@]", context, rem)
  | cxt => context(ppf, cxt)
and args = ppf =>
  fun
  | [Body(x), ...rem] => fprintf(ppf, "(%s)%a", argname(x), args, rem)
  | [Arg(x), ...rem] =>
    fprintf(ppf, "(%a :@ %a) : ...", ident, x, context_mty, rem)
  | cxt => fprintf(ppf, " :@ %a", context_mty, cxt)
and argname = x => {
  let s = Ident.name(x);
  if (s == "*") {
    "";
  } else {
    s;
  };
};

let path_of_context =
  fun
  | [Module(id), ...rem] => {
      let rec subm = path => (
        fun
        | [] => path
        | [Module(id), ...rem] =>
          subm(PExternal(path, Ident.name(id)), rem)
        | _ => assert(false)
      );
      subm(PIdent(id), rem);
    }
  | _ => assert(false);

let context = (ppf, cxt) =>
  if (cxt == []) {
    ();
  } else if (List.for_all(
               fun
               | Module(_) => true
               | _ => false,
               cxt,
             )) {
    fprintf(ppf, "In module %a:@ ", path, path_of_context(cxt));
  } else {
    fprintf(ppf, "@[<hv 2>At position@ %a@]@ ", context, cxt);
  };

let include_err = (ppf, (cxt, env, err)) =>
  Printtyp.wrap_printing_env(~error=true, env, () =>
    fprintf(ppf, "@[<v>%a%a@]", context, List.rev(cxt), include_err, err)
  );

let buffer = ref(Bytes.empty);
let is_big = obj => {
  let size = Clflags.error_size^;
  size > 0
  && {
    if (Bytes.length(buffer^) < size) {
      buffer := Bytes.create(size);
    };
    try(
      {
        ignore(Marshal.to_buffer(buffer^, 0, size, obj, []));
        false;
      }
    ) {
    | _ => true
    };
  };
};

let report_error = (ppf, errs) =>
  if (errs == []) {
    ();
  } else {
    let (errs, err) = split_last(errs);
    let pe = ref(true);
    let include_err' = (ppf, (_, _, obj) as err) =>
      if (!is_big(obj)) {
        fprintf(ppf, "%a@ ", include_err, err);
      } else if (pe^) {
        fprintf(ppf, "...@ ");
        pe := false;
      };

    let print_errs = ppf => List.iter(include_err'(ppf));
    fprintf(ppf, "@[<v>%a%a@]", print_errs, errs, include_err, err);
  };

/* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. */
let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );
