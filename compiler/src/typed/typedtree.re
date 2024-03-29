/* Stripped-down version of OCaml's typedtree. Original copyright: */
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

open Sexplib.Conv;
open Grain_parsing;
open Types;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

type loc('a) = Location.loc('a);

[@deriving sexp]
type attributes = list(loc(attribute))

[@deriving sexp]
and attribute =
  | Disable_gc
  | Unsafe
  | External_name(loc(string));

[@deriving sexp]
type partial =
  | Partial
  | Total;

type provide_flag =
  Asttypes.provide_flag = | NotProvided | Provided | Abstract;
type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
type mut_flag = Asttypes.mut_flag = | Mutable | Immutable;
[@deriving sexp]
type argument_label =
  Asttypes.argument_label =
    | Unlabeled | Labeled(loc(string)) | Default(loc(string));

type wasm_prim_type =
  Parsetree.wasm_prim_type =
    | Wasm_int32 | Wasm_int64 | Wasm_float32 | Wasm_float64 | Grain_bool;

type wasm_op =
  Parsetree.wasm_op =
    | Op_clz_int32
    | Op_ctz_int32
    | Op_popcnt_int32
    | Op_neg_float32
    | Op_abs_float32
    | Op_ceil_float32
    | Op_floor_float32
    | Op_trunc_float32
    | Op_nearest_float32
    | Op_sqrt_float32
    | Op_eq_z_int32
    | Op_clz_int64
    | Op_ctz_int64
    | Op_popcnt_int64
    | Op_neg_float64
    | Op_abs_float64
    | Op_ceil_float64
    | Op_floor_float64
    | Op_trunc_float64
    | Op_nearest_float64
    | Op_sqrt_float64
    | Op_eq_z_int64
    | Op_extend_s_int32
    | Op_extend_u_int32
    | Op_wrap_int64
    | Op_trunc_s_float32_to_int32
    | Op_trunc_s_float32_to_int64
    | Op_trunc_u_float32_to_int32
    | Op_trunc_u_float32_to_int64
    | Op_trunc_s_float64_to_int32
    | Op_trunc_s_float64_to_int64
    | Op_trunc_u_float64_to_int32
    | Op_trunc_u_float64_to_int64
    | Op_reinterpret_float32
    | Op_reinterpret_float64
    | Op_convert_s_int32_to_float32
    | Op_convert_s_int32_to_float64
    | Op_convert_u_int32_to_float32
    | Op_convert_u_int32_to_float64
    | Op_convert_s_int64_to_float32
    | Op_convert_s_int64_to_float64
    | Op_convert_u_int64_to_float32
    | Op_convert_u_int64_to_float64
    | Op_promote_float32
    | Op_demote_float64
    | Op_reinterpret_int32
    | Op_reinterpret_int64
    | Op_extend_s8_int32
    | Op_extend_s16_int32
    | Op_extend_s8_int64
    | Op_extend_s16_int64
    | Op_extend_s32_int64
    | Op_add_int32
    | Op_sub_int32
    | Op_mul_int32
    | Op_div_s_int32
    | Op_div_u_int32
    | Op_rem_s_int32
    | Op_rem_u_int32
    | Op_and_int32
    | Op_or_int32
    | Op_xor_int32
    | Op_shl_int32
    | Op_shr_u_int32
    | Op_shr_s_int32
    | Op_rot_l_int32
    | Op_rot_r_int32
    | Op_eq_int32
    | Op_ne_int32
    | Op_lt_s_int32
    | Op_lt_u_int32
    | Op_le_s_int32
    | Op_le_u_int32
    | Op_gt_s_int32
    | Op_gt_u_int32
    | Op_ge_s_int32
    | Op_ge_u_int32
    | Op_add_int64
    | Op_sub_int64
    | Op_mul_int64
    | Op_div_s_int64
    | Op_div_u_int64
    | Op_rem_s_int64
    | Op_rem_u_int64
    | Op_and_int64
    | Op_or_int64
    | Op_xor_int64
    | Op_shl_int64
    | Op_shr_u_int64
    | Op_shr_s_int64
    | Op_rot_l_int64
    | Op_rot_r_int64
    | Op_eq_int64
    | Op_ne_int64
    | Op_lt_s_int64
    | Op_lt_u_int64
    | Op_le_s_int64
    | Op_le_u_int64
    | Op_gt_s_int64
    | Op_gt_u_int64
    | Op_ge_s_int64
    | Op_ge_u_int64
    | Op_add_float32
    | Op_sub_float32
    | Op_mul_float32
    | Op_div_float32
    | Op_copy_sign_float32
    | Op_min_float32
    | Op_max_float32
    | Op_eq_float32
    | Op_ne_float32
    | Op_lt_float32
    | Op_le_float32
    | Op_gt_float32
    | Op_ge_float32
    | Op_add_float64
    | Op_sub_float64
    | Op_mul_float64
    | Op_div_float64
    | Op_copy_sign_float64
    | Op_min_float64
    | Op_max_float64
    | Op_eq_float64
    | Op_ne_float64
    | Op_lt_float64
    | Op_le_float64
    | Op_gt_float64
    | Op_ge_float64;

type prim0 =
  Parsetree.prim0 =
    | AllocateInt32
    | AllocateInt64
    | AllocateUint32
    | AllocateUint64
    | AllocateFloat32
    | AllocateFloat64
    | AllocateRational
    | WasmMemorySize
    | Unreachable
    | HeapStart
    | HeapTypeMetadata;

type prim1 =
  Parsetree.prim1 =
    | AllocateArray
    | AllocateTuple
    | AllocateBytes
    | AllocateString
    | AllocateBigInt
    | NewInt32
    | NewInt64
    | NewUint32
    | NewUint64
    | NewFloat32
    | NewFloat64
    | BuiltinId
    | LoadAdtVariant
    | StringSize
    | BytesSize
    | TagSimpleNumber
    | UntagSimpleNumber
    | TagChar
    | UntagChar
    | TagInt8
    | UntagInt8
    | TagInt16
    | UntagInt16
    | TagUint8
    | UntagUint8
    | TagUint16
    | UntagUint16
    | Not
    | Box
    | Unbox
    | BoxBind
    | UnboxBind
    | Ignore
    | ArrayLength
    | Assert
    | Throw
    | Magic
    | WasmFromGrain
    | WasmToGrain
    | WasmUnaryI32({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryI64({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryF32({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryF64({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmMemoryGrow;

type prim2 =
  Parsetree.prim2 =
    | NewRational
    | Is
    | Eq
    | And
    | Or
    | WasmLoadI32({
        sz: int,
        signed: bool,
      })
    | WasmLoadI64({
        sz: int,
        signed: bool,
      })
    | WasmLoadF32
    | WasmLoadF64
    | WasmBinaryI32({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryI64({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryF32({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryF64({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      });

type primn =
  Parsetree.primn =
    | WasmStoreI32({sz: int})
    | WasmStoreI64({sz: int})
    | WasmStoreF32
    | WasmStoreF64
    | WasmMemoryCopy
    | WasmMemoryFill
    | WasmMemoryCompare;

let (prim0_of_sexp, sexp_of_prim0) = (
  Parsetree.prim0_of_sexp,
  Parsetree.sexp_of_prim0,
);
let (prim1_of_sexp, sexp_of_prim1) = (
  Parsetree.prim1_of_sexp,
  Parsetree.sexp_of_prim1,
);
let (prim2_of_sexp, sexp_of_prim2) = (
  Parsetree.prim2_of_sexp,
  Parsetree.sexp_of_prim2,
);
let (primn_of_sexp, sexp_of_primn) = (
  Parsetree.primn_of_sexp,
  Parsetree.sexp_of_primn,
);

[@deriving sexp]
type core_type = {
  ctyp_desc: core_type_desc,
  ctyp_type: type_expr,
  ctyp_env: [@sexp.opaque] Env.t,
  [@sexp_drop_if sexp_locs_disabled]
  ctyp_loc: Location.t,
}

[@deriving sexp]
and core_type_desc =
  | TTyAny
  | TTyVar(string)
  | TTyArrow(list((argument_label, core_type)), core_type)
  | TTyTuple(list(core_type))
  | TTyRecord(list((loc(Identifier.t), core_type)))
  | TTyConstr(Path.t, loc(Identifier.t), list(core_type))
  | TTyPoly(list(string), core_type);

[@deriving sexp]
type record_field = {
  rf_name: Ident.t,
  rf_type: core_type,
  rf_mutable: bool,
  [@sexp_drop_if sexp_locs_disabled]
  rf_loc: Location.t,
};

[@deriving sexp]
type constructor_arguments =
  | TConstrTuple(list(core_type))
  | TConstrRecord(list(record_field))
  | TConstrSingleton

[@deriving sexp]
and type_extension = {
  tyext_path: Path.t,
  tyext_txt: loc(Identifier.t),
  tyext_params: list(core_type),
  tyext_constructors: list(extension_constructor),
  tyext_loc: Location.t,
}

[@deriving sexp]
and type_exception = {
  tyexn_constructor: extension_constructor,
  tyexn_loc: Location.t,
}

[@deriving sexp]
and extension_constructor = {
  ext_id: Ident.t,
  ext_name: loc(string),
  ext_type: Types.extension_constructor,
  ext_kind: extension_constructor_kind,
  ext_loc: Location.t,
}

[@deriving sexp]
and extension_constructor_kind =
  | TExtDecl(constructor_arguments)
  | TExtRebind(Path.t, loc(Identifier.t));

[@deriving sexp]
type constructor_declaration = {
  cd_id: Ident.t,
  cd_name: loc(string),
  cd_args: constructor_arguments,
  cd_res: option(core_type),
  [@sexp_drop_if sexp_locs_disabled]
  cd_loc: Location.t,
};

[@deriving sexp]
type data_kind =
  | TDataAbstract
  | TDataVariant(list(constructor_declaration))
  | TDataRecord(list(record_field));

[@deriving sexp]
type data_declaration = {
  data_id: Ident.t,
  data_name: loc(string),
  data_params: list(core_type),
  data_type: Types.type_declaration,
  data_kind,
  data_manifest: option(core_type),
  data_provided: provide_flag,
  [@sexp_drop_if sexp_locs_disabled]
  data_loc: Location.t,
};

[@deriving sexp]
type pattern = {
  pat_desc: pattern_desc,
  [@sexp_drop_if sexp_locs_disabled]
  pat_loc: Location.t,
  [@default []] [@sexp_drop_default (==)]
  pat_extra: list((pat_extra, Location.t)),
  pat_type: type_expr,
  mutable pat_env: [@sexp.opaque] Env.t,
}

[@deriving sexp]
and pat_extra =
  | TPatConstraint(core_type)

[@deriving sexp]
and pattern_desc =
  | TPatAny
  | TPatVar(Ident.t, loc(string))
  | TPatConstant(constant)
  | TPatTuple(list(pattern))
  | TPatArray(list(pattern))
  | TPatRecord(
      list((loc(Identifier.t), label_description, pattern)),
      closed_flag,
    )
  | TPatConstruct(loc(Identifier.t), constructor_description, list(pattern))
  | TPatAlias(pattern, Ident.t, loc(string))
  | TPatOr(pattern, pattern);

[@deriving sexp]
type expression = {
  exp_desc: expression_desc,
  [@sexp_drop_if sexp_locs_disabled]
  exp_loc: Location.t,
  [@default []] [@sexp_drop_default (==)]
  exp_extra: list((exp_extra, Location.t)),
  exp_attributes: attributes,
  exp_type: type_expr,
  exp_env: [@sexp.opaque] Env.t,
}

[@deriving sexp]
and exp_extra =
  | TExpConstraint(core_type)

[@deriving sexp]
and expression_desc =
  | TExpIdent(Path.t, loc(Identifier.t), Types.value_description)
  | TExpConstant(constant)
  | TExpTuple(list(expression))
  | TExpArray(list(expression))
  | TExpArrayGet(expression, expression)
  | TExpArraySet({
      array: expression,
      index: expression,
      value: expression,
      infix_op: option(expression),
    })
  | TExpRecord(
      option(expression),
      array((Types.label_description, record_label_definition)),
    )
  | TExpRecordGet(expression, loc(Identifier.t), Types.label_description)
  | TExpRecordSet(
      expression,
      loc(Identifier.t),
      Types.label_description,
      expression,
    )
  | TExpLet(rec_flag, mut_flag, list(value_binding))
  | TExpMatch(expression, list(match_branch), partial)
  | TExpUse(loc(Path.t), use_items)
  | TExpPrim0(prim0)
  | TExpPrim1(prim1, expression)
  | TExpPrim2(prim2, expression, expression)
  | TExpPrimN(primn, list(expression))
  | TExpBoxAssign(expression, expression)
  | TExpAssign(expression, expression)
  | TExpIf(expression, expression, expression)
  | TExpWhile(expression, expression)
  | TExpFor(
      option(expression),
      option(expression),
      option(expression),
      expression,
    )
  | TExpContinue
  | TExpBreak
  | TExpReturn(option(expression))
  | TExpLambda(list(match_branch), partial)
  | TExpApp(
      expression,
      list(argument_label),
      list((argument_label, expression)),
    )
  | TExpConstruct(
      loc(Identifier.t),
      constructor_description,
      constructor_expression,
    )
  | TExpBlock(list(expression))

and constructor_expression =
  | TExpConstrTuple(list(expression))
  | TExpConstrRecord(
      array((Types.label_description, record_label_definition)),
    )

and record_label_definition =
  | Kept
  | Overridden(loc(Identifier.t), expression)

[@deriving sexp]
and value_binding = {
  vb_pat: pattern,
  vb_expr: expression,
  [@sexp_drop_if sexp_locs_disabled]
  vb_loc: Location.t,
}

[@deriving sexp]
and match_branch = {
  mb_pat: pattern,
  mb_body: expression,
  mb_guard: option(expression),
  [@sexp_drop_if sexp_locs_disabled]
  mb_loc: Location.t,
};

[@deriving sexp]
type include_declaration = {
  tinc_path: Path.t,
  [@sexp_drop_if sexp_locs_disabled]
  tinc_loc: Location.t,
};

[@deriving sexp]
type provide_declaration = {
  tex_id: Ident.t,
  tex_path: Path.t,
  [@sexp_drop_if sexp_locs_disabled]
  tex_loc: Location.t,
};

[@deriving sexp]
type value_description = {
  tvd_id: Ident.t,
  tvd_mod: loc(string),
  tvd_name: loc(string),
  tvd_desc: core_type,
  tvd_val: Types.value_description,
  [@sexp_drop_if sexp_locs_disabled]
  tvd_loc: Location.t,
};

[@deriving sexp]
type module_declaration = {
  tmod_id: Ident.t,
  tmod_decl: Types.module_declaration,
  tmod_statements: list(toplevel_stmt),
  tmod_provided: provide_flag,
  [@sexp_drop_if sexp_locs_disabled]
  tmod_loc: Location.t,
}

[@deriving sexp]
and toplevel_stmt_desc =
  | TTopForeign(value_description)
  | TTopInclude(include_declaration)
  | TTopProvide(list(provide_declaration))
  | TTopData(list(data_declaration))
  | TTopModule(module_declaration)
  | TTopLet(rec_flag, mut_flag, list(value_binding))
  | TTopException(extension_constructor)
  | TTopExpr(expression)

[@deriving sexp]
and toplevel_stmt = {
  ttop_desc: toplevel_stmt_desc,
  ttop_attributes: attributes,
  [@sexp_drop_if sexp_locs_disabled]
  ttop_loc: Location.t,
  ttop_env: [@sexp.opaque] Env.t,
};

[@deriving (sexp, yojson)]
type comment_desc =
  Parsetree.comment_desc = {
    cmt_content: string,
    cmt_source: string,
    cmt_loc: Location.t,
  };

[@deriving (sexp, yojson)]
type comment =
  Parsetree.comment =
    | Line(comment_desc)
    | Shebang(comment_desc)
    | Block(comment_desc)
    | Doc(comment_desc);

[@deriving sexp]
type typed_program = {
  module_name: loc(string),
  statements: list(toplevel_stmt),
  env: [@sexp.opaque] Env.t,
  signature: Cmi_format.cmi_infos,
  comments: list(comment),
};

let iter_pattern_desc = (f, patt) =>
  switch (patt) {
  | TPatTuple(patts)
  | TPatArray(patts)
  | TPatConstruct(_, _, patts) => List.iter(f, patts)
  | TPatRecord(fields, _) => List.iter(((_, _, p)) => f(p), fields)
  | TPatAny
  | TPatVar(_)
  | TPatConstant(_) => ()
  | TPatAlias(p, _, _) => f(p)
  | TPatOr(p1, p2) =>
    f(p1);
    f(p2);
  };

let map_pattern_desc = (f, patt) =>
  switch (patt) {
  | TPatTuple(patts) => TPatTuple(List.map(f, patts))
  | TPatArray(patts) => TPatArray(List.map(f, patts))
  | TPatRecord(fields, c) =>
    TPatRecord(List.map(((id, ld, pat)) => (id, ld, f(pat)), fields), c)
  | TPatAlias(p1, id, s) => TPatAlias(f(p1), id, s)
  | TPatConstruct(lid, c, pats) => TPatConstruct(lid, c, List.map(f, pats))
  | TPatOr(p1, p2) => TPatOr(f(p1), f(p2))
  | _ => patt
  };

let exists_pattern = (f, patt) => {
  let found = ref(false);
  if (f(patt)) {
    found := true;
  };
  iter_pattern_desc(
    pat =>
      if (f(pat)) {
        found := true;
      } else {
        ();
      },
    patt.pat_desc,
  );
  found^;
};

let pattern_bound_idents_and_locs = patt => {
  let ret = ref([]);
  let rec help = ({pat_desc: desc, _}) =>
    switch (desc) {
    | TPatVar(id, s) => ret := [(id, s), ...ret^]
    | TPatAlias(p, id, s) =>
      help(p);
      ret := [(id, s), ...ret^];
    | TPatOr(p1, _) =>
      /* Invariant: both arguments bind the same variables */
      help(p1)
    | _ => iter_pattern_desc(help, desc)
    };
  help(patt);
  ret^;
};

let pattern_bound_idents = patt =>
  pattern_bound_idents_and_locs(patt) |> List.map(fst);

let rev_let_bound_idents_with_loc = bindings =>
  List.map(({vb_pat, _}) => vb_pat, bindings)
  |> List.map(pattern_bound_idents_and_locs)
  |> List.fold_left((acc, cur) => cur @ acc, []);

let let_bound_idents_with_loc = bindings =>
  rev_let_bound_idents_with_loc(bindings) |> List.rev;

let rev_let_bound_idents = pat =>
  List.map(fst, rev_let_bound_idents_with_loc(pat));
let let_bound_idents = pat => List.map(fst, let_bound_idents_with_loc(pat));

let alpha_var = (env, id) => List.assoc(id, env);

let rec alpha_pat = (env, {pat_desc: desc, _} as p) =>
  switch (desc) {
  | TPatVar(id, s) =>
    let new_desc =
      try(TPatVar(alpha_var(env, id), s)) {
      | Not_found => TPatAny
      };
    {...p, pat_desc: new_desc};
  | TPatAlias(p1, id, s) =>
    let new_p = alpha_pat(env, p1);
    try({...p, pat_desc: TPatAlias(new_p, alpha_var(env, id), s)}) {
    | Not_found => new_p
    };
  | _ => {...p, pat_desc: map_pattern_desc(alpha_pat(env), desc)}
  };

let mkloc = Location.mkloc;
let mknoloc = Location.mknoloc;
