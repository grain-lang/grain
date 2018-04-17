(* Modified from OCaml's source. Original copyright below. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree

module type MapArgument = sig
  val enter_typed_program : typed_program -> typed_program
  val enter_data_declaration : data_declaration -> data_declaration

  val enter_pattern : pattern -> pattern
  val enter_expression : expression -> expression

  val enter_core_type : core_type -> core_type
  val enter_toplevel_stmt : toplevel_stmt -> toplevel_stmt

  val leave_typed_program : typed_program -> typed_program
  val leave_data_declaration : data_declaration -> data_declaration
  val leave_pattern : pattern -> pattern
  val leave_expression : expression -> expression

  val leave_core_type : core_type -> core_type
  val leave_toplevel_stmt : toplevel_stmt -> toplevel_stmt
end

module MakeMap(Map : MapArgument) : sig
  val map_typed_program : typed_program -> typed_program
  val map_pattern : pattern -> pattern
  val map_toplevel_stmt : toplevel_stmt -> toplevel_stmt
  val map_expression : expression -> expression
end = struct
  let may_map = Option.map

  let rec map_typed_program tp =
    let tp = Map.enter_typed_program tp in
    let statements = List.map map_toplevel_stmt tp.statements in
    let body = map_expression tp.body in
    Map.leave_typed_program {tp with statements=statements; body=body}

  and map_core_type ct =
    let ct = Map.enter_core_type ct in
    let ctyp_desc = begin match ct.ctyp_desc with
      | TTyAny
      | TTyVar _ -> ct.ctyp_desc
      | TTyArrow(args, ret) ->
        let args = List.map map_core_type args in
        let ret = map_core_type ret in
        TTyArrow(args, ret)
      | TTyConstr(a, b, args) -> TTyConstr(a, b, List.map map_core_type args)
      | TTyTuple(args) -> TTyTuple(List.map map_core_type args)
      | TTyPoly(args, typ) ->
        let typ = map_core_type typ in
        TTyPoly(args, typ)
    end in
    Map.leave_core_type {ct with ctyp_desc}

  and map_binding vb =
    let vb_pat = map_pattern vb.vb_pat in
    let vb_expr = map_expression vb.vb_expr in
    {vb with vb_pat; vb_expr}

  and map_bindings rec_flag binds =
    List.map map_binding binds

  and map_match_branch ({mb_pat; mb_body} as mb) =
    let mb_pat = map_pattern mb_pat in
    let mb_body = map_expression mb_body in
    {mb with mb_pat; mb_body}

  and map_match_branches branches =
    List.map map_match_branch branches

  and map_constructor_arguments = function
    | TConstrTuple(args) -> TConstrTuple(List.map map_core_type args)
    | (TConstrSingleton as ca) -> ca

  and map_constructor_declaration ({cd_args; cd_res} as cd) =
    let cd_args = map_constructor_arguments cd_args in
    let cd_res = may_map map_core_type cd_res in
    {cd with cd_args; cd_res}

  and map_type_parameter ct =
    map_core_type ct

  and map_data_declaration decl =
    let decl = Map.enter_data_declaration decl in
    let data_params = List.map map_type_parameter decl.data_params in
    let data_kind = begin match decl.data_kind with
      | TDataVariant cstrs -> TDataVariant(List.map map_constructor_declaration cstrs)
    end in
    Map.leave_data_declaration {decl with data_params; data_kind}

  and map_toplevel_stmt stmt =
    let stmt = Map.enter_toplevel_stmt stmt in
    let ttop_desc = begin match stmt.ttop_desc with
      | TTopData (decl) -> TTopData(map_data_declaration decl)
      | TTopForeign _
      | TTopImport _  -> stmt.ttop_desc
      | TTopLet (recflag, binds) -> TTopLet(recflag, map_bindings recflag binds)
    end in
    Map.leave_toplevel_stmt {stmt with ttop_desc}

  and map_toplevel_stmts stmts =
    List.map map_toplevel_stmt stmts

  and map_pat_extra (cstr, loc) =
    let cstr = begin match cstr with
      | TPatConstraint ct -> TPatConstraint (map_core_type ct)
    end in
    (cstr, loc)

  and map_pattern pat =
    let pat = Map.enter_pattern pat in
    let pat_extra = List.map map_pat_extra pat.pat_extra in
    let pat_desc = begin match pat.pat_desc with
      | TPatAny
      | TPatVar _
      | TPatConstant _ -> pat.pat_desc
      | TPatAlias(p1, a, b) -> TPatAlias(map_pattern p1, a, b)
      | TPatConstruct(a, b, args) -> TPatConstruct(a, b, List.map map_pattern args)
      | TPatTuple(args) -> TPatTuple(List.map map_pattern args)
      | TPatOr(p1, p2) -> TPatOr(map_pattern p1, map_pattern p2)
    end in
    Map.leave_pattern {pat with pat_extra; pat_desc}

  and map_exp_extra (cstr, loc) =
    let cstr = begin match cstr with
      | TExpConstraint ct -> TExpConstraint (map_core_type ct)
    end in
    (cstr, loc)

  and map_expression exp =
    let exp = Map.enter_expression exp in
    let exp_extra = List.map map_exp_extra exp.exp_extra in
    let exp_desc = begin match exp.exp_desc with
      | TExpNull
      | TExpIdent _
      | TExpConstant _ -> exp.exp_desc
      | TExpLet(recflag, binds, body) ->
        TExpLet(recflag, map_bindings recflag binds, map_expression body)
      | TExpLambda(branches, p) -> TExpLambda(map_match_branches branches, p)
      | TExpApp(exp, args) ->
        TExpApp(map_expression exp, List.map map_expression args)
      | TExpPrim1(o, e) -> TExpPrim1(o, map_expression e)
      | TExpPrim2(o, e1, e2) ->
        TExpPrim2(o, map_expression e1, map_expression e2)
      | TExpMatch(value, branches, p) ->
        TExpMatch(map_expression value, map_match_branches branches, p)
      | TExpTuple(args) -> TExpTuple(List.map map_expression args)
      | TExpBlock(args) -> TExpBlock(List.map map_expression args)
      | TExpConstruct(a, b, args) -> TExpConstruct(a, b, List.map map_expression args)
      | TExpIf(c, t, f) ->
        TExpIf(map_expression c, map_expression t, map_expression f)
    end in
    Map.leave_expression {exp with exp_extra; exp_desc}
end

module DefaultMapArgument : MapArgument = struct
  let enter_typed_program a = a
  let enter_data_declaration a = a
  let enter_pattern a = a
  let enter_expression a = a
  let enter_core_type a = a
  let enter_toplevel_stmt a = a
  let leave_typed_program a = a
  let leave_data_declaration a = a
  let leave_pattern a = a
  let leave_expression a = a
  let leave_core_type a = a
  let leave_toplevel_stmt a = a
end
