(* Modified from OCaml's source. Original copyright below. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Grain_parsing
open Asttypes
open Typedtree


module type IteratorArgument = sig
    val enter_typed_program : typed_program -> unit

    val enter_pattern : pattern -> unit
    val enter_expression : expression -> unit
    val enter_core_type : core_type -> unit
    val enter_toplevel_stmt : toplevel_stmt -> unit


    val leave_typed_program : typed_program -> unit
    val leave_pattern : pattern -> unit
    val leave_expression : expression -> unit
    val leave_core_type : core_type -> unit
    val leave_toplevel_stmt : toplevel_stmt -> unit

    val enter_bindings : export_flag -> rec_flag -> unit
    val enter_binding : value_binding -> unit
    val leave_binding : value_binding -> unit
    val leave_bindings : export_flag -> rec_flag -> unit

    val enter_data_declarations : unit -> unit
    val enter_data_declaration : data_declaration -> unit
    val leave_data_declaration : data_declaration -> unit
    val leave_data_declarations : unit -> unit

end

module MakeIterator(Iter : IteratorArgument) : sig
  val iter_typed_program : typed_program -> unit
  val iter_toplevel_stmt : toplevel_stmt -> unit
  val iter_expression : expression -> unit
  val iter_pattern : pattern -> unit
end = struct

  let may_iter = Option.may

  let rec iter_typed_program ({statements; body} as tp) =
    Iter.enter_typed_program tp;
    iter_toplevel_stmts statements;
    iter_expression body;
    Iter.leave_typed_program tp

  and iter_core_type ct =
    Iter.enter_core_type ct;
    begin match ct.ctyp_desc with
      | TTyAny
      | TTyVar _ -> ()
      | TTyArrow(args, ret) ->
        List.iter iter_core_type args;
        iter_core_type ret
      | TTyConstr(_, _, args)
      | TTyTuple(args) -> List.iter iter_core_type args
      | TTyRecord(args) -> List.iter (fun (_, arg) -> iter_core_type arg) args
      | TTyPoly(_, typ) -> iter_core_type typ
    end;
    Iter.leave_core_type ct

  and iter_binding ({vb_pat; vb_expr} as vb) =
    Iter.enter_binding vb;
    iter_pattern vb_pat;
    iter_expression vb_expr;
    Iter.leave_binding vb

  and iter_bindings export_flag rec_flag binds =
    Iter.enter_bindings export_flag rec_flag;
    List.iter iter_binding binds;
    Iter.leave_bindings export_flag rec_flag

  and iter_match_branch {mb_pat; mb_body} =
    iter_pattern mb_pat;
    iter_expression mb_body

  and iter_match_branches branches =
    List.iter iter_match_branch branches

  and iter_constructor_arguments = function
    | TConstrTuple(args) -> List.iter iter_core_type args
    | TConstrSingleton -> ()

  and iter_constructor_declaration {cd_args; cd_res} =
    iter_constructor_arguments cd_args;
    may_iter iter_core_type cd_res

  and iter_record_field {rf_type} =
    iter_core_type rf_type

  and iter_type_parameter ct =
    iter_core_type ct

  and iter_data_declaration decl =
    Iter.enter_data_declaration decl;
    List.iter iter_type_parameter decl.data_params;
    begin match decl.data_kind with
      | TDataVariant cstrs -> List.iter iter_constructor_declaration cstrs
      | TDataRecord labels -> List.iter iter_record_field labels
    end;
    Iter.leave_data_declaration decl

  (* FIXME: These two functions are gross *)
  and iter_toplevel_stmt stmt =
    Iter.enter_toplevel_stmt stmt;
    begin match stmt.ttop_desc with
      | TTopData decls -> List.iter iter_data_declaration decls
      | TTopForeign _
      | TTopImport _
      | TTopExport _ -> ()
      | TTopLet (exportflag, recflag, binds) -> iter_bindings exportflag recflag binds
    end;
    Iter.leave_toplevel_stmt stmt

  and iter_toplevel_stmts stmts =
    List.iter (fun cur ->
        match cur.ttop_desc with
        | TTopForeign _
        | TTopImport _ 
        | TTopExport _ 
        | TTopLet _ -> iter_toplevel_stmt cur
        | TTopData _ -> 
          Iter.enter_data_declarations();
          iter_toplevel_stmt cur;
          Iter.leave_data_declarations();
        ) stmts

  and iter_pattern pat =
    Iter.enter_pattern pat;
    List.iter (fun (cstr, _) ->
        match cstr with
        | TPatConstraint ct -> iter_core_type ct) pat.pat_extra;
    begin match pat.pat_desc with
      | TPatAny
      | TPatVar _
      | TPatConstant _ -> ()
      | TPatAlias(p1, _, _) -> iter_pattern p1
      | TPatConstruct(_, _, args)
      | TPatTuple(args) -> List.iter iter_pattern args
      | TPatOr(p1, p2) -> iter_pattern p1; iter_pattern p2
    end;
    Iter.leave_pattern pat

  and iter_expression ({exp_desc; exp_extra} as exp) =
    Iter.enter_expression exp;
    List.iter (fun (cstr, _) ->
        match cstr with
        | TExpConstraint ct -> iter_core_type ct) exp_extra;
    begin match exp_desc with
      | TExpNull
      | TExpIdent _
      | TExpConstant _ -> ()
      | TExpLet(recflag, binds, body) ->
        iter_bindings Nonexported recflag binds;
        iter_expression body
      | TExpLambda(branches, _) -> iter_match_branches branches
      | TExpApp(exp, args) ->
        iter_expression exp;
        List.iter iter_expression args
      | TExpPrim1(_, e) -> iter_expression e
      | TExpPrim2(_, e1, e2) ->
        iter_expression e1;
        iter_expression e2
      | TExpAssign(be, e) ->
        iter_expression be;
        iter_expression e
      | TExpMatch(value, branches, _) ->
        iter_expression value;
        iter_match_branches branches
      | TExpRecord(args) ->
        Array.iter (function 
          | (_, Overridden(_, expr)) -> iter_expression expr
          | _ -> ()
        ) args
      | TExpRecordGet(expr, _, _) -> iter_expression expr
      | TExpTuple(args)
      | TExpBlock(args)
      | TExpConstruct(_, _, args) -> List.iter iter_expression args
      | TExpIf(c, t, f) ->
        iter_expression c;
        iter_expression t;
        iter_expression f
      | TExpWhile(c, b) ->
        iter_expression c;
        iter_expression b
    end;
    Iter.leave_expression exp

end

module DefaultIteratorArgument : IteratorArgument = struct
  let enter_typed_program _ = ()
  let enter_pattern _ = ()
  let enter_expression _ = ()
  let enter_core_type _ = ()
  let enter_toplevel_stmt _ = ()
  let enter_bindings _ _ = ()
  let enter_binding _ = ()
  let enter_data_declaration _ = ()
  let enter_data_declarations () = ()


  let leave_typed_program _ = ()
  let leave_pattern _ = ()
  let leave_expression _ = ()
  let leave_core_type _ = ()
  let leave_toplevel_stmt _ = ()
  let leave_binding _ = ()
  let leave_bindings _ _ = ()
  let leave_data_declaration _ = ()
  let leave_data_declarations () = ()
end
