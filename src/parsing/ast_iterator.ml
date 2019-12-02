(* See copyright in ast_iterator.mli *)
open Parsetree

type iterator = {
  constant: iterator -> constant -> unit;
  expr: iterator -> expression -> unit;
  pat: iterator -> pattern -> unit;
  typ: iterator -> parsed_type -> unit;
  data: iterator -> data_declaration -> unit;
  constructor: iterator -> constructor_declaration -> unit;
  label: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  import: iterator -> import_declaration list -> unit;
  export: iterator -> export_declaration list -> unit;
  export_all: iterator -> export_except list -> unit;
  value_binding: iterator -> value_binding -> unit;
  match_branch: iterator -> match_branch -> unit;
  value_description: iterator -> value_description -> unit;
  toplevel: iterator -> toplevel_stmt -> unit;
}

let iter_loc sub {loc; txt} = sub.location sub loc
let iter_opt sub opt = Option.may (iter_loc sub) opt

module Cnst = struct
  let iter sub c = ()
end

module E = struct
  let iter sub {pexp_desc = desc; pexp_loc = loc} =
    sub.location sub loc;
    match desc with
    | PExpId(i) -> iter_loc sub i
    | PExpConstant(c) -> sub.constant sub c
    | PExpTuple(es) -> List.iter (sub.expr sub) es
    | PExpRecord(es) -> List.iter (fun (name, exp) -> iter_loc sub name; (sub.expr sub exp)) es
    | PExpRecordGet(e, f) -> sub.expr sub e; iter_loc sub f
    | PExpLet(r, vbs, e) -> List.iter (sub.value_binding sub) vbs; sub.expr sub e
    | PExpMatch(e, mbs) -> sub.expr sub e; List.iter (sub.match_branch sub) mbs
    | PExpPrim1(p1, e) -> sub.expr sub e
    | PExpPrim2(p2, e1, e2) -> sub.expr sub e1; sub.expr sub e2
    | PExpAssign(be, e) -> sub.expr sub be; sub.expr sub e
    | PExpIf(c, t, f) -> sub.expr sub c; sub.expr sub t; sub.expr sub f
    | PExpWhile(c, b) -> sub.expr sub c; sub.expr sub b
    | PExpConstraint(e, t) -> sub.expr sub e; sub.typ sub t
    | PExpLambda(pl, e) -> List.iter (sub.pat sub) pl; sub.expr sub e
    | PExpApp(e, el) -> sub.expr sub e; List.iter (sub.expr sub) el
    | PExpBlock(el) -> List.iter (sub.expr sub) el
    | PExpNull -> ()
end

module P = struct
  let iter sub {ppat_desc = desc; ppat_loc = loc} =
    sub.location sub loc;
    match desc with
    | PPatAny -> ()
    | PPatVar sl -> iter_loc sub sl
    | PPatTuple pl -> List.iter (sub.pat sub) pl
    | PPatRecord(fs, _) -> List.iter (fun (id, pat) -> iter_loc sub id; sub.pat sub pat) fs
    | PPatConstant c -> sub.constant sub c
    | PPatConstraint(p, pt) -> sub.pat sub p; sub.typ sub pt
    | PPatConstruct(id, pl) -> iter_loc sub id; List.iter (sub.pat sub) pl
    | PPatOr(p1, p2) -> sub.pat sub p1; sub.pat sub p2
    | PPatAlias(p, id) -> sub.pat sub p; iter_loc sub id
end

module C = struct
  let iter sub {pcd_name = name; pcd_args = args; pcd_loc = loc} =
    sub.location sub loc;
    iter_loc sub name;
    match args with
    | PConstrTuple(ptl) -> List.iter (sub.typ sub) ptl
    | PConstrSingleton -> ()
end

module L = struct
  let iter sub {pld_name = name; pld_type = typ; pld_loc = loc} =
    sub.location sub loc;
    iter_loc sub name;
    sub.typ sub typ
end

module D = struct
  let iter sub{pdata_name = name; pdata_params = args; pdata_kind = kind; pdata_loc = loc} =
    sub.location sub loc;
    iter_loc sub name;
    List.iter (sub.typ sub) args;
    match kind with
    | PDataVariant cdl -> List.iter (sub.constructor sub) cdl
    | PDataRecord ldl -> List.iter (sub.label sub) ldl
end

module T = struct
  let iter sub {ptyp_desc = desc; ptyp_loc = loc} =
    sub.location sub loc;
    match desc with
    | PTyAny -> ()
    | PTyVar v -> ()
    | PTyArrow(args, ret) -> List.iter (sub.typ sub) args; sub.typ sub ret
    | PTyTuple ts -> List.iter (sub.typ sub) ts
    | PTyConstr(name, ts) -> iter_loc sub name; List.iter (sub.typ sub) ts
    | PTyPoly(args, t) -> List.iter (iter_loc sub) args; sub.typ sub t
end

module V = struct
  let iter sub {pvb_pat = pat; pvb_expr = expr; pvb_loc = loc} =
    sub.pat sub pat;
    sub.expr sub expr;
    sub.location sub loc;
end

module MB = struct
  let iter sub {pmb_pat = pat; pmb_body = expr; pmb_loc = loc} =
    sub.pat sub pat;
    sub.expr sub expr;
    sub.location sub loc;
end

module I = struct
  let iter_one sub {pimp_mod_alias = alias; pimp_path = path; pimp_val = ival; pimp_loc = loc} =
    iter_opt sub alias;
    iter_loc sub path;
    begin match ival with
      | PImportValues values -> 
        List.iter (fun (name, alias) -> iter_loc sub name; iter_opt sub alias) values
      | PImportAllExcept values -> 
        List.iter (iter_loc sub) values
      | PImportModule -> () end;
    sub.location sub loc
  let iter sub imports = List.iter (iter_one sub) imports
end

module Ex = struct
  let iter sub exports =
    List.iter (fun export -> 
      match export with
      | ExportData({pex_loc=loc})
      | ExportValue({pex_loc=loc}) -> sub.location sub loc
    ) exports
  let iter_export_all sub excepts =
    List.iter (fun except ->
      match except with
      | ExportExceptData(name)
      | ExportExceptValue(name) -> iter_loc sub name
    ) excepts
end

module ExD = struct
  let iter_export_data_all sub excepts =
    List.iter (iter_loc sub) excepts
end

module VD = struct
  let iter sub {pval_mod = vmod; pval_name = vname; pval_loc = loc} =
    sub.location sub loc;
    iter_loc sub vmod;
    iter_loc sub vname
end

module TL = struct
  let iter sub {ptop_desc = desc; ptop_loc = loc} =
    sub.location sub loc;
    match desc with
      | PTopImport id -> sub.import sub id
      | PTopExport ex -> sub.export sub ex
      | PTopExportAll ex -> sub.export_all sub ex
      | PTopForeign(e, vd) -> sub.value_description sub vd
      | PTopData(e, dd) -> sub.data sub dd
      | PTopLet(e, r, vb) -> List.iter (sub.value_binding sub) vb
      | PTopExpr e -> sub.expr sub e
end

let default_iterator = {
  constant = Cnst.iter;
  expr = E.iter;
  pat = P.iter;
  typ = T.iter;
  data = D.iter;
  constructor = C.iter;
  label = L.iter;
  location = (fun _ x -> ());
  import = I.iter;
  export = Ex.iter;
  export_all = Ex.iter_export_all;
  value_binding = V.iter;
  match_branch = MB.iter;
  value_description = VD.iter;
  toplevel = TL.iter;
}
