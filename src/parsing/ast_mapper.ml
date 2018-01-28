(* See copyright in ast_mapper.mli *)
open Parsetree
open Ast_helper

type mapper = {
  constant: mapper -> constant -> constant;
  expr: mapper -> expression -> expression;
  pat: mapper -> pattern -> pattern;
  typ: mapper -> parsed_type -> parsed_type;
  data: mapper -> data_declaration -> data_declaration;
  constructor: mapper -> constructor_declaration -> constructor_declaration;
  location: mapper -> Location.t -> Location.t;
  import: mapper -> import_declaration -> import_declaration;
  value_binding: mapper -> value_binding -> value_binding;
  match_branch: mapper -> match_branch -> match_branch;
  toplevel: mapper -> toplevel_stmt -> toplevel_stmt;
}

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

module Cnst = struct
  let map sub c = c
end

module E = struct
  let map sub {pexp_desc = desc; pexp_loc = loc} =
    let open Exp in
    let loc = sub.location sub loc in
    match desc with
    | PExpId(i) -> ident ~loc (map_loc sub i)
    | PExpConstant(c) -> constant ~loc (sub.constant sub c)
    | PExpTuple(es) -> tuple ~loc (List.map (sub.expr sub) es)
    | PExpLet(r, vbs, e) -> let_ ~loc r (List.map (sub.value_binding sub) vbs) (sub.expr sub e)
    | PExpMatch(e, mbs) -> match_ ~loc (sub.expr sub e) (List.map (sub.match_branch sub) mbs)
    | PExpPrim1(p1, e) -> prim1 ~loc p1 (sub.expr sub e)
    | PExpPrim2(p2, e1, e2) -> prim2 ~loc p2 (sub.expr sub e1) (sub.expr sub e2)
    | PExpIf(c, t, f) -> if_ ~loc (sub.expr sub c) (sub.expr sub t) (sub.expr sub f)
    | PExpLambda(pl, e) -> lambda ~loc (List.map (sub.pat sub) pl) (sub.expr sub e)
    | PExpApp(e, el) -> apply ~loc (sub.expr sub e) (List.map (sub.expr sub) el)
    | PExpBlock(el) -> block ~loc (List.map (sub.expr sub) el)
    | PExpNull -> null ~loc ()
end

module P = struct
  let map sub {ppat_desc = desc; ppat_loc = loc} =
    let open Pat in
    let loc = sub.location sub loc in
    match desc with
    | PPatAny -> any ~loc ()
    | PPatVar sl -> var ~loc (map_loc sub sl)
    | PPatTuple pl -> tuple ~loc (List.map (sub.pat sub) pl)
    | PPatConstant c -> constant ~loc (sub.constant sub c)
    | PPatConstraint(p, pt) -> constraint_ ~loc (sub.pat sub p) (sub.typ sub pt)
    | PPatConstruct(id, pl) -> construct ~loc (map_loc sub id) (List.map (sub.pat sub) pl)
end

module C = struct
  let map sub {pcd_name = name; pcd_args = args; pcd_loc = loc} =
    let open CDecl in
    let loc = sub.location sub loc in
    let sname = map_loc sub name in
    match args with
    | PConstrTuple(ptl) -> tuple ~loc sname (List.map (sub.typ sub) ptl)
    | PConstrSingleton -> singleton ~loc sname
end

module D = struct
  let map sub{pdata_name = name; pdata_params = args; pdata_kind = kind; pdata_loc = loc} =
    let open Dat in
    let loc = sub.location sub loc in
    let sname = map_loc sub name in
    let sargs = List.map (sub.typ sub) args in
    match kind with
    | PDataVariant cdl -> variant ~loc sname sargs (List.map (sub.constructor sub) cdl)
end

module T = struct
  let map sub {ptyp_desc = desc; ptyp_loc = loc} =
    let open Typ in
    let loc = sub.location sub loc in
    match desc with
    | PTyAny -> any ~loc ()
    | PTyVar v -> var ~loc v
    | PTyArrow(args, ret) -> arrow ~loc (List.map (sub.typ sub) args) (sub.typ sub ret)
    | PTyTuple ts -> tuple ~loc (List.map (sub.typ sub) ts)
    | PTyConstr(name, ts) -> constr ~loc (map_loc sub name) (List.map (sub.typ sub) ts)
end

module V = struct
  let map sub {pvb_pat = pat; pvb_expr = expr; pvb_loc = loc} =
    {
      pvb_pat = sub.pat sub pat;
      pvb_expr = sub.expr sub expr;
      pvb_loc = sub.location sub loc;
    }
end

module MB = struct
  let map sub {pmb_pat = pat; pmb_body = expr; pmb_loc = loc} =
    {
      pmb_pat = sub.pat sub pat;
      pmb_body = sub.expr sub expr;
      pmb_loc = sub.location sub loc;
    }
end

module I = struct
  let map sub {pimp_mod = imod; pimp_loc = loc} =
    let open Imp in
    let loc = sub.location sub loc in
    mk ~loc (map_loc sub imod)
end

module TL = struct
  let map sub {ptop_desc = desc; ptop_loc = loc} =
    let open Top in
    let loc = sub.location sub loc in
    match desc with
      | PTopImport id -> Top.import ~loc (sub.import sub id)
      | PTopData dd -> Top.data ~loc (sub.data sub dd)
      | PTopLet(r, vb) -> Top.let_ ~loc r (List.map (sub.value_binding sub) vb)
end

let default_mapper = {
  constant = Cnst.map;
  expr = E.map;
  pat = P.map;
  typ = T.map;
  data = D.map;
  constructor = C.map;
  location = (fun _ x -> x);
  import = I.map;
  value_binding = V.map;
  match_branch = MB.map;
  toplevel = TL.map;
}

