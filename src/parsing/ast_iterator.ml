(* See copyright in ast_iterator.mli *)
open Parsetree

type iterator = {
  expr: iterator -> expression -> unit;
  pat: iterator -> pattern -> unit;
  typ: iterator -> parsed_type -> unit;
  data: iterator -> data_declaration -> unit;
  constructor: iterator -> constructor_declaration -> unit;
  location: iterator -> Location.t -> unit;
  import: iterator -> import_declaration -> unit;
  value_binding: iterator -> value_binding -> unit;
  toplevel: iterator -> toplevel_stmt -> unit;
}

let iter_loc sub {loc; txt} = sub.location sub loc

module E = struct
  let iter sub {pexp_desc = desc; pexp_loc = loc} =
    sub.location sub loc;
    match desc with
    | PExpId(i) -> iter_loc sub i
    | PExpConstant(c) -> ()
    | PExpTuple(es) -> List.iter (sub.expr sub) es
    | PExpLet(r, vbs, e) -> List.iter (sub.value_binding sub) vbs; sub.expr sub e
    | PExpPrim1(p1, e) -> sub.expr sub e
    | PExpPrim2(p2, e1, e2) -> sub.expr sub e1; sub.expr sub e2
    | PExpIf(c, t, f) -> sub.expr sub c; sub.expr sub t; sub.expr sub f
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
    | PPatConstraint(p, pt) -> sub.pat sub p; sub.typ sub pt
end

module C = struct
  let iter sub {pcd_name = name; pcd_args = args; pcd_loc = loc} =
    sub.location sub loc;
    iter_loc sub name;
    match args with
    | PConstrTuple(ptl) -> List.iter (sub.typ sub) ptl
    | PConstrSingleton -> ()
end

module D = struct
  let iter sub{pdata_name = name; pdata_params = args; pdata_kind = kind; pdata_loc = loc} =
    sub.location sub loc;
    iter_loc sub name;
    List.iter (sub.typ sub) args;
    match kind with
    | PDataVariant cdl -> List.iter (sub.constructor sub) cdl
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
end

module V = struct
  let iter sub {pvb_pat = pat; pvb_expr = expr; pvb_loc = loc} =
    sub.pat sub pat;
    sub.expr sub expr;
    sub.location sub loc;
end

module I = struct
  let iter sub {pimp_mod = imod; pimp_loc = loc} =
    sub.location sub loc;
    iter_loc sub imod
end

module TL = struct
  let iter sub {ptop_desc = desc; ptop_loc = loc} =
    sub.location sub loc;
    match desc with
      | PTopImport id -> sub.import sub id
      | PTopData dd -> sub.data sub dd
      | PTopLet(r, vb) -> List.iter (sub.value_binding sub) vb
end

let default_iterator = {
  expr = E.iter;
  pat = P.iter;
  typ = T.iter;
  data = D.iter;
  constructor = C.iter;
  location = (fun _ x -> ());
  import = I.iter;
  value_binding = V.iter;
  toplevel = TL.iter;
}
