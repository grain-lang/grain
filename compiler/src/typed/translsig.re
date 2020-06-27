open Grain_utils;
open Types;

let used_type_variables = ref(Tbl.empty: Tbl.t(int, ref(list(type_expr))));

let reset_type_variables = () => used_type_variables := Tbl.empty;

let rec collect_type_vars = typ =>
  switch (typ.desc) {
  | TTyVar(_) =>
    try({
      let type_exprs = Tbl.find(typ.id, used_type_variables^);
      type_exprs := [typ, ...type_exprs^];
    }) {
    | Not_found =>
      used_type_variables :=
        Tbl.add(typ.id, ref([typ]), used_type_variables^)
    }
  | [@implicit_arity] TTyArrow(ty_args, ty_res, _) =>
    List.iter(collect_type_vars, ty_args);
    collect_type_vars(ty_res);
  | TTyTuple(ty_args) => List.iter(collect_type_vars, ty_args)
  | TTyRecord(ty_args) =>
    List.iter(((_, ty_arg)) => collect_type_vars(ty_arg), ty_args)
  | [@implicit_arity] TTyConstr(_, ty_args, _) =>
    List.iter(collect_type_vars, ty_args)
  | TTyUniVar(_) => ()
  | [@implicit_arity] TTyPoly(ty_arg, ty_args) =>
    collect_type_vars(ty_arg);
    List.iter(collect_type_vars, ty_args);
  | TTyLink(ty_arg)
  | TTySubst(ty_arg) => collect_type_vars(ty_arg)
  };

let link_type_vars = ty => {
  reset_type_variables();
  collect_type_vars(ty);
  let rec link_types = texpr => {
    let desc =
      switch (texpr.desc) {
      | TTyVar(_) as ty =>
        try({
          let vars = Tbl.find(texpr.id, used_type_variables^);
          if (List.length(vars^) < 2) {
            raise(Not_found);
          };
          TTyLink(List.hd(vars^));
        }) {
        | Not_found => ty
        }
      | [@implicit_arity] TTyArrow(tyl, ret, c) =>
        [@implicit_arity]
        TTyArrow(List.map(link_types, tyl), link_types(ret), c)
      | TTyTuple(l) => TTyTuple(List.map(link_types, l))
      | TTyRecord(l) =>
        TTyRecord(List.map(((name, arg)) => (name, link_types(arg)), l))
      | [@implicit_arity] TTyConstr(p, l, m) =>
        [@implicit_arity] TTyConstr(p, List.map(link_types, l), m)
      | TTyUniVar(_) as ty => ty
      | TTySubst(_) => assert(false)
      | TTyLink(ty) => TTyLink(link_types(ty))
      | [@implicit_arity] TTyPoly(ty, tyl) =>
        [@implicit_arity] TTyPoly(link_types(ty), List.map(link_types, tyl))
      };
    {...texpr, desc};
  };
  link_types(ty);
};

let translate_signature = sg =>
  List.map(
    item =>
      switch (item) {
      | [@implicit_arity] TSigValue(id, d) =>
        [@implicit_arity]
        TSigValue(id, {...d, val_type: link_type_vars(d.val_type)})
      | TSigType(_)
      | TSigModule(_)
      | TSigModType(_) => item
      },
    sg,
  );
