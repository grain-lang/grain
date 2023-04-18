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
  | TTyArrow(ty_args, ty_res, _) =>
    List.iter(((_, arg)) => collect_type_vars(arg), ty_args);
    collect_type_vars(ty_res);
  | TTyTuple(ty_args) => List.iter(collect_type_vars, ty_args)
  | TTyRecord(ty_args) =>
    List.iter(((_, ty_arg)) => collect_type_vars(ty_arg), ty_args)
  | TTyConstr(_, ty_args, _) => List.iter(collect_type_vars, ty_args)
  | TTyUniVar(_) => ()
  | TTyPoly(ty_arg, ty_args) =>
    collect_type_vars(ty_arg);
    List.iter(collect_type_vars, ty_args);
  | TTyLink(ty_arg)
  | TTySubst(ty_arg) => collect_type_vars(ty_arg)
  };

let link_type_vars = ty => {
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
      | TTyArrow(tyl, ret, c) =>
        TTyArrow(
          List.map(((l, arg)) => (l, link_types(arg)), tyl),
          link_types(ret),
          c,
        )
      | TTyTuple(l) => TTyTuple(List.map(link_types, l))
      | TTyRecord(l) =>
        TTyRecord(List.map(((name, arg)) => (name, link_types(arg)), l))
      | TTyConstr(p, l, m) => TTyConstr(p, List.map(link_types, l), m)
      | TTyUniVar(_) as ty => ty
      | TTySubst(_) => assert(false)
      | TTyLink(ty) => TTyLink(link_types(ty))
      | TTyPoly(ty, tyl) =>
        TTyPoly(link_types(ty), List.map(link_types, tyl))
      };
    {...texpr, desc};
  };
  link_types(ty);
};

let rec translate_signature = sg =>
  List.map(
    item =>
      switch (item) {
      | TSigValue(id, d) =>
        reset_type_variables();
        collect_type_vars(d.val_type);
        TSigValue(id, {...d, val_type: link_type_vars(d.val_type)});
      | TSigType(id, td, r) =>
        reset_type_variables();
        switch (td.type_kind) {
        | TDataVariant(cds) =>
          List.iter(
            cd => {
              switch (cd.cd_args) {
              | TConstrSingleton => ()
              | TConstrRecord(rfs) =>
                List.iter(rf => collect_type_vars(rf.rf_type), rfs)
              | TConstrTuple(tys) => List.iter(collect_type_vars, tys)
              };
              Option.iter(collect_type_vars, cd.cd_res);
            },
            cds,
          )
        | TDataAbstract => ()
        | TDataRecord(rfs) =>
          List.iter(rf => {collect_type_vars(rf.rf_type)}, rfs)
        | TDataOpen => ()
        };
        List.iter(collect_type_vars, td.type_params);
        Option.iter(collect_type_vars, td.type_manifest);
        let type_kind =
          switch (td.type_kind) {
          | TDataVariant(cds) =>
            TDataVariant(
              List.map(
                cd => {
                  let cd_args =
                    switch (cd.cd_args) {
                    | TConstrSingleton => TConstrSingleton
                    | TConstrTuple(tys) =>
                      TConstrTuple(List.map(link_type_vars, tys))
                    | TConstrRecord(rfs) =>
                      TConstrRecord(
                        List.map(
                          rf => {...rf, rf_type: link_type_vars(rf.rf_type)},
                          rfs,
                        ),
                      )
                    };
                  {
                    ...cd,
                    cd_args,
                    cd_res: Option.map(link_type_vars, cd.cd_res),
                  };
                },
                cds,
              ),
            )
          | TDataAbstract => TDataAbstract
          | TDataRecord(rfs) =>
            TDataRecord(
              List.map(
                rf => {{...rf, rf_type: link_type_vars(rf.rf_type)}},
                rfs,
              ),
            )
          | TDataOpen => TDataOpen
          };
        TSigType(
          id,
          {
            ...td,
            type_params: List.map(link_type_vars, td.type_params),
            type_manifest: Option.map(link_type_vars, td.type_manifest),
            type_kind,
          },
          r,
        );
      | TSigTypeExt(id, ec, ext) =>
        reset_type_variables();
        List.iter(collect_type_vars, ec.ext_type_params);
        switch (ec.ext_args) {
        | TConstrSingleton => ()
        | TConstrTuple(tys) => List.iter(collect_type_vars, tys)
        | TConstrRecord(rfs) =>
          List.iter(rf => collect_type_vars(rf.rf_type), rfs)
        };
        let ext_type_params = List.map(link_type_vars, ec.ext_type_params);
        let ext_args =
          switch (ec.ext_args) {
          | TConstrSingleton => TConstrSingleton
          | TConstrTuple(tys) => TConstrTuple(List.map(link_type_vars, tys))
          | TConstrRecord(rfs) =>
            TConstrRecord(
              List.map(
                rf => {...rf, rf_type: link_type_vars(rf.rf_type)},
                rfs,
              ),
            )
          };
        TSigTypeExt(id, {...ec, ext_type_params, ext_args}, ext);
      | TSigModule(id, mod_decl, rs) =>
        let md_type =
          switch (mod_decl.md_type) {
          | TModIdent(_)
          | TModAlias(_) => mod_decl.md_type
          | TModSignature(signature) =>
            TModSignature(translate_signature(signature))
          };
        TSigModule(id, {...mod_decl, md_type}, rs);
      | TSigModType(_) => failwith("translsig: NYI for module types")
      },
    sg,
  );
