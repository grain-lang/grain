open Anftree;
open Grain_typed;

let used_symbols = ref(Ident.empty: Ident.tbl(unit));
let exported_symbols = ref(Ident.empty: Ident.tbl(unit));

let mark_used = id => used_symbols := Ident.add(id, (), used_symbols^);
let mark_exported = id =>
  exported_symbols := Ident.add(id, (), exported_symbols^);

let get_comp_purity = c =>
  Option.value(~default=false) @@ Analyze_purity.comp_expression_purity(c);

let safe_to_remove_import = i =>
  switch (i.imp_desc) {
  | GrainValue(_) => true
  | _ => false
  };

let is_used = ident =>
  Option.is_some(Ident.find_same_opt(ident, used_symbols^));
let is_exported = ident =>
  Option.is_some(Ident.find_same_opt(ident, exported_symbols^));

let can_remove = (ident, value) =>
  if (is_used(ident) || is_exported(ident)) {
    false;
  } else {
    get_comp_purity(value);
  };

let can_remove_import = import =>
  if (is_used(import.imp_use_id) || is_exported(import.imp_use_id)) {
    false;
  } else {
    safe_to_remove_import(import);
  };

module DAEArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let enter_anf_program = ({signature: {cmi_sign}, imports} as p) => {
    open Types;
    // Consider exported values as used so they are not removed
    let rec process_sig =
      fun
      | TSigValue(id, {val_internalpath: PIdent(full_id)}) => {
          mark_exported(full_id);
        }
      | TSigValue(id, {val_internalpath}) => {
          let id =
            switch (Path_tbl.find_opt(imports.path_map, val_internalpath)) {
            | Some(id) => id
            | None =>
              failwith(
                "Impossible: import path not found "
                ++ Path.name(val_internalpath),
              )
            };
          mark_exported(id);
        }
      | TSigTypeExt(_, {ext_name: id}, _) => {
          mark_exported(id);
        }
      | TSigType(_, {type_kind: TDataVariant(cds)}, _) =>
        List.iter(({cd_id}) => mark_exported(cd_id), cds)
      | TSigType(_) => ()
      | TSigModule(_, {md_type: TModSignature(signature)}, _) =>
        List.iter(process_sig, signature)
      | TSigModule(_) => ()
      | TSigModType(_) => failwith("NYI: module types in module signatures");
    List.iter(process_sig, cmi_sign);
    p;
  };

  let enter_imm_expression = ({imm_desc: desc} as i) => {
    switch (desc) {
    | ImmId(i) => mark_used(i)
    | _ => ()
    };
    i;
  };

  let leave_anf_expression = ({anf_desc: desc} as a) =>
    switch (desc) {
    | AELet(_, _, Mutable, _, _)
    | AESeq(_)
    | AEComp(_) => a
    | AELet(g, Nonrecursive, _, [(bind, value)], body) =>
      switch (body.anf_desc) {
      | AEComp({comp_desc: CImmExpr({imm_desc: ImmId(id)})})
          when Ident.same(id, bind) && !is_exported(id) => {
          ...a,
          anf_desc: AEComp(value),
        }
      | _ =>
        if (can_remove(bind, value)) {
          body;
        } else {
          a;
        }
      }
    | AELet(g, r, m, binds, body) =>
      let new_binds =
        List.fold_right(
          ((id, v), tl) =>
            if (can_remove(id, v)) {
              tl;
            } else {
              [(id, v), ...tl];
            },
          binds,
          [],
        );
      switch (new_binds) {
      | [] => body
      | _ => {...a, anf_desc: AELet(g, r, m, new_binds, body)}
      };
    };
};

module DAEMapper = Anf_mapper.MakeMap(DAEArg);

let optimize = anfprog => {
  /* Reset state */
  used_symbols := Ident.empty;
  exported_symbols := Ident.empty;

  let optimized = DAEMapper.map_anf_program(anfprog);
  let imports = {
    ...optimized.imports,
    specs:
      List.filter(
        import => !can_remove_import(import),
        optimized.imports.specs,
      ),
  };
  {...optimized, imports};
};
