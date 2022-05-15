open Anftree;
open Grain_typed;

let used_symbols = ref(Ident.empty: Ident.tbl(unit));

let mark_used = id => used_symbols := Ident.add(id, (), used_symbols^);

let get_comp_purity = c =>
  Option.value(~default=false) @@ Analyze_purity.comp_expression_purity(c);

let safe_to_remove_import = i =>
  switch (i.imp_desc) {
  | GrainValue(_) => true
  | _ => false
  };

let can_remove = (ident, value) =>
  switch (Ident.find_same_opt(ident, used_symbols^)) {
  | Some(_) => false
  | None => get_comp_purity(value)
  };

let can_remove_import = import =>
  switch (Ident.find_same_opt(import.imp_use_id, used_symbols^)) {
  | Some(_) => false
  | None => safe_to_remove_import(import)
  };

module DAEArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let enter_imm_expression = ({imm_desc: desc} as i) => {
    switch (desc) {
    | ImmId(i) => mark_used(i)
    | _ => ()
    };
    i;
  };

  let leave_anf_expression = ({anf_desc: desc} as a) =>
    switch (desc) {
    | AELet(Global({exported: true}), _, _, _, _)
    | AELet(_, _, Mutable, _, _)
    | AESeq(_)
    | AEComp(_) => a
    | AELet(g, Nonrecursive, _, [(bind, value)], body) =>
      switch (body.anf_desc) {
      | AEComp({comp_desc: CImmExpr({imm_desc: ImmId(id)})})
          when Ident.same(id, bind) => {
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

  let optimized = DAEMapper.map_anf_program(anfprog);
  let imports =
    List.filter(import => !can_remove_import(import), optimized.imports);
  {...optimized, imports};
};
