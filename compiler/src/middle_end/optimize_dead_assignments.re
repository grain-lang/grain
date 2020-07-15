open Anftree;
open Grain_typed;

let used_symbols = ref(Ident.empty: Ident.tbl(bool));

let mark_used = id => used_symbols := Ident.add(id, true, used_symbols^);

let get_comp_purity = c =>
  Option.value(~default=false) @@ Analyze_purity.comp_expression_purity(c);

let can_remove = (ident, value) =>
  try(!Ident.find_same(ident, used_symbols^)) {
  | Not_found => get_comp_purity(value)
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
    | [@implicit_arity] AELet(Global, _, _, _)
    | AESeq(_)
    | AEComp(_) => a
    | [@implicit_arity] AELet(g, Nonrecursive, [(bind, value)], body) =>
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
    | [@implicit_arity] AELet(g, r, binds, body) =>
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
      | _ => {...a, anf_desc: [@implicit_arity] AELet(g, r, new_binds, body)}
      };
    };
};

module DAEMapper = Anf_mapper.MakeMap(DAEArg);

let optimize = anfprog => {
  /* Reset state */
  used_symbols := Ident.empty;
  DAEMapper.map_anf_program(anfprog);
};
