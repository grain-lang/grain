open Anftree;
open Grain_typed;
open Types;

let known_constants = ref(Ident.empty: Ident.tbl(constant));

let add_constant = (id, value) =>
  known_constants := Ident.add(id, value, known_constants^);

module ConstantPropagationArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let enter_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | [@implicit_arity] AELet(g, r, binds, body) =>
      List.iter(
        ((id, v)) =>
          switch (v) {
          | {comp_desc: CImmExpr({imm_desc: ImmConst(c)})} =>
            add_constant(id, c)
          | _ => ()
          },
        binds,
      )
    | _ => ()
    };
    a;
  };

  let leave_imm_expression = ({imm_desc: desc} as i) =>
    switch (desc) {
    | ImmId(id) =>
      try({
        let value = Ident.find_same(id, known_constants^);
        {...i, imm_desc: ImmConst(value)};
      }) {
      | Not_found => i
      }
    | _ => i
    };
};

module ConstantPropagationMapper = Anf_mapper.MakeMap(ConstantPropagationArg);

let optimize = anfprog => {
  /* Reset state */
  known_constants := Ident.empty;
  ConstantPropagationMapper.map_anf_program(anfprog);
};
