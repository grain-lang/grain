open Anftree;
open Grain_typed;
open Grain_utils;
open Types;

let known_constants = ref(Ident.empty: Ident.tbl(imm_expression_desc));

let add_constant = (id, value) =>
  known_constants := Ident.add(id, value, known_constants^);

module ConstantPropagationArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let enter_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | AELet(g, r, Immutable, binds, body) =>
      List.iter(
        ((id, v)) =>
          switch (v) {
          | {comp_desc: CNumber(Const_number_int(n) as c')}
              when
                n <= Literals.simple_number_max
                && n >= Literals.simple_number_min =>
            add_constant(id, ImmConst(Const_number(c')))
          | {comp_desc: CImmExpr({imm_desc})} =>
            switch (imm_desc) {
            // We don't substitute mutable variables, since we do pass-by-value, not pass-by-reference
            | ImmId(rhs_id) when Analyze_mutable_vars.is_mutable(rhs_id) =>
              ()
            | _ => add_constant(id, imm_desc)
            }
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
        {...i, imm_desc: value};
      }) {
      | Not_found => i
      }
    | _ => i
    };

  let leave_anf_program = ({signature} as p) => {
    // Support directly exporting imports
    let cmi_sign =
      List.map(
        fun
        | TSigValue(vid, {val_fullpath: Path.PIdent(id)} as vd) as item => {
            switch (Ident.find_same_opt(id, known_constants^)) {
            | Some(ImmId(immid)) =>
              TSigValue(vid, {...vd, val_fullpath: Path.PIdent(immid)})
            | _ => item
            };
          }
        | item => item,
        signature.cmi_sign,
      );
    {
      ...p,
      signature: {
        ...signature,
        cmi_sign,
      },
    };
  };
};

module ConstantPropagationMapper = Anf_mapper.MakeMap(ConstantPropagationArg);

let optimize = anfprog => {
  /* Reset state */
  known_constants := Ident.empty;
  ConstantPropagationMapper.map_anf_program(anfprog);
};
