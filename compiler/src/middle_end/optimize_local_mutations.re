open Anftree;
open Grain_typed;
open Analyze_closure_scoped_vars;

module LocalMutationsArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | AELet(g, r, m, binds, b) =>
      let mut_flag = ref(m);
      let binds =
        List.map(
          ((bind_id, expr)) => {
            switch (expr.comp_desc) {
            | CPrim1(BoxBind, arg) when !is_closure_scoped_var(bind_id) =>
              mut_flag := Mutable;
              (bind_id, {...expr, comp_desc: CImmExpr(arg)});
            | _ => (bind_id, expr)
            }
          },
          binds,
        );
      {...a, anf_desc: AELet(g, r, mut_flag^, binds, b)};
    | _ => a
    };
  };

  let leave_comp_expression = ({comp_desc: desc} as c) =>
    switch (desc) {
    | CAssign({imm_desc: ImmId(id)}, arg) when !is_closure_scoped_var(id) => {
        ...c,
        comp_desc: CLocalAssign(id, arg),
      }
    | CPrim1(UnboxBind, {imm_desc: ImmId(id)} as arg)
        when !is_closure_scoped_var(id) => {
        ...c,
        comp_desc: CImmExpr(arg),
      }
    | _ => c
    };
};

module LocalMutationsMapper = Anf_mapper.MakeMap(LocalMutationsArg);

let optimize = anfprog => {
  LocalMutationsMapper.map_anf_program(anfprog);
};
