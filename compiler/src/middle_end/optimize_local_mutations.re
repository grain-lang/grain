open Anftree;
open Grain_typed;
open Analyze_closure_scoped_vars;

let imported_vars = ref(Ident.Set.empty);

let is_optimizable_var = id =>
  // Imported vars should be treated as closure scoped wrt this optimization as
  // they must always be unboxed
  !is_closure_scoped_var(id) && !Ident.Set.mem(id, imported_vars^);

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
            | CPrim1(BoxBind, arg) when is_optimizable_var(bind_id) =>
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
    | CAssign({imm_desc: ImmId(id)}, arg) when is_optimizable_var(id) => {
        ...c,
        comp_desc: CLocalAssign(id, arg),
      }
    | CPrim1(UnboxBind, {imm_desc: ImmId(id)} as arg)
        when is_optimizable_var(id) => {
        ...c,
        comp_desc: CImmExpr(arg),
      }
    | _ => c
    };
};

module LocalMutationsMapper = Anf_mapper.MakeMap(LocalMutationsArg);

let optimize = anfprog => {
  imported_vars :=
    Ident.Set.of_list(
      List.map(({imp_use_id}) => imp_use_id, anfprog.imports.specs),
    );
  LocalMutationsMapper.map_anf_program(anfprog);
};
