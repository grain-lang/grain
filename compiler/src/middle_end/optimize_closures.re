open Anftree;
open Grain_typed;

module ClosuresArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_anf_expression = ({anf_desc: desc} as a) =>
    switch (desc) {
    | AELet(_, _, Mutable, _, _)
    | AESeq(_)
    | AEComp(_) => a
    | AELet(g, r, m, binds, b) =>
      let binds =
        List.map(
          ((id, value)) => {
            switch (value.comp_desc) {
            | CLambda(name, args, (body, body_ty), Uncomputed) =>
              if (!Analyze_function_calls.has_indirect_call(id)) {
                let used_var_set =
                  Ident.Set.remove(
                    id,
                    Analyze_free_vars.anf_free_vars(body),
                  );
                let arg_vars = List.map(((arg, _)) => arg, args);
                let accessible_var_set =
                  Ident.Set.union(
                    Analyze_globals.get_globals(),
                    Ident.Set.of_list(arg_vars),
                  );
                let free_var_set =
                  Ident.Set.diff(used_var_set, accessible_var_set);
                if (Ident.Set.is_empty(free_var_set)) {
                  (
                    id,
                    {
                      ...value,
                      comp_desc:
                        CLambda(name, args, (body, body_ty), Unnecessary),
                    },
                  );
                } else {
                  (
                    id,
                    {
                      ...value,
                      comp_desc:
                        CLambda(
                          name,
                          args,
                          (body, body_ty),
                          Precomputed(
                            List.of_seq(Ident.Set.to_seq(free_var_set)),
                          ),
                        ),
                    },
                  );
                };
              } else {
                (id, value);
              }
            | _ => (id, value)
            }
          },
          binds,
        );
      {...a, anf_desc: AELet(g, r, m, binds, b)};
    };
};

module ClosuresMapper = Anf_mapper.MakeMap(ClosuresArg);

let optimize = anfprog => {
  ClosuresMapper.map_anf_program(anfprog);
};
