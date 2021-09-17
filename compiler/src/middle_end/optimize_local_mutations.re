open Anftree;
open Grain_typed;
open Analyze_closure_scoped_vars;
open Analyze_escapes;

let mutable_vars = ref(Ident.Set.empty);

let is_mutable_variable = id => Ident.Set.mem(id, mutable_vars^);
let mark_mutable = id => mutable_vars := Ident.Set.add(id, mutable_vars^);
let clear_mutable_variables = () => mutable_vars := Ident.Set.empty;

let imported_vars = ref(Ident.Set.empty);

let is_reducible = var => {
  !is_closure_scoped_var(var)
  && !var_escapes(var)
  && !Ident.Set.mem(var, imported_vars^);
};

let get_allocation_type_or_default = (env, opt_t, default) => {
  switch (opt_t) {
  | None => default
  | Some(t) => Type_utils.get_allocation_type(env, t)
  };
};

let rec alloc_type = a =>
  switch (a.anf_desc) {
  | AELet(_, _, _, _, b) => alloc_type(b)
  | AESeq(_, b) => alloc_type(b)
  | AEComp(e) => e.comp_allocation_type
  };

let strip_box = (opt_t: option(Types.type_expr)) => {
  switch (opt_t) {
  | None => None
  | Some({desc} as t) =>
    switch (desc) {
    | TTyConstr(path, [t], _) when path == Builtin_types.path_box => Some(t)
    | _ => Some(t)
    }
  };
};

module LocalMutationsArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_anf_expression = ({anf_desc: desc} as a) => {
    let {anf_desc} as aexp =
      switch (desc) {
      | AELet(g, r, m, binds, b) =>
        let mut_flag = ref(m);
        let binds =
          List.map(
            ((bind_id, expr)) => {
              switch (expr.comp_desc) {
              | CPrim1(Box, {imm_type} as arg) when is_reducible(bind_id) =>
                Printf.eprintf(
                  "translating box: %s<%d>\n",
                  bind_id.name,
                  bind_id.stamp,
                );
                mut_flag := Mutable;
                mark_mutable(bind_id);
                if (bind_id.name == "w") {
                  let tsexp =
                    Sexplib.Conv.sexp_of_option(
                      Types.sexp_of_type_expr,
                      imm_type,
                    );
                  Printf.eprintf(
                    "bind_id name: %s\ttype: %s (isNone: %s)\n",
                    bind_id.name,
                    Sexplib.Sexp.to_string_mach(tsexp),
                    string_of_bool(Option.is_none(imm_type)),
                  );
                };
                (
                  bind_id,
                  {
                    ...expr,
                    comp_desc: CImmExpr(arg),
                    comp_allocation_type:
                      get_allocation_type_or_default(
                        expr.comp_env,
                        imm_type,
                        expr.comp_allocation_type,
                      ),
                  },
                );
              | CPrim1(Box, _) =>
                Printf.eprintf(
                  "not translating box: %s<%d>\n",
                  bind_id.name,
                  bind_id.stamp,
                );
                (bind_id, expr);
              | _ => (bind_id, expr)
              }
            },
            binds,
          );
        {...a, anf_desc: AELet(g, r, mut_flag^, binds, b)};
      | _ => a
      };
    switch (anf_desc) {
    | AELet(_, _, _, _, b) => {...aexp, anf_allocation_type: alloc_type(b)}
    | AESeq(_, tl) => {...aexp, anf_allocation_type: alloc_type(tl)}
    | AEComp(c) => {...aexp, anf_allocation_type: alloc_type(aexp)}
    };
  };

  let leave_comp_expression = ({comp_desc: desc, comp_env: env} as c) =>
    switch (desc) {
    | CBoxAssign({imm_desc: ImmId(id), imm_type}, arg) when is_reducible(id) =>
      if (id.name == "w") {
        let tsexp =
          Sexplib.Conv.sexp_of_option(
            Types.sexp_of_type_expr,
            strip_box(imm_type),
          );
        Printf.eprintf(
          "name: %s\ttype: %s (isNone: %s)\n",
          id.name,
          Sexplib.Sexp.to_string_mach(tsexp),
          string_of_bool(Option.is_none(imm_type)),
        );
      };
      {
        ...c,
        comp_desc: CLocalAssign(id, arg),
        comp_allocation_type:
          get_allocation_type_or_default(
            env,
            strip_box(imm_type),
            c.comp_allocation_type,
          ),
      };
    | CPrim1(Unbox, {imm_desc: ImmId(id), imm_type} as arg)
        when is_reducible(id) => {
        ...c,
        comp_desc: CImmExpr(arg),
        comp_allocation_type:
          get_allocation_type_or_default(
            env,
            strip_box(imm_type),
            c.comp_allocation_type,
          ),
      }
    | _ => c
    };
};

module LocalMutationsMapper = Anf_mapper.MakeMap(LocalMutationsArg);

let optimize = anfprog => {
  imported_vars :=
    List.fold_left(
      (acc, impt) => Ident.Set.add(impt.imp_use_id, acc),
      Ident.Set.empty,
      anfprog.imports,
    );
  Printf.eprintf("imported_vars: %s\n", Ident.Set.to_string(imported_vars^));
  LocalMutationsMapper.map_anf_program(anfprog);
};
