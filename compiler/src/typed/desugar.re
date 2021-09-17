open Typedtree;

module DesugarMapArgument = {
  include TypedtreeMap.DefaultMapArgument;

  // Workaround for TExpAssign
  let lhs_stack = ref([]);
  let push_lhs = x => lhs_stack := [x, ...lhs_stack^];
  let pop_lhs = () =>
    switch (lhs_stack^) {
    | [] => failwith("Impossible")
    | [hd, ...tl] =>
      lhs_stack := tl;
      hd;
    };

  let enter_expression = ({exp_desc} as exp) => {
    switch (exp_desc) {
    | TExpAssign(lhs, _) => push_lhs(lhs)
    //| TExpBoxAssign(lhs, _) => push_lhs(lhs)
    | _ => ()
    };
    exp;
  };

  let leave_expression = ({exp_desc} as exp) => {
    switch (exp_desc) {
    // Convert `let mut x = y` into `let x = box(y)`
    | TExpLet(recflag, Mutable, binds) =>
      let new_binds =
        List.map(
          ({vb_expr} as vb) => {
            let {exp_type} = vb_expr;
            {
              ...vb,
              vb_expr: {
                ...vb_expr,
                exp_desc: TExpPrim1(Box, vb_expr),
                exp_type: Builtin_types.type_box(exp_type),
              },
            };
          },
          binds,
        );
      {...exp, exp_desc: TExpLet(recflag, Immutable, new_binds)};
    | TExpAssign(_, b) =>
      let a = pop_lhs();
      {
        ...exp,
        exp_desc:
          TExpBoxAssign(
            {...a, exp_type: Builtin_types.type_box(a.exp_type)},
            b,
          ),
      };
    // Convert use-sites of mutable variables `x` into `unbox(x)`
    | TExpIdent(path, _, {val_mutable}) when val_mutable => {
        ...exp,
        exp_desc:
          TExpPrim1(
            Unbox,
            {...exp, exp_type: Builtin_types.type_box(exp.exp_type)},
          ),
      }
    | _ => exp
    };
  };

  let leave_toplevel_stmt = ({ttop_desc} as stmt) => {
    switch (ttop_desc) {
    // Convert `let mut x = y` into `let x = box(y)`
    | TTopLet(expflag, recflag, Mutable, binds) =>
      let new_binds =
        List.map(
          ({vb_expr} as vb) => {
            {
              ...vb,
              vb_expr: {
                ...vb_expr,
                exp_desc: TExpPrim1(Box, vb_expr),
                exp_type: Builtin_types.type_box(vb_expr.exp_type),
              },
            }
          },
          binds,
        );
      {...stmt, ttop_desc: TTopLet(expflag, recflag, Immutable, new_binds)};
    | _ => stmt
    };
  };
};

module DesugarMapper = TypedtreeMap.MakeMap(DesugarMapArgument);

let desugar = DesugarMapper.map_typed_program;
