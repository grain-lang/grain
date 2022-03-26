open Anftree;
open Anf_iterator;
open Grain_typed;

let direct_function_calls = Ident_tbl.create(32);

let register_function = id => {
  Ident_tbl.add(direct_function_calls, id, 0);
};

let mark_function_call = id => {
  switch (Ident_tbl.find_opt(direct_function_calls, id)) {
  | Some(count) => Ident_tbl.replace(direct_function_calls, id, count - 1)
  | None => ()
  };
};

let mark_ident_seen = id => {
  switch (Ident_tbl.find_opt(direct_function_calls, id)) {
  | Some(count) => Ident_tbl.replace(direct_function_calls, id, count + 1)
  | None => ()
  };
};

let has_indirect_call = id => {
  switch (Ident_tbl.find_opt(direct_function_calls, id)) {
  | Some(count) => count != 0
  | None => true
  };
};

module FuncCallsArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_imm_expression = ({imm_desc: desc}) =>
    switch (desc) {
    | ImmId(id) => mark_ident_seen(id)
    | _ => ()
    };

  let leave_comp_expression = ({comp_desc: desc}) =>
    switch (desc) {
    | CApp(({imm_desc: ImmId(id)}, _), _, _) => mark_function_call(id)
    | _ => ()
    };

  let enter_anf_expression = ({anf_desc: desc}) => {
    switch (desc) {
    | AELet(_, _, _, binds, _) =>
      List.iter(
        ((id, expr)) =>
          switch (expr.comp_desc) {
          | CLambda(_) => register_function(id)
          | _ => ()
          },
        binds,
      )
    | _ => ()
    };
  };

  let enter_anf_program = ({imports}) => {
    List.iter(
      ({imp_use_id, imp_shape}) => {
        switch (imp_shape) {
        | FunctionShape(_) => register_function(imp_use_id)
        | _ => ()
        }
      },
      imports,
    );
  };
};

module FuncCallsIterator = Anf_iterator.MakeIter(FuncCallsArg);

let analyze = anfprog => {
  Ident_tbl.clear(direct_function_calls);
  FuncCallsIterator.iter_anf_program(anfprog);
};
