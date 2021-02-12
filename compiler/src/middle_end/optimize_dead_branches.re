open Anftree;
open Grain_typed;
open Types;

module BranchArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let is_simple_case = ({anf_desc}) =>
    switch (anf_desc) {
    | AEComp(_) => true
    | _ => false
    };

  let has_optimizable_conditional = binds =>
    List.exists(
      ((_, {comp_desc})) =>
        switch (comp_desc) {
        | CIf({imm_desc: ImmConst(Const_bool(true))}, branch, _)
        | CIf({imm_desc: ImmConst(Const_bool(false))}, _, branch) => true
        | _ => false
        },
      binds,
    );

  let has_simple_optimizable_conditional = binds =>
    List.for_all(
      ((_, {comp_desc})) =>
        switch (comp_desc) {
        | CIf({imm_desc: ImmConst(Const_bool(true))}, branch, _)
        | CIf({imm_desc: ImmConst(Const_bool(false))}, _, branch) =>
          is_simple_case(branch)
        | _ => true
        },
      binds,
    );

  let extract_comp = ({anf_desc}) =>
    switch (anf_desc) {
    | AEComp(comp) => comp
    | _ => failwith("No extractable comp")
    };

  let rec relinearize = (id, global, {anf_desc} as a, cont) =>
    switch (anf_desc) {
    | AEComp(comp) => {
        ...a,
        anf_desc:
          AELet(global, Nonrecursive, Immutable, [(id, comp)], cont),
      }
    | AESeq(comp, body) => {
        ...a,
        anf_desc: AESeq(comp, relinearize(id, global, body, cont)),
      }
    | AELet(global, recursive, mutable_, binds, body) => {
        ...a,
        anf_desc:
          AELet(
            global,
            recursive,
            mutable_,
            binds,
            relinearize(id, global, body, cont),
          ),
      }
    };

  let enter_anf_expression = ({anf_desc: desc} as a) =>
    switch (desc) {
    | AEComp({
        comp_desc: CIf({imm_desc: ImmConst(Const_bool(true))}, branch, _),
      })
    | AEComp({
        comp_desc: CIf({imm_desc: ImmConst(Const_bool(false))}, _, branch),
      }) => branch
    | AELet(global, recursive, mutable_, binds, body)
        when has_simple_optimizable_conditional(binds) =>
      let binds =
        List.map(
          ((id, {comp_desc} as comp)) =>
            switch (comp_desc) {
            | CIf({imm_desc: ImmConst(Const_bool(true))}, branch, _)
            | CIf({imm_desc: ImmConst(Const_bool(false))}, _, branch) => (
                id,
                extract_comp(branch),
              )
            | _ => (id, comp)
            },
          binds,
        );
      {...a, anf_desc: AELet(global, recursive, mutable_, binds, body)};
    | AELet(global, Nonrecursive, mutable_, binds, body)
        when has_optimizable_conditional(binds) =>
      /* We can't relinearize recursive bindings since they depend on each other */
      List.fold_right(
        ((name, {comp_desc} as comp), cont) =>
          switch (comp_desc) {
          | CIf({imm_desc: ImmConst(Const_bool(true))}, _true, _) =>
            relinearize(name, global, _true, cont)
          | CIf({imm_desc: ImmConst(Const_bool(false))}, _, _false) =>
            relinearize(name, global, _false, cont)
          | _ => {
              ...a,
              anf_desc:
                AELet(global, Nonrecursive, mutable_, [(name, comp)], cont),
            }
          },
        binds,
        body,
      )
    | _ => a
    };
};

module BranchMapper = Anf_mapper.MakeMap(BranchArg);

let optimize = anfprog =>
  /* Reset state */
  BranchMapper.map_anf_program(anfprog);
