open Anftree;
open Grain_typed;
open Analyze_manual_memory_management;

module IWArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) => {
    switch (desc) {
    | CApp(({imm_desc: ImmId(id)}, _), [arg1], _)
        when is_manual_memory_management_call(id) => {
        ...c,
        // Other optimizations will remove the identifier completely
        // if appropriate
        comp_desc: CImmExpr(arg1),
      }
    | _ => c
    };
  };
};

module IWMapper = Anf_mapper.MakeMap(IWArg);

let optimize = anfprog =>
  if (Grain_utils.Config.no_gc^ && mod_has_manual_memory_management^) {
    IWMapper.map_anf_program(anfprog);
  } else {
    anfprog;
  };
