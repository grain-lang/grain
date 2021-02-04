open Anftree;
open Grain_typed;

let get_comp_purity = c =>
  Option.value(~default=false) @@ Analyze_purity.comp_expression_purity(c);

module DSEArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_anf_expression = ({anf_desc: desc} as a) =>
    switch (desc) {
    | AESeq(hd, tl) when get_comp_purity(hd) => tl
    | AESeq(_)
    | AELet(_)
    | AEComp(_) => a
    };
};

module DSEMapper = Anf_mapper.MakeMap(DSEArg);

let optimize = anfprog => {
  DSEMapper.map_anf_program(anfprog);
};
