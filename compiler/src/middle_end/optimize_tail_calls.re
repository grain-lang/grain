open Anftree;
open Grain_typed;
open Types;
open Analyze_tail_calls;

let comp_is_tail_call = c =>
  Option.value(~default=false) @@ Analyze_tail_calls.comp_is_tail_call(c);

module TailCallsArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) =>
    switch (desc) {
    | CApp(f, args, _) when comp_is_tail_call(c) => {
        ...c,
        comp_desc: CApp(f, args, true),
      }
    | _ => c
    };
};

module TailCallsMapper = Anf_mapper.MakeMap(TailCallsArg);

let optimize = anfprog => TailCallsMapper.map_anf_program(anfprog);
