open Anftree;

let analysis_passes = [
  Analyze_globals.analyze,
  Analyze_function_calls.analyze,
  Analyze_manual_memory_management.analyze,
  Analyze_purity.analyze,
  Analyze_tail_calls.analyze,
  Analyze_inline_wasm.analyze,
  Analyze_free_vars.analyze,
  Analyze_closure_scoped_vars.analyze,
  Analyze_mutable_vars.analyze,
];

let optimization_passes = [
  Optimize_manual_memory_management.optimize,
  Optimize_tail_calls.optimize,
  Optimize_constants.optimize,
  Optimize_simple_expressions.optimize,
  Optimize_dead_assignments.optimize,
  Optimize_dead_branches.optimize,
  Optimize_dead_statements.optimize,
  Optimize_inline_wasm.optimize,
  Optimize_local_mutations.optimize,
];

module ClearAnalysesArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_imm_expression = ({imm_analyses}) => imm_analyses := [];

  let leave_comp_expression = ({comp_analyses}) => comp_analyses := [];

  let leave_anf_expression = ({anf_analyses}) => anf_analyses := [];

  let leave_anf_program = ({analyses}) => analyses := [];
};

module ClearAnalyses = Anf_iterator.MakeIter(ClearAnalysesArg);

let clear_analyses = prog => {
  ClearAnalyses.iter_anf_program(prog);
  prog;
};

let analyze = prog => List.iter(f => f(prog), analysis_passes);

let run_optimization_pass = prog =>
  List.fold_left((prog, f) => f(prog), prog, optimization_passes);

let optimize_program = (prog: Anftree.anf_program): Anftree.anf_program => {
  let rec pass = (n, prog) =>
    if (n <= 0) {
      prog;
    } else {
      analyze(clear_analyses(prog));
      let opt = run_optimization_pass(prog);
      pass(n - 1, opt);
    };

  // TODO: Make 4 a config value
  pass(4, prog);
};
