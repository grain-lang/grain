open Anftree

let analysis_passes = [
  Analyze_purity.analyze;
  Analyze_tail_calls.analyze;
]

let optimization_passes = [
  Optimize_tail_calls.optimize;
  Optimize_constants.optimize;
  Optimize_simple_binops.optimize;
  Optimize_common_subexpressions.optimize;
  Optimize_dead_assignments.optimize;
  Optimize_dead_branches.optimize;
]

module ClearAnalysesArg : Anf_iterator.IterArgument = struct
  include Anf_iterator.DefaultIterArgument

  let leave_imm_expression {imm_analyses} =
    imm_analyses := []

  let leave_comp_expression {comp_analyses} =
    comp_analyses := []

  let leave_anf_expression {anf_analyses} =
    anf_analyses := []

  let leave_anf_program {analyses} =
    analyses := []
end

module ClearAnalyses = Anf_iterator.MakeIter(ClearAnalysesArg)

let clear_analyses prog =
  ClearAnalyses.iter_anf_program prog;
  prog

let analyze prog =
  List.iter (fun f -> f prog) analysis_passes

let run_optimization_pass prog =
  List.fold_left (fun prog f -> f prog) prog optimization_passes


let optimize_program (prog : Anftree.anf_program) : Anftree.anf_program =
  let rec pass n prog =
    if n <= 0 then
      prog
    else begin
      analyze prog;
      let opt = clear_analyses (run_optimization_pass prog) in
      if opt = prog then
        opt
      else
        pass (n - 1) opt
    end
  in
  (* TODO: Make 4 a config value *)
  pass 4 prog
