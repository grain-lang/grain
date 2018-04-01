
type optimization_settings = {
  verbose: bool;
  sound: bool;
  (*initial_functions: initial_func list*)
}

let optimize_program (prog : Anftree.anf_program) (opts : optimization_settings) : Anftree.anf_program =
  (* TODO: Port optimization to new tree *)
  prog
