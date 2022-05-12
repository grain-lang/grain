open Binaryen;
open Grain_utils;

// Defaults from https://github.com/WebAssembly/binaryen/blob/version_107/src/pass.h#L170-L171
let default_optimize_level = 2;
let default_shrink_level = 1;

let has_gc = wasm_mod =>
  List.mem(Module.Feature.gc, Module.get_features(wasm_mod));

// Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L546-L566
let default_global_optimization_pre_passes =
    (~optimize_level, ~shrink_level, wasm_mod) => {
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.duplicate_function_elimination]);
  Format.eprintf(
    "duplicate_function_elimination: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.memory_packing]);
  Format.eprintf("memory_packing: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2) {
      [Passes.once_reduction];
    } else {
      [];
    },
  );
  Format.eprintf("once_reduction: %f\n", Sys.time() -. start_time);
  // This isn't run
  Module.run_passes(
    wasm_mod,
    if (has_gc(wasm_mod)
        && /* TODO: getTypeSystem() == TypeSystem::Nominal && */ optimize_level
        >= 2) {
      [
        Passes.type_refining,
        Passes.signature_refining,
        Passes.global_refining,
        // Global type optimization can remove fields that are not needed, which can
        // remove ref.funcs that were once assigned to vtables but are no longer
        // needed, which can allow more code to be removed globally. After those,
        // constant field propagation can be more effective.
        Passes.gto,
        Passes.remove_unused_module_elements,
        Passes.cfp,
      ];
    } else {
      [];
    },
  );
};

// Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L447-L544
let default_function_optimization_passes =
    (~optimize_level, ~shrink_level, wasm_mod) => {
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // All the additions here are optional if DWARF must be preserved. That is,
    // when DWARF is relevant we run fewer optimizations.
    // FIXME: support DWARF in all of them.
    // Untangling to semi-ssa form is helpful (but best to ignore merges
    // so as to not introduce new copies).
    if (optimize_level >= 3 || shrink_level >= 1) {
      [Passes.ssa_nomerge];
    } else {
      [];
    },
  );
  Format.eprintf("ssa_nomerge: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod, // if we are willing to work very very hard, flatten the IR and do opts
    // that depend on flat IR
    if (optimize_level >= 4) {
      [
        Passes.flatten,
        // LocalCSE is particularly useful after flatten (see comment in the pass
        // itself), but we must simplify locals a little first (as flatten adds many
        // new and redundant ones, which make things seem different if we do not
        // run some amount of simplify-locals first).
        Passes.simplify_locals_notee_nostructure,
        Passes.local_cse,
        // TODO: add rereloop etc. here
      ];
    } else {
      [];
    },
  );
  Format.eprintf("optimize_level >= 4: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.dce]);
  Format.eprintf("dce: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.remove_unused_names]);
  Format.eprintf("remove_unused_names: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.remove_unused_brs]);
  Format.eprintf("remove_unused_brs: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.remove_unused_names]);
  Format.eprintf("remove_unused_names: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.optimize_instructions]);
  Format.eprintf("optimize_instructions: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.pick_load_signs];
    } else {
      [];
    },
  );
  Format.eprintf("pick_load_signs: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // early propagation
    if (optimize_level >= 3 || shrink_level >= 2) {
      [Passes.precompute_propagate];
    } else {
      [];
        //Passes.precompute
    },
  );
  Format.eprintf("precompute: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (Settings.get_low_memory_unused()) {
      if (optimize_level >= 3 || shrink_level >= 1) {
        [Passes.optimize_added_constants_propagate];
      } else {
        [Passes.optimize_added_constants];
      };
    } else {
      [];
    },
  );
  Format.eprintf(
    "optimize_added_constants_propagate: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.code_pushing];
    } else {
      [];
    },
  );
  Format.eprintf("code_pushing: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // don't create if/block return values yet, as coalesce can remove copies that
    // that could inhibit
    [Passes.simplify_locals_nostructure],
  );
  Format.eprintf(
    "simplify_locals_nostructure: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [
      Passes.vacuum // previous pass creates garbage
    ],
  );
  Format.eprintf("vacuum: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.reorder_locals]);
  Format.eprintf("reorder_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // simplify-locals opens opportunities for optimizations
    [Passes.remove_unused_brs],
  );
  Format.eprintf("remove_unused_brs: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level > 1 && has_gc(wasm_mod)) {
      [Passes.heap2local];
    } else {
      [];
    },
  );
  Format.eprintf("heap2local: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // if we are willing to work hard, also optimize copies before coalescing
    if (optimize_level >= 3 || shrink_level >= 2) {
      [
        Passes.merge_locals // very slow on e.g. sqlite
      ];
    } else {
      [];
    },
  );
  Format.eprintf("merge_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level > 1 && has_gc(wasm_mod)) {
      [
        // Coalescing may prevent subtyping (as a coalesced local must have the
        // supertype of all those combined into it), so subtype first.
        // TODO: when optimizing for size, maybe the order should reverse?
        Passes.local_subtyping,
      ];
    } else {
      [];
    },
  );
  Format.eprintf("local_subtyping: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.coalesce_locals]);
  Format.eprintf("coalesce_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 3 || shrink_level >= 1) {
      [Passes.local_cse];
    } else {
      [];
    },
  );
  Format.eprintf("local_cse: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.simplify_locals]);
  Format.eprintf("simplify_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.vacuum]);
  Format.eprintf("vacuum: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.reorder_locals]);
  Format.eprintf("reorder_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.coalesce_locals]);
  Format.eprintf("coalesce_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.reorder_locals]);
  Format.eprintf("reorder_locals: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.vacuum]);
  Format.eprintf("vacuum: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 3 || shrink_level >= 1) {
      [Passes.code_folding];
    } else {
      [];
    },
  );
  Format.eprintf("code_folding: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [
      Passes.merge_blocks // makes remove-unused-brs more effective
    ],
  );
  Format.eprintf("merge_blocks: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [
      Passes.remove_unused_brs // coalesce-locals opens opportunities
    ],
  );
  Format.eprintf("remove_unused_brs: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [
      Passes.remove_unused_names // remove-unused-brs opens opportunities
    ],
  );
  Format.eprintf("remove_unused_brs: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [
      Passes.merge_blocks // clean up remove-unused-brs new blocks
    ],
  );
  Format.eprintf("merge_blocks: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // late propagation
    if (optimize_level >= 3 || shrink_level >= 2) {
      [Passes.precompute_propagate];
    } else {
      [];
        // Passes.precompute
    },
  );
  Format.eprintf("precompute: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.optimize_instructions]);
  Format.eprintf("optimize_instructions: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2 || shrink_level >= 1) {
      [
        Passes.rse // after all coalesce-locals, and before a final vacuum
      ];
    } else {
      [];
    },
  );
  Format.eprintf("rse: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [Passes.vacuum] // just to be safe
  );
  Format.eprintf("vacuum: %f\n", Sys.time() -. start_time);
};

// Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L568-L599
let default_global_optimization_post_passes =
    (~optimize_level, ~shrink_level, wasm_mod) => {
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2 || shrink_level >= 1) {
      [Passes.dae_optimizing];
    } else {
      [];
    },
  );
  Format.eprintf("dae_optimizing: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.inlining_optimizing];
    } else {
      [];
    },
  );
  Format.eprintf("inlining_optimizing: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  // Optimizations show more functions as duplicate, so run this here in Post.
  Module.run_passes(wasm_mod, [Passes.duplicate_function_elimination]);
  Format.eprintf(
    "duplicate_function_elimination: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  // Optimizations show more functions as duplicate, so run this here in Post.
  Module.run_passes(wasm_mod, [Passes.duplicate_import_elimination]);
  Format.eprintf(
    "duplicate_import_elimination: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // perform after the number of functions is reduced by inlining-optimizing
    if (shrink_level >= 2) {
      [Passes.merge_similar_functions];
    } else {
      [];
    },
  );
  Format.eprintf("merge_similar_functions: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.simplify_globals_optimizing];
    } else {
      [Passes.simplify_globals];
    },
  );
  Format.eprintf(
    "simplify_globals_optimizing: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(wasm_mod, [Passes.remove_unused_module_elements]);
  Format.eprintf(
    "remove_unused_module_elements: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    [
      // may allow more inlining/dae/etc., need --converge for that
      Passes.directize,
    ],
  );
  Format.eprintf(
    "remove_unused_module_elements: %f\n",
    Sys.time() -. start_time,
  );
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // perform Stack IR optimizations here, at the very end of the
    // optimization pipeline
    if (optimize_level >= 2 || shrink_level >= 1) {
      [Passes.generate_stack_ir];
    } else {
      [];
    },
  );
  Format.eprintf("generate_stack_ir: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Module.run_passes(
    wasm_mod,
    // perform Stack IR optimizations here, at the very end of the
    // optimization pipeline
    if (optimize_level >= 2 || shrink_level >= 1) {
      [Passes.optimize_stack_ir];
    } else {
      [];
    },
  );
  Format.eprintf("optimize_stack_ir: %f\n", Sys.time() -. start_time);
};

let optimize =
    (
      ~optimize_level=default_optimize_level,
      ~shrink_level=default_shrink_level,
      wasm_mod,
    ) => {
  // Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L441-L445
  Format.eprintf("========= Module ==========\n");
  Format.eprintf("==== Global Pre Passes ====\n");
  let start_time = Sys.time();
  default_global_optimization_pre_passes(
    ~optimize_level,
    ~shrink_level,
    wasm_mod,
  );
  Format.eprintf("== Pre pass: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Format.eprintf("==== Function Passes =====\n");
  default_function_optimization_passes(
    ~optimize_level,
    ~shrink_level,
    wasm_mod,
  );
  Format.eprintf("== Function pass: %f\n", Sys.time() -. start_time);
  let start_time = Sys.time();
  Format.eprintf("==== Global Post Passes ====\n");
  default_global_optimization_post_passes(
    ~optimize_level,
    ~shrink_level,
    wasm_mod,
  );
  Format.eprintf("== Post pass: %f\n", Sys.time() -. start_time);
};
