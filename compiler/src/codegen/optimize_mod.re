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
  List.concat([
    [Passes.duplicate_function_elimination, Passes.memory_packing],
    if (optimize_level >= 2) {
      [Passes.once_reduction];
    } else {
      [];
    },
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
  ]);
};

// Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L447-L544
let default_function_optimization_passes =
    (~optimize_level, ~shrink_level, wasm_mod) => {
  List.concat([
    // All the additions here are optional if DWARF must be preserved. That is,
    // when DWARF is relevant we run fewer optimizations.
    // FIXME(BINARYEN): support DWARF in all of them.
    // Untangling to semi-ssa form is helpful (but best to ignore merges
    // so as to not introduce new copies).
    if (optimize_level >= 3 || shrink_level >= 1) {
      [Passes.ssa_nomerge];
    } else {
      [];
    },
    // if we are willing to work very very hard, flatten the IR and do opts
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
    [
      Passes.dce,
      Passes.remove_unused_names,
      Passes.remove_unused_brs,
      Passes.remove_unused_names,
      Passes.optimize_instructions,
    ],
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.pick_load_signs];
    } else {
      [];
    },
    // early propagation
    if (optimize_level >= 3 || shrink_level >= 2) {
      [Passes.precompute_propagate];
    } else {
      [Passes.precompute];
    },
    if (Settings.get_low_memory_unused()) {
      if (optimize_level >= 3 || shrink_level >= 1) {
        [Passes.optimize_added_constants_propagate];
      } else {
        [Passes.optimize_added_constants];
      };
    } else {
      [];
    },
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.code_pushing];
    } else {
      [];
    },
    [
      // don't create if/block return values yet, as coalesce can remove copies that
      // that could inhibit
      Passes.simplify_locals_nostructure,
      Passes.vacuum, // previous pass creates garbage
      Passes.reorder_locals,
      // simplify-locals opens opportunities for optimizations
      Passes.remove_unused_brs,
    ],
    if (optimize_level > 1 && has_gc(wasm_mod)) {
      [Passes.heap2local];
    } else {
      [];
    },
    // if we are willing to work hard, also optimize copies before coalescing
    if (optimize_level >= 3 || shrink_level >= 2) {
      [
        Passes.merge_locals // very slow on e.g. sqlite
      ];
    } else {
      [];
    },
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
    [Passes.coalesce_locals],
    if (optimize_level >= 3 || shrink_level >= 1) {
      [Passes.local_cse];
    } else {
      [];
    },
    [
      Passes.simplify_locals,
      Passes.vacuum,
      Passes.reorder_locals,
      Passes.coalesce_locals,
      Passes.reorder_locals,
      Passes.vacuum,
    ],
    if (optimize_level >= 3 || shrink_level >= 1) {
      [Passes.code_folding];
    } else {
      [];
    },
    [
      Passes.merge_blocks, // makes remove-unused-brs more effective
      Passes.remove_unused_brs, // coalesce-locals opens opportunities
      Passes.remove_unused_names, // remove-unused-brs opens opportunities
      Passes.merge_blocks // clean up remove-unused-brs new blocks
    ],
    // late propagation
    if (optimize_level >= 3 || shrink_level >= 2) {
      [Passes.precompute_propagate];
    } else {
      [Passes.precompute];
    },
    [Passes.optimize_instructions],
    if (optimize_level >= 2 || shrink_level >= 1) {
      [
        Passes.rse // after all coalesce-locals, and before a final vacuum
      ];
    } else {
      [];
    },
    [Passes.vacuum] // just to be safe
  ]);
};

// Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L568-L599
let default_global_optimization_post_passes =
    (~optimize_level, ~shrink_level, wasm_mod) => {
  List.concat([
    if (optimize_level >= 2 || shrink_level >= 1) {
      [Passes.dae_optimizing];
    } else {
      [];
    },
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.inlining_optimizing];
    } else {
      [];
    },
    // Optimizations show more functions as duplicate, so run this here in Post.
    [
      Passes.duplicate_function_elimination,
      Passes.duplicate_import_elimination,
    ],
    // perform after the number of functions is reduced by inlining-optimizing
    if (shrink_level >= 2) {
      [Passes.merge_similar_functions];
    } else {
      [];
    },
    if (optimize_level >= 2 || shrink_level >= 2) {
      [Passes.simplify_globals_optimizing];
    } else {
      [Passes.simplify_globals];
    },
    [
      Passes.remove_unused_module_elements,
      // may allow more inlining/dae/etc., need --converge for that
      Passes.directize,
    ],
    // perform Stack IR optimizations here, at the very end of the
    // optimization pipeline
    if (optimize_level >= 2 || shrink_level >= 1) {
      [Passes.generate_stack_ir, Passes.optimize_stack_ir];
    } else {
      [];
    },
  ]);
};

let optimize =
    (
      ~optimize_level=default_optimize_level,
      ~shrink_level=default_shrink_level,
      wasm_mod,
    ) => {
  // Translation of https://github.com/WebAssembly/binaryen/blob/version_107/src/passes/pass.cpp#L441-L445
  let default_optimizations_passes =
    List.concat([
      default_global_optimization_pre_passes(
        ~optimize_level,
        ~shrink_level,
        wasm_mod,
      ),
      default_function_optimization_passes(
        ~optimize_level,
        ~shrink_level,
        wasm_mod,
      ),
      default_global_optimization_post_passes(
        ~optimize_level,
        ~shrink_level,
        wasm_mod,
      ),
    ]);

  Module.run_passes(wasm_mod, default_optimizations_passes);
};
