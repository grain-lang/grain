open Grain_typed;
open Mashtree;
open Value_tags;
open Binaryen;
open Concatlist; /* NOTE: This import shadows (@) and introduces (@+) and (+@) */
open Grain_utils;
open Comp_utils;
open Comp_wasm_prim;

let sources: ref(list((Expression.t, Grain_parsing.Location.t))) = ref([]);

/* [TODO] Should probably be a config variable */
let memory_tracing_enabled = false;

/** Environment */

type codegen_env = {
  num_args: int,
  global_offset: int,
  stack_size,
  import_global_offset: int,
  import_offset: int,
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Expression.t, closure_data))),
  imported_funcs: Ident.tbl(Ident.tbl(int32)),
  imported_globals: Ident.tbl(Ident.tbl(string)),
};

let gensym_counter = ref(0);
let gensym_label = s => {
  gensym_counter := gensym_counter^ + 1;
  Printf.sprintf("%s.%d", s, gensym_counter^);
};

/* Number of swap variables to allocate */
let swap_slots_i32 = [|Type.int32, Type.int32|];
let swap_slots_i64 = [|Type.int64|];
let swap_i32_offset = 0;
let swap_i64_offset = Array.length(swap_slots_i32);
let swap_slots = Array.append(swap_slots_i32, swap_slots_i64);

/* These are the bare-minimum imports needed for basic runtime support */
let module_runtime_id = Ident.create_persistent("moduleRuntimeId");
let reloc_base = Ident.create_persistent("relocBase");
let table_size = Ident.create_persistent("GRAIN$TABLE_SIZE");
let runtime_mod = Ident.create_persistent("grainRuntime");
let stdlib_external_runtime_mod =
  Ident.create_persistent("stdlib-external/runtime");
let console_mod = Ident.create_persistent("console");
let check_memory_ident = Ident.create_persistent("checkMemory");
let throw_error_ident = Ident.create_persistent("throwError");
let malloc_ident = Ident.create_persistent("malloc");
let incref_ident = Ident.create_persistent("incRef");
let new_rational_ident = Ident.create_persistent("newRational");
let new_float32_ident = Ident.create_persistent("newFloat32");
let new_float64_ident = Ident.create_persistent("newFloat64");
let new_int32_ident = Ident.create_persistent("newInt32");
let new_int64_ident = Ident.create_persistent("newInt64");
let number_to_int64_ident = Ident.create_persistent("coerceNumberToInt64");
let int64_to_number_ident = Ident.create_persistent("coerceInt64ToNumber");
let int32_to_number_ident = Ident.create_persistent("coerceInt32ToNumber");
let float32_to_number_ident =
  Ident.create_persistent("coerceFloat32ToNumber");
let float64_to_number_ident =
  Ident.create_persistent("coerceFloat64ToNumber");
let equal_ident = Ident.create_persistent("equal");
/* Variants used for tracing */
let incref_adt_ident = Ident.create_persistent("incRefADT");
let incref_array_ident = Ident.create_persistent("incRefArray");
let incref_tuple_ident = Ident.create_persistent("incRefTuple");
let incref_box_ident = Ident.create_persistent("incRefBox");
let incref_backpatch_ident = Ident.create_persistent("incRefBackpatch");
let incref_swap_bind_ident = Ident.create_persistent("incRefSwapBind");
let incref_arg_bind_ident = Ident.create_persistent("incRefArgBind");
let incref_local_bind_ident = Ident.create_persistent("incRefLocalBind");
let incref_global_bind_ident = Ident.create_persistent("incRefGlobalBind");
let incref_closure_bind_ident = Ident.create_persistent("incRefClosureBind");
let incref_cleanup_locals_ident =
  Ident.create_persistent("incRefCleanupLocals");
let incref64_ident = Ident.create_persistent("incRef64");
let decref_ident = Ident.create_persistent("decRef");
let decref64_ident = Ident.create_persistent("decRef64");
let decref_array_ident = Ident.create_persistent("decRefArray");
let decref_tuple_ident = Ident.create_persistent("decRefTuple");
let decref_box_ident = Ident.create_persistent("decRefBox");
let decref_swap_bind_ident = Ident.create_persistent("decRefSwapBind");
let decref_arg_bind_ident = Ident.create_persistent("decRefArgBind");
let decref_local_bind_ident = Ident.create_persistent("decRefLocalBind");
let decref_global_bind_ident = Ident.create_persistent("decRefGlobalBind");
let decref_closure_bind_ident = Ident.create_persistent("decRefClosureBind");
let decref_cleanup_locals_ident =
  Ident.create_persistent("decRefCleanupLocals");
let decref_cleanup_globals_ident =
  Ident.create_persistent("decRefCleanupGlobals");
let decref_ignore_zeros_ident = Ident.create_persistent("decRefIgnoreZeros");
let decref_drop_ident = Ident.create_persistent("decRefDrop");
let tracepoint_ident = Ident.create_persistent("tracepoint");

let traced_imports =
  if (memory_tracing_enabled) {
    [
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_adt_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_array_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_tuple_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_box_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_backpatch_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_swap_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_arg_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_local_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_global_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_closure_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_cleanup_locals_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_array_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_tuple_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_box_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_swap_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_arg_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_local_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_global_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_closure_bind_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_cleanup_locals_ident,
        mimp_type: MFuncImport([I32Type, I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_cleanup_globals_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_drop_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
    ];
  } else {
    [
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_ignore_zeros_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
    ];
  };

let runtime_global_imports = [
  {
    mimp_mod: runtime_mod,
    mimp_name: reloc_base,
    mimp_type: MGlobalImport(I32Type),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: runtime_mod,
    mimp_name: module_runtime_id,
    mimp_type: MGlobalImport(I32Type),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
];

let runtime_function_imports =
  List.append(
    [
      {
        mimp_mod: runtime_mod,
        mimp_name: check_memory_ident,
        mimp_type: MFuncImport([I32Type], []),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: malloc_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref64_ident,
        mimp_type: MFuncImport([I64Type], [I64Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref64_ident,
        mimp_type: MFuncImport([I64Type], [I64Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: console_mod,
        mimp_name: tracepoint_ident,
        mimp_type: MFuncImport([I32Type], []),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: throw_error_ident,
        mimp_type:
          MFuncImport(
            List.init(Runtime_errors.max_arity + 1, _ => I32Type),
            [],
          ),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: new_rational_ident,
        mimp_type: MFuncImport([I32Type, I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: new_float32_ident,
        mimp_type: MFuncImport([F32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: new_float64_ident,
        mimp_type: MFuncImport([F64Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: new_int32_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: new_int64_ident,
        mimp_type: MFuncImport([I64Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: number_to_int64_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: int64_to_number_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: int32_to_number_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: float32_to_number_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: float64_to_number_ident,
        mimp_type: MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: stdlib_external_runtime_mod,
        mimp_name: equal_ident,
        mimp_type: MFuncImport([I32Type, I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
    ],
    traced_imports,
  ); /* <- 2nd argument to List.append */

let runtime_imports =
  List.append(runtime_global_imports, runtime_function_imports);

let init_codegen_env = () => {
  num_args: 0,
  global_offset: 2,
  stack_size: {
    stack_size_i32: 0,
    stack_size_i64: 0,
    stack_size_f32: 0,
    stack_size_f64: 0,
  },
  import_global_offset: 0,
  import_offset: 0,
  backpatches: ref([]),
  imported_funcs: Ident.empty,
  imported_globals: Ident.empty,
};

let lookup_ext_global = (env, modname, itemname) =>
  Ident.find_same(itemname, Ident.find_same(modname, env.imported_globals));

let var_of_ext_global = (env, modname, itemname) =>
  lookup_ext_global(env, modname, itemname);

let lookup_ext_func = (env, modname, itemname) =>
  Ident.find_same(itemname, Ident.find_same(modname, env.imported_funcs));

let get_imported_name = (mod_, name) =>
  Printf.sprintf(
    "import_%s_%s",
    Ident.unique_name(mod_),
    Ident.unique_name(name),
  );

let name_of_memory_tracing_func = (mod_, itemname, default) =>
  get_imported_name(mod_) @@
  (if (memory_tracing_enabled) {itemname} else {default});

let call_runtime_check_memory = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, check_memory_ident),
    args,
    Type.int32,
  );
let call_runtime_throw_error = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, throw_error_ident),
    args,
    Type.none,
  );

let call_malloc = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, malloc_ident),
    args,
    Type.int32,
  );
let call_incref = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      get_imported_name(runtime_mod, incref_ident),
      [arg],
      Type.int32,
    );
  };
let call_incref_adt = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_adt_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_array = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_array_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_tuple = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_tuple_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_box = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_box_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_backpatch = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_backpatch_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_swap_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_swap_bind_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_arg_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_arg_bind_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_local_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_local_bind_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_global_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_global_bind_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_closure_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_closure_bind_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_cleanup_locals = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        incref_cleanup_locals_ident,
        incref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_incref_64 = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      get_imported_name(runtime_mod, incref64_ident),
      [arg],
      Type.int32,
    );
  };
let call_decref = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      get_imported_name(runtime_mod, decref_ident),
      [arg],
      Type.int32,
    );
  };
let call_decref_64 = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      get_imported_name(runtime_mod, decref64_ident),
      [arg],
      Type.int32,
    );
  };
let call_decref_array = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_array_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_tuple = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_tuple_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_box = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_box_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_swap_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_swap_bind_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_arg_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_arg_bind_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_local_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_local_bind_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_global_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_global_bind_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_closure_bind = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_closure_bind_ident,
        decref_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_decref_cleanup_locals = (wasm_mod, env, args) =>
  if (Config.no_gc^) {
    List.hd(args);
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_cleanup_locals_ident,
        decref_ignore_zeros_ident,
      ),
      args,
      Type.int32,
    );
  };
let decref_cleanup_globals_name =
  name_of_memory_tracing_func(
    runtime_mod,
    decref_cleanup_globals_ident,
    decref_ignore_zeros_ident,
  );
let call_decref_cleanup_globals = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      decref_cleanup_globals_name,
      [arg],
      Type.int32,
    );
  };
let call_decref_drop = (wasm_mod, env, arg) =>
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      name_of_memory_tracing_func(
        runtime_mod,
        decref_drop_ident,
        decref_ignore_zeros_ident,
      ),
      [arg],
      Type.int32,
    );
  };
let call_new_rational = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, new_rational_ident),
    args,
    Type.int32,
  );
let call_new_float32 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, new_float32_ident),
    args,
    Type.int32,
  );
let call_new_float64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, new_float64_ident),
    args,
    Type.int32,
  );
let call_new_int32 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, new_int32_ident),
    args,
    Type.int32,
  );
let call_new_int64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, new_int64_ident),
    args,
    Type.int32,
  );
let call_number_to_int64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, number_to_int64_ident),
    args,
    Type.int32,
  );
let call_int64_to_number = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, int64_to_number_ident),
    args,
    Type.int32,
  );
let call_int32_to_number = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, int32_to_number_ident),
    args,
    Type.int32,
  );
let call_float32_to_number = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, float32_to_number_ident),
    args,
    Type.int32,
  );
let call_float64_to_number = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, float64_to_number_ident),
    args,
    Type.int32,
  );
let call_equal = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(stdlib_external_runtime_mod, equal_ident),
    args,
    Type.int32,
  );

/** Will print "tracepoint <n> reached" to the console when executed (for debugging WASM output) */

let tracepoint = (wasm_mod, env, n) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, tracepoint_ident),
    [Expression.const(wasm_mod, const_int32(n))],
    Type.int32,
  );

/** Untags the number */

let untag_number = (wasm_mod, value) =>
  Expression.binary(
    wasm_mod,
    Op.shr_s_int32,
    value,
    Expression.const(wasm_mod, const_int32(1)),
  );

let tag_number = (wasm_mod, value) =>
  Expression.binary(
    wasm_mod,
    Op.or_int32,
    Expression.binary(
      wasm_mod,
      Op.shl_int32,
      value,
      Expression.const(wasm_mod, const_int32(1)),
    ),
    Expression.const(wasm_mod, const_int32(1)),
  );

let encode_bool = (wasm_mod, value) =>
  Expression.binary(
    wasm_mod,
    Op.or_int32,
    Expression.binary(
      wasm_mod,
      Op.shl_int32,
      value,
      Expression.const(wasm_mod, const_int32(31)),
    ),
    Expression.const(wasm_mod, const_false()),
  );

let decode_bool = (wasm_mod, value) =>
  Expression.binary(
    wasm_mod,
    Op.shr_u_int32,
    value,
    Expression.const(wasm_mod, const_int32(31)),
  );

let encoded_const_int32 = n => const_int32(encoded_int32(n));

type bind_action =
  | BindGet
  | BindSet(Expression.t)
  | BindTee(Expression.t);

let cleanup_local_slot_instructions = (wasm_mod, env: codegen_env) => {
  let instrs =
    List.init(
      env.stack_size.stack_size_i32,
      i => {
        let slot_no = i + env.num_args + Array.length(swap_slots);
        let args = [Expression.local_get(wasm_mod, slot_no, Type.int32)];
        let args =
          if (memory_tracing_enabled) {
            List.append(
              args,
              [Expression.const(wasm_mod, const_int32(slot_no))],
            );
          } else {
            args;
          };
        singleton @@
        Expression.drop(
          wasm_mod,
          call_decref_cleanup_locals(wasm_mod, env, args),
        );
      },
    );
  flatten(instrs);
};

let compile_bind =
    (
      ~action,
      ~skip_incref=false,
      ~skip_decref=false,
      wasm_mod: Module.t,
      env: codegen_env,
      b: binding,
    )
    : Expression.t => {
  let appropriate_incref = (env, arg) =>
    switch (b) {
    | _ when skip_incref => arg /* This case is used for storing swap values that have freshly been heap-allocated. */
    | MArgBind(_, I32Type) => call_incref_arg_bind(wasm_mod, env, arg)
    | MArgBind(_) => arg
    | MLocalBind(_, I32Type) => call_incref_local_bind(wasm_mod, env, arg)
    | MLocalBind(_) => arg
    | MSwapBind(_, I32Type) => call_incref_swap_bind(wasm_mod, env, arg)
    | MSwapBind(_) => arg
    | MGlobalBind(_, I32Type, true) =>
      call_incref_global_bind(wasm_mod, env, arg)
    | MGlobalBind(_) => arg
    | MClosureBind(_) => call_incref_closure_bind(wasm_mod, env, arg)
    | _ => call_incref(wasm_mod, env, arg)
    };

  let appropriate_decref = (env, arg) =>
    switch (b) {
    | _ when skip_decref => arg /* This case is used for storing swap values that have freshly been heap-allocated. */
    | MArgBind(_, I32Type) => call_decref_arg_bind(wasm_mod, env, arg)
    | MArgBind(_) => arg
    | MLocalBind(_, I32Type) => call_decref_local_bind(wasm_mod, env, arg)
    | MLocalBind(_) => arg
    | MSwapBind(_, I32Type) => call_decref_swap_bind(wasm_mod, env, arg)
    | MSwapBind(_) => arg
    | MGlobalBind(_, I32Type, _) =>
      call_decref_global_bind(wasm_mod, env, arg)
    | MGlobalBind(_) => arg
    | MClosureBind(_) => call_decref_closure_bind(wasm_mod, env, arg)
    | _ => call_decref(wasm_mod, env, arg)
    };

  switch (b) {
  | MArgBind(i, wasm_ty) =>
    /* No adjustments are needed for argument bindings */
    let typ =
      switch (wasm_ty) {
      | I32Type => Type.int32
      | I64Type => Type.int64
      | F32Type => Type.float32
      | F64Type => Type.float64
      };
    let slot = Int32.to_int(i);
    switch (action) {
    | BindGet => Expression.local_get(wasm_mod, slot, typ)
    | BindSet(arg) =>
      Expression.local_set(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.local_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
      )
    | BindTee(arg) =>
      Expression.local_tee(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.local_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
        typ,
      )
    };
  | MLocalBind(i, wasm_ty) =>
    /* Local bindings need to be offset to account for arguments and swap variables */
    let (typ, slot) =
      switch (wasm_ty) {
      | I32Type => (
          Type.int32,
          env.num_args + Array.length(swap_slots) + Int32.to_int(i),
        )
      | I64Type => (
          Type.int64,
          env.num_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_i32
          + Int32.to_int(i),
        )
      | F32Type => (
          Type.float32,
          env.num_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_i32
          + env.stack_size.stack_size_i64
          + Int32.to_int(i),
        )
      | F64Type => (
          Type.float64,
          env.num_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_i32
          + env.stack_size.stack_size_i64
          + env.stack_size.stack_size_f32
          + Int32.to_int(i),
        )
      };
    switch (action) {
    | BindGet => Expression.local_get(wasm_mod, slot, typ)
    | BindSet(arg) =>
      Expression.local_set(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.local_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
      )
    | BindTee(arg) =>
      Expression.local_tee(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.local_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
        typ,
      )
    };
  | MSwapBind(i, wasm_ty) =>
    /* Swap bindings need to be offset to account for arguments */
    let slot = env.num_args + Int32.to_int(i);
    let typ =
      switch (wasm_ty) {
      | I32Type => Type.int32
      | I64Type => Type.int64
      | F32Type => Type.float32
      | F64Type => Type.float64
      };
    switch (action) {
    | BindGet => Expression.local_get(wasm_mod, slot, typ)
    | BindSet(arg) =>
      Expression.local_set(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.local_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
      )
    | BindTee(arg) =>
      Expression.local_tee(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.local_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
        typ,
      )
    };
  | MGlobalBind(slot, wasm_ty, gc) =>
    let typ =
      switch (wasm_ty) {
      | I32Type => Type.int32
      | I64Type => Type.int64
      | F32Type => Type.float32
      | F64Type => Type.float64
      };
    switch (action) {
    | BindGet => Expression.global_get(wasm_mod, slot, typ)
    | BindSet(arg) when !gc => Expression.global_set(wasm_mod, slot, arg)
    | BindSet(arg) =>
      Expression.global_set(
        wasm_mod,
        slot,
        Expression.tuple_extract(
          wasm_mod,
          Expression.tuple_make(
            wasm_mod,
            [
              /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                 Note that this preserves the stack. */
              appropriate_incref(env, arg),
              /* Get old value of slot and call decref() on it */
              appropriate_decref(
                env,
                Expression.global_get(wasm_mod, slot, typ),
              ),
            ],
          ),
          0,
        ),
      )
    | BindTee(arg) when !gc =>
      Expression.block(
        wasm_mod,
        gensym_label("BindTee"),
        [
          Expression.global_set(wasm_mod, slot, arg),
          Expression.global_get(wasm_mod, slot, typ),
        ],
      )
    | BindTee(arg) =>
      Expression.block(
        wasm_mod,
        gensym_label("BindTee"),
        [
          Expression.global_set(
            wasm_mod,
            slot,
            Expression.tuple_extract(
              wasm_mod,
              Expression.tuple_make(
                wasm_mod,
                [
                  /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
                     Note that this preserves the stack. */
                  appropriate_incref(env, arg),
                  /* Get old value of slot and call decref() on it */
                  appropriate_decref(
                    env,
                    Expression.global_get(wasm_mod, slot, typ),
                  ),
                ],
              ),
              0,
            ),
          ),
          Expression.global_get(wasm_mod, slot, typ),
        ],
      )
    };
  | MClosureBind(i) =>
    /* Closure bindings need to be calculated */
    if (!(action == BindGet)) {
      failwith(
        "Internal error: attempted to emit instruction which would mutate closure contents",
      );
    };
    load(
      ~offset=4 * (4 + Int32.to_int(i)),
      wasm_mod,
      Expression.local_get(wasm_mod, 0, Type.int32),
    );
  | MImport(i) =>
    if (!(action == BindGet)) {
      failwith(
        "Internal error: attempted to emit instruction which would mutate an import",
      );
    };
    /* Adjust for runtime functions */
    let slot =
      Printf.sprintf("global_%d", env.import_offset + Int32.to_int(i));
    Expression.global_get(wasm_mod, slot, Type.int32);
  };
};

let safe_drop = (wasm_mod, env, arg) =>
  Expression.drop(wasm_mod, call_decref_drop(wasm_mod, env, arg));

let get_swap = (~ty as typ=I32Type, wasm_mod, env, idx) =>
  switch (typ) {
  | I32Type =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), I32Type),
    );
  | I64Type =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), I64Type),
    );
  | _ => raise(Not_found)
  };

let set_swap =
    (
      ~skip_incref=true,
      ~skip_decref=true,
      ~ty as typ=I32Type,
      wasm_mod,
      env,
      idx,
      arg,
    ) =>
  switch (typ) {
  | I32Type =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet(arg),
      ~skip_incref,
      ~skip_decref,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), I32Type),
    );
  | I64Type =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet(arg),
      ~skip_incref,
      ~skip_decref,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), I64Type),
    );
  | _ => raise(Not_found)
  };

let tee_swap =
    (
      ~ty as typ=I32Type,
      ~skip_incref=true,
      ~skip_decref=true,
      wasm_mod,
      env,
      idx,
      arg,
    ) =>
  switch (typ) {
  | I32Type =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee(arg),
      ~skip_incref,
      ~skip_decref,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), I32Type),
    );
  | I64Type =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee(arg),
      ~skip_incref,
      ~skip_decref,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), I64Type),
    );
  | _ => raise(Not_found)
  };

/* [TODO] This is going to need some sort of type argument as well...I think this could be indicative of a code smell */
let cleanup_locals = (wasm_mod, env: codegen_env, arg): Expression.t => {
  /* Do the following:
     - Move the current stack value into a designated return-value holder slot (maybe swap is fine)
     - Call incref() on the return value (to prevent premature free)
     - Call decref() on all locals (should include return value) */
  Expression.block(wasm_mod, gensym_label("cleanup_locals")) @@
  Concatlist.list_of_t(
    singleton(
      Expression.drop(wasm_mod) @@
      call_incref_cleanup_locals(
        wasm_mod,
        env,
        tee_swap(wasm_mod, env, 0, arg),
      ),
    )
    @ cleanup_local_slot_instructions(wasm_mod, env)
    +@ [get_swap(wasm_mod, env, 0)],
  );
};

let compile_imm = (wasm_mod, env: codegen_env, i: immediate): Expression.t =>
  switch (i) {
  | MImmConst(c) => Expression.const(wasm_mod, compile_const(c))
  | MImmBinding(b) => compile_bind(~action=BindGet, wasm_mod, env, b)
  };

let call_error_handler = (wasm_mod, env, err, args) => {
  let pad_val = MImmConst(MConstI32(Int32.zero));
  let args = Runtime_errors.pad_args(pad_val, args);
  let err_code =
    Expression.const(
      wasm_mod,
      const_int32 @@ Runtime_errors.code_of_error(err),
    );
  let compiled_args = [
    err_code,
    ...List.map(compile_imm(wasm_mod, env), args),
  ];
  Expression.block(
    wasm_mod,
    gensym_label("call_error_handler"),
    [
      call_runtime_throw_error(wasm_mod, env, compiled_args),
      Expression.unreachable(wasm_mod),
    ],
  );
};

let error_if_true = (wasm_mod, env, cond, err, args) =>
  Expression.if_(
    wasm_mod,
    cond,
    call_error_handler(wasm_mod, env, err, args),
    Expression.null(),
  );

let dummy_err_val = MImmConst(MConstI32(Int32.zero));
/* Checks whether an Int64 overflowed */
let check_overflow = (wasm_mod, env, arg) =>
  Expression.block(
    wasm_mod,
    gensym_label("check_overflow"),
    [
      /* WASM has no concept of overflows, so we have to check manually */
      set_swap(~ty=I64Type, wasm_mod, env, 0, arg),
      error_if_true(
        wasm_mod,
        env,
        Expression.binary(
          wasm_mod,
          Op.gt_s_int64,
          get_swap(~ty=I64Type, wasm_mod, env, 0),
          Expression.const(
            wasm_mod,
            const_int64(Int32.to_int(Int32.max_int)),
          ),
        ),
        OverflowError,
        [dummy_err_val, dummy_err_val],
      ),
      error_if_true(
        wasm_mod,
        env,
        Expression.binary(
          wasm_mod,
          Op.lt_s_int64,
          get_swap(~ty=I64Type, wasm_mod, env, 0),
          Expression.const(
            wasm_mod,
            const_int64(Int32.to_int(Int32.min_int)),
          ),
        ),
        OverflowError,
        [dummy_err_val, dummy_err_val],
      ),
      get_swap(~ty=I64Type, wasm_mod, env, 0),
    ],
  );

let compile_tuple_op = (~is_box=false, wasm_mod, env, tup_imm, op) => {
  let tup = () => compile_imm(wasm_mod, env, tup_imm);
  switch (op) {
  | MTupleGet(idx) =>
    let idx_int = Int32.to_int(idx);
    /* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. */
    load(~offset=4 * (idx_int + 2), wasm_mod, tup());
  | MTupleSet(idx, imm) =>
    let idx_int = Int32.to_int(idx);
    Expression.block(
      wasm_mod,
      gensym_label("MTupleSet"),
      [
        store(
          ~offset=4 * (idx_int + 2),
          wasm_mod,
          tup(),
          Expression.tuple_extract(
            wasm_mod,
            Expression.tuple_make(
              wasm_mod,
              [
                (if (is_box) {call_incref_box} else {call_incref_tuple})(
                  wasm_mod,
                  env,
                  compile_imm(wasm_mod, env, imm),
                ),
                (if (is_box) {call_decref_box} else {call_decref_tuple})(
                  wasm_mod,
                  env,
                  load(~offset=4 * (idx_int + 2), wasm_mod, tup()),
                ),
              ],
            ),
            0,
          ),
        ),
        compile_imm(wasm_mod, env, imm),
      ],
    );
  };
};

let compile_box_op = (wasm_mod, env, box_imm, op) =>
  /* At the moment, we make no runtime distinction between boxes and tuples */
  switch (op) {
  | MBoxUnbox =>
    compile_tuple_op(
      ~is_box=true,
      wasm_mod,
      env,
      box_imm,
      MTupleGet(Int32.zero),
    )
  | MBoxUpdate(imm) =>
    compile_tuple_op(
      ~is_box=true,
      wasm_mod,
      env,
      box_imm,
      MTupleSet(Int32.zero, imm),
    )
  };

let compile_array_op = (wasm_mod, env, arr_imm, op) => {
  let get_swap = n => get_swap(wasm_mod, env, n);
  let set_swap = n => set_swap(wasm_mod, env, n);
  let tee_swap = n => tee_swap(wasm_mod, env, n);
  let get_arr = () => compile_imm(wasm_mod, env, arr_imm);
  switch (op) {
  | MArrayGet(idx_imm) =>
    let idx = compile_imm(wasm_mod, env, idx_imm);
    let get_idx = () => get_swap(1);
    /* Check that the index is in bounds */
    Expression.block(
      wasm_mod,
      gensym_label("MArrayGet"),
      [
        set_swap(1, untag_number(wasm_mod, idx)),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.gt_s_int32,
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              load(~offset=4, wasm_mod, get_arr()),
              Expression.const(wasm_mod, const_int32(-1)),
            ),
            get_idx(),
          ),
          ArrayIndexOutOfBounds,
          [],
        ),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.le_s_int32,
            load(~offset=4, wasm_mod, get_arr()),
            get_idx(),
          ),
          ArrayIndexOutOfBounds,
          [],
        ),
        load(
          ~offset=8,
          wasm_mod,
          Expression.binary(
            wasm_mod,
            Op.add_int32,
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              /* Resolve a negative index */
              Expression.if_(
                wasm_mod,
                Expression.binary(
                  wasm_mod,
                  Op.lt_s_int32,
                  get_idx(),
                  Expression.const(wasm_mod, const_int32(0)),
                ),
                Expression.binary(
                  wasm_mod,
                  Op.add_int32,
                  get_idx(),
                  load(~offset=4, wasm_mod, get_arr()),
                ),
                get_idx(),
              ),
              Expression.const(wasm_mod, const_int32(4)),
            ),
            get_arr(),
          ),
        ),
      ],
    );
  | MArrayLength =>
    tag_number(wasm_mod, load(~offset=4, wasm_mod, get_arr()))
  | MArraySet(idx_imm, val_imm) =>
    let idx = compile_imm(wasm_mod, env, idx_imm);
    let val_ = compile_imm(wasm_mod, env, val_imm);
    let get_idx = () => get_swap(1);
    /* Check that the index is in bounds */
    Expression.block(
      wasm_mod,
      gensym_label("MArrayGet"),
      [
        set_swap(1, untag_number(wasm_mod, idx)),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.gt_s_int32,
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              load(~offset=4, wasm_mod, get_arr()),
              Expression.const(wasm_mod, const_int32(-1)),
            ),
            get_idx(),
          ),
          ArrayIndexOutOfBounds,
          [],
        ),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.le_s_int32,
            load(~offset=4, wasm_mod, get_arr()),
            get_idx(),
          ),
          ArrayIndexOutOfBounds,
          [],
        ),
        store(
          ~offset=8,
          wasm_mod,
          Expression.binary(
            wasm_mod,
            Op.add_int32,
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              /* Resolve a negative index */
              Expression.if_(
                wasm_mod,
                Expression.binary(
                  wasm_mod,
                  Op.lt_s_int32,
                  get_idx(),
                  Expression.const(wasm_mod, const_int32(0)),
                ),
                Expression.binary(
                  wasm_mod,
                  Op.add_int32,
                  get_idx(),
                  load(~offset=4, wasm_mod, get_arr()),
                ),
                get_idx(),
              ),
              Expression.const(wasm_mod, const_int32(4)),
            ),
            get_arr(),
          ),
          /* [TODO] decref the old item */
          call_incref(wasm_mod, env, tee_swap(1, val_)),
        ),
        get_swap(1),
      ],
    );
  };
};

let compile_adt_op = (wasm_mod, env, adt_imm, op) => {
  let adt = compile_imm(wasm_mod, env, adt_imm);
  switch (op) {
  | MAdtGet(idx) =>
    let idx_int = Int32.to_int(idx);
    load(~offset=4 * (idx_int + 5), wasm_mod, adt);
  | MAdtGetModule => load(~offset=4, wasm_mod, adt)
  | MAdtGetTag => load(~offset=12, wasm_mod, adt)
  };
};

let compile_record_op = (wasm_mod, env, rec_imm, op) => {
  let record = () => compile_imm(wasm_mod, env, rec_imm);
  switch (op) {
  | MRecordGet(idx) =>
    let idx_int = Int32.to_int(idx);
    load(~offset=4 * (idx_int + 4), wasm_mod, record());
  | MRecordSet(idx, arg_imm) =>
    let idx_int = Int32.to_int(idx);
    let arg = () => compile_imm(wasm_mod, env, arg_imm);
    Expression.block(
      wasm_mod,
      gensym_label("record_set"),
      [
        store(
          ~offset=4 * (idx_int + 4),
          wasm_mod,
          record(),
          Expression.tuple_extract(
            wasm_mod,
            Expression.tuple_make(
              wasm_mod,
              [
                call_incref(wasm_mod, env, arg()),
                call_decref(
                  wasm_mod,
                  env,
                  load(~offset=4 * (idx_int + 4), wasm_mod, record()),
                ),
              ],
            ),
            0,
          ),
        ),
        arg(),
      ],
    );
  };
};

/** Heap allocations. */

let round_up = (num: int, multiple: int): int => num + num mod multiple;

/** Rounds the given number of words to be aligned correctly */

let round_allocation_size = (num_words: int): int => round_up(num_words, 4);

let heap_allocate = (wasm_mod, env, num_words: int) => {
  let words_to_allocate = round_allocation_size(num_words);
  call_malloc(
    wasm_mod,
    env,
    [Expression.const(wasm_mod, const_int32(4 * words_to_allocate))],
  );
};

let heap_allocate_imm =
    (~additional_words=0, wasm_mod, env, num_words: immediate) => {
  let num_words = () =>
    untag_number(wasm_mod, compile_imm(wasm_mod, env, num_words));
  if (additional_words == 0) {
    call_malloc(
      wasm_mod,
      env,
      [
        Expression.binary(
          wasm_mod,
          Op.mul_int32,
          num_words(),
          Expression.const(wasm_mod, const_int32(4)),
        ),
      ],
    );
  } else {
    call_malloc(
      wasm_mod,
      env,
      [
        Expression.binary(
          wasm_mod,
          Op.add_int32,
          Expression.binary(
            wasm_mod,
            Op.mul_int32,
            num_words(),
            Expression.const(wasm_mod, const_int32(4)),
          ),
          Expression.const(wasm_mod, const_int32(additional_words * 4)),
        ),
      ],
    );
  };
};

let buf_to_ints = (buf: Buffer.t): list(int64) => {
  let num_bytes = Buffer.length(buf);
  let num_ints = num_bytes / 8;
  let num_ints = num_bytes mod 8 == 0 ? num_ints : num_ints + 1;
  let total_bytes = num_ints * 8;

  let bytes = Buffer.to_bytes(buf);
  let bytes = Bytes.extend(bytes, 0, total_bytes - num_bytes);
  // Clear out those uninitialized bytes
  Bytes.fill(bytes, num_bytes, total_bytes - num_bytes, '\000');

  List.init(num_ints, i => {Bytes.get_int64_ne(bytes, i * 8)});
};

let call_lambda = (~tail=false, wasm_mod, env, func, (argsty, retty), args) => {
  let compiled_func = () => compile_imm(wasm_mod, env, func);
  let compiled_args = List.map(compile_imm(wasm_mod, env), args);
  let instr =
    if (tail) {Expression.return_call_indirect} else {
      Expression.call_indirect
    };
  instr(
    wasm_mod,
    load(~offset=8, wasm_mod, compiled_func()),
    [compiled_func(), ...compiled_args],
    Type.create @@ Array.map(wasm_type, Array.of_list([I32Type, ...argsty])),
    wasm_type(retty),
  );
};

let allocate_string = (wasm_mod, env, str) => {
  let buf = Buffer.create(80);
  Buffer.add_string(buf, str);

  let ints_to_push: list(int64) = buf_to_ints(buf);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(~skip_incref=true, wasm_mod, env, 0);
  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        call_malloc(
          wasm_mod,
          env,
          [
            Expression.const(
              wasm_mod,
              const_int32 @@ 4 * (2 + 2 * List.length(ints_to_push)),
            ),
          ],
        ),
      ),
      Expression.const(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(StringType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, const_int32 @@ String.length(str)),
    ),
  ];
  let elts =
    List.mapi(
      (idx, i: int64) =>
        store(
          ~ty=Type.int64,
          ~offset=8 * (idx + 1),
          wasm_mod,
          get_swap(),
          Expression.const(wasm_mod, wrap_int64(i)),
        ),
      ints_to_push,
    );
  Expression.block(
    wasm_mod,
    gensym_label("allocate_string"),
    List.concat([preamble, elts, [get_swap()]]),
  );
};

let allocate_char = (wasm_mod, env, char) => {
  // Copy bytes into a fresh buffer so we can guarantee a copy of a full word
  let bytes = Bytes.make(4, Char.chr(0));
  // OCaml String#length is byte length, not Unicode character length
  // Guaranteed not to be longer than 4 bytes by well-formedness
  Bytes.blit_string(char, 0, bytes, 0, String.length(char));
  let value = Bytes.get_int32_le(bytes, 0);

  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(~skip_incref=true, wasm_mod, env, 0);
  Expression.block(
    wasm_mod,
    gensym_label("allocate_char"),
    [
      store(
        ~offset=0,
        wasm_mod,
        tee_swap(
          call_malloc(
            wasm_mod,
            env,
            [Expression.const(wasm_mod, const_int32(8))],
          ),
        ),
        Expression.const(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(CharType)),
        ),
      ),
      store(
        ~offset=4,
        wasm_mod,
        get_swap(),
        Expression.const(wasm_mod, wrap_int32(value)),
      ),
      get_swap(),
    ],
  );
};

let allocate_float32 = (wasm_mod, env, i) => {
  call_new_float32(wasm_mod, env, [i]);
};

let allocate_float64 = (wasm_mod, env, i) => {
  call_new_float64(wasm_mod, env, [i]);
};

let allocate_int32 = (wasm_mod, env, i) => {
  call_new_int32(wasm_mod, env, [i]);
};

let allocate_int64 = (wasm_mod, env, i) => {
  call_new_int64(wasm_mod, env, [i]);
};

let allocate_rational = (wasm_mod, env, n, d) => {
  call_new_rational(wasm_mod, env, [n, d]);
};

let allocate_closure =
    (
      wasm_mod,
      env,
      ~lambda=?,
      ~skip_patching=false,
      {func_idx, arity, variables} as closure_data,
    ) => {
  let num_free_vars = List.length(variables);
  let closure_size = num_free_vars + 4;
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let patches = ref([]);
  if (skip_patching) {
    let access_lambda =
      Option.value(
        ~default=
          Expression.binary(
            wasm_mod,
            Op.sub_int32,
            get_swap(),
            Expression.const(
              wasm_mod,
              const_int32 @@ 4 * round_allocation_size(closure_size),
            ),
          ),
        lambda,
      );
    env.backpatches := [(access_lambda, closure_data), ...env.backpatches^];
  } else {
    let patch_var = (idx, var) =>
      store(
        ~offset=4 * (idx + 4),
        wasm_mod,
        get_swap(),
        call_incref_backpatch(
          wasm_mod,
          env,
          compile_imm(wasm_mod, env, var),
        ),
      );
    patches := List.mapi(patch_var, variables);
  };
  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        ~skip_incref=true,
        wasm_mod,
        env,
        0,
        heap_allocate(wasm_mod, env, closure_size),
      ),
      Expression.const(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(LambdaType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, wrap_int32(arity)),
    ),
    store(
      ~offset=8,
      wasm_mod,
      get_swap(),
      Expression.binary(
        wasm_mod,
        Op.add_int32,
        Expression.global_get(
          wasm_mod,
          get_imported_name(runtime_mod, reloc_base),
          Type.int32,
        ),
        Expression.const(wasm_mod, wrap_int32(func_idx)),
      ),
    ),
    store(
      ~offset=12,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, const_int32(num_free_vars)),
    ),
  ];
  let postamble = [get_swap()];
  Expression.block(
    wasm_mod,
    gensym_label("allocate_closure"),
    List.concat([preamble, patches^, postamble]),
  );
};

let allocate_adt = (wasm_mod, env, ttag, vtag, elts) => {
  /* Heap memory layout of ADT types:
      [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
     */
  let num_elts = List.length(elts);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let compile_elt = (idx, elt) =>
    store(
      ~offset=4 * (idx + 5),
      wasm_mod,
      get_swap(),
      call_incref_adt(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        ~skip_incref=true,
        wasm_mod,
        env,
        0,
        heap_allocate(wasm_mod, env, num_elts + 5),
      ),
      Expression.const(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ADTType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.binary(
        wasm_mod,
        Op.mul_int32,
        Expression.global_get(
          wasm_mod,
          get_imported_name(runtime_mod, module_runtime_id),
          Type.int32,
        ),
        Expression.const(wasm_mod, const_int32(2)),
      ),
    ),
    store(~offset=8, wasm_mod, get_swap(), compile_imm(wasm_mod, env, ttag)),
    store(
      ~offset=12,
      wasm_mod,
      get_swap(),
      compile_imm(wasm_mod, env, vtag),
    ),
    store(
      ~offset=16,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.block(
    wasm_mod,
    gensym_label("allocate_adt"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let allocate_tuple = (~is_box=false, wasm_mod, env, elts) => {
  let num_elts = List.length(elts);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let compile_elt = (idx, elt) =>
    store(
      ~offset=4 * (idx + 2),
      wasm_mod,
      get_swap(),
      (if (is_box) {call_incref_box} else {call_incref_tuple})(
        wasm_mod,
        env,
        compile_imm(wasm_mod, env, elt),
      ),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        ~skip_incref=true,
        wasm_mod,
        env,
        0,
        heap_allocate(wasm_mod, env, num_elts + 2),
      ),
      Expression.const(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(TupleType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.block(
    wasm_mod,
    gensym_label("allocate_tuple"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let allocate_box = (wasm_mod, env, elt) =>
  /* At the moment, we make no runtime distinction between boxes and tuples */
  allocate_tuple(~is_box=true, wasm_mod, env, [elt]);

let allocate_array = (wasm_mod, env, elts) => {
  let num_elts = List.length(elts);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let compile_elt = (idx, elt) =>
    store(
      ~offset=4 * (idx + 2),
      wasm_mod,
      get_swap(),
      call_incref_array(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        ~skip_incref=true,
        wasm_mod,
        env,
        0,
        heap_allocate(wasm_mod, env, num_elts + 2),
      ),
      Expression.const(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ArrayType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.block(
    wasm_mod,
    gensym_label("allocate_array"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let allocate_array_n = (wasm_mod, env, num_elts, elt) => {
  let get_arr_addr = () => get_swap(wasm_mod, env, 0);
  let get_loop_counter = () => get_swap(wasm_mod, env, 1);
  let set_loop_counter = set_swap(wasm_mod, env, 1);

  let compiled_num_elts = () => compile_imm(wasm_mod, env, num_elts);
  let elt = compile_imm(wasm_mod, env, elt);

  let outer_label = gensym_label("outer");
  let inner_label = gensym_label("inner");
  Expression.block(
    wasm_mod,
    gensym_label("allocate_array_n"),
    [
      error_if_true(
        wasm_mod,
        env,
        Expression.binary(
          wasm_mod,
          Op.lt_s_int32,
          compiled_num_elts(),
          Expression.const(wasm_mod, encoded_const_int32(0)),
        ),
        InvalidArgument,
        [num_elts],
      ),
      store(
        ~offset=0,
        wasm_mod,
        tee_swap(
          ~skip_incref=true,
          wasm_mod,
          env,
          0,
          heap_allocate_imm(~additional_words=2, wasm_mod, env, num_elts),
        ),
        Expression.const(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(ArrayType)),
        ),
      ),
      store(
        ~offset=4,
        wasm_mod,
        get_arr_addr(),
        untag_number(wasm_mod, compiled_num_elts()),
      ),
      set_loop_counter(Expression.const(wasm_mod, encoded_const_int32(0))),
      Expression.block(
        wasm_mod,
        outer_label,
        [
          Expression.loop(
            wasm_mod,
            inner_label,
            Expression.block(
              wasm_mod,
              gensym_label("loop"),
              [
                Expression.break(
                  wasm_mod,
                  outer_label,
                  Expression.binary(
                    wasm_mod,
                    Op.ge_s_int32,
                    get_loop_counter(),
                    compiled_num_elts(),
                  ),
                  Expression.null(),
                ),
                store(
                  ~offset=8,
                  wasm_mod,
                  Expression.binary(
                    wasm_mod,
                    Op.add_int32,
                    get_arr_addr(),
                    Expression.binary(
                      wasm_mod,
                      Op.mul_int32,
                      untag_number(wasm_mod, get_loop_counter()),
                      Expression.const(wasm_mod, const_int32(4)),
                    ),
                  ),
                  call_incref_array(wasm_mod, env, elt),
                ),
                set_loop_counter(
                  Expression.binary(
                    wasm_mod,
                    Op.add_int32,
                    get_loop_counter(),
                    Expression.const(wasm_mod, const_int32(2)),
                  ),
                ),
                Expression.break(
                  wasm_mod,
                  inner_label,
                  Expression.null(),
                  Expression.null(),
                ),
              ],
            ),
          ),
        ],
      ),
      get_arr_addr(),
    ],
  );
};

let allocate_array_init = (wasm_mod, env, num_elts, init_f) => {
  let get_arr_addr = () => get_swap(wasm_mod, env, 0);
  let get_loop_counter = () => get_swap(wasm_mod, env, 1);
  let set_loop_counter = set_swap(wasm_mod, env, 1);

  let compiled_num_elts = () => compile_imm(wasm_mod, env, num_elts);

  let compiled_func = () => compile_imm(wasm_mod, env, init_f);

  let outer_label = gensym_label("outer");
  let inner_label = gensym_label("inner");
  Expression.block(
    wasm_mod,
    gensym_label("allocate_array_init"),
    [
      error_if_true(
        wasm_mod,
        env,
        Expression.binary(
          wasm_mod,
          Op.lt_s_int32,
          compiled_num_elts(),
          Expression.const(wasm_mod, encoded_const_int32(0)),
        ),
        InvalidArgument,
        [num_elts],
      ),
      store(
        ~offset=0,
        wasm_mod,
        tee_swap(
          ~skip_incref=true,
          wasm_mod,
          env,
          0,
          heap_allocate_imm(~additional_words=2, wasm_mod, env, num_elts),
        ),
        Expression.const(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(ArrayType)),
        ),
      ),
      store(
        ~offset=4,
        wasm_mod,
        get_arr_addr(),
        untag_number(wasm_mod, compiled_num_elts()),
      ),
      set_loop_counter(Expression.const(wasm_mod, encoded_const_int32(0))),
      Expression.block(
        wasm_mod,
        outer_label,
        [
          Expression.loop(
            wasm_mod,
            inner_label,
            Expression.block(
              wasm_mod,
              gensym_label("loop"),
              [
                Expression.break(
                  wasm_mod,
                  outer_label,
                  Expression.binary(
                    wasm_mod,
                    Op.ge_s_int32,
                    get_loop_counter(),
                    compiled_num_elts(),
                  ),
                  Expression.null(),
                ),
                store(
                  ~offset=8,
                  wasm_mod,
                  Expression.binary(
                    wasm_mod,
                    Op.add_int32,
                    get_arr_addr(),
                    Expression.binary(
                      wasm_mod,
                      Op.mul_int32,
                      untag_number(wasm_mod, get_loop_counter()),
                      Expression.const(wasm_mod, const_int32(4)),
                    ),
                  ),
                  call_incref_array(
                    wasm_mod,
                    env,
                    Expression.call_indirect(
                      wasm_mod,
                      load(~offset=8, wasm_mod, compiled_func()),
                      [compiled_func(), get_loop_counter()],
                      Type.create([|Type.int32, Type.int32|]),
                      Type.int32,
                    ),
                  ),
                ),
                set_loop_counter(
                  Expression.binary(
                    wasm_mod,
                    Op.add_int32,
                    get_loop_counter(),
                    Expression.const(wasm_mod, const_int32(2)),
                  ),
                ),
                Expression.break(
                  wasm_mod,
                  inner_label,
                  Expression.null(),
                  Expression.null(),
                ),
              ],
            ),
          ),
        ],
      ),
      get_arr_addr(),
    ],
  );
};

let allocate_record = (wasm_mod, env, ttag, elts) => {
  let (_, elts) = List.split(elts);
  /* Heap memory layout of records:
      [ <value type tag>, <module_tag>, <type_tag>, <arity> ordered elts ... ]
     */
  let num_elts = List.length(elts);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let compile_elt = (idx, elt) =>
    store(
      ~offset=4 * (idx + 4),
      wasm_mod,
      get_swap(),
      call_incref_box(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        ~skip_incref=true,
        wasm_mod,
        env,
        0,
        heap_allocate(wasm_mod, env, num_elts + 4),
      ),
      Expression.const(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(RecordType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      /* Tag the runtime id */
      Expression.binary(
        wasm_mod,
        Op.mul_int32,
        Expression.global_get(
          wasm_mod,
          get_imported_name(runtime_mod, module_runtime_id),
          Type.int32,
        ),
        Expression.const(wasm_mod, const_int32(2)),
      ),
    ),
    store(~offset=8, wasm_mod, get_swap(), compile_imm(wasm_mod, env, ttag)),
    store(
      ~offset=12,
      wasm_mod,
      get_swap(),
      Expression.const(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.block(
    wasm_mod,
    gensym_label("allocate_record"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let compile_prim1 = (wasm_mod, env, p1, arg): Expression.t => {
  let compiled_arg = compile_imm(wasm_mod, env, arg);
  /* TODO: Overflow checks? */
  switch (p1) {
  | Incr =>
    /*
       2a + 1 -> number representation
       2(a + 1) + 1 -> adding 1 to a
       2a + 2 + 1
       (2a + 1) + 2 -> can just add 2
     */
    Expression.binary(
      wasm_mod,
      Op.add_int32,
      compiled_arg,
      Expression.const(wasm_mod, const_int32(2)),
    )
  | Decr =>
    /* Likewise, just subtract 2 */
    Expression.binary(
      wasm_mod,
      Op.sub_int32,
      compiled_arg,
      Expression.const(wasm_mod, const_int32(2)),
    )
  | Not =>
    /* Flip the first bit */
    Expression.binary(
      wasm_mod,
      Op.xor_int32,
      compiled_arg,
      Expression.const(wasm_mod, const_int32(0x80000000)),
    )
  | Ignore =>
    Expression.block(
      wasm_mod,
      gensym_label("Ignore"),
      [
        Expression.drop(
          wasm_mod,
          call_decref_drop(wasm_mod, env, compiled_arg),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    )
  | ArrayLength => compile_array_op(wasm_mod, env, arg, MArrayLength)
  | Assert =>
    Expression.block(
      wasm_mod,
      gensym_label("Assert"),
      [
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.eq_int32,
            compiled_arg,
            Expression.const(wasm_mod, const_false()),
          ),
          AssertionError,
          [],
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    )
  | FailWith => call_error_handler(wasm_mod, env, Failure, [arg])
  | Int64FromNumber => call_number_to_int64(wasm_mod, env, [compiled_arg])
  | Int64ToNumber => call_int64_to_number(wasm_mod, env, [compiled_arg])
  | Int32ToNumber => call_int32_to_number(wasm_mod, env, [compiled_arg])
  | Float32ToNumber => call_float32_to_number(wasm_mod, env, [compiled_arg])
  | Float64ToNumber => call_float64_to_number(wasm_mod, env, [compiled_arg])
  | Int64Lnot =>
    failwith("Unreachable case; should never get here: Int64LNot")
  | Box => failwith("Unreachable case; should never get here: Box")
  | Unbox => failwith("Unreachable case; should never get here: Unbox")
  | BoxBind => failwith("Unreachable case; should never get here: BoxBind")
  | UnboxBind =>
    failwith("Unreachable case; should never get here: UnboxBind")
  | WasmFromGrain
  | WasmToGrain => compiled_arg // These are no-ops
  | WasmUnaryI32({wasm_op, ret_type})
  | WasmUnaryI64({wasm_op, ret_type})
  | WasmUnaryF32({wasm_op, ret_type})
  | WasmUnaryF64({wasm_op, ret_type}) =>
    compile_wasm_prim1(wasm_mod, env, wasm_op, ret_type, compiled_arg)
  };
};

let compile_wasm_load =
    (~sz=?, ~ty=?, ~signed=?, wasm_mod, compiled_arg1, compiled_arg2, offset) => {
  switch (offset) {
  | MImmConst(MConstLiteral(MConstI32(offset))) =>
    load(
      ~sz?,
      ~ty?,
      ~signed?,
      ~offset=Int32.to_int(offset),
      wasm_mod,
      compiled_arg1(),
    )
  | _ =>
    load(
      ~sz?,
      ~ty?,
      ~signed?,
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.add_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  };
};

let compile_wasm_store = (~sz=?, ~ty=?, wasm_mod, env, args) => {
  switch (List.nth(args, 2)) {
  | MImmConst(MConstLiteral(MConstI32(offset))) =>
    Expression.block(
      wasm_mod,
      gensym_label("wasm_prim_store"),
      [
        store(
          ~sz?,
          ~ty?,
          ~offset=Int32.to_int(offset),
          wasm_mod,
          compile_imm(wasm_mod, env, List.nth(args, 0)),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    )

  | _ =>
    Expression.block(
      wasm_mod,
      gensym_label("wasm_prim_store"),
      [
        store(
          ~sz?,
          ~ty?,
          wasm_mod,
          Expression.binary(
            wasm_mod,
            Op.add_int32,
            compile_imm(wasm_mod, env, List.nth(args, 0)),
            compile_imm(wasm_mod, env, List.nth(args, 2)),
          ),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    )
  };
};

let compile_prim2 = (wasm_mod, env: codegen_env, p2, arg1, arg2): Expression.t => {
  let compiled_arg1 = () => compile_imm(wasm_mod, env, arg1);
  let compiled_arg2 = () => compile_imm(wasm_mod, env, arg2);
  let swap_get = () => get_swap(wasm_mod, env, 0);
  let swap_tee = tee_swap(wasm_mod, env, 0);
  // [TODO] (#300) Clean out a lot of these unreachable cases

  switch (p2) {
  | Plus => failwith("Unreachable case; should never get here: Plus")
  | Minus => failwith("Unreachable case; should never get here: Minus")
  | Times => failwith("Unreachable case; should never get here: Times")
  | Divide => failwith("Unreachable case; should never get here: Divide")
  | Mod => failwith("Unreachable case; should never get here: Mod")
  | And =>
    Expression.if_(
      wasm_mod,
      decode_bool(wasm_mod, swap_tee(compiled_arg1())),
      compiled_arg2(),
      swap_get(),
    )
  | Or =>
    Expression.if_(
      wasm_mod,
      decode_bool(wasm_mod, swap_tee(compiled_arg1())),
      swap_get(),
      compiled_arg2(),
    )
  | Greater
  | Int64Gt =>
    failwith("Unreachable case; should never get here: Greater/Int64Gt")
  | GreaterEq
  | Int64Gte =>
    failwith("Unreachable case; should never get here: GreaterEq/Int64Gte")
  | Less
  | Int64Lt =>
    failwith("Unreachable case; should never get here: Less/Int64Lt")
  | LessEq
  | Int64Lte =>
    failwith("Unreachable case; should never get here: LessEq/Int64Lte")
  | Eq => call_equal(wasm_mod, env, [compiled_arg1(), compiled_arg2()])
  | Is =>
    // Physical equality check
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.eq_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  | Int64Land =>
    failwith("Unreachable case; should never get here: Int64Land")
  | Int64Lor => failwith("Unreachable case; should never get here: Int64Lor")
  | Int64Lxor =>
    failwith("Unreachable case; should never get here: Int64Lxor")
  | Int64Lsl => failwith("Unreachable case; should never get here: Int64Lsl")
  | Int64Lsr => failwith("Unreachable case; should never get here: Int64Lsr")
  | Int64Asr => failwith("Unreachable case; should never get here: Int64Asr")
  | ArrayMake => allocate_array_n(wasm_mod, env, arg1, arg2)
  | ArrayInit => allocate_array_init(wasm_mod, env, arg1, arg2)
  | WasmLoadI32({sz, signed}) =>
    compile_wasm_load(
      ~sz,
      ~ty=Type.int32,
      ~signed,
      wasm_mod,
      compiled_arg1,
      compiled_arg2,
      arg2,
    )
  | WasmLoadI64({sz, signed}) =>
    compile_wasm_load(
      ~sz,
      ~ty=Type.int64,
      ~signed,
      wasm_mod,
      compiled_arg1,
      compiled_arg2,
      arg2,
    )
  | WasmLoadF32 =>
    compile_wasm_load(
      ~ty=Type.float32,
      wasm_mod,
      compiled_arg1,
      compiled_arg2,
      arg2,
    )
  | WasmLoadF64 =>
    compile_wasm_load(
      ~ty=Type.float64,
      wasm_mod,
      compiled_arg1,
      compiled_arg2,
      arg2,
    )
  | WasmBinaryI32({wasm_op, ret_type})
  | WasmBinaryI64({wasm_op, ret_type})
  | WasmBinaryF32({wasm_op, ret_type})
  | WasmBinaryF64({wasm_op, ret_type}) =>
    compile_wasm_prim2(
      wasm_mod,
      env,
      wasm_op,
      ret_type,
      compiled_arg1(),
      compiled_arg2(),
    )
  };
};

let compile_primn = (wasm_mod, env: codegen_env, p, args): Expression.t => {
  switch (p) {
  | WasmStoreI32({sz}) =>
    compile_wasm_store(~sz, ~ty=Type.int32, wasm_mod, env, args)
  | WasmStoreI64({sz}) =>
    compile_wasm_store(~sz, ~ty=Type.int64, wasm_mod, env, args)
  | WasmStoreF32 => compile_wasm_store(~ty=Type.float32, wasm_mod, env, args)
  | WasmStoreF64 => compile_wasm_store(~ty=Type.float64, wasm_mod, env, args)
  | WasmMemoryCopy =>
    Expression.block(
      wasm_mod,
      gensym_label("memory_copy"),
      [
        Expression.memory_copy(
          wasm_mod,
          compile_imm(wasm_mod, env, List.nth(args, 0)),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
          compile_imm(wasm_mod, env, List.nth(args, 2)),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    )
  | WasmMemoryFill =>
    Expression.block(
      wasm_mod,
      gensym_label("memory_fill"),
      [
        Expression.memory_fill(
          wasm_mod,
          compile_imm(wasm_mod, env, List.nth(args, 0)),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
          compile_imm(wasm_mod, env, List.nth(args, 2)),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    )
  };
};

let compile_allocation = (wasm_mod, env, alloc_type) =>
  switch (alloc_type) {
  | MClosure(cdata) => allocate_closure(wasm_mod, env, cdata)
  | MTuple(elts) => allocate_tuple(wasm_mod, env, elts)
  | MBox(elt) => allocate_box(wasm_mod, env, elt)
  | MArray(elts) => allocate_array(wasm_mod, env, elts)
  | MRecord(ttag, elts) => allocate_record(wasm_mod, env, ttag, elts)
  | MString(str) => allocate_string(wasm_mod, env, str)
  | MChar(char) => allocate_char(wasm_mod, env, char)
  | MADT(ttag, vtag, elts) => allocate_adt(wasm_mod, env, ttag, vtag, elts)
  | MInt32(i) =>
    allocate_int32(
      wasm_mod,
      env,
      Expression.const(wasm_mod, Literal.int32(i)),
    )
  | MInt64(i) =>
    allocate_int64(
      wasm_mod,
      env,
      Expression.const(wasm_mod, Literal.int64(i)),
    )
  | MFloat32(i) =>
    allocate_float32(
      wasm_mod,
      env,
      Expression.const(wasm_mod, Literal.float32(i)),
    )
  | MFloat64(i) =>
    allocate_float64(
      wasm_mod,
      env,
      Expression.const(wasm_mod, Literal.float64(i)),
    )
  | MRational(n, d) =>
    allocate_rational(
      wasm_mod,
      env,
      Expression.const(wasm_mod, Literal.int32(n)),
      Expression.const(wasm_mod, Literal.int32(d)),
    )
  };

let collect_backpatches = (env, f) => {
  let nested_backpatches = ref([]);
  let res = f({...env, backpatches: nested_backpatches});
  (res, nested_backpatches^);
};

let do_backpatches = (wasm_mod, env, backpatches) => {
  let do_backpatch = ((lam, {variables})) => {
    let get_swap = () => get_swap(wasm_mod, env, 0);
    let set_swap = set_swap(wasm_mod, env, 0);
    let preamble = set_swap(lam);
    let backpatch_var = (idx, var) =>
      store(
        ~offset=4 * (idx + 4),
        wasm_mod,
        get_swap(),
        call_incref_backpatch(
          wasm_mod,
          env,
          compile_imm(wasm_mod, env, var),
        ),
      );
    [preamble, ...List.mapi(backpatch_var, variables)];
  };
  Expression.block(
    wasm_mod,
    gensym_label("do_backpatches"),
    List.concat @@ List.map(do_backpatch, backpatches),
  );
};

let loop_stack = ref([]: list((string, string)));

let rec compile_store = (wasm_mod, env, binds) => {
  let process_binds = env => {
    let process_bind = ((b, instr), acc) => {
      let store_bind = arg =>
        compile_bind(~action=BindSet(arg), wasm_mod, env, b);
      let store_bind_no_incref = arg =>
        compile_bind(
          ~action=BindSet(arg),
          wasm_mod,
          ~skip_incref=true,
          env,
          b,
        );
      let get_bind = compile_bind(~action=BindGet, wasm_mod, env, b);
      let (compiled_instr, store_bind) =
        switch (instr.instr_desc) {
        | MAllocate(MClosure(cdata)) => (
            allocate_closure(
              wasm_mod,
              env,
              ~lambda=get_bind,
              ~skip_patching=true,
              cdata,
            ),
            store_bind_no_incref,
          )
        /* HACK: We expect values returned from functions to have a refcount of 1, so we don't increment it when storing */
        | MReturnCallIndirect(_)
        | MCallIndirect(_)
        | MCallKnown(_)
        | MAllocate(_) => (
            compile_instr(wasm_mod, env, instr),
            store_bind_no_incref,
          )
        | _ => (compile_instr(wasm_mod, env, instr), store_bind)
        };
      [store_bind(compiled_instr), ...acc];
    };
    List.fold_right(process_bind, binds, []);
  };
  let (instrs, backpatches) = collect_backpatches(env, process_binds);
  Expression.block(wasm_mod, gensym_label("compile_store")) @@
  List.append(instrs, [do_backpatches(wasm_mod, env, backpatches)]);
}

and compile_set = (wasm_mod, env, b, i) => {
  compile_bind(
    ~action=BindTee(compile_instr(wasm_mod, env, i)),
    wasm_mod,
    env,
    b,
  );
}

and compile_switch = (wasm_mod, env, arg, branches, default) => {
  /* Constructs the jump table. Assumes that branch 0 is the default */
  let switch_label = gensym_label("switch");
  let outer_label = switch_label ++ "_outer";
  let default_label = switch_label ++ "_default";
  let create_table = stack => {
    let label_blocks =
      stack |> List.sort(((l1, _), (l2, _)) => compare(l1, l2));
    let matching = (i, (lbl, name)) => lbl == i;
    let get_slot = i =>
      switch (List.find_opt(matching(i), label_blocks)) {
      | None => default_label
      | Some((_, b)) => b
      };
    List.init(List.length(stack) + 1, get_slot);
  };
  let rec process_branches = (count, stack, bs) => {
    let branch_name = Printf.sprintf("%s_branch_%d", switch_label, count);
    let target_branch_name =
      Printf.sprintf("%s_branch_%d", switch_label, count + 1);
    switch (bs) {
    | [] =>
      let inner_block_body =
        /* Expression.nop wasm_mod in */
        Expression.switch_(
          wasm_mod,
          create_table(stack),
          default_label,
          untag_number(wasm_mod, compile_imm(wasm_mod, env, arg)),
          Expression.const(wasm_mod, const_int32(0)),
        );
      let default_block_body = compile_block(wasm_mod, env, default);
      Expression.block(
        ~return_type=Type.int32,
        wasm_mod,
        branch_name,
        [
          Expression.drop(wasm_mod) @@
          Expression.block(wasm_mod, default_label, [inner_block_body]),
          Expression.break(
            wasm_mod,
            outer_label,
            Expression.null(),
            default_block_body,
          ),
        ],
      );
    | [(lbl, hd), ...tl] =>
      Expression.block(
        ~return_type=Type.int32,
        wasm_mod,
        branch_name,
        [
          Expression.drop(
            wasm_mod,
            process_branches(
              count + 1,
              [(Int32.to_int(lbl), target_branch_name), ...stack],
              tl,
            ),
          ),
          Expression.break(
            wasm_mod,
            outer_label,
            Expression.null(),
            compile_block(wasm_mod, env, hd),
          ),
        ],
      )
    };
  };
  Expression.block(
    ~return_type=Type.int32,
    wasm_mod,
    outer_label,
    [process_branches(0, [], branches)],
  );
}
and compile_block = (wasm_mod, env, block) => {
  let compiled_instrs = List.map(compile_instr(wasm_mod, env), block);
  if (Config.source_map^) {
    sources :=
      List.fold_left2(
        (sources, compiled, raw) => {
          [(compiled, raw.instr_loc), ...sources]
        },
        sources^,
        compiled_instrs,
        block,
      );
  };
  Expression.block(wasm_mod, gensym_label("compile_block"), compiled_instrs);
}
and compile_instr = (wasm_mod, env, instr) =>
  switch (instr.instr_desc) {
  | MDrop(arg) =>
    Expression.drop(wasm_mod, compile_instr(wasm_mod, env, arg))
  | MTracepoint(x) => tracepoint(wasm_mod, env, x)
  | MImmediate(imm) => compile_imm(wasm_mod, env, imm)
  | MAllocate(alloc) => compile_allocation(wasm_mod, env, alloc)
  | MTupleOp(tuple_op, tup) => compile_tuple_op(wasm_mod, env, tup, tuple_op)
  | MBoxOp(box_op, box) => compile_box_op(wasm_mod, env, box, box_op)
  | MArrayOp(array_op, ret) => compile_array_op(wasm_mod, env, ret, array_op)
  | MAdtOp(adt_op, adt) => compile_adt_op(wasm_mod, env, adt, adt_op)
  | MRecordOp(record_op, record) =>
    compile_record_op(wasm_mod, env, record, record_op)
  | MPrim1(p1, arg) => compile_prim1(wasm_mod, env, p1, arg)
  | MPrim2(p2, arg1, arg2) => compile_prim2(wasm_mod, env, p2, arg1, arg2)
  | MPrimN(p, args) => compile_primn(wasm_mod, env, p, args)
  | MSwitch(arg, branches, default) =>
    compile_switch(wasm_mod, env, arg, branches, default)
  | MStore(binds) => compile_store(wasm_mod, env, binds)
  | MSet(b, i) => compile_set(wasm_mod, env, b, i)
  | MCallIndirect({func, func_type, args}) =>
    call_lambda(wasm_mod, env, func, func_type, args)
  | MReturnCallIndirect({func, func_type, args}) =>
    call_lambda(~tail=true, wasm_mod, env, func, func_type, args)

  | MIf(cond, thn, els) =>
    let compiled_cond = compile_imm(wasm_mod, env, cond);
    let compiled_thn = compile_block(wasm_mod, env, thn);
    let compiled_els = compile_block(wasm_mod, env, els);
    Expression.if_(
      wasm_mod,
      decode_bool(wasm_mod, compiled_cond),
      compiled_thn,
      compiled_els,
    );

  | MFor(cond, inc, body) =>
    let block_label = gensym_label("MFor");
    let loop_label = gensym_label("MFor_loop");
    let continue_label = gensym_label("MFor_continue");
    let compiled_cond =
      switch (cond) {
      | Some(cond) => [
          Expression.drop(wasm_mod) @@
          Expression.break(
            wasm_mod,
            block_label,
            Expression.unary(
              wasm_mod,
              Op.eq_z_int32,
              decode_bool(wasm_mod, compile_block(wasm_mod, env, cond)),
            ),
            Expression.const(wasm_mod, const_void()),
          ),
        ]
      | None => []
      };
    let compiled_inc =
      switch (inc) {
      | Some(inc) => [
          Expression.drop(wasm_mod, compile_block(wasm_mod, env, inc)),
        ]
      | None => []
      };
    loop_stack := [(continue_label, block_label), ...loop_stack^];
    let compiled_body = compile_block(wasm_mod, env, body);
    loop_stack := List.tl(loop_stack^);
    Expression.block(
      wasm_mod,
      block_label,
      [
        Expression.drop(wasm_mod) @@
        Expression.loop(
          wasm_mod,
          loop_label,
          Expression.block(
            wasm_mod,
            gensym_label("MFor_loop_body"),
            List.concat([
              compiled_cond,
              [
                Expression.block(
                  wasm_mod,
                  continue_label,
                  [Expression.drop(wasm_mod, compiled_body)],
                ),
              ],
              compiled_inc,
              [
                Expression.break(
                  wasm_mod,
                  loop_label,
                  Expression.null(),
                  Expression.null(),
                ),
              ],
            ]),
          ),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    );
  | MContinue =>
    let (continue_label, _) = List.hd(loop_stack^);
    Expression.break(
      wasm_mod,
      continue_label,
      Expression.null(),
      Expression.null(),
    );
  | MBreak =>
    let (_, block_label) = List.hd(loop_stack^);
    Expression.break(
      wasm_mod,
      block_label,
      Expression.null(),
      Expression.const(wasm_mod, const_void()),
    );
  | MError(err, args) => call_error_handler(wasm_mod, env, err, args)
  | MCallKnown({func, func_type: (_, retty), args}) =>
    let compiled_args = List.map(compile_imm(wasm_mod, env), args);
    Expression.call(wasm_mod, func, compiled_args, wasm_type(retty));
  | MArityOp(_) => failwith("NYI: (compile_instr): MArityOp")
  | MTagOp(_) => failwith("NYI: (compile_instr): MTagOp")
  };

let compile_function =
    (
      ~start=false,
      wasm_mod,
      env,
      {index, args, return_type, stack_size, body: body_instrs, func_loc},
    ) => {
  sources := [];
  let arity = List.length(args);
  let index_int = Int32.to_int(index);
  let func_name =
    if (start) {
      "_start";
    } else {
      Printf.sprintf("func_%d", index_int);
    };
  let body_env = {...env, num_args: arity, stack_size};
  let inner_body =
    if (Config.no_gc^) {
      compile_block(wasm_mod, body_env, body_instrs);
    } else {
      cleanup_locals(
        wasm_mod,
        body_env,
        compile_block(wasm_mod, body_env, body_instrs),
      );
    };
  let body = Expression.return(wasm_mod, inner_body);
  let locals =
    [
      swap_slots,
      Array.make(stack_size.stack_size_i32, Type.int32),
      Array.make(stack_size.stack_size_i64, Type.int64),
      Array.make(stack_size.stack_size_f32, Type.float32),
      Array.make(stack_size.stack_size_f64, Type.float64),
    ]
    |> Array.concat;
  let func_ref =
    Function.add_function(
      wasm_mod,
      func_name,
      Type.create @@ Array.of_list @@ List.map(wasm_type, args),
      wasm_type @@ return_type,
      locals,
      body,
    );
  if (Config.source_map^) {
    open Grain_parsing.Location;
    List.iter(
      ((exp, loc)) => {
        Function.set_debug_location(
          func_ref,
          exp,
          0,
          loc.loc_start.pos_lnum,
          loc.loc_start.pos_cnum - loc.loc_start.pos_bol,
        )
      },
      sources^,
    );
    Function.set_debug_location(
      func_ref,
      body,
      0,
      func_loc.loc_start.pos_lnum,
      func_loc.loc_start.pos_cnum - func_loc.loc_start.pos_bol,
    );
  };
  func_ref;
};

let compute_table_size = (env, {functions}) => {
  List.length(functions);
};

let compile_imports = (wasm_mod, env, {imports}) => {
  let compile_asm_type = t =>
    switch (t) {
    | I32Type => Type.int32
    | I64Type => Type.int64
    | F32Type => Type.float32
    | F64Type => Type.float64
    };

  let compile_module_name = name =>
    fun
    | MImportWasm => Ident.name(name)
    | MImportGrain => "GRAIN$MODULE$" ++ Ident.name(name);

  let compile_import_name = name =>
    fun
    | MImportWasm => Ident.name(name)
    | MImportGrain => "GRAIN$EXPORT$" ++ Ident.name(name);

  let compile_import = ({mimp_mod, mimp_name, mimp_type, mimp_kind}) => {
    let module_name = compile_module_name(mimp_mod, mimp_kind);
    let item_name = compile_import_name(mimp_name, mimp_kind);
    let internal_name = get_imported_name(mimp_mod, mimp_name);
    switch (mimp_kind, mimp_type) {
    | (MImportGrain, MGlobalImport(ty)) =>
      Import.add_global_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        wasm_type(ty),
        true,
      )
    | (_, MFuncImport(args, ret)) =>
      let proc_list = l =>
        Type.create @@ Array.of_list @@ List.map(compile_asm_type, l);
      Import.add_function_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        proc_list(args),
        proc_list(ret),
      );
    | (_, MGlobalImport(typ)) =>
      let typ = compile_asm_type(typ);
      Import.add_global_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        typ,
        false,
      );
    };
  };

  List.iter(compile_import, imports);
  Import.add_memory_import(
    wasm_mod,
    "mem",
    Ident.name(runtime_mod),
    "mem",
    false,
  );
  Import.add_table_import(wasm_mod, "tbl", Ident.name(runtime_mod), "tbl");
};

let compile_exports = (wasm_mod, env, {functions, imports, exports, globals}) => {
  let compile_export = (i, {ex_name, ex_global_index}) => {
    let internal_name = Printf.sprintf("global_%ld", ex_global_index);
    let exported_name = "GRAIN$EXPORT$" ++ Ident.name(ex_name);
    ignore @@ Export.add_global_export(wasm_mod, internal_name, exported_name);
  };

  let compile_external_function_export = ((internal_name, external_name)) => {
    ignore @@
    Export.add_function_export(wasm_mod, internal_name, external_name);
  };

  let exports = {
    let exported = Hashtbl.create(14);
    /* Exports are already reversed, so keeping the first of any name is the correct behavior. */
    List.filter(
      ({ex_name}) =>
        if (Hashtbl.mem(exported, Ident.name(ex_name))) {
          false;
        } else {
          Hashtbl.add(exported, Ident.name(ex_name), ());
          true;
        },
      exports,
    );
  };
  let functions = {
    let exported = Hashtbl.create(14);
    /* Functions will be reversed, so keeping the first of any name is the correct behavior. */
    List.filter_map(
      ({index, name}) =>
        switch (name) {
        | Some(name) =>
          if (Hashtbl.mem(exported, name)) {
            None;
          } else {
            Hashtbl.add(exported, name, ());
            let internal_name = Printf.sprintf("func_%ld", index);
            Some((internal_name, name));
          }
        | None => None
        },
      functions,
    );
  };
  List.iteri(compile_export, exports);
  List.iter(compile_external_function_export, List.rev(functions));
  ignore @@ Export.add_function_export(wasm_mod, "_start", "_start");
  ignore @@
  Export.add_global_export(
    wasm_mod,
    Printf.sprintf("global_%d", List.length(globals) + 1),
    Ident.name(table_size),
  );
};

let compile_tables = (wasm_mod, env, {functions, imports} as prog) => {
  let table_size = compute_table_size(env, prog);
  let function_name = i => Printf.sprintf("func_%d", i);
  let function_names = List.mapi((i, _) => function_name(i), functions);
  Function_table.set_function_table(
    wasm_mod,
    table_size,
    max_int,
    function_names,
    Expression.global_get(
      wasm_mod,
      get_imported_name(runtime_mod, reloc_base),
      Type.int32,
    ),
  );
};

let compile_globals = (wasm_mod, env, {globals} as prog) => {
  let initial_value =
    fun
    | I32Type => const_int32(0)
    | I64Type => const_int64(0)
    | F32Type => const_float32(0.)
    | F64Type => const_float64(0.);
  List.iter(
    ((i, ty)) =>
      ignore @@
      Global.add_global(
        wasm_mod,
        Printf.sprintf("global_%ld", i),
        wasm_type(ty),
        true,
        Expression.const(wasm_mod, initial_value(ty)),
      ),
    globals,
  );
  ignore @@
  Global.add_global(
    wasm_mod,
    Printf.sprintf("global_%d", 1 + List.length(globals)),
    Type.int32,
    false,
    Expression.const(wasm_mod, const_int32(compute_table_size(env, prog))),
  );
};

let compile_main = (wasm_mod, env, prog) => {
  ignore @@
  compile_function(
    ~start=true,
    wasm_mod,
    env,
    {
      index: Int32.of_int(-99),
      name: Some("_start"),
      args: [],
      return_type: I32Type,
      body: prog.main_body,
      stack_size: prog.main_body_stack_size,
      attrs: [],
      func_loc: Grain_parsing.Location.dummy_loc,
    },
  );
};

let compile_functions = (wasm_mod, env, {functions} as prog) => {
  let handle_attrs = ({attrs} as func) =>
    if (List.mem(Disable_gc, attrs)) {
      Config.preserve_config(() => {
        Config.no_gc := true;
        compile_function(wasm_mod, env, func);
      });
    } else {
      compile_function(wasm_mod, env, func);
    };
  ignore @@ List.map(handle_attrs, functions);
  ignore @@ compile_main(wasm_mod, env, prog);
};

exception WasmRunnerError(Module.t, option(string), string);

let validate_module = (~name=?, wasm_mod: Module.t) =>
  try(assert(Module.validate(wasm_mod) == 1)) {
  | Assert_failure(_) =>
    raise(WasmRunnerError(wasm_mod, name, "WARNING: Invalid module"))
  };

let prepare = (env, {imports} as prog) => {
  let process_import =
      (
        ~dynamic_offset=0,
        ~is_runtime_import=false,
        acc_env,
        idx,
        {mimp_mod, mimp_name, mimp_type, mimp_kind},
      ) => {
    let rt_idx =
      if (is_runtime_import) {
        idx + dynamic_offset;
      } else {
        idx;
      };
    let rt_idx_name = Printf.sprintf("global_%d", rt_idx);
    let register = (name, tbl) => {
      let tbl =
        switch (Ident.find_same_opt(mimp_mod, tbl)) {
        | None => Ident.add(mimp_mod, Ident.empty, tbl)
        | Some(_) => tbl
        };
      Ident.add(
        mimp_mod,
        Ident.add(mimp_name, name, Ident.find_same(mimp_mod, tbl)),
        tbl,
      );
    };

    let (imported_funcs, imported_globals) =
      switch (mimp_type) {
      | MFuncImport(_) => (
          register(Int32.of_int(rt_idx), acc_env.imported_funcs),
          acc_env.imported_globals,
        )
      | MGlobalImport(_) => (
          acc_env.imported_funcs,
          register(rt_idx_name, acc_env.imported_globals),
        )
      };
    {...acc_env, imported_funcs, imported_globals};
  };
  let import_offset = List.length(runtime_imports);
  let import_global_offset = import_offset + List.length(imports);

  let new_imports = List.append(runtime_imports, imports);
  let new_env =
    List_utils.fold_lefti(
      process_import(~is_runtime_import=true),
      env,
      runtime_global_imports,
    );
  let new_env =
    List_utils.fold_lefti(
      process_import(~is_runtime_import=true),
      new_env,
      runtime_function_imports,
    );
  let new_env =
    List_utils.fold_lefti(
      process_import(~dynamic_offset=import_global_offset),
      new_env,
      imports,
    );
  let global_offset = import_global_offset;
  (
    {...new_env, import_offset, import_global_offset, global_offset},
    {...prog, imports: new_imports},
  );
};

let compile_wasm_module = (~env=?, ~name=?, prog) => {
  let env =
    switch (env) {
    | None => init_codegen_env()
    | Some(e) => e
    };
  let (env, prog) = prepare(env, prog);
  let wasm_mod = Module.create();
  if (Config.source_map^) {
    ignore @@
    Module.add_debug_info_filename(
      wasm_mod,
      Filename.basename(Option.get(name)),
    );
  };
  let _ =
    Module.set_features(
      wasm_mod,
      [
        Features.mvp,
        Features.multivalue,
        Features.tail_call,
        Features.sign_ext,
        Features.bulk_memory,
        Features.mutable_globals,
      ],
    );
  let _ =
    Memory.set_memory(wasm_mod, 0, Memory.unlimited, "memory", [], false);
  let () = ignore @@ compile_functions(wasm_mod, env, prog);
  let () = ignore @@ compile_imports(wasm_mod, env, prog);
  let () = ignore @@ compile_exports(wasm_mod, env, prog);
  let () = ignore @@ compile_globals(wasm_mod, env, prog);
  let () = ignore @@ compile_tables(wasm_mod, env, prog);

  let serialized_cmi = Cmi_format.serialize_cmi(prog.signature);
  Module.add_custom_section(
    wasm_mod,
    "cmi",
    Bytes.to_string(serialized_cmi),
  );
  validate_module(~name?, wasm_mod);
  // TODO: Enable Binaryen optimizations
  // https://github.com/grain-lang/grain/issues/196
  // Module.optimize(wasm_mod);
  wasm_mod;
};

let module_to_bytes = wasm_mod => {
  /* Print module to string */
  let (wasm_bytes, _) = Module.write(wasm_mod, None);
  wasm_bytes;
};

let () =
  Printexc.register_printer(exc =>
    switch (exc) {
    | WasmRunnerError(wasm_mod, name, msg) =>
      let formatted_name =
        switch (name) {
        | None => "<unknown>"
        | Some(n) => n
        };
      let s =
        Printf.sprintf(
          "WASM Runner Exception in %s: %s\n",
          formatted_name,
          msg,
        );
      Some(s);
    | _ => None
    }
  );
