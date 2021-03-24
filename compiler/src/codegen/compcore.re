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
let memory_debugging_enabled = false;

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
let swap_slots_i32 = [|Type.int32, Type.int32, Type.int32|];
let swap_slots_i64 = [|Type.int64|];
let swap_i32_offset = 0;
let swap_i64_offset = Array.length(swap_slots_i32);
let swap_slots = Array.append(swap_slots_i32, swap_slots_i64);

/* These are the bare-minimum imports needed for basic runtime support */
let module_runtime_id = Ident.create_persistent("moduleRuntimeId");
let reloc_base = Ident.create_persistent("relocBase");
let table_size = Ident.create_persistent("GRAIN$TABLE_SIZE");
let runtime_mod = Ident.create_persistent("grainRuntime");
let malloc_mod = Ident.create_persistent("GRAIN$MODULE$runtime/malloc");
let gc_mod = Ident.create_persistent("GRAIN$MODULE$runtime/gc");
let exception_mod = Ident.create_persistent("GRAIN$MODULE$runtime/exception");
let data_structures_mod =
  Ident.create_persistent("GRAIN$MODULE$runtime/dataStructures");
let console_mod = Ident.create_persistent("console");
let print_exception_ident = Ident.create_persistent("printException");
let print_exception_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$printException");
let assertion_error_ident =
  Ident.create_persistent("GRAIN$EXPORT$AssertionError");
let index_out_of_bounds_ident =
  Ident.create_persistent("GRAIN$EXPORT$IndexOutOfBounds");
let match_failure_ident =
  Ident.create_persistent("GRAIN$EXPORT$MatchFailure");
let malloc_ident = Ident.create_persistent("malloc");
let malloc_closure_ident = Ident.create_persistent("GRAIN$EXPORT$malloc");
let incref_ident = Ident.create_persistent("incRef");
let incref_closure_ident = Ident.create_persistent("GRAIN$EXPORT$incRef");
let new_rational_ident = Ident.create_persistent("newRational");
let new_rational_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$newRational");
let new_float32_ident = Ident.create_persistent("newFloat32");
let new_float32_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$newFloat32");
let new_float64_ident = Ident.create_persistent("newFloat64");
let new_float64_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$newFloat64");
let new_int32_ident = Ident.create_persistent("newInt32");
let new_int32_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$newInt32");
let new_int64_ident = Ident.create_persistent("newInt64");
let new_int64_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$newInt64");
let equal_mod = Ident.create_persistent("GRAIN$MODULE$runtime/equal");
let equal_ident = Ident.create_persistent("equal");
let equal_closure_ident = Ident.create_persistent("GRAIN$EXPORT$equal");
let decref_ident = Ident.create_persistent("decRef");
let decref_closure_ident = Ident.create_persistent("GRAIN$EXPORT$decRef");
let decref_ignore_zeros_ident = Ident.create_persistent("decRefIgnoreZeros");
let decref_ignore_zeros_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$decRefIgnoreZeros");
let tracepoint_ident = Ident.create_persistent("tracepoint");

let required_global_imports = [
  {
    mimp_mod: runtime_mod,
    mimp_name: reloc_base,
    mimp_type: MGlobalImport(I32Type, false),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: runtime_mod,
    mimp_name: module_runtime_id,
    mimp_type: MGlobalImport(I32Type, false),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: exception_mod,
    mimp_name: print_exception_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: exception_mod,
    mimp_name: assertion_error_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: exception_mod,
    mimp_name: index_out_of_bounds_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: exception_mod,
    mimp_name: match_failure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
];

let grain_runtime_imports = [
  {
    mimp_mod: gc_mod,
    mimp_name: malloc_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: gc_mod,
    mimp_name: incref_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: gc_mod,
    mimp_name: decref_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: gc_mod,
    mimp_name: decref_ignore_zeros_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_rational_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_float32_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_float64_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_int32_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_int64_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: equal_mod,
    mimp_name: equal_closure_ident,
    mimp_type: MGlobalImport(I32Type, true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
];

let runtime_global_imports =
  List.append(required_global_imports, grain_runtime_imports);

let required_function_imports = [
  {
    mimp_mod: exception_mod,
    mimp_name: print_exception_ident,
    mimp_type: MFuncImport([I32Type, I32Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
];

let grain_function_imports = [
  {
    mimp_mod: gc_mod,
    mimp_name: malloc_ident,
    mimp_type: MFuncImport([I32Type, I32Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: gc_mod,
    mimp_name: incref_ident,
    mimp_type: MFuncImport([I32Type, I32Type], [I32Type]), /* Returns same pointer as argument */
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: gc_mod,
    mimp_name: decref_ident,
    mimp_type: MFuncImport([I32Type, I32Type], [I32Type]), /* Returns same pointer as argument */
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: gc_mod,
    mimp_name: decref_ignore_zeros_ident,
    mimp_type: MFuncImport([I32Type, I32Type], [I32Type]), /* Returns same pointer as argument */
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
    mimp_mod: data_structures_mod,
    mimp_name: new_rational_ident,
    mimp_type: MFuncImport([I32Type, I32Type, I32Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_float32_ident,
    mimp_type: MFuncImport([I32Type, F32Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_float64_ident,
    mimp_type: MFuncImport([I32Type, F64Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_int32_ident,
    mimp_type: MFuncImport([I32Type, I32Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: data_structures_mod,
    mimp_name: new_int64_ident,
    mimp_type: MFuncImport([I32Type, I64Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
  {
    mimp_mod: equal_mod,
    mimp_name: equal_ident,
    mimp_type: MFuncImport([I32Type, I32Type, I32Type], [I32Type]),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
  },
];

let runtime_function_imports =
  List.append(grain_function_imports, required_function_imports);

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

/** Static runtime values */

// Static pointer to the runtime heap
// Leaves low 1000 memory unused for Binaryen optimizations
let runtime_heap_ptr = 0x400;
// Start pointer for the runtime heap
let runtime_heap_start = 0x410;
// Static pointer to runtime type information
let runtime_type_metadata_ptr = 0x408;

let global_function_table = "tbl";

let get_imported_name = (mod_, name) =>
  Printf.sprintf(
    "import_%s_%s",
    Ident.unique_name(mod_),
    Ident.unique_name(name),
  );

let call_exception_printer = (wasm_mod, env, args) => {
  let args = [
    Expression.global_get(
      wasm_mod,
      get_imported_name(exception_mod, print_exception_closure_ident),
      Type.int32,
    ),
    ...args,
  ];
  Expression.call(
    wasm_mod,
    get_imported_name(exception_mod, print_exception_ident),
    args,
    Type.int32,
  );
};

let call_malloc = (wasm_mod, env, args) => {
  let args = [
    Expression.global_get(
      wasm_mod,
      get_imported_name(gc_mod, malloc_closure_ident),
      Type.int32,
    ),
    ...args,
  ];
  Expression.call(
    wasm_mod,
    get_imported_name(gc_mod, malloc_ident),
    args,
    Type.int32,
  );
};
let call_incref = (wasm_mod, env, arg) => {
  let args = [
    Expression.global_get(
      wasm_mod,
      get_imported_name(gc_mod, incref_closure_ident),
      Type.int32,
    ),
    arg,
  ];
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      get_imported_name(gc_mod, incref_ident),
      args,
      Type.int32,
    );
  };
};
let call_decref_ignore_zeros = (wasm_mod, env, arg) => {
  let args = [
    Expression.global_get(
      wasm_mod,
      get_imported_name(gc_mod, decref_ignore_zeros_closure_ident),
      Type.int32,
    ),
    arg,
  ];
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.call(
      wasm_mod,
      get_imported_name(gc_mod, decref_ignore_zeros_ident),
      args,
      Type.int32,
    );
  };
};
let call_decref = (wasm_mod, env, arg) =>
  if (!memory_debugging_enabled) {
    call_decref_ignore_zeros(wasm_mod, env, arg);
  } else {
    let args = [
      Expression.global_get(
        wasm_mod,
        get_imported_name(gc_mod, decref_closure_ident),
        Type.int32,
      ),
      arg,
    ];
    if (Config.no_gc^) {
      arg;
    } else {
      Expression.call(
        wasm_mod,
        get_imported_name(gc_mod, decref_ident),
        args,
        Type.int32,
      );
    };
  };
let call_new_rational = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(data_structures_mod, new_rational_ident),
    [
      Expression.global_get(
        wasm_mod,
        get_imported_name(data_structures_mod, new_rational_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
    Type.int32,
  );
let call_new_float32 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(data_structures_mod, new_float32_ident),
    [
      Expression.global_get(
        wasm_mod,
        get_imported_name(data_structures_mod, new_float32_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
    Type.int32,
  );
let call_new_float64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(data_structures_mod, new_float64_ident),
    [
      Expression.global_get(
        wasm_mod,
        get_imported_name(data_structures_mod, new_float64_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
    Type.int32,
  );
let call_new_int32 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(data_structures_mod, new_int32_ident),
    [
      Expression.global_get(
        wasm_mod,
        get_imported_name(data_structures_mod, new_int32_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
    Type.int32,
  );
let call_new_int64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(data_structures_mod, new_int64_ident),
    [
      Expression.global_get(
        wasm_mod,
        get_imported_name(data_structures_mod, new_int64_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
    Type.int32,
  );
let call_equal = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(equal_mod, equal_ident),
    [
      Expression.global_get(
        wasm_mod,
        get_imported_name(equal_mod, equal_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
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
        let arg = Expression.local_get(wasm_mod, slot_no, Type.int32);
        singleton @@
        Expression.drop(
          wasm_mod,
          call_decref_ignore_zeros(wasm_mod, env, arg),
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
    | MArgBind(_, I32Type)
    | MLocalBind(_, I32Type)
    | MSwapBind(_, I32Type)
    | MClosureBind(_)
    | MGlobalBind(_, I32Type, true) => call_incref(wasm_mod, env, arg)
    | MArgBind(_)
    | MLocalBind(_)
    | MSwapBind(_)
    | MGlobalBind(_) => arg
    | _ => call_incref(wasm_mod, env, arg)
    };

  let appropriate_decref = (env, arg) =>
    switch (b) {
    | _ when skip_decref => arg /* This case is used for storing swap values that have freshly been heap-allocated. */
    | MArgBind(_, I32Type)
    | MLocalBind(_, I32Type)
    | MSwapBind(_, I32Type)
    | MClosureBind(_)
    | MGlobalBind(_, I32Type, true) => call_decref(wasm_mod, env, arg)
    | MArgBind(_)
    | MLocalBind(_)
    | MSwapBind(_)
    | MGlobalBind(_) => arg
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
  Expression.drop(wasm_mod, call_decref_ignore_zeros(wasm_mod, env, arg));

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
      call_incref(wasm_mod, env, tee_swap(wasm_mod, env, 0, arg)),
    )
    @ cleanup_local_slot_instructions(wasm_mod, env)
    +@ [get_swap(wasm_mod, env, 0)],
  );
};

let compile_imm = (wasm_mod, env: codegen_env, i: immediate): Expression.t =>
  switch (i) {
  | MImmConst(c) => Expression.const(wasm_mod, compile_const(c))
  | MImmBinding(b) => compile_bind(~action=BindGet, wasm_mod, env, b)
  | MImmTrap => Expression.unreachable(wasm_mod)
  };

let call_error_handler = (wasm_mod, env, err, args) => {
  let err =
    switch (err) {
    | Runtime_errors.MatchFailure => match_failure_ident
    | Runtime_errors.IndexOutOfBounds => index_out_of_bounds_ident
    | Runtime_errors.AssertionError => assertion_error_ident
    };
  let err =
    Expression.global_get(
      wasm_mod,
      get_imported_name(exception_mod, err),
      Type.int32,
    );
  Expression.block(
    wasm_mod,
    gensym_label("call_error_handler"),
    [
      Expression.drop(
        wasm_mod,
        call_exception_printer(wasm_mod, env, [err]),
      ),
      Expression.unreachable(wasm_mod),
    ],
  );
};

let error_if_true = (wasm_mod, env, cond, err, args) =>
  Expression.if_(
    wasm_mod,
    cond,
    Expression.drop(wasm_mod, call_error_handler(wasm_mod, env, err, args)),
    Expression.null(),
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
                call_incref(wasm_mod, env, compile_imm(wasm_mod, env, imm)),
                call_decref(
                  wasm_mod,
                  env,
                  load(~offset=4 * (idx_int + 2), wasm_mod, tup()),
                ),
              ],
            ),
            0,
          ),
        ),
        Expression.const(wasm_mod, const_void()),
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
          IndexOutOfBounds,
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
          IndexOutOfBounds,
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
          IndexOutOfBounds,
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
          IndexOutOfBounds,
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
          call_incref(wasm_mod, env, val_),
        ),
        Expression.const(wasm_mod, const_void()),
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
        Expression.const(wasm_mod, const_void()),
      ],
    );
  };
};

/** Heap allocations. */

/** Rounds the given number of words to be aligned correctly */
let round_to_even = num_words =>
  if (num_words mod 2 == 0) {
    num_words;
  } else {
    num_words + 1;
  };

let heap_allocate = (wasm_mod, env, num_words: int) =>
  if (Env.is_runtime_mode()) {
    let addition =
      Expression.binary(
        wasm_mod,
        Op.add_int32,
        load(
          wasm_mod,
          Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
        ),
        Expression.const(
          wasm_mod,
          const_int32(round_to_even(num_words + 2) * 4),
        ),
      );
    Expression.tuple_extract(
      wasm_mod,
      Expression.tuple_make(
        wasm_mod,
        [
          Expression.block(
            wasm_mod,
            gensym_label("heap_allocate_runtime"),
            [
              store(
                wasm_mod,
                load(
                  wasm_mod,
                  Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
                ),
                Expression.const(wasm_mod, const_int32(1)),
              ),
              Expression.binary(
                wasm_mod,
                Op.add_int32,
                load(
                  wasm_mod,
                  Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
                ),
                Expression.const(wasm_mod, const_int32(8)),
              ),
            ],
          ),
          Expression.block(
            wasm_mod,
            gensym_label("store_runtime_heap_ptr"),
            [
              store(
                wasm_mod,
                Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
                addition,
              ),
              // Binaryen tuples must include a concrete value (and tuples are
              // the only way to use the stack)
              Expression.const(wasm_mod, const_int32(0)),
            ],
          ),
        ],
      ),
      0,
    );
  } else {
    call_malloc(
      wasm_mod,
      env,
      [Expression.const(wasm_mod, const_int32(4 * num_words))],
    );
  };

let heap_runtime_allocate_imm =
    (~additional_words=0, wasm_mod, env, num_words: immediate) => {
  let num_words = () =>
    untag_number(wasm_mod, compile_imm(wasm_mod, env, num_words));
  let addition =
    Expression.binary(
      wasm_mod,
      Op.add_int32,
      load(
        wasm_mod,
        Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
      ),
      Expression.binary(
        wasm_mod,
        Op.mul_int32,
        Expression.binary(
          wasm_mod,
          Op.and_int32,
          Expression.binary(
            wasm_mod,
            Op.add_int32,
            num_words(),
            // Add 3 extra and clear final bit to round up to an even number of words + 2
            Expression.const(wasm_mod, const_int32(3 + additional_words)),
          ),
          Expression.const(wasm_mod, const_int32(0xfffffffe)),
        ),
        Expression.const(wasm_mod, const_int32(4)),
      ),
    );
  Expression.tuple_extract(
    wasm_mod,
    Expression.tuple_make(
      wasm_mod,
      [
        Expression.block(
          wasm_mod,
          gensym_label("heap_allocate_runtime_imm"),
          [
            store(
              wasm_mod,
              load(
                wasm_mod,
                Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
              ),
              Expression.const(wasm_mod, const_int32(1)),
            ),
            Expression.binary(
              wasm_mod,
              Op.add_int32,
              load(
                wasm_mod,
                Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
              ),
              Expression.const(wasm_mod, const_int32(8)),
            ),
          ],
        ),
        Expression.block(
          wasm_mod,
          gensym_label("store_runtime_heap_ptr"),
          [
            store(
              wasm_mod,
              Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
              addition,
            ),
            // Binaryen tuples must include a concrete value (and tuples are
            // the only way to use the stack)
            Expression.const(wasm_mod, const_int32(0)),
          ],
        ),
      ],
    ),
    0,
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

let heap_allocate_imm =
    (~additional_words=0, wasm_mod, env, num_words: immediate) =>
  if (Env.is_runtime_mode()) {
    heap_runtime_allocate_imm(~additional_words, wasm_mod, env, num_words);
  } else {
    heap_allocate_imm(~additional_words, wasm_mod, env, num_words);
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
    global_function_table,
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
        heap_allocate(wasm_mod, env, 2 + 2 * List.length(ints_to_push)),
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
        tee_swap(heap_allocate(wasm_mod, env, 2)),
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
            Expression.const(wasm_mod, const_int32 @@ 4 * closure_size),
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
        call_incref(wasm_mod, env, compile_imm(wasm_mod, env, var)),
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
      call_incref(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
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
      call_incref(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
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
      call_incref(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
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
        IndexOutOfBounds,
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
                  call_incref(wasm_mod, env, elt),
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
        IndexOutOfBounds,
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
                  call_incref(
                    wasm_mod,
                    env,
                    Expression.call_indirect(
                      wasm_mod,
                      global_function_table,
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
      call_incref(wasm_mod, env, compile_imm(wasm_mod, env, elt)),
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
        safe_drop(wasm_mod, env, compiled_arg),
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
  | Throw => call_exception_printer(wasm_mod, env, [compiled_arg])
  | Box => failwith("Unreachable case; should never get here: Box")
  | Unbox => failwith("Unreachable case; should never get here: Unbox")
  | BoxBind => failwith("Unreachable case; should never get here: BoxBind")
  | UnboxBind =>
    failwith("Unreachable case; should never get here: UnboxBind")
  | WasmFromGrain
  | WasmToGrain => compiled_arg // These are no-ops
  | WasmMemoryGrow => Expression.memory_grow(wasm_mod, compiled_arg)
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

  switch (p2) {
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
  | WasmMemorySize => Expression.memory_size(wasm_mod)
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
  | WasmMemoryCompare =>
    let lbl = gensym_label("memory_compare");
    let loop_lbl = gensym_label("memory_compare_loop");
    let set_ptr1 = set_swap(wasm_mod, env, 0);
    let set_ptr2 = set_swap(wasm_mod, env, 1);
    let set_count = set_swap(wasm_mod, env, 2);
    let get_ptr1 = () => get_swap(wasm_mod, env, 0);
    let get_ptr2 = () => get_swap(wasm_mod, env, 1);
    let get_count = () => get_swap(wasm_mod, env, 2);
    Expression.block(
      wasm_mod,
      lbl,
      [
        set_ptr1(compile_imm(wasm_mod, env, List.nth(args, 0))),
        set_ptr2(compile_imm(wasm_mod, env, List.nth(args, 1))),
        set_count(compile_imm(wasm_mod, env, List.nth(args, 2))),
        Expression.loop(
          wasm_mod,
          loop_lbl,
          Expression.block(
            wasm_mod,
            gensym_label("memory_compare_loop_inner"),
            [
              Expression.drop(wasm_mod) @@
              Expression.break(
                wasm_mod,
                lbl,
                Expression.unary(wasm_mod, Op.eq_z_int32, get_count()),
                Expression.const(wasm_mod, const_int32(0)),
              ),
              Expression.if_(
                wasm_mod,
                Expression.binary(
                  wasm_mod,
                  Op.ne_int32,
                  load(~sz=1, ~signed=false, wasm_mod, get_ptr1()),
                  load(~sz=1, ~signed=false, wasm_mod, get_ptr2()),
                ),
                Expression.break(
                  wasm_mod,
                  lbl,
                  Expression.null(),
                  Expression.select(
                    wasm_mod,
                    Expression.binary(
                      wasm_mod,
                      Op.lt_u_int32,
                      load(~sz=1, ~signed=false, wasm_mod, get_ptr1()),
                      load(~sz=1, ~signed=false, wasm_mod, get_ptr2()),
                    ),
                    Expression.const(wasm_mod, const_int32(-1)),
                    Expression.const(wasm_mod, const_int32(1)),
                  ),
                ),
                Expression.block(
                  wasm_mod,
                  gensym_label("memory_compare_loop_incr"),
                  [
                    set_ptr1(
                      Expression.binary(
                        wasm_mod,
                        Op.add_int32,
                        get_ptr1(),
                        Expression.const(wasm_mod, const_int32(1)),
                      ),
                    ),
                    set_ptr2(
                      Expression.binary(
                        wasm_mod,
                        Op.add_int32,
                        get_ptr2(),
                        Expression.const(wasm_mod, const_int32(1)),
                      ),
                    ),
                    set_count(
                      Expression.binary(
                        wasm_mod,
                        Op.sub_int32,
                        get_count(),
                        Expression.const(wasm_mod, const_int32(1)),
                      ),
                    ),
                    Expression.break(
                      wasm_mod,
                      loop_lbl,
                      Expression.null(),
                      Expression.null(),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ],
    );
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
        call_incref(wasm_mod, env, compile_imm(wasm_mod, env, var)),
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
  Expression.block(
    wasm_mod,
    gensym_label("compile_set"),
    [
      compile_bind(
        ~action=BindSet(compile_instr(wasm_mod, env, i)),
        wasm_mod,
        env,
        b,
      ),
      Expression.const(wasm_mod, const_void()),
    ],
  );
}

and compile_switch = (wasm_mod, env, arg, branches, default, ty) => {
  /* Constructs the jump table. Assumes that branch 0 is the default */
  let switch_label = gensym_label("switch");
  let outer_label = switch_label ++ "_outer";
  let default_label = switch_label ++ "_default";
  let switch_ty = wasm_type(ty);
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
      // This default value is never used, but is necessary to make the wasm types work out
      let default_value =
        switch (ty) {
        | I32Type => const_int32(0)
        | I64Type => const_int64(0)
        | F32Type => const_float32(0.)
        | F64Type => const_float64(0.)
        };
      let default_value = Expression.const(wasm_mod, default_value);
      let inner_block_body =
        Expression.switch_(
          wasm_mod,
          create_table(stack),
          default_label,
          untag_number(wasm_mod, compile_imm(wasm_mod, env, arg)),
          default_value,
        );
      let default_block_body =
        compile_block(~return_type=switch_ty, wasm_mod, env, default);
      Expression.block(
        ~return_type=switch_ty,
        wasm_mod,
        branch_name,
        [
          Expression.drop(wasm_mod) @@
          Expression.block(
            ~return_type=switch_ty,
            wasm_mod,
            default_label,
            [inner_block_body],
          ),
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
        ~return_type=switch_ty,
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
    ~return_type=switch_ty,
    wasm_mod,
    outer_label,
    [process_branches(0, [], branches)],
  );
}
and compile_block = (~return_type=?, wasm_mod, env, block) => {
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
  Expression.block(
    ~return_type?,
    wasm_mod,
    gensym_label("compile_block"),
    compiled_instrs,
  );
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
  | MSwitch(arg, branches, default, ty) =>
    compile_switch(wasm_mod, env, arg, branches, default, ty)
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

type type_metadata =
  | ADTMeta(int, list((int, string)))
  | RecordMeta(int, list(string));

let compile_type_metadata = (wasm_mod, env, type_metadata) => {
  open Types;

  // More information about this function can be found in the printing.md
  // contributor document.

  // Extension constructors defined by the module must be grouped together.
  // For now, this only includes exceptions.
  let (exception_meta, non_exception_meta) =
    List.fold_left(
      ((exception_meta, non_exception_meta), meta) => {
        switch (meta) {
        | ExceptionMetadata(_) => (
            [meta, ...exception_meta],
            non_exception_meta,
          )
        | ADTMetadata(id, variants) => (
            exception_meta,
            [ADTMeta(id, variants), ...non_exception_meta],
          )
        | RecordMetadata(id, fields) => (
            exception_meta,
            [RecordMeta(id, fields), ...non_exception_meta],
          )
        }
      },
      ([], []),
      type_metadata,
    );
  let type_metadata =
    switch (exception_meta) {
    | [ExceptionMetadata(id, _, _), ..._] => [
        ADTMeta(
          id,
          List.map(
            meta => {
              switch (meta) {
              | ExceptionMetadata(_, variant, name) => (variant, name)
              | _ => failwith("impossible by partition")
              }
            },
            exception_meta,
          ),
        ),
        ...non_exception_meta,
      ]
    | _ => non_exception_meta
    };

  let round_to_8 = n => Int.logand(n + 7, Int.lognot(7));

  let buf = Buffer.create(256);
  Buffer.add_int32_le(buf, 0l); // Slot where pointer to next will live
  Buffer.add_int32_le(buf, 0l); // Slot where module id will live
  Buffer.add_int32_le(buf, Int32.of_int(List.length(type_metadata)));

  let alignBuffer = amount => {
    for (_ in 1 to amount) {
      Buffer.add_int8(buf, 0);
    };
  };

  List.iter(
    meta => {
      switch (meta) {
      | ADTMeta(id, cstrs) =>
        let section_length =
          List.fold_left(
            (total, (_, cstr)) =>
              total + 12 + round_to_8(String.length(cstr)),
            8,
            cstrs,
          );
        Buffer.add_int32_le(buf, Int32.of_int(section_length));
        Buffer.add_int32_le(buf, Int32.of_int(id));
        List.iter(
          ((id, cstr)) => {
            let length = String.length(cstr);
            let aligned_length = round_to_8(length);
            Buffer.add_int32_le(buf, Int32.of_int(aligned_length + 12));
            Buffer.add_int32_le(buf, Int32.of_int(id));
            Buffer.add_int32_le(buf, Int32.of_int(length));
            Buffer.add_string(buf, cstr);
            alignBuffer(aligned_length - length);
          },
          cstrs,
        );
      | RecordMeta(id, fields) =>
        let section_length =
          List.fold_left(
            (total, field) => total + 8 + round_to_8(String.length(field)),
            8,
            fields,
          );
        Buffer.add_int32_le(buf, Int32.of_int(section_length));
        Buffer.add_int32_le(buf, Int32.of_int(id));
        List.iter(
          field => {
            let length = String.length(field);
            let aligned_length = round_to_8(length);
            Buffer.add_int32_le(buf, Int32.of_int(aligned_length + 8));
            Buffer.add_int32_le(buf, Int32.of_int(length));
            Buffer.add_string(buf, field);
            alignBuffer(aligned_length - length);
          },
          fields,
        );
      }
    },
    type_metadata,
  );

  Expression.block(
    wasm_mod,
    gensym_label("compile_type_metadata"),
    [
      set_swap(
        wasm_mod,
        env,
        0,
        Expression.binary(
          wasm_mod,
          Op.add_int32,
          allocate_string(wasm_mod, env, Buffer.contents(buf)),
          Expression.const(wasm_mod, const_int32(8)),
        ),
      ),
      store(
        wasm_mod,
        get_swap(wasm_mod, env, 0),
        load(
          wasm_mod,
          Expression.const(wasm_mod, const_int32(runtime_type_metadata_ptr)),
        ),
      ),
      store(
        ~offset=4,
        wasm_mod,
        get_swap(wasm_mod, env, 0),
        Expression.global_get(
          wasm_mod,
          get_imported_name(runtime_mod, module_runtime_id),
          Type.int32,
        ),
      ),
      store(
        wasm_mod,
        Expression.const(wasm_mod, const_int32(runtime_type_metadata_ptr)),
        get_swap(wasm_mod, env, 0),
      ),
    ],
  );
};

let compile_function =
    (
      ~start=false,
      ~preamble=?,
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
    switch (preamble) {
    | Some(preamble) =>
      Expression.block(
        wasm_mod,
        gensym_label("compile_function_preamble"),
        [preamble, compile_block(wasm_mod, body_env, body_instrs)],
      )
    | None => compile_block(wasm_mod, body_env, body_instrs)
    };
  let inner_body =
    if (Config.no_gc^) {
      inner_body;
    } else {
      cleanup_locals(wasm_mod, body_env, inner_body);
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
    | (MImportGrain, MGlobalImport(ty, mut)) =>
      Import.add_global_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        wasm_type(ty),
        mut,
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
    | (_, MGlobalImport(typ, mut)) =>
      let typ = compile_asm_type(typ);
      Import.add_global_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        typ,
        mut,
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
  Import.add_table_import(
    wasm_mod,
    global_function_table,
    Ident.name(runtime_mod),
    global_function_table,
  );
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
    Function_table.unlimited,
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
  let runtime_preamble =
    if (Env.is_runtime_mode()) {
      Some(
        Expression.if_(
          wasm_mod,
          Expression.unary(
            wasm_mod,
            Op.eq_z_int32,
            load(
              wasm_mod,
              Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
            ),
          ),
          store(
            wasm_mod,
            Expression.const(wasm_mod, const_int32(runtime_heap_ptr)),
            Expression.const(wasm_mod, const_int32(runtime_heap_start)),
          ),
          Expression.null(),
        ),
      );
    } else {
      None;
    };
  let module_preamble =
    if (Config.elide_type_info^ || List.length(prog.type_metadata) == 0) {
      None;
    } else {
      Some(compile_type_metadata(wasm_mod, env, prog.type_metadata));
    };
  let preamble =
    switch (runtime_preamble, module_preamble) {
    | (Some(x), Some(y)) =>
      Some(
        Expression.block(wasm_mod, gensym_label("main_preamble"), [x, y]),
      )
    | (Some(x), None)
    | (None, Some(x)) => Some(x)
    | (None, None) => None
    };
  ignore @@
  compile_function(
    ~start=true,
    ~preamble?,
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

  let new_imports =
    if (Env.is_runtime_mode()) {
      List.concat([
        required_global_imports,
        required_function_imports,
        imports,
      ]);
    } else {
      List.append(runtime_imports, imports);
    };
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
  let _ = Module.set_low_memory_unused(1);
  let _ =
    Memory.set_memory(wasm_mod, 0, Memory.unlimited, "memory", [], false);

  let compile_all = () => {
    ignore @@ compile_functions(wasm_mod, env, prog);
    ignore @@ compile_imports(wasm_mod, env, prog);
    ignore @@ compile_exports(wasm_mod, env, prog);
    ignore @@ compile_globals(wasm_mod, env, prog);
    ignore @@ compile_tables(wasm_mod, env, prog);
  };

  if (Env.is_runtime_mode()) {
    Config.preserve_config(() => {
      Config.no_gc := true;
      compile_all();
    });
  } else {
    compile_all();
  };

  let serialized_cmi = Cmi_format.serialize_cmi(prog.signature);
  Module.add_custom_section(
    wasm_mod,
    "cmi",
    Bytes.to_string(serialized_cmi),
  );
  validate_module(~name?, wasm_mod);
  Module.optimize(wasm_mod);
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
