open Grain_typed;
open Mashtree;
open Value_tags;
open Binaryen;
open Concatlist; /* NOTE: This import shadows (@) and introduces (@+) and (+@) */
open Grain_utils;
open Comp_utils;
open Comp_wasm_prim;

let sources: ref(list((Expression.t, Grain_parsing.Location.t))) = ref([]);

/** Environment */

type codegen_env = {
  name: option(string),
  num_args: int,
  num_closure_args: int,
  stack_size,
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Expression.t, closure_data))),
  required_imports: list(import),
};

let gensym_counter = ref(0);
let gensym_label = s => {
  gensym_counter := gensym_counter^ + 1;
  Printf.sprintf("%s.%d", s, gensym_counter^);
};
let reset_labels = () => gensym_counter := 0;

/* Number of swap variables to allocate */
let swap_slots_i32 = [|Type.int32, Type.int32, Type.int32|];
let swap_slots_i64 = [|Type.int64|];
let swap_slots_f32 = [|Type.float32|];
let swap_slots_f64 = [|Type.float64|];
let swap_i32_offset = 0;
let swap_i64_offset = Array.length(swap_slots_i32);
let swap_f32_offset = swap_i64_offset + Array.length(swap_slots_i64);
let swap_f64_offset = swap_f32_offset + Array.length(swap_slots_f32);
let swap_slots =
  Array.concat([
    swap_slots_i32,
    swap_slots_i64,
    swap_slots_f32,
    swap_slots_f64,
  ]);

/** These are the bare-minimum imports needed for basic runtime support */

/* The Grain environment */
let grain_env_mod = grain_env_name;
let module_runtime_id = Ident.create_persistent("moduleRuntimeId");
let runtime_heap_start = Ident.create_persistent("runtimeHeapStart");
let runtime_heap_next_ptr = Ident.create_persistent("runtimeHeapNextPtr");
let metadata_ptr = Ident.create_persistent("metadataPtr");
let reloc_base = Ident.create_persistent("relocBase");
let table_size = Ident.create_persistent("GRAIN$TABLE_SIZE");

/* Memory allocation */
let malloc_mod = "GRAIN$MODULE$runtime/malloc.gr";
let malloc_ident = Ident.create_persistent("malloc");
let malloc_closure_ident = Ident.create_persistent("GRAIN$EXPORT$malloc");

/* Garbage collection */
let gc_mod = "GRAIN$MODULE$runtime/gc.gr";
let incref_ident = Ident.create_persistent("incRef");
let incref_closure_ident = Ident.create_persistent("GRAIN$EXPORT$incRef");
let decref_ident = Ident.create_persistent("decRef");
let decref_closure_ident = Ident.create_persistent("GRAIN$EXPORT$decRef");

/* Exceptions */
let exception_mod = "GRAIN$MODULE$runtime/exception.gr";
let panic_with_exception_ident =
  Ident.create_persistent("panicWithException");
let panic_with_exception_closure_ident =
  Ident.create_persistent("GRAIN$EXPORT$panicWithException");

/* Equality checking */
let equal_mod = "GRAIN$MODULE$runtime/equal.gr";
let equal_ident = Ident.create_persistent("equal");
let equal_closure_ident = Ident.create_persistent("GRAIN$EXPORT$equal");

let required_global_imports = [
  {
    mimp_id: reloc_base,
    mimp_mod: grain_env_mod,
    mimp_name: Ident.name(reloc_base),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), false),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: true,
  },
  {
    mimp_id: module_runtime_id,
    mimp_mod: grain_env_mod,
    mimp_name: Ident.name(module_runtime_id),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), false),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: runtime_heap_start,
    mimp_mod: grain_env_mod,
    mimp_name: Ident.name(runtime_heap_start),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), false),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: runtime_heap_next_ptr,
    mimp_mod: grain_env_mod,
    mimp_name: Ident.name(runtime_heap_next_ptr),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: metadata_ptr,
    mimp_mod: grain_env_mod,
    mimp_name: Ident.name(metadata_ptr),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), false),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: panic_with_exception_closure_ident,
    mimp_mod: exception_mod,
    mimp_name: Ident.name(panic_with_exception_closure_ident),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
];

let grain_runtime_imports = [
  {
    mimp_id: malloc_closure_ident,
    mimp_mod: gc_mod,
    mimp_name: Ident.name(malloc_closure_ident),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: incref_closure_ident,
    mimp_mod: gc_mod,
    mimp_name: Ident.name(incref_closure_ident),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: decref_closure_ident,
    mimp_mod: gc_mod,
    mimp_name: Ident.name(decref_closure_ident),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: equal_closure_ident,
    mimp_mod: equal_mod,
    mimp_name: Ident.name(equal_closure_ident),
    mimp_type: MGlobalImport(Types.Unmanaged(WasmI32), true),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
];

let runtime_global_imports =
  List.append(required_global_imports, grain_runtime_imports);

let required_function_imports = [
  {
    mimp_id: panic_with_exception_ident,
    mimp_mod: exception_mod,
    mimp_name: Ident.name(panic_with_exception_ident),
    mimp_type:
      MFuncImport(
        [Types.Unmanaged(WasmI32), Types.Unmanaged(WasmI32)],
        [Types.Unmanaged(WasmI32)],
      ),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
];

let grain_function_imports = [
  {
    mimp_id: malloc_ident,
    mimp_mod: gc_mod,
    mimp_name: Ident.name(malloc_ident),
    mimp_type:
      MFuncImport(
        [Types.Unmanaged(WasmI32), Types.Unmanaged(WasmI32)],
        [Types.Unmanaged(WasmI32)],
      ),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: incref_ident,
    mimp_mod: gc_mod,
    mimp_name: Ident.name(incref_ident),
    mimp_type:
      MFuncImport(
        [Types.Unmanaged(WasmI32), Types.Unmanaged(WasmI32)],
        [Types.Unmanaged(WasmI32)],
      ), /* Returns same pointer as argument */
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: decref_ident,
    mimp_mod: gc_mod,
    mimp_name: Ident.name(decref_ident),
    mimp_type:
      MFuncImport(
        [Types.Unmanaged(WasmI32), Types.Unmanaged(WasmI32)],
        [Types.Unmanaged(WasmI32)],
      ), /* Returns same pointer as argument */
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
  {
    mimp_id: equal_ident,
    mimp_mod: equal_mod,
    mimp_name: Ident.name(equal_ident),
    mimp_type:
      MFuncImport(
        [Types.Managed, Types.Managed, Types.Managed],
        [Types.Unmanaged(WasmI32)],
      ),
    mimp_kind: MImportWasm,
    mimp_setup: MSetupNone,
    mimp_used: false,
  },
];

let runtime_function_imports =
  List.append(grain_function_imports, required_function_imports);

let runtime_imports =
  List.append(runtime_global_imports, runtime_function_imports);

let runtime_imports_tbl = {
  let tbl = Ident_tbl.create(64);
  List.iter(
    ({mimp_id} as imp) => Ident_tbl.add(tbl, mimp_id, imp),
    runtime_imports,
  );
  tbl;
};

let init_codegen_env = name => {
  name,
  num_args: 0,
  num_closure_args: 0,
  stack_size: {
    stack_size_ptr: 0,
    stack_size_i32: 0,
    stack_size_i64: 0,
    stack_size_f32: 0,
    stack_size_f64: 0,
  },
  backpatches: ref([]),
  required_imports: [],
};

let reset = () => {
  reset_labels();
  List.iter(
    imp => imp.mimp_used = imp.mimp_mod == grain_env_mod,
    runtime_imports,
  );
};

let get_wasm_imported_name = (~runtime_import=true, mod_, name) => {
  if (runtime_import) {
    // Mark runtime import as used
    Ident_tbl.find(runtime_imports_tbl, name).mimp_used =
      true;
  };
  Ident.unique_name(name);
};

let get_runtime_heap_start = wasm_mod =>
  Expression.Global_get.make(
    wasm_mod,
    get_wasm_imported_name(grain_env_mod, runtime_heap_start),
    Type.int32,
  );

let get_metadata_ptr = wasm_mod =>
  Expression.Global_get.make(
    wasm_mod,
    get_wasm_imported_name(grain_env_mod, metadata_ptr),
    Type.int32,
  );

let get_grain_imported_name = (mod_, name) => Ident.unique_name(name);

let call_panic_handler = (wasm_mod, env, args) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      get_wasm_imported_name(
        exception_mod,
        panic_with_exception_closure_ident,
      ),
      Type.int32,
    ),
    ...args,
  ];
  Expression.Call.make(
    wasm_mod,
    get_wasm_imported_name(exception_mod, panic_with_exception_ident),
    args,
    Type.int32,
  );
};

let call_malloc = (wasm_mod, env, args) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      get_wasm_imported_name(gc_mod, malloc_closure_ident),
      Type.int32,
    ),
    ...args,
  ];
  Expression.Call.make(
    wasm_mod,
    get_wasm_imported_name(gc_mod, malloc_ident),
    args,
    Type.int32,
  );
};
let call_incref = (wasm_mod, env, arg) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      get_wasm_imported_name(gc_mod, incref_closure_ident),
      Type.int32,
    ),
    arg,
  ];
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.Call.make(
      wasm_mod,
      get_wasm_imported_name(gc_mod, incref_ident),
      args,
      Type.int32,
    );
  };
};
let call_decref = (wasm_mod, env, arg) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      get_wasm_imported_name(gc_mod, decref_closure_ident),
      Type.int32,
    ),
    arg,
  ];
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.Call.make(
      wasm_mod,
      get_wasm_imported_name(gc_mod, decref_ident),
      args,
      Type.int32,
    );
  };
};
let call_equal = (wasm_mod, env, args) =>
  Expression.Call.make(
    wasm_mod,
    get_wasm_imported_name(equal_mod, equal_ident),
    [
      call_incref(wasm_mod, env) @@
      Expression.Global_get.make(
        wasm_mod,
        get_wasm_imported_name(equal_mod, equal_closure_ident),
        Type.int32,
      ),
      ...args,
    ],
    Type.int32,
  );

/** Untags the number */

let untag_number = (wasm_mod, value) =>
  Expression.Binary.make(
    wasm_mod,
    Op.shr_s_int32,
    value,
    Expression.Const.make(wasm_mod, const_int32(1)),
  );

let tag_number = (wasm_mod, value) =>
  Expression.Binary.make(
    wasm_mod,
    Op.or_int32,
    Expression.Binary.make(
      wasm_mod,
      Op.shl_int32,
      value,
      Expression.Const.make(wasm_mod, const_int32(1)),
    ),
    Expression.Const.make(wasm_mod, const_int32(1)),
  );

let encode_bool = (wasm_mod, value) =>
  Expression.Binary.make(
    wasm_mod,
    Op.or_int32,
    Expression.Binary.make(
      wasm_mod,
      Op.shl_int32,
      value,
      Expression.Const.make(wasm_mod, const_int32(31)),
    ),
    Expression.Const.make(wasm_mod, const_false()),
  );

let decode_bool = (wasm_mod, value) =>
  Expression.Binary.make(
    wasm_mod,
    Op.shr_u_int32,
    value,
    Expression.Const.make(wasm_mod, const_int32(31)),
  );

let encoded_const_int32 = n => const_int32(encoded_int32(n));

type bind_action =
  | BindGet
  | BindSet({
      value: Expression.t,
      initial: bool,
    })
  | BindTee({value: Expression.t});

let should_refcount = b =>
  switch (b) {
  | MArgBind(_, Types.Managed)
  | MLocalBind(_, Types.Managed)
  | MSwapBind(_, Types.Managed)
  | MClosureBind(_)
  | MGlobalBind(_, Types.Managed) => true
  | MArgBind(_)
  | MLocalBind(_)
  | MSwapBind(_)
  | MGlobalBind(_) => false
  };

let appropriate_incref = (wasm_mod, env, arg, b) =>
  if (should_refcount(b)) {
    call_incref(wasm_mod, env, arg);
  } else {
    arg;
  };

let appropriate_decref = (wasm_mod, env, arg, b) =>
  if (should_refcount(b)) {
    call_decref(wasm_mod, env, arg);
  } else {
    arg;
  };

let compile_bind =
    (~action, wasm_mod: Module.t, env: codegen_env, b: binding): Expression.t => {
  let get_slot = (slot, typ) => {
    Expression.Local_get.make(wasm_mod, slot, typ);
  };
  let set_slot = (slot, typ, arg, initial) => {
    Expression.Local_set.make(
      wasm_mod,
      slot,
      if (initial || !should_refcount(b)) {
        arg;
      } else {
        Expression.Tuple_extract.make(
          wasm_mod,
          Expression.Tuple_make.make(
            wasm_mod,
            [
              appropriate_incref(wasm_mod, env, arg, b),
              appropriate_decref(
                wasm_mod,
                env,
                Expression.Local_get.make(wasm_mod, slot, typ),
                b,
              ),
            ],
          ),
          0,
        );
      },
    );
  };
  let tee_slot = (slot, typ, arg) => {
    Expression.Local_tee.make(wasm_mod, slot, arg, typ);
  };
  switch (b) {
  | MArgBind(i, alloc) =>
    /* No adjustments are needed for argument bindings */
    let typ =
      switch (alloc) {
      | Types.Managed
      | Types.Unmanaged(WasmI32) => Type.int32
      | Types.Unmanaged(WasmI64) => Type.int64
      | Types.Unmanaged(WasmF32) => Type.float32
      | Types.Unmanaged(WasmF64) => Type.float64
      };
    let slot = Int32.to_int(i);
    switch (action) {
    | BindGet => get_slot(slot, typ)
    | BindSet({value, initial}) => set_slot(slot, typ, value, initial)
    | BindTee({value}) => tee_slot(slot, typ, value)
    };
  | MClosureBind(i) =>
    /* Closure bindings need to be offset to account for arguments */
    let slot = env.num_args + Int32.to_int(i);
    switch (action) {
    | BindGet => get_slot(slot, Type.int32)
    | BindSet({value, initial}) => set_slot(slot, Type.int32, value, initial)
    | BindTee({value}) => tee_slot(slot, Type.int32, value)
    };
  | MLocalBind(i, alloc) =>
    /* Local bindings need to be offset to account for arguments, closure arguments, and swap variables */
    let (typ, slot) =
      switch (alloc) {
      | Types.Managed => (
          Type.int32,
          env.num_args
          + env.num_closure_args
          + Array.length(swap_slots)
          + Int32.to_int(i),
        )
      | Types.Unmanaged(WasmI32) => (
          Type.int32,
          env.num_args
          + env.num_closure_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_ptr
          + Int32.to_int(i),
        )
      | Types.Unmanaged(WasmI64) => (
          Type.int64,
          env.num_args
          + env.num_closure_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_ptr
          + env.stack_size.stack_size_i32
          + Int32.to_int(i),
        )
      | Types.Unmanaged(WasmF32) => (
          Type.float32,
          env.num_args
          + env.num_closure_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_ptr
          + env.stack_size.stack_size_i32
          + env.stack_size.stack_size_i64
          + Int32.to_int(i),
        )
      | Types.Unmanaged(WasmF64) => (
          Type.float64,
          env.num_args
          + env.num_closure_args
          + Array.length(swap_slots)
          + env.stack_size.stack_size_ptr
          + env.stack_size.stack_size_i32
          + env.stack_size.stack_size_i64
          + env.stack_size.stack_size_f32
          + Int32.to_int(i),
        )
      };
    switch (action) {
    | BindGet => get_slot(slot, typ)
    | BindSet({value, initial}) => set_slot(slot, typ, value, initial)
    | BindTee({value}) => tee_slot(slot, typ, value)
    };
  | MSwapBind(i, wasm_ty) =>
    /* Swap bindings need to be offset to account for arguments and closure arguments */
    let slot = env.num_args + env.num_closure_args + Int32.to_int(i);
    let typ = wasm_type(wasm_ty);
    switch (action) {
    | BindGet => get_slot(slot, typ)
    | BindSet({value, initial}) => set_slot(slot, typ, value, initial)
    | BindTee({value}) => tee_slot(slot, typ, value)
    };
  | MGlobalBind(slot, wasm_ty) =>
    let typ = wasm_type(wasm_ty);
    switch (action) {
    | BindGet => Expression.Global_get.make(wasm_mod, slot, typ)
    | BindSet({value, initial}) =>
      Expression.Global_set.make(
        wasm_mod,
        slot,
        if (initial) {
          value;
        } else {
          Expression.Tuple_extract.make(
            wasm_mod,
            Expression.Tuple_make.make(
              wasm_mod,
              [
                appropriate_incref(wasm_mod, env, value, b),
                appropriate_decref(
                  wasm_mod,
                  env,
                  Expression.Global_get.make(wasm_mod, slot, typ),
                  b,
                ),
              ],
            ),
            0,
          );
        },
      )
    | BindTee({value}) =>
      Expression.Block.make(
        wasm_mod,
        gensym_label("BindTee"),
        [
          Expression.Global_set.make(wasm_mod, slot, value),
          Expression.Global_get.make(wasm_mod, slot, typ),
        ],
      )
    };
  };
};

let get_swap = (~ty as typ=Types.Unmanaged(WasmI32), wasm_mod, env, idx) =>
  switch (typ) {
  | Types.Managed
  | Types.Unmanaged(WasmI32) =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), typ),
    );
  | Types.Unmanaged(WasmI64) =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), typ),
    );
  | Types.Unmanaged(WasmF32) =>
    if (idx > Array.length(swap_slots_f32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f32_offset), typ),
    );
  | Types.Unmanaged(WasmF64) =>
    if (idx > Array.length(swap_slots_f64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f64_offset), typ),
    );
  };

let set_swap =
    (~ty as typ=Types.Unmanaged(WasmI32), wasm_mod, env, idx, value) => {
  let initial = false;
  switch (typ) {
  | Types.Managed
  | Types.Unmanaged(WasmI32) =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value, initial}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), typ),
    );
  | Types.Unmanaged(WasmI64) =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value, initial}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), typ),
    );
  | Types.Unmanaged(WasmF32) =>
    if (idx > Array.length(swap_slots_f32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value, initial}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f32_offset), typ),
    );
  | Types.Unmanaged(WasmF64) =>
    if (idx > Array.length(swap_slots_f64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value, initial}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f64_offset), typ),
    );
  };
};

let tee_swap =
    (~ty as typ=Types.Unmanaged(WasmI32), wasm_mod, env, idx, value) =>
  switch (typ) {
  | Types.Managed
  | Types.Unmanaged(WasmI32) =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), typ),
    );
  | Types.Unmanaged(WasmI64) =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), typ),
    );
  | Types.Unmanaged(WasmF32) =>
    if (idx > Array.length(swap_slots_f32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f32_offset), typ),
    );
  | Types.Unmanaged(WasmF64) =>
    if (idx > Array.length(swap_slots_f64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f64_offset), typ),
    );
  };

let rec compile_imm = (wasm_mod, env: codegen_env, i: immediate): Expression.t =>
  switch (i.immediate_desc) {
  | MImmConst(c) => Expression.Const.make(wasm_mod, compile_const(c))
  | MImmBinding(b) => compile_bind(~action=BindGet, wasm_mod, env, b)
  | MImmTrap => Expression.Unreachable.make(wasm_mod)
  | MIncRef(imm) =>
    call_incref(wasm_mod, env, compile_imm(wasm_mod, env, imm))
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
      Expression.Binary.make(
        wasm_mod,
        Op.add_int32,
        Expression.Global_get.make(
          wasm_mod,
          get_wasm_imported_name(grain_env_mod, runtime_heap_next_ptr),
          Type.int32,
        ),
        Expression.Const.make(
          wasm_mod,
          const_int32(round_to_even(num_words + 2) * 4),
        ),
      );
    Expression.Tuple_extract.make(
      wasm_mod,
      Expression.Tuple_make.make(
        wasm_mod,
        [
          Expression.Block.make(
            wasm_mod,
            gensym_label("heap_allocate_runtime"),
            [
              store(
                wasm_mod,
                Expression.Global_get.make(
                  wasm_mod,
                  get_wasm_imported_name(
                    grain_env_mod,
                    runtime_heap_next_ptr,
                  ),
                  Type.int32,
                ),
                // fake GC refcount of 1
                Expression.Const.make(wasm_mod, const_int32(1)),
              ),
              Expression.Binary.make(
                wasm_mod,
                Op.add_int32,
                Expression.Global_get.make(
                  wasm_mod,
                  get_wasm_imported_name(
                    grain_env_mod,
                    runtime_heap_next_ptr,
                  ),
                  Type.int32,
                ),
                Expression.Const.make(wasm_mod, const_int32(8)),
              ),
            ],
          ),
          Expression.Block.make(
            wasm_mod,
            gensym_label("store_runtime_heap_ptr"),
            [
              Expression.Global_set.make(
                wasm_mod,
                get_wasm_imported_name(grain_env_mod, runtime_heap_next_ptr),
                addition,
              ),
              // Binaryen tuples must include a concrete value (and tuples are
              // the only way to use the stack)
              Expression.Const.make(wasm_mod, const_int32(0)),
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
      [Expression.Const.make(wasm_mod, const_int32(4 * num_words))],
    );
  };

type heap_allocation_type =
  | Words(immediate)
  | Bytes(immediate);

let heap_allocate_imm =
    (~additional_words=0, wasm_mod, env, amount: heap_allocation_type) => {
  let num_bytes =
    switch (amount) {
    | Words(num_words) when additional_words > 0 =>
      Expression.Binary.make(
        wasm_mod,
        Op.mul_int32,
        Expression.Binary.make(
          wasm_mod,
          Op.add_int32,
          compile_imm(wasm_mod, env, num_words),
          Expression.Const.make(wasm_mod, const_int32(additional_words)),
        ),
        Expression.Const.make(wasm_mod, const_int32(4)),
      )
    | Words(num_words) =>
      Expression.Binary.make(
        wasm_mod,
        Op.mul_int32,
        compile_imm(wasm_mod, env, num_words),
        Expression.Const.make(wasm_mod, const_int32(4)),
      )
    | Bytes(num_bytes) when additional_words > 0 =>
      Expression.Binary.make(
        wasm_mod,
        Op.add_int32,
        compile_imm(wasm_mod, env, num_bytes),
        Expression.Const.make(wasm_mod, const_int32(additional_words * 4)),
      )
    | Bytes(num_bytes) => compile_imm(wasm_mod, env, num_bytes)
    };

  call_malloc(wasm_mod, env, [num_bytes]);
};

let allocate_adt = (wasm_mod, env, type_hash, ttag, vtag, elts) => {
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
      compile_imm(wasm_mod, env, elt),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, heap_allocate(wasm_mod, env, num_elts + 5)),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ADTType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      compile_imm(wasm_mod, env, type_hash),
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
      Expression.Const.make(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_adt"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let call_error_handler = (wasm_mod, env, err, args) => {
  let imm = i => {
    immediate_desc: i,
    immediate_analyses: {
      last_usage: Unknown,
    },
  };

  // Use a special hash value for exceptions
  let type_hash = imm(MImmConst(MConstI32(0l)));
  let ty_id =
    imm(
      MImmConst(
        MConstI32(Int32.of_int(Path.stamp(Builtin_types.path_exception))),
      ),
    );
  let cstr_id =
    imm(
      MImmConst(
        MConstI32(
          Int32.of_int(
            switch (err) {
            | Runtime_errors.MatchFailure =>
              Builtin_types.ident_match_failure.stamp
            | Runtime_errors.IndexOutOfBounds =>
              Builtin_types.ident_index_out_of_bounds.stamp
            | Runtime_errors.IndexNonInteger =>
              Builtin_types.ident_index_non_integer.stamp
            },
          ),
        ),
      ),
    );
  let err = allocate_adt(wasm_mod, env, type_hash, ty_id, cstr_id, args);
  call_panic_handler(wasm_mod, env, [err]);
};

let error_if_true = (wasm_mod, env, cond, err) =>
  Expression.If.make(
    wasm_mod,
    cond,
    Expression.Drop.make(
      wasm_mod,
      call_error_handler(wasm_mod, env, err, []),
    ),
    Expression.Null.make(),
  );

let compile_tuple_op = (~is_box=false, wasm_mod, env, tup_imm, op) => {
  let tup = () => compile_imm(wasm_mod, env, tup_imm);
  switch (op) {
  | MTupleGet(idx) =>
    let idx_int = Int32.to_int(idx);
    /* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. */
    call_incref(
      wasm_mod,
      env,
      load(~offset=4 * (idx_int + 2), wasm_mod, tup()),
    );
  | MTupleSet(idx, imm) =>
    let idx_int = Int32.to_int(idx);
    Expression.Block.make(
      wasm_mod,
      gensym_label("MTupleSet"),
      [
        store(
          ~offset=4 * (idx_int + 2),
          wasm_mod,
          tup(),
          Expression.Tuple_extract.make(
            wasm_mod,
            Expression.Tuple_make.make(
              wasm_mod,
              [
                compile_imm(wasm_mod, env, imm),
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
        Expression.Const.make(wasm_mod, const_void()),
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
  let get_arr_value = () => compile_imm(wasm_mod, env, arr_imm);
  let resolve_idx = () => {
    // PRECONDITION: idx is in swap slot 1
    // PRECONDITION: arr is in swap slot 2
    Expression.Block.make(
      wasm_mod,
      gensym_label("resolve_idx"),
      [
        /* Maximum array length in 32-bit mode is less than 2^30, so any heap-allocated int is out of bounds. */
        /* Check that the index is a simple int. */
        Expression.If.make(
          wasm_mod,
          Expression.Unary.make(
            wasm_mod,
            Op.eq_z_int32,
            Expression.Binary.make(
              wasm_mod,
              Op.and_int32,
              get_swap(1),
              Expression.Const.make(wasm_mod, const_int32(1)),
            ),
          ),
          Expression.Block.make(
            wasm_mod,
            gensym_label("IndexNotSimpleInteger"),
            [
              set_swap(1, load(~offset=4, wasm_mod, get_swap(1))),
              Expression.Drop.make(
                wasm_mod,
                Expression.If.make(
                  wasm_mod,
                  Expression.Binary.make(
                    wasm_mod,
                    Op.or_int32,
                    Expression.Binary.make(
                      wasm_mod,
                      Op.eq_int32,
                      get_swap(1),
                      Expression.Const.make(
                        wasm_mod,
                        const_int32(
                          tag_val_of_boxed_number_tag_type(BoxedInt64),
                        ),
                      ),
                    ),
                    Expression.Binary.make(
                      wasm_mod,
                      Op.eq_int32,
                      get_swap(1),
                      Expression.Const.make(
                        wasm_mod,
                        const_int32(
                          tag_val_of_boxed_number_tag_type(BoxedBigInt),
                        ),
                      ),
                    ),
                  ),
                  call_error_handler(wasm_mod, env, IndexOutOfBounds, []),
                  call_error_handler(wasm_mod, env, IndexNonInteger, []),
                ),
              ),
            ],
          ),
          Expression.Null.make(),
        ),
        set_swap(1, untag_number(wasm_mod, get_swap(1))),
        /* Resolve a negative index */
        Expression.If.make(
          wasm_mod,
          Expression.Binary.make(
            wasm_mod,
            Op.lt_s_int32,
            get_swap(1),
            Expression.Const.make(wasm_mod, const_int32(0)),
          ),
          set_swap(
            1,
            Expression.Binary.make(
              wasm_mod,
              Op.add_int32,
              get_swap(1),
              load(~offset=4, wasm_mod, get_swap(2)),
            ),
          ),
          Expression.Null.make(),
        ),
        /*
         Check index not out of bounds
         */
        error_if_true(
          wasm_mod,
          env,
          Expression.Binary.make(
            wasm_mod,
            Op.le_u_int32,
            load(~offset=4, wasm_mod, get_swap(2)),
            get_swap(1),
          ),
          IndexOutOfBounds,
        ),
      ],
    );
  };
  switch (op) {
  | MArrayLength =>
    tag_number(wasm_mod, load(~offset=4, wasm_mod, get_arr_value()))
  | MArrayGet(idx_imm) =>
    let idx = compile_imm(wasm_mod, env, idx_imm);
    Expression.Block.make(
      wasm_mod,
      gensym_label("MArrayGet"),
      [
        set_swap(1, idx),
        set_swap(2, get_arr_value()),
        resolve_idx(),
        /*
         Load item at array+8+(4*idx) and incRef it
         */
        call_incref(
          wasm_mod,
          env,
          load(
            ~offset=8,
            wasm_mod,
            Expression.Binary.make(
              wasm_mod,
              Op.add_int32,
              Expression.Binary.make(
                wasm_mod,
                Op.shl_int32,
                get_swap(1),
                Expression.Const.make(wasm_mod, const_int32(2)),
              ),
              get_swap(2),
            ),
          ),
        ),
      ],
    );
  | MArraySet(idx_imm, val_imm) =>
    let idx = compile_imm(wasm_mod, env, idx_imm);
    let val_ = compile_imm(wasm_mod, env, val_imm);
    Expression.Block.make(
      wasm_mod,
      gensym_label("MArraySet"),
      [
        set_swap(1, idx),
        set_swap(2, get_arr_value()),
        resolve_idx(),
        set_swap(
          2,
          Expression.Binary.make(
            wasm_mod,
            Op.add_int32,
            Expression.Binary.make(
              wasm_mod,
              Op.shl_int32,
              get_swap(1),
              Expression.Const.make(wasm_mod, const_int32(2)),
            ),
            get_swap(2),
          ),
        ),
        store(
          ~offset=8,
          wasm_mod,
          get_swap(2),
          Expression.Tuple_extract.make(
            wasm_mod,
            Expression.Tuple_make.make(
              wasm_mod,
              [
                val_,
                call_decref(
                  wasm_mod,
                  env,
                  load(~offset=8, wasm_mod, get_swap(2)),
                ),
              ],
            ),
            0,
          ),
        ),
        Expression.Const.make(wasm_mod, const_void()),
      ],
    );
  };
};

let compile_adt_op = (wasm_mod, env, adt_imm, op) => {
  let adt = compile_imm(wasm_mod, env, adt_imm);
  switch (op) {
  | MAdtGet(idx) =>
    let idx_int = Int32.to_int(idx);
    call_incref(
      wasm_mod,
      env,
      load(~offset=4 * (idx_int + 5), wasm_mod, adt),
    );
  | MAdtGetModule => load(~offset=4, wasm_mod, adt)
  | MAdtGetTag => load(~offset=12, wasm_mod, adt)
  };
};

let compile_record_op = (wasm_mod, env, rec_imm, op) => {
  let record = () => compile_imm(wasm_mod, env, rec_imm);
  switch (op) {
  | MRecordGet(idx) =>
    let idx_int = Int32.to_int(idx);
    call_incref(
      wasm_mod,
      env,
      load(~offset=4 * (idx_int + 4), wasm_mod, record()),
    );
  | MRecordSet(idx, arg_imm) =>
    let idx_int = Int32.to_int(idx);
    let arg = () => compile_imm(wasm_mod, env, arg_imm);
    Expression.Block.make(
      wasm_mod,
      gensym_label("record_set"),
      [
        store(
          ~offset=4 * (idx_int + 4),
          wasm_mod,
          record(),
          Expression.Tuple_extract.make(
            wasm_mod,
            Expression.Tuple_make.make(
              wasm_mod,
              [
                arg(),
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
        Expression.Const.make(wasm_mod, const_void()),
      ],
    );
  };
};

let compile_closure_op = (wasm_mod, env, closure_imm, op) => {
  let closure = () => compile_imm(wasm_mod, env, closure_imm);
  switch (op) {
  | MClosureSetPtr(idx) =>
    store(
      ~offset=8,
      wasm_mod,
      closure(),
      Expression.Binary.make(
        wasm_mod,
        Op.add_int32,
        Expression.Global_get.make(
          wasm_mod,
          get_wasm_imported_name(grain_env_mod, reloc_base),
          Type.int32,
        ),
        Expression.Const.make(wasm_mod, wrap_int32(idx)),
      ),
    )
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

  List.init(num_ints, i => {Bytes.get_int64_le(bytes, i * 8)});
};

let call_lambda =
    (~tail=false, ~known=?, wasm_mod, env, func, (argsty, retty), args) => {
  let compiled_func = () => compile_imm(wasm_mod, env, func);
  let compiled_args = List.map(compile_imm(wasm_mod, env), args);
  let retty = Type.create @@ Array.map(wasm_type, Array.of_list(retty));
  switch (known) {
  | Some(name) =>
    let instr =
      if (tail) {
        if (Config.no_tail_call^) {
          (
            (wasm_mod, name, args, retty) =>
              Expression.Return.make(
                wasm_mod,
                Expression.Call.make(wasm_mod, name, args, retty),
              )
          );
        } else {
          Expression.Call.make_return;
        };
      } else {
        Expression.Call.make;
      };
    let args = [compiled_func(), ...compiled_args];
    instr(wasm_mod, name, args, retty);
  | None =>
    let instr =
      if (tail) {
        if (Config.no_tail_call^) {
          (
            (wasm_mod, table, ptr, args, argty, retty) =>
              Expression.Return.make(
                wasm_mod,
                Expression.Call_indirect.make(
                  wasm_mod,
                  table,
                  ptr,
                  args,
                  argty,
                  retty,
                ),
              )
          );
        } else {
          Expression.Call_indirect.make_return;
        };
      } else {
        Expression.Call_indirect.make;
      };
    let get_func_swap = () => get_swap(wasm_mod, env, 0);
    let args = [get_func_swap(), ...compiled_args];
    Expression.Block.make(
      wasm_mod,
      gensym_label("call_lambda"),
      [
        // compiled_func() returns a refcount of n+1, so we need to place it in a swap to avoid
        // an excess reference caused by the load() call below
        set_swap(wasm_mod, env, 0, compiled_func()),
        instr(
          wasm_mod,
          grain_global_function_table,
          load(~offset=8, wasm_mod, get_func_swap()),
          args,
          Type.create @@
          Array.map(
            wasm_type,
            Array.of_list([Types.Unmanaged(WasmI32), ...argsty]),
          ),
          retty,
        ),
      ],
    );
  };
};

let allocate_byte_like_from_buffer = (wasm_mod, env, buf, tag, label) => {
  let ints_to_push: list(int64) = buf_to_ints(buf);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(wasm_mod, env, 0);
  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        heap_allocate(wasm_mod, env, 2 + 2 * List.length(ints_to_push)),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(tag)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.Const.make(wasm_mod, const_int32 @@ Buffer.length(buf)),
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
          Expression.Const.make(wasm_mod, wrap_int64(i)),
        ),
      ints_to_push,
    );
  Expression.Block.make(
    wasm_mod,
    gensym_label(label),
    List.concat([preamble, elts, [get_swap()]]),
  );
};

let allocate_byte_like_uninitialized = (wasm_mod, env, size, tag, label) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(wasm_mod, env, 0);
  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        heap_allocate_imm(~additional_words=2, wasm_mod, env, Bytes(size)),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(tag)),
      ),
    ),
    store(~offset=4, wasm_mod, get_swap(), compile_imm(wasm_mod, env, size)),
  ];
  Expression.Block.make(
    wasm_mod,
    gensym_label(label),
    List.concat([preamble, [get_swap()]]),
  );
};

let allocate_string = (wasm_mod, env, str) => {
  let buf = Buffer.create(80);
  Buffer.add_string(buf, str);
  allocate_byte_like_from_buffer(
    wasm_mod,
    env,
    buf,
    StringType,
    "allocate_string",
  );
};

let allocate_string_uninitialized = (wasm_mod, env, size) => {
  allocate_byte_like_uninitialized(
    wasm_mod,
    env,
    size,
    StringType,
    "allocate_string_uninitialized",
  );
};

let allocate_bytes = (wasm_mod, env, bytes) => {
  let buf = Buffer.create(80);
  Buffer.add_bytes(buf, bytes);
  allocate_byte_like_from_buffer(
    wasm_mod,
    env,
    buf,
    BytesType,
    "allocate_bytes",
  );
};

let allocate_bytes_uninitialized = (wasm_mod, env, size) => {
  allocate_byte_like_uninitialized(
    wasm_mod,
    env,
    size,
    BytesType,
    "allocate_bytes_uninitialized",
  );
};

type int_type =
  | Int8Type
  | Int16Type
  | Uint8Type
  | Uint16Type;

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
          Expression.Binary.make(
            wasm_mod,
            Op.sub_int32,
            get_swap(),
            Expression.Const.make(wasm_mod, const_int32 @@ 4 * closure_size),
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
        compile_imm(wasm_mod, env, var),
      );
    patches := List.mapi(patch_var, variables);
  };
  let func_idx =
    switch (func_idx) {
    | Some(idx) =>
      Expression.Binary.make(
        wasm_mod,
        Op.add_int32,
        Expression.Global_get.make(
          wasm_mod,
          get_wasm_imported_name(grain_env_mod, reloc_base),
          Type.int32,
        ),
        Expression.Const.make(wasm_mod, wrap_int32(idx)),
      )
    // Use as a sentinel
    | None => Expression.Const.make(wasm_mod, wrap_int32(-1l))
    };
  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, heap_allocate(wasm_mod, env, closure_size)),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(LambdaType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.Const.make(wasm_mod, wrap_int32(arity)),
    ),
    store(~offset=8, wasm_mod, get_swap(), func_idx),
    store(
      ~offset=12,
      wasm_mod,
      get_swap(),
      Expression.Const.make(wasm_mod, const_int32(num_free_vars)),
    ),
  ];
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_closure"),
    List.concat([preamble, patches^, postamble]),
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
      compile_imm(wasm_mod, env, elt),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, heap_allocate(wasm_mod, env, num_elts + 2)),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(TupleType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.Const.make(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_tuple"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let allocate_uninitialized_tuple = (~is_box=false, wasm_mod, env, num_elts) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        wasm_mod,
        env,
        0,
        heap_allocate_imm(
          ~additional_words=2,
          wasm_mod,
          env,
          Words(num_elts),
        ),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(TupleType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      compile_imm(wasm_mod, env, num_elts),
    ),
  ];
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_tuple"),
    List.concat([preamble, postamble]),
  );
};

let allocate_box = (wasm_mod, env, elt) =>
  /* At the moment, we make no runtime distinction between boxes and tuples */
  allocate_tuple(~is_box=true, wasm_mod, env, [elt]);

let allocate_uninitialized_array = (wasm_mod, env, num_elts) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        wasm_mod,
        env,
        0,
        heap_allocate_imm(
          ~additional_words=2,
          wasm_mod,
          env,
          Words(num_elts),
        ),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ArrayType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      compile_imm(wasm_mod, env, num_elts),
    ),
  ];
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_uninitialized_array"),
    List.concat([preamble, postamble]),
  );
};

let allocate_array = (wasm_mod, env, elts) => {
  let num_elts = List.length(elts);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let compile_elt = (idx, elt) =>
    store(
      ~offset=4 * (idx + 2),
      wasm_mod,
      get_swap(),
      compile_imm(wasm_mod, env, elt),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, heap_allocate(wasm_mod, env, num_elts + 2)),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ArrayType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.Const.make(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_array"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let allocate_record = (wasm_mod, env, type_hash, ttag, elts) => {
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
      compile_imm(wasm_mod, env, elt),
    );

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, heap_allocate(wasm_mod, env, num_elts + 4)),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(RecordType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      compile_imm(wasm_mod, env, type_hash),
    ),
    store(~offset=8, wasm_mod, get_swap(), compile_imm(wasm_mod, env, ttag)),
    store(
      ~offset=12,
      wasm_mod,
      get_swap(),
      Expression.Const.make(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_record"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

// "Alt" number here is defined as one not belonging to the `Number` type
let allocate_alt_num_uninitialized = (wasm_mod, env, tag) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let (num_words, label) =
    switch (tag) {
    | Int32Type => (2, "allocate_unitialized_int32")
    | Float32Type => (2, "allocate_unitialized_float32")
    | Uint32Type => (2, "allocate_unitialized_uint32")
    | Uint64Type => (4, "allocate_unitialized_uint64")
    | _ =>
      failwith(
        "Impossible: allocate_alt_num_uninitialized given non-alt-num tag",
      )
    };
  let make_alloc = () => heap_allocate(wasm_mod, env, num_words);

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, make_alloc()),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(tag)),
      ),
    ),
  ];
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label(label),
    List.concat([preamble, postamble]),
  );
};

type alloc_alt_num_type =
  | Int32(Expression.t)
  | Float32(Expression.t)
  | Uint32(Expression.t)
  | Uint64(Expression.t);

let allocate_alt_num = (wasm_mod, env, num_value) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);

  let (tag, instrs, needed_words, label) =
    switch (num_value) {
    | Int32(int32) => (
        Int32Type,
        [store(~offset=4, ~ty=Type.int32, wasm_mod, get_swap(), int32)],
        2,
        "allocate_int32",
      )
    | Float32(float32) => (
        Float32Type,
        [store(~offset=4, ~ty=Type.float32, wasm_mod, get_swap(), float32)],
        2,
        "allocate_float32",
      )
    | Uint32(uint32) => (
        Uint32Type,
        [store(~offset=4, ~ty=Type.int32, wasm_mod, get_swap(), uint32)],
        2,
        "allocate_uint32",
      )
    | Uint64(uint64) => (
        Uint64Type,
        [store(~offset=8, ~ty=Type.int64, wasm_mod, get_swap(), uint64)],
        // Allocate 4 words to store with 8-byte alignment
        4,
        "allocate_uint64",
      )
    };

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, heap_allocate(wasm_mod, env, needed_words)),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(tag)),
      ),
    ),
  ];
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label(label),
    List.concat([preamble, instrs, postamble]),
  );
};

type alloc_number_type =
  | Int64(Expression.t)
  | Float64(Expression.t)
  | Rational(Expression.t, Expression.t)
  | BigInt(Expression.t, list(Expression.t));

let allocate_number = (wasm_mod, env, number) => {
  /* Heap memory layout of numbers:
     [ <value type tag>, <number_tag>, <payload>]
     */
  let get_swap = get_swap(wasm_mod, env);

  let (number_tag, swap_slot, instrs, needed_words) =
    switch (number) {
    | Int64(int64) =>
      let slot = 0;
      (
        BoxedInt64,
        slot,
        [store(~offset=8, ~ty=Type.int64, wasm_mod, get_swap(slot), int64)],
        4,
      );
    | Float64(float64) =>
      let slot = 0;
      (
        BoxedFloat64,
        slot,
        [
          store(
            ~offset=8,
            ~ty=Type.float64,
            wasm_mod,
            get_swap(slot),
            float64,
          ),
        ],
        4,
      );
    | Rational(numerator, denominator) =>
      // Rationals use a different swap slot to allow allocation of
      // intermediate bigints
      let slot = 1;
      (
        BoxedRational,
        slot,
        [
          store(
            ~offset=8,
            ~ty=Type.int32,
            wasm_mod,
            get_swap(slot),
            numerator,
          ),
          store(
            ~offset=12,
            ~ty=Type.int32,
            wasm_mod,
            get_swap(slot),
            denominator,
          ),
        ],
        4,
      );
    | BigInt(flags, limbs) =>
      let slot = 0;
      (
        BoxedBigInt,
        slot,
        List.append(
          [
            store(
              ~offset=8,
              ~ty=Type.int32,
              wasm_mod,
              get_swap(slot),
              Expression.Const.make(
                wasm_mod,
                const_int32(List.length(limbs)),
              ),
            ),
            store(
              ~offset=12,
              ~ty=Type.int32,
              wasm_mod,
              get_swap(slot),
              flags,
            ),
          ],
          List.mapi(
            (i, limb) => {
              store(
                ~offset=16 + i * 8,
                ~ty=Type.int64,
                wasm_mod,
                get_swap(slot),
                limb,
              )
            },
            limbs,
          ),
        ),
        4 + 2 * List.length(limbs),
      );
    };

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(
        wasm_mod,
        env,
        swap_slot,
        heap_allocate(wasm_mod, env, needed_words),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(BoxedNumberType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(swap_slot),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_boxed_number_tag_type(number_tag)),
      ),
    ),
  ];
  let postamble = [get_swap(swap_slot)];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_number"),
    List.concat([preamble, instrs, postamble]),
  );
};

let allocate_number_uninitialized =
    (~additional_words=?, wasm_mod, env, number_tag) => {
  /* Heap memory layout of numbers:
     [ <value type tag>, <number_tag>, <payload>]
     */
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let make_alloc = () =>
    switch (additional_words) {
    // Grain allocations are 8-byte aligned, so no space is saved by
    // allocating 3 words for 32-bit numbers
    | None => heap_allocate(wasm_mod, env, 4)
    | Some(n) =>
      heap_allocate_imm(~additional_words=4, wasm_mod, env, Words(n))
    };

  let preamble = [
    store(
      ~offset=0,
      wasm_mod,
      tee_swap(wasm_mod, env, 0, make_alloc()),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(BoxedNumberType)),
      ),
    ),
    store(
      ~offset=4,
      wasm_mod,
      get_swap(),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_boxed_number_tag_type(number_tag)),
      ),
    ),
  ];
  let postamble = [get_swap()];
  Expression.Block.make(
    wasm_mod,
    gensym_label("allocate_number"),
    List.concat([preamble, postamble]),
  );
};

let allocate_float32 = (wasm_mod, env, i) => {
  allocate_alt_num(wasm_mod, env, Float32(i));
};

let allocate_float64 = (wasm_mod, env, i) => {
  allocate_number(wasm_mod, env, Float64(i));
};

let allocate_int32 = (wasm_mod, env, i) => {
  allocate_alt_num(wasm_mod, env, Int32(i));
};

let allocate_int64 = (wasm_mod, env, i) => {
  allocate_number(wasm_mod, env, Int64(i));
};

let allocate_rational = (wasm_mod, env, n, d) => {
  allocate_number(wasm_mod, env, Rational(n, d));
};

let allocate_big_int = (wasm_mod, env, n, d) => {
  allocate_number(wasm_mod, env, BigInt(n, d));
};

let allocate_uint32 = (wasm_mod, env, i) => {
  allocate_alt_num(wasm_mod, env, Uint32(i));
};

let allocate_uint64 = (wasm_mod, env, i) => {
  allocate_alt_num(wasm_mod, env, Uint64(i));
};

let tag_short_value = (wasm_mod, compiled_arg, tag) => {
  Expression.Binary.make(
    wasm_mod,
    Op.xor_int32,
    Expression.Binary.make(
      wasm_mod,
      Op.shl_int32,
      compiled_arg,
      Expression.Const.make(wasm_mod, const_int32(0x8)),
    ),
    Expression.Const.make(wasm_mod, const_int32(tag)),
  );
};

let compile_prim0 = (wasm_mod, env, p0): Expression.t => {
  switch (p0) {
  | AllocateInt32 => allocate_alt_num_uninitialized(wasm_mod, env, Int32Type)
  | AllocateInt64 => allocate_number_uninitialized(wasm_mod, env, BoxedInt64)
  | AllocateFloat32 =>
    allocate_alt_num_uninitialized(wasm_mod, env, Float32Type)
  | AllocateFloat64 =>
    allocate_number_uninitialized(wasm_mod, env, BoxedFloat64)
  | AllocateRational =>
    allocate_number_uninitialized(wasm_mod, env, BoxedRational)
  | AllocateUint32 =>
    allocate_alt_num_uninitialized(wasm_mod, env, Uint32Type)
  | AllocateUint64 =>
    allocate_alt_num_uninitialized(wasm_mod, env, Uint64Type)
  | WasmMemorySize =>
    Expression.Memory_size.make(wasm_mod, grain_memory, false)
  | Unreachable => Expression.Unreachable.make(wasm_mod)
  | HeapStart => get_runtime_heap_start(wasm_mod)
  | HeapTypeMetadata => get_metadata_ptr(wasm_mod)
  };
};

let compile_prim1 = (wasm_mod, env, p1, arg, loc): Expression.t => {
  let compiled_arg = compile_imm(wasm_mod, env, arg);
  // TODO: Overflow checks?
  switch (p1) {
  | AllocateArray => allocate_uninitialized_array(wasm_mod, env, arg)
  | AllocateTuple => allocate_uninitialized_tuple(wasm_mod, env, arg)
  | AllocateBytes => allocate_bytes_uninitialized(wasm_mod, env, arg)
  | AllocateString => allocate_string_uninitialized(wasm_mod, env, arg)
  | AllocateBigInt =>
    allocate_number_uninitialized(
      ~additional_words=arg,
      wasm_mod,
      env,
      BoxedBigInt,
    )
  | NewInt32 => allocate_alt_num(wasm_mod, env, Int32(compiled_arg))
  | NewInt64 => allocate_number(wasm_mod, env, Int64(compiled_arg))
  | NewFloat32 => allocate_alt_num(wasm_mod, env, Float32(compiled_arg))
  | NewFloat64 => allocate_number(wasm_mod, env, Float64(compiled_arg))
  | NewUint32 => allocate_alt_num(wasm_mod, env, Uint32(compiled_arg))
  | NewUint64 => allocate_alt_num(wasm_mod, env, Uint64(compiled_arg))
  | LoadAdtVariant => load(~offset=12, wasm_mod, compiled_arg)
  | StringSize
  | BytesSize => load(~offset=4, wasm_mod, compiled_arg)
  | TagSimpleNumber =>
    Expression.Binary.make(
      wasm_mod,
      Op.xor_int32,
      Expression.Binary.make(
        wasm_mod,
        Op.shl_int32,
        compiled_arg,
        Expression.Const.make(wasm_mod, const_int32(0x1)),
      ),
      Expression.Const.make(wasm_mod, const_int32(0x1)),
    )
  | UntagSimpleNumber =>
    Expression.Binary.make(
      wasm_mod,
      Op.shr_s_int32,
      compiled_arg,
      Expression.Const.make(wasm_mod, const_int32(0x1)),
    )
  | TagChar => tag_short_value(wasm_mod, compiled_arg, 0b10)
  | TagInt8 => tag_short_value(wasm_mod, compiled_arg, 0b1010)
  | TagInt16 => tag_short_value(wasm_mod, compiled_arg, 0b10010)
  | TagUint8 => tag_short_value(wasm_mod, compiled_arg, 0b11010)
  | TagUint16 => tag_short_value(wasm_mod, compiled_arg, 0b100010)
  | UntagChar
  | UntagInt8
  | UntagInt16
  | UntagUint8
  | UntagUint16 =>
    Expression.Binary.make(
      wasm_mod,
      Op.shr_s_int32,
      compiled_arg,
      Expression.Const.make(wasm_mod, const_int32(0x8)),
    )
  | Not =>
    /* Flip the first bit */
    Expression.Binary.make(
      wasm_mod,
      Op.xor_int32,
      compiled_arg,
      Expression.Const.make(wasm_mod, const_int32(0x80000000)),
    )
  | Ignore =>
    Expression.Block.make(
      wasm_mod,
      gensym_label("Ignore"),
      [
        Expression.Drop.make(wasm_mod, compiled_arg),
        Expression.Const.make(wasm_mod, const_void()),
      ],
    )
  | ArrayLength => compile_array_op(wasm_mod, env, arg, MArrayLength)
  | Throw =>
    // TODO(#813): When we have exception handling, revisit whether there is any GC required here
    Expression.Block.make(
      wasm_mod,
      gensym_label("throw"),
      [
        Expression.Drop.make(
          wasm_mod,
          call_panic_handler(wasm_mod, env, [compiled_arg]),
        ),
        Expression.Unreachable.make(wasm_mod),
      ],
    )
  | Magic => failwith("Unreachable case; should never get here: Magic")
  | Assert => failwith("Unreachable case; should never get here: Assert")
  | BuiltinId =>
    failwith("Unreachable case; should never get here: BuiltinId")
  | Box => failwith("Unreachable case; should never get here: Box")
  | Unbox => failwith("Unreachable case; should never get here: Unbox")
  | BoxBind => failwith("Unreachable case; should never get here: BoxBind")
  | UnboxBind =>
    failwith("Unreachable case; should never get here: UnboxBind")
  | WasmFromGrain =>
    // no-op
    compile_imm(wasm_mod, env, arg)
  | WasmToGrain => compiled_arg // no-op
  | WasmMemoryGrow =>
    Expression.Memory_grow.make(wasm_mod, compiled_arg, grain_memory, false)
  | WasmUnaryI32({wasm_op, ret_type})
  | WasmUnaryI64({wasm_op, ret_type})
  | WasmUnaryF32({wasm_op, ret_type})
  | WasmUnaryF64({wasm_op, ret_type}) =>
    compile_wasm_prim1(wasm_mod, env, wasm_op, ret_type, compiled_arg)
  };
};

let compile_wasm_load =
    (~sz=?, ~ty=?, ~signed=?, wasm_mod, compiled_arg1, compiled_arg2, offset) => {
  switch (offset.immediate_desc) {
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
      Expression.Binary.make(
        wasm_mod,
        Op.add_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  };
};

let compile_wasm_store = (~sz=?, ~ty=?, wasm_mod, env, args) => {
  switch (List.nth(args, 2).immediate_desc) {
  | MImmConst(MConstLiteral(MConstI32(offset))) =>
    Expression.Block.make(
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
        Expression.Const.make(wasm_mod, const_void()),
      ],
    )

  | _ =>
    Expression.Block.make(
      wasm_mod,
      gensym_label("wasm_prim_store"),
      [
        store(
          ~sz?,
          ~ty?,
          wasm_mod,
          Expression.Binary.make(
            wasm_mod,
            Op.add_int32,
            compile_imm(wasm_mod, env, List.nth(args, 0)),
            compile_imm(wasm_mod, env, List.nth(args, 2)),
          ),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
        ),
        Expression.Const.make(wasm_mod, const_void()),
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
    Expression.If.make(
      wasm_mod,
      decode_bool(wasm_mod, swap_tee(compiled_arg1())),
      compiled_arg2(),
      swap_get(),
    )
  | Or =>
    Expression.If.make(
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
      Expression.Binary.make(
        wasm_mod,
        Op.eq_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  | NewRational =>
    allocate_number(
      wasm_mod,
      env,
      Rational(compiled_arg1(), compiled_arg2()),
    )
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
    Expression.Block.make(
      wasm_mod,
      gensym_label("memory_copy"),
      [
        Expression.Memory_copy.make(
          wasm_mod,
          compile_imm(wasm_mod, env, List.nth(args, 0)),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
          compile_imm(wasm_mod, env, List.nth(args, 2)),
          grain_memory,
          grain_memory,
        ),
        Expression.Const.make(wasm_mod, const_void()),
      ],
    )
  | WasmMemoryFill =>
    Expression.Block.make(
      wasm_mod,
      gensym_label("memory_fill"),
      [
        Expression.Memory_fill.make(
          wasm_mod,
          compile_imm(wasm_mod, env, List.nth(args, 0)),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
          compile_imm(wasm_mod, env, List.nth(args, 2)),
          grain_memory,
        ),
        Expression.Const.make(wasm_mod, const_void()),
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
    Expression.Block.make(
      wasm_mod,
      lbl,
      [
        set_ptr1(compile_imm(wasm_mod, env, List.nth(args, 0))),
        set_ptr2(compile_imm(wasm_mod, env, List.nth(args, 1))),
        set_count(compile_imm(wasm_mod, env, List.nth(args, 2))),
        Expression.Loop.make(
          wasm_mod,
          loop_lbl,
          Expression.Block.make(
            wasm_mod,
            gensym_label("memory_compare_loop_inner"),
            [
              Expression.Drop.make(wasm_mod) @@
              Expression.Break.make(
                wasm_mod,
                lbl,
                Expression.Unary.make(wasm_mod, Op.eq_z_int32, get_count()),
                Expression.Const.make(wasm_mod, const_int32(0)),
              ),
              Expression.If.make(
                wasm_mod,
                Expression.Binary.make(
                  wasm_mod,
                  Op.ne_int32,
                  load(~sz=1, ~signed=false, wasm_mod, get_ptr1()),
                  load(~sz=1, ~signed=false, wasm_mod, get_ptr2()),
                ),
                Expression.Break.make(
                  wasm_mod,
                  lbl,
                  Expression.Null.make(),
                  Expression.Select.make(
                    wasm_mod,
                    Expression.Binary.make(
                      wasm_mod,
                      Op.lt_u_int32,
                      load(~sz=1, ~signed=false, wasm_mod, get_ptr1()),
                      load(~sz=1, ~signed=false, wasm_mod, get_ptr2()),
                    ),
                    Expression.Const.make(wasm_mod, const_int32(-1)),
                    Expression.Const.make(wasm_mod, const_int32(1)),
                  ),
                ),
                Expression.Block.make(
                  wasm_mod,
                  gensym_label("memory_compare_loop_incr"),
                  [
                    set_ptr1(
                      Expression.Binary.make(
                        wasm_mod,
                        Op.add_int32,
                        get_ptr1(),
                        Expression.Const.make(wasm_mod, const_int32(1)),
                      ),
                    ),
                    set_ptr2(
                      Expression.Binary.make(
                        wasm_mod,
                        Op.add_int32,
                        get_ptr2(),
                        Expression.Const.make(wasm_mod, const_int32(1)),
                      ),
                    ),
                    set_count(
                      Expression.Binary.make(
                        wasm_mod,
                        Op.sub_int32,
                        get_count(),
                        Expression.Const.make(wasm_mod, const_int32(1)),
                      ),
                    ),
                    Expression.Break.make(
                      wasm_mod,
                      loop_lbl,
                      Expression.Null.make(),
                      Expression.Null.make(),
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
  | MRecord(type_hash, ttag, elts) =>
    allocate_record(wasm_mod, env, type_hash, ttag, elts)
  | MBytes(bytes) => allocate_bytes(wasm_mod, env, bytes)
  | MString(str) => allocate_string(wasm_mod, env, str)
  | MADT(type_hash, ttag, vtag, elts) =>
    allocate_adt(wasm_mod, env, type_hash, ttag, vtag, elts)
  | MInt32(i) =>
    allocate_int32(
      wasm_mod,
      env,
      Expression.Const.make(wasm_mod, Literal.int32(i)),
    )
  | MInt64(i) =>
    allocate_int64(
      wasm_mod,
      env,
      Expression.Const.make(wasm_mod, Literal.int64(i)),
    )
  | MUint32(i) =>
    allocate_uint32(
      wasm_mod,
      env,
      Expression.Const.make(wasm_mod, Literal.int32(i)),
    )
  | MUint64(i) =>
    allocate_uint64(
      wasm_mod,
      env,
      Expression.Const.make(wasm_mod, Literal.int64(i)),
    )
  | MFloat32(i) =>
    allocate_float32(
      wasm_mod,
      env,
      Expression.Const.make(wasm_mod, Literal.float32(i)),
    )
  | MFloat64(i) =>
    allocate_float64(
      wasm_mod,
      env,
      Expression.Const.make(wasm_mod, Literal.float64(i)),
    )
  | MBigInt({flags, limbs}) =>
    allocate_big_int(
      wasm_mod,
      env,
      Expression.Const.make(
        wasm_mod,
        Literal.int32(Bigint_flags.all_to_int32(flags)),
      ),
      List.map(
        n => Expression.Const.make(wasm_mod, Literal.int64(n)),
        Array.to_list(limbs),
      ),
    )
  | MRational({
      numerator_flags,
      numerator_limbs,
      denominator_flags,
      denominator_limbs,
    }) =>
    let numerator =
      allocate_big_int(
        wasm_mod,
        env,
        Expression.Const.make(
          wasm_mod,
          Literal.int32(Bigint_flags.all_to_int32(numerator_flags)),
        ),
        List.map(
          n => Expression.Const.make(wasm_mod, Literal.int64(n)),
          Array.to_list(numerator_limbs),
        ),
      );
    let denominator =
      allocate_big_int(
        wasm_mod,
        env,
        Expression.Const.make(
          wasm_mod,
          Literal.int32(Bigint_flags.all_to_int32(denominator_flags)),
        ),
        List.map(
          n => Expression.Const.make(wasm_mod, Literal.int64(n)),
          Array.to_list(denominator_limbs),
        ),
      );
    allocate_rational(wasm_mod, env, numerator, denominator);
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
        compile_imm(wasm_mod, env, var),
      );
    [preamble, ...List.mapi(backpatch_var, variables)];
  };
  Expression.Block.make(
    wasm_mod,
    gensym_label("do_backpatches"),
    List.concat @@ List.map(do_backpatch, backpatches),
  );
};

let current_function = ref(None);
let get_current_function = () => {
  switch (current_function^) {
  | Some(func) => func
  | None => failwith("No current function set")
  };
};
let set_current_function = func => current_function := Some(func);
let loop_stack = ref([]: list((string, string)));

let rec compile_store = (wasm_mod, env, binds) => {
  let process_binds = env => {
    let process_bind = ((b, instr), acc) => {
      let store_bind = value =>
        compile_bind(
          ~action=BindSet({value, initial: true}),
          wasm_mod,
          env,
          b,
        );
      let compiled_instr =
        switch (instr.instr_desc) {
        // special logic here for letrec
        | MAllocate(MClosure(cdata)) =>
          // We skip the incref here as this is akin to using a swap slot (the
          // reference we create here cannot escape, so there isn't a need to add an
          // incref/decref pair). Since it won't live in a local, it wouldn't be
          // cleaned up automatically anyway.
          let get_bind = compile_bind(~action=BindGet, wasm_mod, env, b);
          allocate_closure(
            wasm_mod,
            env,
            ~lambda=get_bind,
            ~skip_patching=true,
            cdata,
          );
        | MReturnCallIndirect(_)
        | MReturnCallKnown(_)
        | MCallIndirect(_)
        | MCallKnown(_)
        | MCallRaw(_)
        | MAllocate(_)
        | _ => compile_instr(wasm_mod, env, instr)
        };
      [store_bind(compiled_instr), ...acc];
    };
    List.fold_right(process_bind, binds, []);
  };
  let (instrs, backpatches) = collect_backpatches(env, process_binds);
  Expression.Block.make(wasm_mod, gensym_label("compile_store")) @@
  List.append(instrs, [do_backpatches(wasm_mod, env, backpatches)]);
}

and compile_set = (wasm_mod, env, b, i) => {
  Expression.Block.make(
    wasm_mod,
    gensym_label("compile_set"),
    [
      compile_bind(
        ~action=
          BindSet({value: compile_instr(wasm_mod, env, i), initial: false}),
        wasm_mod,
        env,
        b,
      ),
      Expression.Const.make(wasm_mod, const_void()),
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
        | Types.Managed
        | Types.Unmanaged(WasmI32) => const_int32(0)
        | Types.Unmanaged(WasmI64) => const_int64(0)
        | Types.Unmanaged(WasmF32) => const_float32(0.)
        | Types.Unmanaged(WasmF64) => const_float64(0.)
        };
      let default_value = Expression.Const.make(wasm_mod, default_value);
      let inner_block_body =
        Expression.Switch.make(
          wasm_mod,
          create_table(stack),
          default_label,
          untag_number(wasm_mod, compile_imm(wasm_mod, env, arg)),
          default_value,
        );
      let default_block_body =
        compile_block(~return_type=switch_ty, wasm_mod, env, default);
      Expression.Block.make(
        ~return_type=switch_ty,
        wasm_mod,
        branch_name,
        [
          Expression.Drop.make(wasm_mod) @@
          Expression.Block.make(
            ~return_type=switch_ty,
            wasm_mod,
            default_label,
            [inner_block_body],
          ),
          Expression.Break.make(
            wasm_mod,
            outer_label,
            Expression.Null.make(),
            default_block_body,
          ),
        ],
      );
    | [(lbl, hd), ...tl] =>
      Expression.Block.make(
        ~return_type=switch_ty,
        wasm_mod,
        branch_name,
        [
          Expression.Drop.make(
            wasm_mod,
            process_branches(
              count + 1,
              [(Int32.to_int(lbl), target_branch_name), ...stack],
              tl,
            ),
          ),
          Expression.Break.make(
            wasm_mod,
            outer_label,
            Expression.Null.make(),
            compile_block(wasm_mod, env, hd),
          ),
        ],
      )
    };
  };
  Expression.Block.make(
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
  Expression.Block.make(
    ~return_type?,
    wasm_mod,
    gensym_label("compile_block"),
    compiled_instrs,
  );
}
and compile_instr = (wasm_mod, env, instr) =>
  switch (instr.instr_desc) {
  | MDrop(arg) =>
    Expression.Drop.make(wasm_mod, compile_instr(wasm_mod, env, arg))
  | MImmediate(imm) => compile_imm(wasm_mod, env, imm)
  | MAllocate(alloc) => compile_allocation(wasm_mod, env, alloc)
  | MTupleOp(tuple_op, tup) => compile_tuple_op(wasm_mod, env, tup, tuple_op)
  | MBoxOp(box_op, box) => compile_box_op(wasm_mod, env, box, box_op)
  | MArrayOp(array_op, ret) => compile_array_op(wasm_mod, env, ret, array_op)
  | MAdtOp(adt_op, adt) => compile_adt_op(wasm_mod, env, adt, adt_op)
  | MRecordOp(record_op, record) =>
    compile_record_op(wasm_mod, env, record, record_op)
  | MClosureOp(closure_op, closure) =>
    compile_closure_op(wasm_mod, env, closure, closure_op)
  | MPrim0(p0) => compile_prim0(wasm_mod, env, p0)
  | MPrim1(p1, arg) => compile_prim1(wasm_mod, env, p1, arg, instr.instr_loc)
  | MPrim2(p2, arg1, arg2) => compile_prim2(wasm_mod, env, p2, arg1, arg2)
  | MPrimN(p, args) => compile_primn(wasm_mod, env, p, args)
  | MSwitch(arg, branches, default, ty) =>
    compile_switch(wasm_mod, env, arg, branches, default, ty)
  | MStore(binds) => compile_store(wasm_mod, env, binds)
  | MSet(b, i) => compile_set(wasm_mod, env, b, i)
  | MCallKnown({func, closure, func_type, args}) =>
    call_lambda(~known=func, wasm_mod, env, closure, func_type, args)
  | MReturnCallKnown({func, closure, func_type, args}) =>
    call_lambda(
      ~tail=true,
      ~known=func,
      wasm_mod,
      env,
      closure,
      func_type,
      args,
    )
  | MCallIndirect({func, func_type, args}) =>
    call_lambda(wasm_mod, env, func, func_type, args)
  | MReturnCallIndirect({func, func_type, args}) =>
    call_lambda(~tail=true, wasm_mod, env, func, func_type, args)
  | MCallRaw({func, func_type: (_, retty), args}) =>
    let compiled_args = List.map(compile_imm(wasm_mod, env), args);
    Expression.Call.make(
      wasm_mod,
      func,
      compiled_args,
      Type.create(Array.of_list(List.map(wasm_type, retty))),
    );
  | MIf(cond, thn, els) =>
    let compiled_cond = compile_imm(wasm_mod, env, cond);
    let compiled_thn = compile_block(wasm_mod, env, thn);
    let compiled_els = compile_block(wasm_mod, env, els);
    Expression.If.make(
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
          Expression.Drop.make(wasm_mod) @@
          Expression.Break.make(
            wasm_mod,
            block_label,
            Expression.Unary.make(
              wasm_mod,
              Op.eq_z_int32,
              decode_bool(wasm_mod, compile_block(wasm_mod, env, cond)),
            ),
            Expression.Const.make(wasm_mod, const_void()),
          ),
        ]
      | None => []
      };
    let compiled_inc =
      switch (inc) {
      | Some(inc) => [
          Expression.Drop.make(wasm_mod, compile_block(wasm_mod, env, inc)),
        ]
      | None => []
      };
    loop_stack := [(continue_label, block_label), ...loop_stack^];
    let compiled_body = compile_block(wasm_mod, env, body);
    loop_stack := List.tl(loop_stack^);
    Expression.Block.make(
      wasm_mod,
      block_label,
      [
        Expression.Drop.make(wasm_mod) @@
        Expression.Loop.make(
          wasm_mod,
          loop_label,
          Expression.Block.make(
            wasm_mod,
            gensym_label("MFor_loop_body"),
            List.concat([
              compiled_cond,
              [
                Expression.Block.make(
                  wasm_mod,
                  continue_label,
                  [Expression.Drop.make(wasm_mod, compiled_body)],
                ),
              ],
              compiled_inc,
              [
                Expression.Break.make(
                  wasm_mod,
                  loop_label,
                  Expression.Null.make(),
                  Expression.Null.make(),
                ),
              ],
            ]),
          ),
        ),
        Expression.Const.make(wasm_mod, const_void()),
      ],
    );
  | MContinue =>
    let (continue_label, _) = List.hd(loop_stack^);
    Expression.Break.make(
      wasm_mod,
      continue_label,
      Expression.Null.make(),
      Expression.Null.make(),
    );
  | MBreak =>
    let (_, block_label) = List.hd(loop_stack^);
    Expression.Break.make(
      wasm_mod,
      block_label,
      Expression.Null.make(),
      Expression.Const.make(wasm_mod, const_void()),
    );
  | MError(err, args) => call_error_handler(wasm_mod, env, err, args)
  | MReturn(value) =>
    let value =
      Option.fold(
        ~none=Expression.Const.make(wasm_mod, const_void()),
        ~some=compile_imm(wasm_mod, env),
        value,
      );
    Expression.Return.make(wasm_mod, value);
  | MCleanup(Some(value), items) =>
    Expression.Tuple_extract.make(
      wasm_mod,
      Expression.Tuple_make.make(
        wasm_mod,
        [
          compile_instr(wasm_mod, env, value),
          Expression.Block.make(
            wasm_mod,
            gensym_label("cleanup"),
            List.fold_left(
              (acc, item) => {
                [
                  Expression.Drop.make(
                    wasm_mod,
                    call_decref(
                      wasm_mod,
                      env,
                      compile_imm(wasm_mod, env, item),
                    ),
                  ),
                  ...acc,
                ]
              },
              [Expression.Const.make(wasm_mod, const_void())],
              items,
            ),
          ),
        ],
      ),
      0,
    )
  | MCleanup(None, items) =>
    Expression.Block.make(
      wasm_mod,
      gensym_label("cleanup"),
      List.rev_map(
        item => {
          Expression.Drop.make(
            wasm_mod,
            call_decref(wasm_mod, env, compile_imm(wasm_mod, env, item)),
          )
        },
        items,
      ),
    )
  | MArityOp(_) => failwith("NYI: (compile_instr): MArityOp")
  | MTagOp(_) => failwith("NYI: (compile_instr): MTagOp")
  };

let compile_function =
    (
      ~name=?,
      ~preamble=?,
      wasm_mod,
      env,
      {
        id,
        args,
        return_type,
        stack_size,
        closure,
        body: body_instrs,
        func_loc,
      } as func,
    ) => {
  sources := [];
  set_current_function(func);
  let arity = List.length(args);
  let func_name =
    switch (name) {
    | Some(name) => name
    | None => Ident.unique_name(id)
    };
  let body_env = {
    ...env,
    num_args: arity,
    num_closure_args: Option.value(~default=0, closure),
    stack_size,
  };
  let inner_body =
    switch (preamble) {
    | Some(preamble) =>
      Expression.Block.make(
        wasm_mod,
        gensym_label("compile_function_preamble"),
        [preamble, compile_block(wasm_mod, body_env, body_instrs)],
      )
    | None => compile_block(wasm_mod, body_env, body_instrs)
    };
  let body =
    switch (closure) {
    | Some(num_closure_args) =>
      Expression.Block.make(
        wasm_mod,
        gensym_label("closure_elements"),
        List.rev_append(
          List.init(num_closure_args, closure_slot =>
            compile_bind(
              ~action=
                BindSet({
                  value:
                    call_incref(
                      wasm_mod,
                      body_env,
                      load(
                        ~offset=4 * (4 + closure_slot),
                        wasm_mod,
                        Expression.Local_get.make(wasm_mod, 0, Type.int32),
                      ),
                    ),
                  initial: true,
                }),
              wasm_mod,
              body_env,
              MClosureBind(Int32.of_int(closure_slot)),
            )
          ),
          [compile_block(wasm_mod, body_env, body_instrs)],
        ),
      )
    | None => inner_body
    };
  let locals =
    [
      Array.make(body_env.num_closure_args, Type.int32),
      swap_slots,
      Array.make(stack_size.stack_size_ptr, Type.int32),
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
      Type.create @@ Array.of_list @@ List.map(wasm_type, return_type),
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

let compute_table_size = (env, {function_table_elements}) => {
  List.length(function_table_elements);
};

let compile_imports = (wasm_mod, env, {imports}) => {
  let compile_module_name = name =>
    fun
    | MImportWasm => name
    | MImportGrain => "GRAIN$MODULE$" ++ name;

  let compile_import_name = (name, kind, ty) =>
    switch (kind, ty) {
    | (MImportGrain, MGlobalImport(_)) => "GRAIN$EXPORT$" ++ name
    | _ => name
    };

  let compile_import = ({mimp_id, mimp_mod, mimp_name, mimp_type, mimp_kind}) => {
    let module_name = compile_module_name(mimp_mod, mimp_kind);
    let item_name = compile_import_name(mimp_name, mimp_kind, mimp_type);
    let internal_name =
      switch (mimp_kind) {
      | MImportGrain => get_grain_imported_name(mimp_mod, mimp_id)
      | MImportWasm =>
        get_wasm_imported_name(~runtime_import=false, mimp_mod, mimp_id)
      };
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
        Type.create @@ Array.of_list @@ List.map(wasm_type, l);
      Import.add_function_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        proc_list(args),
        proc_list(ret),
      );
    | (_, MGlobalImport(typ, mut)) =>
      let typ = wasm_type(typ);
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

  let imports =
    List.append(
      List.filter(({mimp_used}) => mimp_used, env.required_imports),
      imports,
    );

  List.iter(compile_import, imports);
  Import.add_table_import(
    wasm_mod,
    grain_global_function_table,
    grain_env_mod,
    grain_global_function_table,
  );
};

let compile_exports = (wasm_mod, env, {imports, exports, globals}) => {
  let compile_export = (i, export) => {
    switch (export) {
    | WasmGlobalExport({ex_global_internal_name, ex_global_name}) =>
      let ex_global_name = "GRAIN$EXPORT$" ++ ex_global_name;
      ignore @@
      Export.add_global_export(
        wasm_mod,
        ex_global_internal_name,
        ex_global_name,
      );
    | WasmFunctionExport({ex_function_internal_name, ex_function_name}) =>
      ignore @@
      Export.add_function_export(
        wasm_mod,
        ex_function_internal_name,
        ex_function_name,
      )
    };
  };

  let exports = {
    module StringSet = Set.Make(String);
    let exported_globals = ref(StringSet.empty);
    let exported_functions = ref(StringSet.empty);
    /* Exports are already reversed, so keeping the first of any name is the correct behavior. */
    List.filter(
      fun
      | WasmGlobalExport({ex_global_name}) =>
        if (StringSet.mem(ex_global_name, exported_globals^)) {
          false;
        } else {
          exported_globals := StringSet.add(ex_global_name, exported_globals^);
          true;
        }
      | WasmFunctionExport({ex_function_name}) =>
        if (StringSet.mem(ex_function_name, exported_functions^)) {
          false;
        } else {
          exported_functions :=
            StringSet.add(ex_function_name, exported_functions^);
          true;
        },
      exports,
    );
  };
  List.iteri(compile_export, exports);
  ignore @@ Export.add_function_export(wasm_mod, grain_main, grain_main);
  if (! Config.use_start_section^) {
    ignore @@ Export.add_function_export(wasm_mod, grain_start, grain_start);
  };
  ignore @@
  Export.add_global_export(
    wasm_mod,
    Ident.name(table_size),
    Ident.name(table_size),
  );
};

let compile_tables = (wasm_mod, env, {function_table_elements}) => {
  Table.add_active_element_segment(
    wasm_mod,
    grain_global_function_table,
    "elem",
    function_table_elements,
    Expression.Global_get.make(
      wasm_mod,
      get_wasm_imported_name(grain_env_mod, reloc_base),
      Type.int32,
    ),
  );
};

let compile_globals = (wasm_mod, env, {globals} as prog) => {
  let initial_value =
    fun
    | Types.Managed
    | Types.Unmanaged(WasmI32) => const_int32(0)
    | Types.Unmanaged(WasmI64) => const_int64(0)
    | Types.Unmanaged(WasmF32) => const_float32(0.)
    | Types.Unmanaged(WasmF64) => const_float64(0.);
  List.iter(
    ((id, ty)) =>
      ignore @@
      Global.add_global(
        wasm_mod,
        Ident.unique_name(id),
        wasm_type(ty),
        true,
        Expression.Const.make(wasm_mod, initial_value(ty)),
      ),
    globals,
  );
  ignore @@
  Global.add_global(
    wasm_mod,
    Ident.name(table_size),
    Type.int32,
    false,
    Expression.Const.make(
      wasm_mod,
      const_int32(compute_table_size(env, prog)),
    ),
  );
};

let compile_main = (wasm_mod, env, prog) => {
  ignore @@
  compile_function(
    ~name=grain_main,
    wasm_mod,
    env,
    {
      id: Ident.create(grain_main),
      name: Some(grain_main),
      args: [],
      return_type: [Types.Unmanaged(WasmI32)],
      closure: None,
      body: prog.main_body,
      stack_size: prog.main_body_stack_size,
      attrs: [],
      func_loc: Grain_parsing.Location.dummy_loc,
    },
  );
  if (! Config.use_start_section^) {
    ignore @@
    Function.add_function(
      wasm_mod,
      grain_start,
      Type.none,
      Type.none,
      [||],
      Expression.Drop.make(
        wasm_mod,
        Expression.Call.make(wasm_mod, grain_main, [], Type.int32),
      ),
    );
  };
};

let compile_functions = (wasm_mod, env, {functions} as prog) => {
  let handle_attrs = ({attrs} as func) =>
    if (List.exists(
          ({Grain_parsing.Location.txt}) => txt == Typedtree.Disable_gc,
          attrs,
        )) {
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

let prepare = env => {
  let required_imports =
    if (Env.is_runtime_mode()) {
      List.concat([required_global_imports, required_function_imports]);
    } else {
      runtime_imports;
    };
  {...env, required_imports};
};

let compile_wasm_module = (~env=?, ~name=?, prog) => {
  reset();
  let env =
    switch (env) {
    | None => init_codegen_env(name)
    | Some(e) => e
    };
  let env = prepare(env);
  let wasm_mod = Module.create();
  if (Config.source_map^) {
    ignore @@
    Module.add_debug_info_filename(
      wasm_mod,
      Filepath.String.basename(Option.get(name)),
    );
  };
  let default_features = [
    Module.Feature.mvp,
    Module.Feature.multivalue,
    Module.Feature.tail_call,
    Module.Feature.sign_ext,
    Module.Feature.mutable_globals,
  ];
  let features =
    if (Config.bulk_memory^) {
      [Module.Feature.bulk_memory, ...default_features];
    } else {
      default_features;
    };
  let _ = Module.set_features(wasm_mod, features);
  // we set low_memory_unused := true if and only if the user has not specified a memory base.
  // This is because in many use cases in which this is specified (e.g. wasm4), users
  // will expect the static region of memory below the heap base to all be available.
  let _ =
    Settings.set_low_memory_unused(
      Option.is_none(Grain_utils.Config.memory_base^),
    );
  let _ =
    Memory.set_memory(
      wasm_mod,
      0,
      Memory.unlimited,
      "memory",
      [],
      false,
      false,
      grain_memory,
    );

  let compile_all = () => {
    ignore @@ compile_globals(wasm_mod, env, prog);
    ignore @@ compile_functions(wasm_mod, env, prog);
    ignore @@ compile_exports(wasm_mod, env, prog);
    ignore @@ compile_imports(wasm_mod, env, prog);
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

  if (compiling_wasi_polyfill(name)) {
    write_universal_exports(
      wasm_mod,
      prog.signature,
      get_exported_names(wasm_mod),
    );
  };

  let serialized_cmi = Cmi_format.serialize_cmi(prog.signature);
  Module.add_custom_section(
    wasm_mod,
    "cmi",
    Bytes.to_string(serialized_cmi),
  );
  validate_module(~name?, wasm_mod);

  switch (Config.profile^) {
  | Some(Release) => Optimize_mod.optimize(wasm_mod)
  | None => ()
  };
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
