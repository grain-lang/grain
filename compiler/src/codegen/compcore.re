open Grain_typed;
open Mashtree;
open Value_tags;
open Binaryen;
open Concatlist; /* NOTE: This import shadows (@) and introduces (@+) and (+@) */
open Grain_utils;

/* [TODO] Should probably be a config variable */
let memory_tracing_enabled = false;

/** Environment */

type codegen_env = {
  num_args: int,
  func_offset: int,
  global_offset: int,
  stack_size: int,
  import_global_offset: int,
  import_func_offset: int,
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
let console_mod = Ident.create_persistent("console");
let check_memory_ident = Ident.create_persistent("checkMemory");
let throw_error_ident = Ident.create_persistent("throwError");
let log_ident = Ident.create_persistent("log");
let malloc_ident = Ident.create_persistent("malloc");
let incref_ident = Ident.create_persistent("incRef");
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
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_array_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_tuple_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_box_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_backpatch_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_swap_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_arg_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_local_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_global_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_closure_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_cleanup_locals_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_array_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_tuple_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_box_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_swap_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_arg_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_local_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_global_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_closure_bind_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_cleanup_locals_ident,
        mimp_type:
          [@implicit_arity] MFuncImport([I32Type, I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_cleanup_globals_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_drop_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
    ];
  } else {
    [
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_ignore_zeros_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
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
        mimp_mod: console_mod,
        mimp_name: log_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: check_memory_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], []),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: malloc_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: incref64_ident,
        mimp_type: [@implicit_arity] MFuncImport([I64Type], [I64Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], [I32Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: decref64_ident,
        mimp_type: [@implicit_arity] MFuncImport([I64Type], [I64Type]), /* Returns same pointer as argument */
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: console_mod,
        mimp_name: tracepoint_ident,
        mimp_type: [@implicit_arity] MFuncImport([I32Type], []),
        mimp_kind: MImportWasm,
        mimp_setup: MSetupNone,
      },
      {
        mimp_mod: runtime_mod,
        mimp_name: throw_error_ident,
        mimp_type:
          [@implicit_arity]
          MFuncImport(
            List.init(Runtime_errors.max_arity + 1, _ => I32Type),
            [],
          ),
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
  func_offset: 0,
  global_offset: 2,
  stack_size: 0,
  import_global_offset: 0,
  import_func_offset: 0,
  import_offset: 0,
  backpatches: ref([]),
  imported_funcs: Ident.empty,
  imported_globals: Ident.empty,
};

let encoded_int32 = n => n * 2;

let const_int32 = n => Literal.int32(Int32.of_int(n));
let const_int64 = n => Literal.int64(Int64.of_int(n));
let const_float32 = n => Literal.float32(n);
let const_float64 = n => Literal.float64(n);

/* These are like the above 'const' functions, but take inputs
   of the underlying types instead */
let wrap_int32 = n => Literal.int32(n);
let wrap_int64 = n => Literal.int64(n);
let wrap_float32 = n => Literal.float32(n);
let wrap_float64 = n => Literal.float64(n);

let grain_number_max = 0x3fffffff;
let grain_number_min = (-0x3fffffff); // 0xC0000001

/** Constant compilation */

let rec compile_const = (c): Literal.t => {
  let identity: 'a. 'a => 'a = x => x;
  let conv_int32 = Int32.(mul(of_int(2)));
  let conv_int64 = Int64.(mul(of_int(2)));
  let conv_float32 = identity;
  let conv_float64 = identity;
  switch (c) {
  | MConstLiteral(MConstLiteral(_) as c) => compile_const(c)
  | MConstI32(n) => Literal.int32(conv_int32(n))
  | MConstI64(n) => Literal.int64(conv_int64(n))
  | MConstF32(n) => Literal.float32(conv_float32(n))
  | MConstF64(n) => Literal.float64(conv_float64(n))
  | MConstLiteral(MConstI32(n)) => Literal.int32(n)
  | MConstLiteral(MConstI64(n)) => Literal.int64(n)
  | MConstLiteral(MConstF32(n)) => Literal.float32(n)
  | MConstLiteral(MConstF64(n)) => Literal.float64(n)
  };
};

/* Translate constants to WASM */
let const_true = () => compile_const(const_true);
let const_false = () => compile_const(const_false);
let const_void = () => compile_const(const_void);

/* WebAssembly helpers */

/* These instructions get helpers due to their verbosity */
let store =
    (~ty=Type.int32, ~align=2, ~offset=0, ~sz=None, wasm_mod, ptr, arg) => {
  let sz =
    Option.value(
      ~default=
        switch (ty) {
        | a when a === Type.int32 || a === Type.float32 => 4
        | a when a === Type.int64 || a === Type.float64 => 8
        | _ => failwith("sizing not defined for this type")
        },
      sz,
    );
  Expression.store(wasm_mod, sz, offset, align, ptr, arg, ty);
};

let load = (~ty=Type.int32, ~align=2, ~offset=0, ~sz=None, wasm_mod, ptr) => {
  let sz =
    Option.value(
      ~default=
        switch (ty) {
        | a when a === Type.int32 || a === Type.float32 => 4
        | a when a === Type.int64 || a === Type.float64 => 8
        | _ => failwith("sizing not defined for this type")
        },
      sz,
    );
  Expression.load(wasm_mod, sz, offset, align, ty, ptr);
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
let call_console_log = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, log_ident),
    args,
    Type.int32,
  );

let call_malloc = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, malloc_ident),
    args,
    Type.int32,
  );
let call_incref = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, incref_ident),
    args,
    Type.int32,
  );
let call_incref_adt = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(runtime_mod, incref_adt_ident, incref_ident),
    args,
    Type.int32,
  );
let call_incref_array = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_array_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_tuple = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_tuple_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_box = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(runtime_mod, incref_box_ident, incref_ident),
    args,
    Type.int32,
  );
let call_incref_backpatch = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_backpatch_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_swap_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_swap_bind_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_arg_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_arg_bind_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_local_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_local_bind_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_global_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_global_bind_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_closure_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_closure_bind_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_cleanup_locals = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      incref_cleanup_locals_ident,
      incref_ident,
    ),
    args,
    Type.int32,
  );
let call_incref_64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, incref64_ident),
    args,
    Type.int32,
  );
let call_decref = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, decref_ident),
    args,
    Type.int32,
  );
let call_decref_64 = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    get_imported_name(runtime_mod, decref64_ident),
    args,
    Type.int32,
  );
let call_decref_array = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_array_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_tuple = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_tuple_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_box = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(runtime_mod, decref_box_ident, decref_ident),
    args,
    Type.int32,
  );
let call_decref_swap_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_swap_bind_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_arg_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_arg_bind_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_local_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_local_bind_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_global_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_global_bind_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_closure_bind = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_closure_bind_ident,
      decref_ident,
    ),
    args,
    Type.int32,
  );
let call_decref_cleanup_locals = (wasm_mod, env, args) =>
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
let decref_cleanup_globals_name =
  name_of_memory_tracing_func(
    runtime_mod,
    decref_cleanup_globals_ident,
    decref_ignore_zeros_ident,
  );
let call_decref_cleanup_globals = (wasm_mod, env, args) =>
  Expression.call(wasm_mod, decref_cleanup_globals_name, args, Type.int32);
let call_decref_drop = (wasm_mod, env, args) =>
  Expression.call(
    wasm_mod,
    name_of_memory_tracing_func(
      runtime_mod,
      decref_drop_ident,
      decref_ignore_zeros_ident,
    ),
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

/** Untags the number at the top of the stack */

let untag_number = (wasm_mod, value) =>
  Expression.binary(
    wasm_mod,
    Op.shr_s_int32,
    value,
    Expression.const(wasm_mod, const_int32(1)),
  );

let untag = (wasm_mod, tag, value) =>
  Expression.binary(
    wasm_mod,
    Op.xor_int32,
    value,
    Expression.const(wasm_mod, const_int32(tag_val_of_tag_type(tag))),
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
      env.stack_size,
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
      ~ty as typ=Type.int32,
      ~skip_incref=false,
      ~skip_decref=false,
      wasm_mod: Module.t,
      env: codegen_env,
      b: binding,
    )
    : Expression.t => {
  let appropriate_incref = (env, arg) =>
    switch (typ) {
    | _ when skip_incref => arg /* This case is used for storing swap values that have freshly been heap-allocated. */
    | typ when typ === Type.int32 =>
      let args = [arg];
      switch (b) {
      | MArgBind(_) => call_incref_arg_bind(wasm_mod, env, args)
      | MLocalBind(_) => call_incref_local_bind(wasm_mod, env, args)
      | MSwapBind(_) => call_incref_swap_bind(wasm_mod, env, args)
      | MGlobalBind(_) => call_incref_global_bind(wasm_mod, env, args)
      | MClosureBind(_) => call_incref_closure_bind(wasm_mod, env, args)
      | _ => call_incref(wasm_mod, env, args)
      };
    | typ when typ === Type.int64 =>
      /* https://github.com/dcodeIO/webassembly/issues/26#issuecomment-410157370 */
      /* call_incref_64 env */
      arg
    | _ => failwith("appropriate_incref called with non-i32/i64 type")
    };

  let appropriate_decref = (env, arg) =>
    switch (typ) {
    | _ when skip_decref => arg /* This case is used for storing swap values that have freshly been heap-allocated. */
    | typ when typ === Type.int32 =>
      let args = [arg];
      switch (b) {
      | MArgBind(_) => call_decref_arg_bind(wasm_mod, env, args)
      | MLocalBind(_) => call_decref_local_bind(wasm_mod, env, args)
      | MSwapBind(_) => call_decref_swap_bind(wasm_mod, env, args)
      | MGlobalBind(_) => call_decref_global_bind(wasm_mod, env, args)
      | MClosureBind(_) => call_decref_closure_bind(wasm_mod, env, args)
      | _ => call_decref(wasm_mod, env, args)
      };
    | typ when typ === Type.int64 =>
      /* https://github.com/dcodeIO/webassembly/issues/26#issuecomment-410157370 */
      /* call_decref_64 env */
      arg
    | _ => failwith("appropriate_decref called with non-i32/i64 type")
    };

  switch (b) {
  | MArgBind(i) =>
    /* No adjustments are needed for argument bindings */
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
  | MLocalBind(i) =>
    /* Local bindings need to be offset to account for arguments and swap variables */
    let slot = env.num_args + Array.length(swap_slots) + Int32.to_int(i);
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
  | MSwapBind(i) =>
    /* Swap bindings need to be offset to account for arguments */
    let slot = env.num_args + Int32.to_int(i);
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
  | MGlobalBind(i) =>
    /* Global bindings need to be offset to account for any imports */
    let slot =
      Printf.sprintf("global_%d") @@ env.global_offset + Int32.to_int(i);
    switch (action) {
    | BindGet => Expression.global_get(wasm_mod, slot, typ)
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
      ~offset=4 * (3 + Int32.to_int(i)),
      wasm_mod,
      Expression.local_get(wasm_mod, 0, typ),
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
    Expression.global_get(wasm_mod, slot, typ);
  };
};

let safe_drop = (wasm_mod, env, arg) =>
  Expression.drop(wasm_mod, call_decref_drop(wasm_mod, env, [arg]));

let get_swap = (~ty as typ=Type.int32, wasm_mod, env, idx) =>
  switch (typ) {
  | typ when typ === Type.int32 =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset)),
    );
  | typ when typ === Type.int64 =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      ~ty=Type.int64,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset)),
    );
  | _ => raise(Not_found)
  };

let set_swap =
    (
      ~skip_incref=true,
      ~skip_decref=true,
      ~ty as typ=Type.int32,
      wasm_mod,
      env,
      idx,
      arg,
    ) =>
  switch (typ) {
  | typ when typ === Type.int32 =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet(arg),
      ~skip_incref,
      ~skip_decref,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset)),
    );
  | typ when typ === Type.int64 =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet(arg),
      ~skip_incref,
      ~skip_decref,
      ~ty=Type.int64,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset)),
    );
  | _ => raise(Not_found)
  };

let tee_swap =
    (
      ~ty as typ=Type.int32,
      ~skip_incref=true,
      ~skip_decref=true,
      wasm_mod,
      env,
      idx,
      arg,
    ) =>
  switch (typ) {
  | typ when typ === Type.int32 =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee(arg),
      ~skip_incref,
      ~skip_decref,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset)),
    );
  | typ when typ === Type.int64 =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee(arg),
      ~skip_incref,
      ~skip_decref,
      ~ty=Type.int64,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset)),
    );
  | _ => raise(Not_found)
  };

/* [TODO] This is going to need some sort of type argument as well...I think this could be indicative of a code smell */
let cleanup_locals = (wasm_mod, env: codegen_env, arg): Expression.t => {
  /* Do the following:
     - Move the current stack value into a designated return-value holder slot (maybe swap is fine)
     - Call incref() on the return value (to prevent premature free)
     - Call decref() on all locals (should include return value) */
  let decref_args = [get_swap(wasm_mod, env, 0)];
  let decref_args =
    if (memory_tracing_enabled) {
      List.append(
        decref_args,
        [Expression.const(wasm_mod, const_int32(-1))],
      );
    } else {
      decref_args;
    };
  Expression.block(wasm_mod, gensym_label("cleanup_locals")) @@
  Concatlist.list_of_t(
    singleton(
      Expression.drop(wasm_mod) @@
      call_incref_cleanup_locals(
        wasm_mod,
        env,
        [tee_swap(wasm_mod, env, 0, arg)],
      ),
    )
    @ cleanup_local_slot_instructions(wasm_mod, env)
    +@ [call_decref_cleanup_locals(wasm_mod, env, decref_args)],
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
      set_swap(~ty=Type.int64, wasm_mod, env, 0, arg),
      error_if_true(
        wasm_mod,
        env,
        Expression.binary(
          wasm_mod,
          Op.gt_s_int64,
          get_swap(~ty=Type.int64, wasm_mod, env, 0),
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
          get_swap(~ty=Type.int64, wasm_mod, env, 0),
          Expression.const(
            wasm_mod,
            const_int64(Int32.to_int(Int32.min_int)),
          ),
        ),
        OverflowError,
        [dummy_err_val, dummy_err_val],
      ),
      get_swap(~ty=Type.int64, wasm_mod, env, 0),
    ],
  );

let compile_tuple_op = (~is_box=false, wasm_mod, env, tup_imm, op) => {
  let tup = compile_imm(wasm_mod, env, tup_imm);
  switch (op) {
  | MTupleGet(idx) =>
    let idx_int = Int32.to_int(idx);
    /* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. */
    load(
      ~offset=4 * (idx_int + 1),
      wasm_mod,
      untag(wasm_mod, TupleTagType, tup),
    );
  | [@implicit_arity] MTupleSet(idx, imm) =>
    let idx_int = Int32.to_int(idx);
    let get_swap = get_swap(wasm_mod, env, 0);
    let set_swap = set_swap(wasm_mod, env, 0);
    Expression.block(
      wasm_mod,
      gensym_label("MTupleSet"),
      [
        set_swap(untag(wasm_mod, TupleTagType, tup)),
        store(
          ~offset=4 * (idx_int + 1),
          wasm_mod,
          get_swap,
          Expression.tuple_extract(
            wasm_mod,
            Expression.tuple_make(
              wasm_mod,
              [
                (if (is_box) {call_incref_box} else {call_incref_tuple})(
                  wasm_mod,
                  env,
                  [compile_imm(wasm_mod, env, imm)],
                ),
                (if (is_box) {call_decref_box} else {call_decref_tuple})(
                  wasm_mod,
                  env,
                  [load(~offset=4 * (idx_int + 1), wasm_mod, get_swap)],
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
      [@implicit_arity] MTupleSet(Int32.zero, imm),
    )
  };

let compile_array_op = (wasm_mod, env, arr_imm, op) => {
  let get_swap = n => get_swap(wasm_mod, env, n);
  let set_swap = n => set_swap(wasm_mod, env, n);
  let tee_swap = n => tee_swap(wasm_mod, env, n);
  let arr = compile_imm(wasm_mod, env, arr_imm);
  switch (op) {
  | MArrayGet(idx_imm) =>
    let idx = compile_imm(wasm_mod, env, idx_imm);
    let get_arr = () => get_swap(0);
    let get_idx = () => get_swap(1);
    /* Check that the index is in bounds */
    Expression.block(
      wasm_mod,
      gensym_label("MArrayGet"),
      [
        set_swap(0, arr),
        set_swap(1, idx),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.gt_s_int32,
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              load(
                ~offset=4,
                wasm_mod,
                untag(
                  wasm_mod,
                  GenericHeapType(Some(ArrayType)),
                  get_arr(),
                ),
              ),
              Expression.const(wasm_mod, const_int32(-2)),
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
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              load(
                ~offset=4,
                wasm_mod,
                untag(
                  wasm_mod,
                  GenericHeapType(Some(ArrayType)),
                  get_arr(),
                ),
              ),
              Expression.const(wasm_mod, const_int32(2)),
            ),
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
                  Expression.binary(
                    wasm_mod,
                    Op.shr_s_int32,
                    get_idx(),
                    Expression.const(wasm_mod, const_int32(1)),
                  ),
                  load(
                    ~offset=4,
                    wasm_mod,
                    untag(
                      wasm_mod,
                      GenericHeapType(Some(ArrayType)),
                      get_arr(),
                    ),
                  ),
                ),
                Expression.binary(
                  wasm_mod,
                  Op.shr_s_int32,
                  get_idx(),
                  Expression.const(wasm_mod, const_int32(1)),
                ),
              ),
              Expression.const(wasm_mod, const_int32(4)),
            ),
            untag(wasm_mod, GenericHeapType(Some(ArrayType)), get_arr()),
          ),
        ),
      ],
    );
  | MArrayLength =>
    Expression.binary(
      wasm_mod,
      Op.shl_int32,
      load(
        ~offset=4,
        wasm_mod,
        untag(wasm_mod, GenericHeapType(Some(ArrayType)), arr),
      ),
      Expression.const(wasm_mod, const_int32(1)),
    )
  | [@implicit_arity] MArraySet(idx_imm, val_imm) =>
    let idx = compile_imm(wasm_mod, env, idx_imm);
    let val_ = compile_imm(wasm_mod, env, val_imm);
    let get_arr = () => get_swap(0);
    let get_idx = () => get_swap(1);
    /* Check that the index is in bounds */
    Expression.block(
      wasm_mod,
      gensym_label("MArrayGet"),
      [
        set_swap(0, arr),
        set_swap(1, idx),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.gt_s_int32,
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              load(
                ~offset=4,
                wasm_mod,
                untag(
                  wasm_mod,
                  GenericHeapType(Some(ArrayType)),
                  get_arr(),
                ),
              ),
              Expression.const(wasm_mod, const_int32(-2)),
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
            Expression.binary(
              wasm_mod,
              Op.mul_int32,
              load(
                ~offset=4,
                wasm_mod,
                untag(
                  wasm_mod,
                  GenericHeapType(Some(ArrayType)),
                  get_arr(),
                ),
              ),
              Expression.const(wasm_mod, const_int32(2)),
            ),
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
                  Expression.binary(
                    wasm_mod,
                    Op.shr_s_int32,
                    get_idx(),
                    Expression.const(wasm_mod, const_int32(1)),
                  ),
                  load(
                    ~offset=4,
                    wasm_mod,
                    untag(
                      wasm_mod,
                      GenericHeapType(Some(ArrayType)),
                      get_arr(),
                    ),
                  ),
                ),
                Expression.binary(
                  wasm_mod,
                  Op.shr_s_int32,
                  get_idx(),
                  Expression.const(wasm_mod, const_int32(1)),
                ),
              ),
              Expression.const(wasm_mod, const_int32(4)),
            ),
            untag(wasm_mod, GenericHeapType(Some(ArrayType)), get_arr()),
          ),
          /* [TODO] decref the old item */
          call_incref(wasm_mod, env, [tee_swap(1, val_)]),
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
    load(
      ~offset=4 * (idx_int + 5),
      wasm_mod,
      untag(wasm_mod, GenericHeapType(Some(ADTType)), adt),
    );
  | MAdtGetModule =>
    load(
      ~offset=4,
      wasm_mod,
      untag(wasm_mod, GenericHeapType(Some(ADTType)), adt),
    )
  | MAdtGetTag =>
    load(
      ~offset=12,
      wasm_mod,
      untag(wasm_mod, GenericHeapType(Some(ADTType)), adt),
    )
  };
};

let compile_record_op = (wasm_mod, env, rec_imm, op) => {
  let record = compile_imm(wasm_mod, env, rec_imm);
  switch (op) {
  | MRecordGet(idx) =>
    let idx_int = Int32.to_int(idx);
    load(
      ~offset=4 * (idx_int + 4),
      wasm_mod,
      untag(wasm_mod, GenericHeapType(Some(RecordType)), record),
    );
  | MRecordSet(idx, arg_imm) =>
    let idx_int = Int32.to_int(idx);
    let arg = compile_imm(wasm_mod, env, arg_imm);
    Expression.block(
      wasm_mod,
      gensym_label("record_set"),
      [
        store(
          ~offset=4 * (idx_int + 4),
          wasm_mod,
          tee_swap(
            wasm_mod,
            env,
            0,
            untag(wasm_mod, GenericHeapType(Some(RecordType)), record),
          ),
          Expression.tuple_extract(
            wasm_mod,
            Expression.tuple_make(
              wasm_mod,
              [
                call_incref(
                  wasm_mod,
                  env,
                  [tee_swap(wasm_mod, env, 1, arg)],
                ),
                call_decref(
                  wasm_mod,
                  env,
                  [
                    load(
                      ~offset=4 * (idx_int + 4),
                      wasm_mod,
                      get_swap(wasm_mod, env, 0),
                    ),
                  ],
                ),
              ],
            ),
            0,
          ),
        ),
        get_swap(wasm_mod, env, 1),
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
  let num_words = () => compile_imm(wasm_mod, env, num_words);
  /*
    Normally, this would be:
      Untag num_words
      Find remainder by 4
      Untag num_words
      Add (to get rounded value)
      Multiply by 4 (to get bytes)

    Instead, we do:
      Find remainder by 8
      Add (to get rounded value)
      Multiply by 2 (to get bytes)

    Proof:
      This is an exercise left to the reader.
   */
  if (additional_words == 0) {
    call_malloc(
      wasm_mod,
      env,
      [
        Expression.binary(
          wasm_mod,
          Op.mul_int32,
          Expression.binary(
            wasm_mod,
            Op.add_int32,
            num_words(),
            Expression.binary(
              wasm_mod,
              Op.rem_s_int32,
              num_words(),
              Expression.const(wasm_mod, const_int32(8)),
            ),
          ),
          Expression.const(wasm_mod, const_int32(2)),
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
          Op.mul_int32,
          Expression.binary(
            wasm_mod,
            Op.add_int32,
            Expression.binary(
              wasm_mod,
              Op.add_int32,
              num_words(),
              Expression.binary(
                wasm_mod,
                Op.rem_s_int32,
                Expression.binary(
                  wasm_mod,
                  Op.add_int32,
                  num_words(),
                  Expression.const(
                    wasm_mod,
                    const_int32(additional_words * 2),
                  ),
                ),
                Expression.const(wasm_mod, const_int32(8)),
              ),
            ),
            Expression.const(wasm_mod, const_int32(additional_words * 2)),
          ),
          Expression.const(wasm_mod, const_int32(2)),
        ),
      ],
    );
  };
};

let heap_check_memory = (wasm_mod, env, num_words: int) => {
  let words_to_allocate = round_allocation_size(num_words);
  call_runtime_check_memory(
    wasm_mod,
    env,
    [Expression.const(wasm_mod, const_int32(4 * words_to_allocate))],
  );
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

let call_lambda = (wasm_mod, env, func, args) => {
  let compiled_func = () => compile_imm(wasm_mod, env, func);
  let compiled_args = List.map(compile_imm(wasm_mod, env), args);
  let untagged_fn = () => untag(wasm_mod, LambdaTagType, compiled_func());
  Expression.call_indirect(
    wasm_mod,
    load(~offset=4, wasm_mod, untagged_fn()),
    [untagged_fn(), ...compiled_args],
    Type.create @@ Array.make(1 + List.length(args), Type.int32),
    Type.int32,
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
    List.concat([
      preamble,
      elts,
      [
        tee_swap(
          Expression.binary(
            wasm_mod,
            Op.or_int32,
            get_swap(),
            Expression.const(
              wasm_mod,
              const_int32 @@
              tag_val_of_tag_type(GenericHeapType(Some(StringType))),
            ),
          ),
        ),
      ],
    ]),
  );
};

let allocate_int32 = (wasm_mod, env, i) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(wasm_mod, env, 0);
  Expression.block(
    wasm_mod,
    gensym_label("allocate_int32"),
    [
      store(
        ~offset=0,
        wasm_mod,
        tee_swap(heap_allocate(wasm_mod, env, 2)),
        Expression.const(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(Int32Type)),
        ),
      ),
      store(
        ~offset=4,
        wasm_mod,
        get_swap(),
        Expression.const(wasm_mod, wrap_int32(i)),
      ),
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_swap(),
        Expression.const(
          wasm_mod,
          const_int32 @@
          tag_val_of_tag_type(GenericHeapType(Some(Int32Type))),
        ),
      ),
    ],
  );
};

let allocate_int64 = (wasm_mod, env, i) => {
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(wasm_mod, env, 0);
  Expression.block(
    wasm_mod,
    gensym_label("allocate_int64"),
    [
      store(
        ~offset=0,
        wasm_mod,
        tee_swap(heap_allocate(wasm_mod, env, 3)),
        Expression.const(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(Int64Type)),
        ),
      ),
      store(
        ~ty=Type.int64,
        ~offset=4,
        wasm_mod,
        get_swap(),
        Expression.const(wasm_mod, wrap_int64(i)),
      ),
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_swap(),
        Expression.const(
          wasm_mod,
          const_int32 @@
          tag_val_of_tag_type(GenericHeapType(Some(Int64Type))),
        ),
      ),
    ],
  );
};

/* Store an int64 */
let allocate_int64_imm = (wasm_mod, env, i) => {
  let get_swap64 = () => get_swap(~ty=Type.int64, wasm_mod, env, 0);
  let set_swap64 = set_swap(~ty=Type.int64, wasm_mod, env, 0);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(wasm_mod, env, 0);
  Expression.block(
    wasm_mod,
    gensym_label("allocate_int64_imm"),
    [
      set_swap64(i),
      store(
        ~offset=0,
        wasm_mod,
        tee_swap(heap_allocate(wasm_mod, env, 3)),
        Expression.const(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(Int64Type)),
        ),
      ),
      store(~ty=Type.int64, ~offset=4, wasm_mod, get_swap(), get_swap64()),
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_swap(),
        Expression.const(
          wasm_mod,
          const_int32 @@
          tag_val_of_tag_type(GenericHeapType(Some(Int64Type))),
        ),
      ),
    ],
  );
};

let allocate_closure =
    (wasm_mod, env, ~lambda=?, {func_idx, arity, variables} as closure_data) => {
  let num_free_vars = List.length(variables);
  let closure_size = num_free_vars + 3;
  let get_swap = () => get_swap(wasm_mod, env, 0);
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
  Expression.block(
    wasm_mod,
    gensym_label("allocate_closure"),
    [
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
        Expression.const(wasm_mod, wrap_int32(arity)),
      ),
      store(
        ~offset=4,
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
          Expression.const(
            wasm_mod,
            wrap_int32(Int32.(add(func_idx, of_int(env.func_offset)))),
          ),
        ),
      ),
      store(
        ~offset=8,
        wasm_mod,
        get_swap(),
        Expression.const(wasm_mod, const_int32(num_free_vars)),
      ),
      tee_swap(
        ~skip_incref=true,
        ~skip_decref=true,
        wasm_mod,
        env,
        0,
        Expression.binary(
          wasm_mod,
          Op.or_int32,
          get_swap(),
          Expression.const(
            wasm_mod,
            const_int32 @@ tag_val_of_tag_type(LambdaTagType),
          ),
        ),
      ),
    ],
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
      call_incref_adt(wasm_mod, env, [compile_imm(wasm_mod, env, elt)]),
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
  let postamble = [
    tee_swap(
      ~skip_incref=true,
      ~skip_decref=true,
      wasm_mod,
      env,
      0,
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_swap(),
        Expression.const(
          wasm_mod,
          const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(ADTType))),
        ),
      ),
    ),
  ];
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
      ~offset=4 * (idx + 1),
      wasm_mod,
      get_swap(),
      (if (is_box) {call_incref_box} else {call_incref_tuple})(
        wasm_mod,
        env,
        [compile_imm(wasm_mod, env, elt)],
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
        heap_allocate(wasm_mod, env, num_elts + 1),
      ),
      Expression.const(wasm_mod, const_int32(num_elts)),
    ),
  ];
  let compiled_elts = List.mapi(compile_elt, elts);
  let postamble = [
    tee_swap(
      ~skip_incref=true,
      ~skip_decref=true,
      wasm_mod,
      env,
      0,
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_swap(),
        Expression.const(
          wasm_mod,
          const_int32 @@ tag_val_of_tag_type(TupleTagType),
        ),
      ),
    ),
  ];
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
      call_incref_array(wasm_mod, env, [compile_imm(wasm_mod, env, elt)]),
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
  let postamble = [
    Expression.binary(
      wasm_mod,
      Op.or_int32,
      get_swap(),
      Expression.const(
        wasm_mod,
        const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(ArrayType))),
      ),
    ),
  ];
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
          Expression.const(wasm_mod, const_int32(0)),
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
        Expression.binary(
          wasm_mod,
          Op.shr_s_int32,
          compiled_num_elts(),
          Expression.const(wasm_mod, const_int32(1)),
        ),
      ),
      set_loop_counter(Expression.const(wasm_mod, const_int32(0))),
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
                      get_loop_counter(),
                      Expression.const(wasm_mod, const_int32(2)),
                    ),
                  ),
                  elt,
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
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_arr_addr(),
        Expression.const(
          wasm_mod,
          const_int32 @@
          tag_val_of_tag_type(GenericHeapType(Some(ArrayType))),
        ),
      ),
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
    gensym_label("allocate_array_n"),
    [
      error_if_true(
        wasm_mod,
        env,
        Expression.binary(
          wasm_mod,
          Op.lt_s_int32,
          compiled_num_elts(),
          Expression.const(wasm_mod, const_int32(0)),
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
        Expression.binary(
          wasm_mod,
          Op.shr_s_int32,
          compiled_num_elts(),
          Expression.const(wasm_mod, const_int32(1)),
        ),
      ),
      set_loop_counter(Expression.const(wasm_mod, const_int32(0))),
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
                      get_loop_counter(),
                      Expression.const(wasm_mod, const_int32(2)),
                    ),
                  ),
                  Expression.call_indirect(
                    wasm_mod,
                    load(
                      ~offset=4,
                      wasm_mod,
                      untag(wasm_mod, LambdaTagType, compiled_func()),
                    ),
                    [
                      untag(wasm_mod, LambdaTagType, compiled_func()),
                      get_loop_counter(),
                    ],
                    Type.create([|Type.int32, Type.int32|]),
                    Type.int32,
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
      Expression.binary(
        wasm_mod,
        Op.or_int32,
        get_arr_addr(),
        Expression.const(
          wasm_mod,
          const_int32 @@
          tag_val_of_tag_type(GenericHeapType(Some(ArrayType))),
        ),
      ),
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
      compile_imm(wasm_mod, env, elt),
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
  let postamble = [
    Expression.binary(
      wasm_mod,
      Op.or_int32,
      get_swap(),
      Expression.const(
        wasm_mod,
        const_int32 @@
        tag_val_of_tag_type(GenericHeapType(Some(RecordType))),
      ),
    ),
  ];
  Expression.block(
    wasm_mod,
    gensym_label("allocate_record"),
    List.concat([preamble, compiled_elts, postamble]),
  );
};

let compile_prim1 = (wasm_mod, env, p1, arg): Expression.t => {
  let compiled_arg = compile_imm(wasm_mod, env, arg);
  let get_swap_i64 = () => get_swap(~ty=Type.int64, wasm_mod, env, 0);
  let tee_swap_i64 = tee_swap(~ty=Type.int64, wasm_mod, env, 0);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let tee_swap = tee_swap(wasm_mod, env, 0);
  /* TODO: Overflow checks? */
  switch (p1) {
  | Incr =>
    Expression.binary(
      wasm_mod,
      Op.add_int32,
      compiled_arg,
      Expression.const(wasm_mod, encoded_const_int32(1)),
    )
  | Decr =>
    Expression.binary(
      wasm_mod,
      Op.sub_int32,
      compiled_arg,
      Expression.const(wasm_mod, encoded_const_int32(1)),
    )
  | Not =>
    Expression.binary(
      wasm_mod,
      Op.xor_int32,
      compiled_arg,
      Expression.const(wasm_mod, const_int32(2147483648)),
    )
  | Ignore =>
    Expression.block(
      wasm_mod,
      gensym_label("Ignore"),
      [
        Expression.drop(
          wasm_mod,
          call_decref_drop(wasm_mod, env, [compiled_arg]),
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
  | Int64FromNumber =>
    Expression.block(
      wasm_mod,
      gensym_label("Int64FromNumber"),
      [
        store(
          ~offset=0,
          wasm_mod,
          tee_swap(heap_allocate(wasm_mod, env, 3)),
          Expression.const(
            wasm_mod,
            const_int32(tag_val_of_heap_tag_type(Int64Type)),
          ),
        ),
        store(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          get_swap(),
          Expression.unary(
            wasm_mod,
            Op.extend_s_int32,
            untag_number(wasm_mod, compiled_arg),
          ),
        ),
        Expression.binary(
          wasm_mod,
          Op.or_int32,
          get_swap(),
          Expression.const(
            wasm_mod,
            const_int32 @@
            tag_val_of_tag_type(GenericHeapType(Some(Int64Type))),
          ),
        ),
      ],
    )
  | Int64ToNumber =>
    Expression.block(
      wasm_mod,
      gensym_label("Int64ToNumber"),
      [
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.gt_s_int64,
            tee_swap_i64(
              load(
                ~ty=Type.int64,
                ~offset=4,
                wasm_mod,
                untag(
                  wasm_mod,
                  GenericHeapType(Some(Int64Type)),
                  compiled_arg,
                ),
              ),
            ),
            Expression.const(wasm_mod, const_int64(grain_number_max)),
          ),
          OverflowError,
          [],
        ),
        error_if_true(
          wasm_mod,
          env,
          Expression.binary(
            wasm_mod,
            Op.lt_s_int64,
            get_swap_i64(),
            Expression.const(wasm_mod, const_int64(grain_number_min)),
          ),
          OverflowError,
          [],
        ),
        Expression.binary(
          wasm_mod,
          Op.shl_int32,
          Expression.unary(wasm_mod, Op.wrap_int64, get_swap_i64()),
          Expression.const(wasm_mod, const_int32(1)),
        ),
      ],
    )
  | Int64Lnot =>
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.sub_int64,
        /* 2's complement */
        Expression.const(wasm_mod, const_int64(-1)),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(wasm_mod, GenericHeapType(Some(Int64Type)), compiled_arg),
        ),
      ),
    )
  | Box => failwith("Unreachable case; should never get here: Box")
  | Unbox => failwith("Unreachable case; should never get here: Unbox")
  };
};

let compile_prim2 = (wasm_mod, env: codegen_env, p2, arg1, arg2): Expression.t => {
  let compiled_arg1 = () => compile_imm(wasm_mod, env, arg1);
  let compiled_arg2 = () => compile_imm(wasm_mod, env, arg2);
  let swap_get = () => get_swap(wasm_mod, env, 0);
  let swap_tee = tee_swap(wasm_mod, env, 0);
  let overflow_safe = arg =>
    Expression.unary(
      wasm_mod,
      Op.wrap_int64,
      check_overflow(wasm_mod, env, arg),
    );

  switch (p2) {
  | Plus =>
    overflow_safe @@
    Expression.binary(
      wasm_mod,
      Op.add_int64,
      Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg1()),
      Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg2()),
    )
  | Minus =>
    overflow_safe @@
    Expression.binary(
      wasm_mod,
      Op.sub_int64,
      Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg1()),
      Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg2()),
    )
  | Times =>
    /* Untag one of the numbers:
          ((a * 2) / 2) * (b * 2) = (a * b) * 2
       */
    overflow_safe @@
    Expression.binary(
      wasm_mod,
      Op.mul_int64,
      Expression.unary(
        wasm_mod,
        Op.extend_s_int32,
        untag_number(wasm_mod, compiled_arg1()),
      ),
      Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg2()),
    )
  | Divide =>
    /*
     While (2a) / b = 2(a/b), we can't just untag b since b could be a multiple of 2,
           yielding an odd (untagged) result.
           Instead, perform the division and retag after:
           (2a / 2b) * 2 = (a / b) * 2
        */
    overflow_safe @@
    Expression.block(
      wasm_mod,
      gensym_label("Divide"),
      [
        error_if_true(
          wasm_mod,
          env,
          Expression.unary(wasm_mod, Op.eq_z_int32, compiled_arg2()),
          DivisionByZeroError,
          [],
        ),
        Expression.binary(
          wasm_mod,
          Op.mul_int64,
          Expression.binary(
            wasm_mod,
            Op.div_s_int64,
            Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg1()),
            Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg2()),
          ),
          Expression.const(wasm_mod, const_int64(2)),
        ),
      ],
    )
  | Mod =>
    /* Mod is not commutative, so untag everything and retag at the end */
    overflow_safe @@
    Expression.block(
      wasm_mod,
      gensym_label("Mod"),
      [
        error_if_true(
          wasm_mod,
          env,
          Expression.unary(wasm_mod, Op.eq_z_int32, compiled_arg2()),
          ModuloByZeroError,
          [],
        ),
        set_swap(
          ~ty=Type.int64,
          wasm_mod,
          env,
          0,
          Expression.binary(
            wasm_mod,
            Op.mul_int64,
            Expression.binary(
              wasm_mod,
              Op.rem_s_int64,
              Expression.unary(
                wasm_mod,
                Op.extend_s_int32,
                untag_number(wasm_mod, compiled_arg1()),
              ),
              Expression.unary(
                wasm_mod,
                Op.extend_s_int32,
                untag_number(wasm_mod, compiled_arg2()),
              ),
            ),
            Expression.const(wasm_mod, const_int64(2)),
          ),
        ),
        /* Convert remainder result into modulo result */
        Expression.if_(
          wasm_mod,
          Expression.binary(
            wasm_mod,
            Op.or_int32,
            Expression.binary(
              wasm_mod,
              Op.eq_int32,
              Expression.binary(
                wasm_mod,
                Op.shr_u_int32,
                compiled_arg1(),
                Expression.const(wasm_mod, const_int32(31)),
              ),
              Expression.binary(
                wasm_mod,
                Op.shr_u_int32,
                compiled_arg2(),
                Expression.const(wasm_mod, const_int32(31)),
              ),
            ),
            Expression.unary(
              wasm_mod,
              Op.eq_z_int64,
              get_swap(~ty=Type.int64, wasm_mod, env, 0),
            ),
          ),
          get_swap(~ty=Type.int64, wasm_mod, env, 0),
          Expression.binary(
            wasm_mod,
            Op.add_int64,
            get_swap(~ty=Type.int64, wasm_mod, env, 0),
            Expression.unary(wasm_mod, Op.extend_s_int32, compiled_arg2()),
          ),
        ),
      ],
    )
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
  | Greater =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.gt_s_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  | GreaterEq =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.ge_s_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  | Less =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.lt_s_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  | LessEq =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.le_s_int32,
        compiled_arg1(),
        compiled_arg2(),
      ),
    )
  | Eq =>
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
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.and_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | Int64Lor =>
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.or_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | Int64Lxor =>
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.xor_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | Int64Lsl =>
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.shl_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        Expression.unary(
          wasm_mod,
          Op.extend_s_int32,
          untag_number(wasm_mod, compiled_arg2()),
        ),
      ),
    )
  | Int64Lsr =>
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.shr_u_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        Expression.unary(
          wasm_mod,
          Op.extend_s_int32,
          untag_number(wasm_mod, compiled_arg2()),
        ),
      ),
    )
  | Int64Asr =>
    allocate_int64_imm(
      wasm_mod,
      env,
      Expression.binary(
        wasm_mod,
        Op.shr_s_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        Expression.unary(
          wasm_mod,
          Op.extend_s_int32,
          untag_number(wasm_mod, compiled_arg2()),
        ),
      ),
    )
  | Int64Gt =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.gt_s_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | Int64Gte =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.ge_s_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | Int64Lt =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.lt_s_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | Int64Lte =>
    encode_bool(
      wasm_mod,
      Expression.binary(
        wasm_mod,
        Op.le_s_int64,
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg1(),
          ),
        ),
        load(
          ~ty=Type.int64,
          ~offset=4,
          wasm_mod,
          untag(
            wasm_mod,
            GenericHeapType(Some(Int64Type)),
            compiled_arg2(),
          ),
        ),
      ),
    )
  | ArrayMake => allocate_array_n(wasm_mod, env, arg1, arg2)
  | ArrayInit => allocate_array_init(wasm_mod, env, arg1, arg2)
  };
};

let compile_allocation = (wasm_mod, env, alloc_type) =>
  switch (alloc_type) {
  | MClosure(cdata) => allocate_closure(wasm_mod, env, cdata)
  | MTuple(elts) => allocate_tuple(wasm_mod, env, elts)
  | MBox(elt) => allocate_box(wasm_mod, env, elt)
  | MArray(elts) => allocate_array(wasm_mod, env, elts)
  | [@implicit_arity] MRecord(ttag, elts) =>
    allocate_record(wasm_mod, env, ttag, elts)
  | MString(str) => allocate_string(wasm_mod, env, str)
  | [@implicit_arity] MADT(ttag, vtag, elts) =>
    allocate_adt(wasm_mod, env, ttag, vtag, elts)
  | MInt32(i) => allocate_int32(wasm_mod, env, i)
  | MInt64(i) => allocate_int64(wasm_mod, env, i)
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
    let preamble =
      set_swap(
        Expression.binary(
          wasm_mod,
          Op.xor_int32,
          lam,
          Expression.const(
            wasm_mod,
            const_int32 @@ tag_val_of_tag_type(LambdaTagType),
          ),
        ),
      );
    let backpatch_var = (idx, var) =>
      store(
        ~offset=4 * (idx + 3),
        wasm_mod,
        get_swap(),
        call_incref_backpatch(
          wasm_mod,
          env,
          [compile_imm(wasm_mod, env, var)],
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
            allocate_closure(wasm_mod, env, ~lambda=get_bind, cdata),
            store_bind_no_incref,
          )
        /* HACK: We expect values returned from functions to have a refcount of 1, so we don't increment it when storing */
        /* | (MCallIndirect _) */
        | MCallKnown(_)
        | MAllocate(_) => (
            compile_instr(wasm_mod, env, instr),
            store_bind_no_incref,
          )
        /* [TODO] I think this is wrong? See commented out line above */
        | MCallIndirect(_)
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
    wasm_mod,
    outer_label,
    [process_branches(0, [], branches)],
  );
}
and compile_block = (wasm_mod, env, block) =>
  Expression.block(
    wasm_mod,
    gensym_label("compile_block"),
    List.map(compile_instr(wasm_mod, env), block),
  )
and compile_instr = (wasm_mod, env, instr) =>
  switch (instr.instr_desc) {
  | MDrop(arg) =>
    Expression.drop(wasm_mod, compile_instr(wasm_mod, env, arg))
  | MTracepoint(x) => tracepoint(wasm_mod, env, x)
  | MImmediate(imm) => compile_imm(wasm_mod, env, imm)
  | MAllocate(alloc) => compile_allocation(wasm_mod, env, alloc)
  | [@implicit_arity] MTupleOp(tuple_op, tup) =>
    compile_tuple_op(wasm_mod, env, tup, tuple_op)
  | [@implicit_arity] MBoxOp(box_op, box) =>
    compile_box_op(wasm_mod, env, box, box_op)
  | [@implicit_arity] MArrayOp(array_op, ret) =>
    compile_array_op(wasm_mod, env, ret, array_op)
  | [@implicit_arity] MAdtOp(adt_op, adt) =>
    compile_adt_op(wasm_mod, env, adt, adt_op)
  | [@implicit_arity] MRecordOp(record_op, record) =>
    compile_record_op(wasm_mod, env, record, record_op)
  | [@implicit_arity] MPrim1(p1, arg) =>
    compile_prim1(wasm_mod, env, p1, arg)
  | [@implicit_arity] MPrim2(p2, arg1, arg2) =>
    compile_prim2(wasm_mod, env, p2, arg1, arg2)
  | [@implicit_arity] MSwitch(arg, branches, default) =>
    compile_switch(wasm_mod, env, arg, branches, default)
  | MStore(binds) => compile_store(wasm_mod, env, binds)

  | [@implicit_arity] MCallIndirect(func, args) =>
    call_lambda(wasm_mod, env, func, args)

  | [@implicit_arity] MIf(cond, thn, els) =>
    let compiled_cond = compile_imm(wasm_mod, env, cond);
    let compiled_thn = compile_block(wasm_mod, env, thn);
    let compiled_els = compile_block(wasm_mod, env, els);
    Expression.if_(
      wasm_mod,
      decode_bool(wasm_mod, compiled_cond),
      compiled_thn,
      compiled_els,
    );

  | [@implicit_arity] MWhile(cond, body) =>
    let compiled_cond = compile_block(wasm_mod, env, cond);
    let compiled_body = compile_block(wasm_mod, env, body);
    let block_label = gensym_label("MWhile");
    let loop_label = gensym_label("MWhile_loop");
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
            gensym_label("MWhile_loop_body"),
            [
              Expression.drop(wasm_mod) @@
              Expression.break(
                wasm_mod,
                block_label,
                Expression.unary(
                  wasm_mod,
                  Op.eq_z_int32,
                  decode_bool(wasm_mod, compiled_cond),
                ),
                Expression.const(wasm_mod, const_void()),
              ),
              Expression.drop(wasm_mod, compiled_body),
              Expression.break(
                wasm_mod,
                loop_label,
                Expression.null(),
                Expression.null(),
              ),
            ],
          ),
        ),
        Expression.const(wasm_mod, const_void()),
      ],
    );

  | [@implicit_arity] MError(err, args) =>
    call_error_handler(wasm_mod, env, err, args)
  | [@implicit_arity] MCallKnown(func_name, args) =>
    let compiled_args = List.map(compile_imm(wasm_mod, env), args);
    Expression.call(wasm_mod, func_name, compiled_args, Type.int32);
  | MArityOp(_) => failwith("NYI: (compile_instr): MArityOp")
  | MTagOp(_) => failwith("NYI: (compile_instr): MTagOp")
  };

let compile_function =
    (
      ~start=false,
      wasm_mod,
      env,
      {index, arity, stack_size, body: body_instrs},
    ) => {
  let arity_int = Int32.to_int(arity);
  let index_int = Int32.to_int(index);
  let func_name =
    if (start) {
      "_start";
    } else {
      Printf.sprintf("func_%d", env.func_offset + index_int);
    };
  let body_env = {...env, num_args: arity_int, stack_size};
  let body =
    Expression.return(
      wasm_mod,
      cleanup_locals(
        wasm_mod,
        body_env,
        compile_block(wasm_mod, body_env, body_instrs),
      ),
    );
  let locals =
    List.init(stack_size, n => Type.int32)
    |> Array.of_list
    |> Array.append(swap_slots);
  Function.add_function(
    wasm_mod,
    func_name,
    Type.create @@ Array.make(arity_int, Type.int32),
    Type.int32,
    locals,
    body,
  );
};

let compute_table_size = (env, {imports, exports, functions}) =>
  List.length(functions)
  + (List.length(imports) - List.length(runtime_global_imports))
  + 2;

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
    | MImportGrain => "GRAIN$EXPORT$GET$" ++ Ident.name(name);

  let compile_import = ({mimp_mod, mimp_name, mimp_type, mimp_kind}) => {
    /* TODO: When user imports become a thing, we'll need to worry about hygiene */
    let module_name = compile_module_name(mimp_mod, mimp_kind);
    let item_name = compile_import_name(mimp_name, mimp_kind);
    let internal_name = get_imported_name(mimp_mod, mimp_name);
    switch (mimp_kind, mimp_type) {
    | (MImportGrain, _) =>
      Import.add_function_import(
        wasm_mod,
        internal_name,
        module_name,
        item_name,
        Type.create([||]),
        Type.int32,
      )
    | (_, [@implicit_arity] MFuncImport(args, ret)) =>
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

let compile_exports =
    (wasm_mod, env, {functions, imports, exports, num_globals}) => {
  let compile_getter = (i, {ex_name, ex_global_index, ex_getter_index}) => {
    let fidx = Int32.to_int(ex_getter_index) + env.func_offset;
    let internal_name = Printf.sprintf("func_%d", fidx);
    let exported_name = "GRAIN$EXPORT$GET$" ++ Ident.name(ex_name);
    let export =
      Export.add_function_export(wasm_mod, internal_name, exported_name);
    export;
  };

  let compile_lambda_export = (i, _) => {
    let internal_name = "func_" ++ string_of_int(i + env.func_offset);
    let external_name = "GRAIN$LAM_" ++ string_of_int(i);
    Export.add_function_export(wasm_mod, internal_name, external_name);
  };
  let main_idx = env.func_offset + List.length(functions);
  let cleanup_globals_idx = main_idx + 1;
  ignore @@ List.mapi(compile_lambda_export, functions);
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
  ignore @@ List.mapi(compile_getter, exports);
  ignore @@ Export.add_function_export(wasm_mod, "_start", "_start");
  ignore @@
  Export.add_function_export(
    wasm_mod,
    Printf.sprintf("func_%d", cleanup_globals_idx),
    "GRAIN$CLEANUP_GLOBALS",
  );
  ignore @@
  Export.add_global_export(
    wasm_mod,
    Printf.sprintf("global_%d", num_globals + 1),
    Ident.name(table_size),
  );
};

let compile_tables = (wasm_mod, env, {functions, imports} as prog) => {
  let table_size = compute_table_size(env, prog);
  let import_names =
    List.filter_map(
      ({mimp_kind, mimp_type, mimp_mod, mimp_name}) =>
        switch (mimp_kind, mimp_type) {
        | (MImportGrain, _)
        | (_, MFuncImport(_)) =>
          Some(get_imported_name(mimp_mod, mimp_name))
        | _ => None
        },
      imports,
    );
  let function_name = i => Printf.sprintf("func_%d", i);
  let function_names =
    List.mapi((i, _) => function_name(env.func_offset + i), functions);
  let all_import_names =
    List.concat([
      import_names,
      function_names,
      ["_start", function_name(table_size - 1)],
    ]);
  Function_table.set_function_table(
    wasm_mod,
    table_size,
    max_int,
    all_import_names,
    Expression.global_get(
      wasm_mod,
      get_imported_name(runtime_mod, reloc_base),
      Type.int32,
    ),
  );
};

let compile_elems = (wasm_mod, env, prog) => ();

let compile_globals = (wasm_mod, env, {num_globals} as prog) => {
  ignore @@
  List.init(1 + num_globals, i =>
    Global.add_global(
      wasm_mod,
      Printf.sprintf("global_%d", i),
      Type.int32,
      true,
      Expression.const(wasm_mod, const_int32(0)),
    )
  );
  ignore @@
  Global.add_global(
    wasm_mod,
    Printf.sprintf("global_%d", 1 + num_globals),
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
      arity: Int32.zero,
      body: prog.main_body,
      stack_size: prog.main_body_stack_size,
      func_loc: Grain_parsing.Location.dummy_loc,
    },
  );
};

let compile_global_cleanup_function =
    (wasm_mod, env, {num_globals, functions}) => {
  let cleanup_calls =
    List.init(num_globals, n =>
      Expression.drop(
        wasm_mod,
        call_decref_cleanup_globals(
          wasm_mod,
          env,
          [
            Expression.global_get(
              wasm_mod,
              Printf.sprintf("global_%d", n),
              Type.int32,
            ),
          ],
        ),
      )
    );
  let body =
    Expression.return(
      wasm_mod,
      Expression.block(
        wasm_mod,
        gensym_label("global_cleanup_function"),
        List.append(
          cleanup_calls,
          [Expression.const(wasm_mod, wrap_int32(Int32.zero))],
        ),
      ),
    );
  Function.add_function(
    wasm_mod,
    Printf.sprintf("func_%d", env.func_offset + List.length(functions) + 1),
    Type.none,
    Type.int32,
    [||],
    body,
  );
};

let compile_functions = (wasm_mod, env, {functions, num_globals} as prog) => {
  ignore @@ List.map(compile_function(wasm_mod, env), functions);
  ignore @@ compile_main(wasm_mod, env, prog);
  ignore @@ compile_global_cleanup_function(wasm_mod, env, prog);
};

exception WasmRunnerError(Module.t, option(string), string);

let validate_module = (~name=?, wasm_mod: Module.t) =>
  try(assert(Module.validate(wasm_mod) == 1)) {
  | Assert_failure(_) =>
    raise(
      [@implicit_arity]
      WasmRunnerError(wasm_mod, name, "WARNING: Invalid module"),
    )
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
  let import_func_offset = List.length(runtime_function_imports);
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
  let func_offset =
    List.length(runtime_function_imports) + List.length(imports);
  (
    {
      ...new_env,
      import_offset,
      import_func_offset,
      import_global_offset,
      global_offset,
      func_offset,
    },
    {
      ...prog,
      imports: new_imports,
      num_globals:
        prog.num_globals + List.length(new_imports) + List.length(imports),
    },
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
  let _ = Module.set_features(wasm_mod, [Features.mvp, Features.multivalue]);
  let _ = Memory.set_memory(wasm_mod, 0, max_int, "memory", [], false);
  let () = ignore @@ compile_functions(wasm_mod, env, prog);
  let () = ignore @@ compile_imports(wasm_mod, env, prog);
  let () = ignore @@ compile_exports(wasm_mod, env, prog);
  let () = ignore @@ compile_globals(wasm_mod, env, prog);
  let () = ignore @@ compile_tables(wasm_mod, env, prog);
  let () = ignore @@ compile_elems(wasm_mod, env, prog);

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
    | [@implicit_arity] WasmRunnerError(wasm_mod, name, msg) =>
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
