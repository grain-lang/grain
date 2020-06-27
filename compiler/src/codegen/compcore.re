open Grain_typed;
open Mashtree;
open Value_tags;
open Wasm;
open Concatlist; /* NOTE: This import shadows (@) and introduces (@+) and (+@) */

/* [TODO] Should probably be a config variable */
let memory_tracing_enabled = false;

let add_dummy_loc = (x: 'a): Source.phrase('a) => Source.(x @@ no_region);
let source_it = (x: Source.phrase('a)): 'a => Source.(x.it);

/** Environment */

type codegen_env = {
  num_args: int,
  func_offset: int,
  global_offset: int,
  stack_size: int,
  import_global_offset: int,
  import_func_offset: int,
  import_offset: int,
  func_types: ref(BatDeque.t(Wasm.Types.func_type)),
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Concatlist.t(Wasm.Ast.instr'), closure_data))),
  imported_funcs: Ident.tbl(Ident.tbl(int32)),
  imported_globals: Ident.tbl(Ident.tbl(int32)),
};

/* Number of swap variables to allocate */
let swap_slots_i32 = [Types.I32Type, Types.I32Type];
let swap_slots_i64 = [Types.I64Type];
let swap_i32_offset = 0;
let swap_i64_offset = List.length(swap_slots_i32);
let swap_slots = List.append(swap_slots_i32, swap_slots_i64);

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
            BatList.init(Runtime_errors.max_arity + 1, _ => I32Type),
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
  func_types: ref(BatDeque.empty),
  backpatches: ref([]),
  imported_funcs: Ident.empty,
  imported_globals: Ident.empty,
};

/* Seems a little silly when named this way, but it makes a little more sense in the
   context of this file, since we are encoding the given OCaml string in a WASM-compatible format
   (its UTF-8 byte sequence). */
let encode_string: string => list(int) = (Utf8.decode: string => list(int));

let encoded_int32 = n => n * 2;

let const_int32 = n =>
  add_dummy_loc(Values.I32Value.to_value @@ Int32.of_int(n));
let const_int64 = n =>
  add_dummy_loc(Values.I64Value.to_value @@ Int64.of_int(n));
let const_float32 = n =>
  add_dummy_loc(Values.F32Value.to_value @@ Wasm.F32.of_float(n));
let const_float64 = n =>
  add_dummy_loc(Values.F64Value.to_value @@ Wasm.F64.of_float(n));

/* These are like the above 'const' functions, but take inputs
   of the underlying types instead */
let wrap_int32 = n => add_dummy_loc(Values.I32Value.to_value(n));
let wrap_int64 = n => add_dummy_loc(Values.I64Value.to_value(n));
let wrap_float32 = n => add_dummy_loc(Values.F32Value.to_value(n));
let wrap_float64 = n => add_dummy_loc(Values.F64Value.to_value(n));

let grain_number_max = 0x3fffffff;
let grain_number_min = (-0x3fffffff); /* 0xC0000001 */

/** Constant compilation */

let rec compile_const = (c): Wasm.Values.value => {
  let identity: 'a. 'a => 'a = x => x;
  let conv_int32 = Int32.(mul(of_int(2)));
  let conv_int64 = Int64.(mul(of_int(2)));
  let conv_float32 = identity;
  let conv_float64 = identity;
  switch (c) {
  | MConstLiteral(MConstLiteral(_) as c) => compile_const(c)
  | MConstI32(n) => Values.I32Value.to_value(conv_int32(n))
  | MConstI64(n) => Values.I64Value.to_value(conv_int64(n))
  | MConstF32(n) =>
    Values.F32Value.to_value(Wasm.F32.of_float @@ conv_float32(n))
  | MConstF64(n) =>
    Values.F64Value.to_value(Wasm.F64.of_float @@ conv_float64(n))
  | MConstLiteral(MConstI32(n)) => Values.I32Value.to_value(n)
  | MConstLiteral(MConstI64(n)) => Values.I64Value.to_value(n)
  | MConstLiteral(MConstF32(n)) =>
    Values.F32Value.to_value(Wasm.F32.of_float(n))
  | MConstLiteral(MConstF64(n)) =>
    Values.F64Value.to_value(Wasm.F64.of_float(n))
  };
};

/* Translate constants to WASM */
let const_true = add_dummy_loc @@ compile_const(const_true);
let const_false = add_dummy_loc @@ compile_const(const_false);
let const_void = add_dummy_loc @@ compile_const(const_void);

/* WebAssembly helpers */

/* These instructions get helpers due to their verbosity */
let store = (~ty=Wasm.Types.I32Type, ~align=2, ~offset=0, ~sz=None, ()) =>
  Wasm.Ast.(Store({ty, align, sz, offset: Int32.of_int(offset)}));

let load = (~ty=Wasm.Types.I32Type, ~align=2, ~offset=0, ~sz=None, ()) =>
  Wasm.Ast.(Load({ty, align, sz, offset: Int32.of_int(offset)}));

let lookup_ext_global = (env, modname, itemname) =>
  Ident.find_same(itemname, Ident.find_same(modname, env.imported_globals));

let var_of_ext_global = (env, modname, itemname) =>
  add_dummy_loc @@ lookup_ext_global(env, modname, itemname);

let lookup_ext_func = (env, modname, itemname) =>
  Ident.find_same(itemname, Ident.find_same(modname, env.imported_funcs));

let var_of_ext_func = (env, modname, itemname) =>
  add_dummy_loc @@ lookup_ext_func(env, modname, itemname);

let var_of_memory_tracing_func = (env, modname, itemname, default) =>
  if (memory_tracing_enabled) {
    var_of_ext_func(env, modname, itemname);
  } else {
    var_of_ext_func(env, modname, default);
  };

let call_runtime_check_memory = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, check_memory_ident));
let call_runtime_throw_error = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, throw_error_ident));
let call_console_log = env =>
  Ast.Call(var_of_ext_func(env, console_mod, log_ident));

let call_malloc = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, malloc_ident));
let call_incref = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, incref_ident));
let call_incref_adt = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_adt_ident,
      incref_ident,
    ),
  );
let call_incref_array = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_array_ident,
      incref_ident,
    ),
  );
let call_incref_tuple = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_tuple_ident,
      incref_ident,
    ),
  );
let call_incref_box = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_box_ident,
      incref_ident,
    ),
  );
let call_incref_backpatch = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_backpatch_ident,
      incref_ident,
    ),
  );
let call_incref_swap_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_swap_bind_ident,
      incref_ident,
    ),
  );
let call_incref_arg_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_arg_bind_ident,
      incref_ident,
    ),
  );
let call_incref_local_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_local_bind_ident,
      incref_ident,
    ),
  );
let call_incref_global_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_global_bind_ident,
      incref_ident,
    ),
  );
let call_incref_closure_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_closure_bind_ident,
      incref_ident,
    ),
  );
let call_incref_cleanup_locals = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      incref_cleanup_locals_ident,
      incref_ident,
    ),
  );
let call_incref_64 = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, incref64_ident));
let call_decref = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, decref_ident));
let call_decref_64 = env =>
  Ast.Call(var_of_ext_func(env, runtime_mod, decref64_ident));
let call_decref_array = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_array_ident,
      decref_ident,
    ),
  );
let call_decref_tuple = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_tuple_ident,
      decref_ident,
    ),
  );
let call_decref_box = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_box_ident,
      decref_ident,
    ),
  );
let call_decref_swap_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_swap_bind_ident,
      decref_ident,
    ),
  );
let call_decref_arg_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_arg_bind_ident,
      decref_ident,
    ),
  );
let call_decref_local_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_local_bind_ident,
      decref_ident,
    ),
  );
let call_decref_global_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_global_bind_ident,
      decref_ident,
    ),
  );
let call_decref_closure_bind = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_closure_bind_ident,
      decref_ident,
    ),
  );
let call_decref_cleanup_locals = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_cleanup_locals_ident,
      decref_ignore_zeros_ident,
    ),
  );
let call_decref_cleanup_globals = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_cleanup_globals_ident,
      decref_ignore_zeros_ident,
    ),
  );
let call_decref_drop = env =>
  Ast.Call(
    var_of_memory_tracing_func(
      env,
      runtime_mod,
      decref_drop_ident,
      decref_ignore_zeros_ident,
    ),
  );

/** Will print "tracepoint <n> reached" to the console when executed (for debugging WASM output) */

let tracepoint = (env, n) =>
  Concatlist.t_of_list([
    Ast.Const(const_int32(n)),
    Ast.Call(var_of_ext_func(env, console_mod, tracepoint_ident)),
  ]);

let get_func_type_idx = (env, typ) =>
  switch (BatDeque.find((==)(typ), env.func_types^)) {
  | None =>
    env.func_types := BatDeque.snoc(env.func_types^, typ);
    BatDeque.size(env.func_types^) - 1;
  | Some((i, _)) => i
  };

let get_arity_func_type_idx = (env, arity) => {
  let has_arity = ([@implicit_arity] Types.FuncType(args, _)) =>
    List.length(args) == arity;
  switch (BatDeque.find(has_arity, env.func_types^)) {
  | None =>
    let args = BatList.init(arity, _ => Types.I32Type);
    let ftype = [@implicit_arity] Types.FuncType(args, [Types.I32Type]);
    env.func_types := BatDeque.snoc(env.func_types^, ftype);
    BatDeque.size(env.func_types^) - 1;
  | Some((i, _)) => i
  };
};

/** Untags the number at the top of the stack */

let untag_number =
  Concatlist.t_of_list([
    Ast.Const(const_int32(1)),
    Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
  ]);

let untag = tag =>
  Concatlist.t_of_list([
    Ast.Const(const_int32(tag_val_of_tag_type(tag))),
    Ast.Binary(Values.I32(Ast.IntOp.Xor)),
  ]);

let encode_bool =
  Concatlist.t_of_list([
    Ast.Const(const_int32(31)),
    Ast.Binary(Values.I32(Ast.IntOp.Shl)),
    Ast.Const(const_false),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ]);

let decode_bool =
  Concatlist.t_of_list([
    Ast.Const(const_int32(31)),
    Ast.Binary(Values.I32(Ast.IntOp.ShrU)),
  ]);

let encoded_const_int32 = n => const_int32(encoded_int32(n));

type bind_action =
  | BindGet
  | BindSet
  | BindTee;

let cleanup_local_slot_instructions = (env: codegen_env) => {
  let instrs =
    BatList.init(
      env.stack_size,
      i => {
        let slot_no = i + env.num_args + List.length(swap_slots);
        let slot = add_dummy_loc(Int32.of_int(slot_no));
        wrapped([
          Ast.LocalGet(slot),
          if (memory_tracing_enabled) {
            Ast.Const(const_int32(slot_no));
          } else {
            Ast.Nop;
          },
          call_decref_cleanup_locals(env),
          Ast.Drop,
        ]);
      },
    );
  flatten(instrs);
};

let compile_bind =
    (
      ~action,
      ~ty as typ=Types.I32Type,
      ~skip_incref=false,
      ~skip_decref=false,
      env: codegen_env,
      b: binding,
    )
    : Concatlist.t(Wasm.Ast.instr') => {
  let (++) = (a, b) => Int32.(add(of_int(a), b));
  let appropriate_incref = env =>
    switch (typ) {
    | _ when skip_incref => Ast.Nop /* This case is used for storing swap values that have freshly been heap-allocated. */
    | Types.I32Type =>
      switch (b) {
      | MArgBind(_) => call_incref_arg_bind(env)
      | MLocalBind(_) => call_incref_local_bind(env)
      | MSwapBind(_) => call_incref_swap_bind(env)
      | MGlobalBind(_) => call_incref_global_bind(env)
      | MClosureBind(_) => call_incref_closure_bind(env)
      | _ => call_incref(env)
      }
    | Types.I64Type =>
      /* https://github.com/dcodeIO/webassembly/issues/26#issuecomment-410157370 */
      /* call_incref_64 env */
      Ast.Nop
    | _ => failwith("appropriate_incref called with non-i32/i64 type")
    };

  let appropriate_decref = env =>
    switch (typ) {
    | _ when skip_decref => Ast.Nop /* This case is used for storing swap values that have freshly been heap-allocated. */
    | Types.I32Type =>
      switch (b) {
      | MArgBind(_) => call_decref_arg_bind(env)
      | MLocalBind(_) => call_decref_local_bind(env)
      | MSwapBind(_) => call_decref_swap_bind(env)
      | MGlobalBind(_) => call_decref_global_bind(env)
      | MClosureBind(_) => call_decref_closure_bind(env)
      | _ => call_decref(env)
      }
    | Types.I64Type =>
      /* https://github.com/dcodeIO/webassembly/issues/26#issuecomment-410157370 */
      /* call_decref_64 env */
      Ast.Nop
    | _ => failwith("appropriate_decref called with non-i32/i64 type")
    };

  switch (b) {
  | MArgBind(i) =>
    /* No adjustments are needed for argument bindings */
    let slot = add_dummy_loc(i);
    switch (action) {
    | BindGet => singleton(Ast.LocalGet(slot))
    | BindSet =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.LocalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.LocalSet(slot),
        /* POST: Stack: <rest> */
      ])
    | BindTee =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.LocalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.LocalTee(slot),
        /* POST: Stack: <rest> */
      ])
    };
  | MLocalBind(i) =>
    /* Local bindings need to be offset to account for arguments and swap variables */
    let slot = add_dummy_loc(env.num_args + List.length(swap_slots) ++ i);
    switch (action) {
    | BindGet => singleton(Ast.LocalGet(slot))
    | BindSet =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.LocalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.LocalSet(slot),
        /* POST: Stack: <rest> */
      ])
    | BindTee =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.LocalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.LocalTee(slot),
        /* POST: Stack: <rest> */
      ])
    };
  | MSwapBind(i) =>
    /* Swap bindings need to be offset to account for arguments */
    let slot = add_dummy_loc(env.num_args ++ i);
    switch (action) {
    | BindGet => singleton(Ast.LocalGet(slot))
    | BindSet =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.LocalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.LocalSet(slot),
        /* POST: Stack: <rest> */
      ])
    | BindTee =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.LocalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.LocalTee(slot),
        /* POST: Stack: <rest> */
      ])
    };
  | MGlobalBind(i) =>
    /* Global bindings need to be offset to account for any imports */
    let slot = add_dummy_loc(env.global_offset ++ i);
    switch (action) {
    | BindGet => singleton(Ast.GlobalGet(slot))
    | BindSet =>
      wrapped([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.GlobalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.GlobalSet(slot),
        /* POST: Stack: <rest> */
      ])
    | BindTee =>
      Concatlist.t_of_list([
        /* PRE: Stack: value to set, <rest> */
        /* Call incref() on new value we're setting (we do this first, since we don't know if new == old).
           Note that this preserves the stack. */
        appropriate_incref(env),
        /* Get old value of slot and call decref() on it */
        Ast.GlobalGet(slot),
        appropriate_decref(env),
        /* Drop old value from stack */
        Ast.Drop,
        /* Set new stack value */
        Ast.GlobalSet(slot),
        /* POST: Stack: <rest> */
        Ast.GlobalGet(slot),
      ])
    };
  | MClosureBind(i) =>
    /* Closure bindings need to be calculated */
    if (!(action == BindGet)) {
      failwith(
        "Internal error: attempted to emit instruction which would mutate closure contents",
      );
    };
    cons(
      Ast.LocalGet(add_dummy_loc(Int32.zero)),
      singleton(load(~offset=4 * (3 + Int32.to_int(i)), ())),
    );
  | MImport(i) =>
    if (!(action == BindGet)) {
      failwith(
        "Internal error: attempted to emit instruction which would mutate an import",
      );
    };
    /* Adjust for runtime functions */
    let slot = add_dummy_loc(env.import_offset ++ i);
    singleton(Ast.GlobalGet(slot));
  };
};

let safe_drop = env =>
  Concatlist.t_of_list([call_decref_drop(env), Ast.Drop]);

let get_swap = (~ty as typ=Types.I32Type, env, idx) =>
  switch (typ) {
  | Types.I32Type =>
    if (idx > List.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset)),
    );
  | Types.I64Type =>
    if (idx > List.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      ~ty=Types.I64Type,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset)),
    );
  | _ => raise(Not_found)
  };

let set_swap =
    (~skip_incref=true, ~skip_decref=true, ~ty as typ=Types.I32Type, env, idx) =>
  switch (typ) {
  | Types.I32Type =>
    if (idx > List.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet,
      ~skip_incref,
      ~skip_decref,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset)),
    );
  | Types.I64Type =>
    if (idx > List.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet,
      ~skip_incref,
      ~skip_decref,
      ~ty=Types.I64Type,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset)),
    );
  | _ => raise(Not_found)
  };

let tee_swap =
    (~ty as typ=Types.I32Type, ~skip_incref=true, ~skip_decref=true, env, idx) =>
  switch (typ) {
  | Types.I32Type =>
    if (idx > List.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee,
      ~skip_incref,
      ~skip_decref,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset)),
    );
  | Types.I64Type =>
    if (idx > List.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee,
      ~skip_incref,
      ~skip_decref,
      ~ty=Types.I64Type,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset)),
    );
  | _ => raise(Not_found)
  };

/* [TODO] This is going to need some sort of type argument as well...I think this could be indicative of a code smell */
let cleanup_locals = (env: codegen_env): Concatlist.t(Wasm.Ast.instr') =>
  /* Do the following:
     - Move the current stack value into a designated return-value holder slot (maybe swap is fine)
     - Call incref() on the return value (to prevent premature free)
     - Call decref() on all locals (should include return value) */
  set_swap(env, 0)
  @ get_swap(env, 0)
  +@ [call_incref_cleanup_locals(env), Ast.Drop]
  @ cleanup_local_slot_instructions(env)
  @ get_swap(env, 0)
  +@ [
    if (memory_tracing_enabled) {
      Ast.Const(const_int32(-1));
    } else {
      Ast.Nop;
    },
    call_decref_cleanup_locals(env),
  ];

let compile_imm =
    (env: codegen_env, i: immediate): Concatlist.t(Wasm.Ast.instr') =>
  switch (i) {
  | MImmConst(c) => singleton(Ast.Const(add_dummy_loc @@ compile_const(c)))
  | MImmBinding(b) => compile_bind(~action=BindGet, env, b)
  };

let call_error_handler = (env, err, args) => {
  let pad_val = MImmConst(MConstI32(Int32.zero));
  let args = Runtime_errors.pad_args(pad_val, args);
  let compiled_args = Concatlist.flatten @@ List.map(compile_imm(env), args);
  singleton(Ast.Const(const_int32 @@ Runtime_errors.code_of_error(err)))
  @ compiled_args
  +@ [call_runtime_throw_error(env), Ast.Unreachable];
};

let error_if_true = (env, err, args) =>
  [@implicit_arity]
  Ast.If(
    [],
    Concatlist.mapped_list_of_t(
      add_dummy_loc,
      call_error_handler(env, err, args),
    ),
    [add_dummy_loc(Ast.Nop)],
  );

let dummy_err_val = MImmConst(MConstI32(Int32.zero));
/* Checks whether the two Int64s at the top of the stack overflowed */
let check_overflow = env =>
  Concatlist.t_of_list([
    /* WASM has no concept of overflows, so we have to check manually */
    Ast.Const(const_int64(Int32.to_int(Int32.max_int))),
    Ast.Compare(Values.I64(Ast.IntOp.GtS)),
    error_if_true(env, OverflowError, [dummy_err_val, dummy_err_val]),
    Ast.Const(const_int64(Int32.to_int(Int32.min_int))),
    Ast.Compare(Values.I64(Ast.IntOp.LtS)),
    error_if_true(env, OverflowError, [dummy_err_val, dummy_err_val]),
  ]);

let compile_tuple_op = (~is_box=false, env, tup_imm, op) => {
  let tup = compile_imm(env, tup_imm);
  switch (op) {
  | MTupleGet(idx) =>
    let idx_int = Int32.to_int(idx);
    /* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. */
    tup @ untag(TupleTagType) +@ [load(~offset=4 * (idx_int + 1), ())];
  | [@implicit_arity] MTupleSet(idx, imm) =>
    let idx_int = Int32.to_int(idx);
    let get_swap = get_swap(env, 0);
    let set_swap = set_swap(env, 0);
    tup
    @ untag(TupleTagType)
    @ set_swap
    @ get_swap
    @ compile_imm(env, imm)
    +@ [(if (is_box) {call_incref_box} else {call_incref_tuple})(env)]
    @ get_swap
    +@ [
      load(~offset=4 * (idx_int + 1), ()),
      (if (is_box) {call_decref_box} else {call_decref_tuple})(env),
      Ast.Drop,
      store(~offset=4 * (idx_int + 1), ()),
    ]
    @ compile_imm(env, imm);
  };
};

let compile_box_op = (env, box_imm, op) =>
  /* At the moment, we make no runtime distinction between boxes and tuples */
  switch (op) {
  | MBoxUnbox =>
    compile_tuple_op(~is_box=true, env, box_imm, MTupleGet(Int32.zero))
  | MBoxUpdate(imm) =>
    compile_tuple_op(
      ~is_box=true,
      env,
      box_imm,
      [@implicit_arity] MTupleSet(Int32.zero, imm),
    )
  };

let compile_array_op = (env, arr_imm, op) => {
  let arr = compile_imm(env, arr_imm);
  switch (op) {
  | MArrayGet(idx_imm) =>
    let idx = compile_imm(env, idx_imm);
    /* Check that the index is in bounds */
    arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [load(~offset=4, ())]
    @ arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [load(~offset=4, ())]
    +@ [
      Ast.Const(const_int32(-2)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    ]
    @ idx
    +@ [
      Ast.Compare(Values.I32(Ast.IntOp.GtS)),
      error_if_true(env, ArrayIndexOutOfBounds, []),
      Ast.Const(const_int32(2)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    ]
    @ idx
    +@ [
      Ast.Compare(Values.I32(Ast.IntOp.LeS)),
      error_if_true(env, ArrayIndexOutOfBounds, []),
      /* Resolve a negative index */
    ]
    @ idx
    +@ [
      Ast.Const(const_int32(0)),
      Ast.Compare(Values.I32(Ast.IntOp.LtS)),
      [@implicit_arity]
      Ast.If(
        [Types.I32Type],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        idx
        +@ [
          Ast.Const(const_int32(1)),
          Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
        ]
        @ arr
        @ untag(GenericHeapType(Some(ArrayType)))
        +@ [load(~offset=4, ()), Ast.Binary(Values.I32(Ast.IntOp.Add))],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        idx
        +@ [
          Ast.Const(const_int32(1)),
          Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
        ],
      ),
      /* Get the item */
      Ast.Const(const_int32(4)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    ]
    @ arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [Ast.Binary(Values.I32(Ast.IntOp.Add)), load(~offset=8, ())];
  | MArrayLength =>
    arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [
      load(~offset=4, ()),
      Ast.Const(const_int32(1)),
      Ast.Binary(Values.I32(Ast.IntOp.Shl)),
    ]
  | [@implicit_arity] MArraySet(idx_imm, val_imm) =>
    let idx = compile_imm(env, idx_imm);
    let val_ = compile_imm(env, val_imm);
    /* Check that the index is in bounds */
    arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [load(~offset=4, ())]
    @ arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [load(~offset=4, ())]
    +@ [
      Ast.Const(const_int32(-2)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    ]
    @ idx
    +@ [
      Ast.Compare(Values.I32(Ast.IntOp.GtS)),
      error_if_true(env, ArrayIndexOutOfBounds, []),
      Ast.Const(const_int32(2)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    ]
    @ idx
    +@ [
      Ast.Compare(Values.I32(Ast.IntOp.LeS)),
      error_if_true(env, ArrayIndexOutOfBounds, []),
      /* Resolve a negative index */
    ]
    @ idx
    +@ [
      Ast.Const(const_int32(0)),
      Ast.Compare(Values.I32(Ast.IntOp.LtS)),
      [@implicit_arity]
      Ast.If(
        [Types.I32Type],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        idx
        +@ [
          Ast.Const(const_int32(1)),
          Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
        ]
        @ arr
        @ untag(GenericHeapType(Some(ArrayType)))
        +@ [load(~offset=4, ()), Ast.Binary(Values.I32(Ast.IntOp.Add))],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        idx
        +@ [
          Ast.Const(const_int32(1)),
          Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
        ],
      ),
      /* Store the item */
      Ast.Const(const_int32(4)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    ]
    @ arr
    @ untag(GenericHeapType(Some(ArrayType)))
    +@ [Ast.Binary(Values.I32(Ast.IntOp.Add))]
    @ val_
    +@ [
      /* [TODO] decref the old item */
      call_incref(env),
      store(~offset=8, ()),
    ]
    @ val_;
  };
};

let compile_adt_op = (env, adt_imm, op) => {
  let adt = compile_imm(env, adt_imm);
  switch (op) {
  | MAdtGet(idx) =>
    let idx_int = Int32.to_int(idx);
    adt
    @ untag(GenericHeapType(Some(ADTType)))
    +@ [load(~offset=4 * (idx_int + 5), ())];
  | MAdtGetModule =>
    adt @ untag(GenericHeapType(Some(ADTType))) +@ [load(~offset=4, ())]
  | MAdtGetTag =>
    adt @ untag(GenericHeapType(Some(ADTType))) +@ [load(~offset=12, ())]
  };
};

let compile_record_op = (env, rec_imm, op) => {
  let record = compile_imm(env, rec_imm);
  switch (op) {
  | MRecordGet(idx) =>
    let idx_int = Int32.to_int(idx);
    record
    @ untag(GenericHeapType(Some(RecordType)))
    +@ [load(~offset=4 * (idx_int + 4), ())];
  };
};

/** Heap allocations. */

let round_up = (num: int, multiple: int): int => num + num mod multiple;

/** Rounds the given number of words to be aligned correctly */

let round_allocation_size = (num_words: int): int => round_up(num_words, 4);

let heap_allocate = (env, num_words: int) => {
  let words_to_allocate = round_allocation_size(num_words);
  Concatlist.t_of_list([
    Ast.Const(const_int32(4 * words_to_allocate)),
    call_malloc(env),
  ]);
};

let heap_allocate_imm = (~additional_words=0, env, num_words: immediate) => {
  let num_words = compile_imm(env, num_words);
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
    num_words
    @ num_words
    +@ [
      /* Round up to nearest multiple of 8 */
      Ast.Const(const_int32(8)),
      Ast.Binary(Values.I32(Ast.IntOp.RemS)),
      Ast.Binary(Values.I32(Ast.IntOp.Add)),
      /* Get number of bytes */
      Ast.Const(const_int32(2)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
      call_malloc(env),
    ];
  } else {
    num_words
    @ num_words
    +@ [
      /* Add in additional words (tagged) before rounding */
      Ast.Const(const_int32(additional_words * 2)),
      Ast.Binary(Values.I32(Ast.IntOp.Add)),
      /* Round up to nearest multiple of 8 */
      Ast.Const(const_int32(8)),
      Ast.Binary(Values.I32(Ast.IntOp.RemS)),
      Ast.Binary(Values.I32(Ast.IntOp.Add)),
      /* Add in the additional words (tagged) again */
      Ast.Const(const_int32(additional_words * 2)),
      Ast.Binary(Values.I32(Ast.IntOp.Add)),
      /* Get number of bytes */
      Ast.Const(const_int32(2)),
      Ast.Binary(Values.I32(Ast.IntOp.Mul)),
      call_malloc(env),
    ];
  };
};

let heap_check_memory = (env, num_words: int) => {
  let words_to_allocate = round_allocation_size(num_words);
  Concatlist.t_of_list([
    Ast.Const(const_int32(4 * words_to_allocate)),
    call_runtime_check_memory(env),
  ]);
};

let buf_to_ints = (buf: Buffer.t): list(int64) => {
  let num_bytes = Buffer.length(buf);
  let num_ints = round_up(num_bytes, 8);
  let out_ints = ref([]);

  let byte_buf = Bytes.create(num_ints * 8);
  Buffer.blit(buf, 0, byte_buf, 0, num_bytes);
  let bytes_to_int =
    if (Sys.big_endian) {Stdint.Uint64.of_bytes_big_endian} else {
      Stdint.Uint64.of_bytes_little_endian
    };
  for (i in 0 to num_ints - 1) {
    out_ints := [bytes_to_int(byte_buf, i * 8), ...out_ints^];
  };
  List.rev @@ List.map(Stdint.Uint64.to_int64, out_ints^);
};

let call_lambda = (env, func, args) => {
  let compiled_func = compile_imm(env, func);
  let compiled_args = Concatlist.flatten @@ List.map(compile_imm(env), args);
  let ftype =
    add_dummy_loc @@
    Int32.of_int(get_arity_func_type_idx(env, 1 + List.length(args)));
  compiled_func
  @ untag(LambdaTagType)
  @ compiled_args
  @ compiled_func
  @ untag(LambdaTagType)
  +@ [load(~offset=4, ()), Ast.CallIndirect(ftype)];
};

let allocate_string = (env, str) => {
  let str_as_bytes = Bytes.of_string(str);
  let num_bytes = Bytes.length(str_as_bytes);
  let num_ints = round_up(num_bytes, 8);
  let buf = Buffer.create(num_ints);
  BatUTF8.iter(BatUTF8.Buf.add_char(buf), str);

  let ints_to_push: list(int64) = (buf_to_ints(buf): list(int64));
  let get_swap = get_swap(env, 0);
  let tee_swap = tee_swap(~skip_incref=true, env, 0);
  let preamble =
    Concatlist.t_of_list([
      Ast.Const(const_int32 @@ 4 * (2 + 2 * List.length(ints_to_push))),
      call_malloc(env),
    ])
    @ tee_swap
    +@ [Ast.Const(const_int32 @@ String.length(str))]
    @ get_swap
    +@ [
      Ast.Const(const_int32(tag_val_of_heap_tag_type(StringType))),
      store(~offset=0, ()),
      store(~offset=4, ()),
    ];
  let elts =
    List.flatten @@
    List.mapi(
      (idx, i: int64) =>
        Concatlist.list_of_t(
          get_swap
          +@ [
            Ast.Const(add_dummy_loc(Values.I64(i))),
            store(~ty=Types.I64Type, ~offset=8 * (idx + 1), ()),
          ],
        ),
      ints_to_push,
    );
  preamble
  +@ elts
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(StringType))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ]
  @ tee_swap;
};

let allocate_int32 = (env, i) => {
  let get_swap = get_swap(env, 0);
  let tee_swap = tee_swap(env, 0);
  heap_allocate(env, 2)
  @ tee_swap
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(Int32Type))),
    store(~offset=0, ()),
  ]
  @ get_swap
  +@ [Ast.Const(wrap_int32(i)), store(~ty=I32Type, ~offset=4, ())]
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(Int32Type))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

let allocate_int64 = (env, i) => {
  let get_swap = get_swap(env, 0);
  let tee_swap = tee_swap(env, 0);
  heap_allocate(env, 3)
  @ tee_swap
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(Int64Type))),
    store(~offset=0, ()),
  ]
  @ get_swap
  +@ [Ast.Const(wrap_int64(i)), store(~ty=I64Type, ~offset=4, ())]
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(Int64Type))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

/* Store the int64 at the top of the stack */
let allocate_int64_imm = env => {
  let get_swap64 = get_swap(~ty=I64Type, env, 0);
  let set_swap64 = set_swap(~ty=I64Type, env, 0);
  let get_swap = get_swap(env, 0);
  let tee_swap = tee_swap(env, 0);
  set_swap64
  @ heap_allocate(env, 3)
  @ tee_swap
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(Int64Type))),
    store(~offset=0, ()),
  ]
  @ get_swap
  @ get_swap64
  +@ [store(~ty=I64Type, ~offset=4, ())]
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(Int64Type))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

let allocate_closure =
    (env, ~lambda=?, {func_idx, arity, variables} as closure_data) => {
  let num_free_vars = List.length(variables);
  let closure_size = num_free_vars + 3;
  let get_swap = get_swap(env, 0);
  let access_lambda =
    Option.default(
      get_swap
      @+ [
        Ast.Const(const_int32 @@ 4 * round_allocation_size(closure_size)),
        Ast.Binary(Values.I32(Ast.IntOp.Sub)),
      ],
      lambda,
    );
  env.backpatches := [(access_lambda, closure_data), ...env.backpatches^];
  heap_allocate(env, closure_size)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [Ast.Const(const_int32(num_free_vars))]
  @ get_swap
  +@ [
    Ast.GlobalGet(var_of_ext_global(env, runtime_mod, reloc_base)),
    Ast.Const(wrap_int32(Int32.(add(func_idx, of_int(env.func_offset))))),
    Ast.Binary(Values.I32(Ast.IntOp.Add)),
  ]
  @ get_swap
  +@ [
    Ast.Const(add_dummy_loc(Values.I32Value.to_value(arity))),
    store(~offset=0, ()),
    store(~offset=4, ()),
    store(~offset=8, ()),
  ]
  @ get_swap
  +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type(LambdaTagType)),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ]
  @ tee_swap(~skip_incref=true, ~skip_decref=true, env, 0);
};

let allocate_adt = (env, ttag, vtag, elts) => {
  /* Heap memory layout of ADT types:
      [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
     */
  let num_elts = List.length(elts);
  let get_swap = get_swap(env, 0);
  let compile_elt = (idx, elt) =>
    get_swap
    @ compile_imm(env, elt)
    +@ [call_incref_adt(env), store(~offset=4 * (idx + 5), ())];

  heap_allocate(env, num_elts + 5)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(ADTType))),
    store(~offset=0, ()),
  ]
  @ get_swap
  +@ [
    Ast.GlobalGet(var_of_ext_global(env, runtime_mod, module_runtime_id)),
    /* Tag the runtime id */
    Ast.Const(const_int32(2)),
    Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    store(~offset=4, ()),
  ]
  @ get_swap
  @ compile_imm(env, ttag)
  +@ [store(~offset=8, ())]
  @ get_swap
  @ compile_imm(env, vtag)
  +@ [store(~offset=12, ())]
  @ get_swap
  +@ [Ast.Const(const_int32(num_elts)), store(~offset=16, ())]
  @ (Concatlist.flatten @@ List.mapi(compile_elt, elts))
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(ADTType))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ]
  @ tee_swap(~skip_incref=true, ~skip_decref=true, env, 0);
};

let allocate_tuple = (~is_box=false, env, elts) => {
  let num_elts = List.length(elts);
  let get_swap = get_swap(env, 0);
  let compile_elt = (idx, elt) =>
    get_swap
    @ compile_imm(env, elt)
    +@ [
      (if (is_box) {call_incref_box} else {call_incref_tuple})(env),
      store(~offset=4 * (idx + 1), ()),
    ];

  heap_allocate(env, num_elts + 1)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [Ast.Const(const_int32(num_elts)), store(~offset=0, ())]
  @ (Concatlist.flatten @@ List.mapi(compile_elt, elts))
  @ get_swap
  +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type(TupleTagType)),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ]
  @ tee_swap(~skip_incref=true, ~skip_decref=true, env, 0);
};

let allocate_box = (env, elt) =>
  /* At the moment, we make no runtime distinction between boxes and tuples */
  allocate_tuple(~is_box=true, env, [elt]);

let allocate_array = (env, elts) => {
  let num_elts = List.length(elts);
  let get_swap = get_swap(env, 0);
  let compile_elt = (idx, elt) =>
    get_swap
    @ compile_imm(env, elt)
    +@ [call_incref_array(env), store(~offset=4 * (idx + 2), ())];

  heap_allocate(env, num_elts + 2)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(ArrayType))),
    store(~offset=0, ()),
  ]
  @ get_swap
  +@ [Ast.Const(const_int32(num_elts)), store(~offset=4, ())]
  @ (Concatlist.flatten @@ List.mapi(compile_elt, elts))
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(ArrayType))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

let allocate_array_n = (env, num_elts, elt) => {
  let get_arr_addr = get_swap(env, 0);
  let get_loop_counter = get_swap(env, 1);
  let set_loop_counter = set_swap(env, 1);

  let compiled_num_elts = compile_imm(env, num_elts);
  let elt = compile_imm(env, elt);

  compiled_num_elts
  +@ [
    Ast.Const(const_int32(0)),
    Ast.Compare(Values.I32(Ast.IntOp.LtS)),
    error_if_true(env, InvalidArgument, [num_elts]),
  ]
  @ heap_allocate_imm(~additional_words=2, env, num_elts)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(ArrayType))),
    store(~offset=0, ()),
  ]
  @ get_arr_addr
  @ compiled_num_elts
  +@ [
    Ast.Const(const_int32(1)),
    Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
    store(~offset=4, ()),
    Ast.Const(const_int32(0)),
  ]
  @ set_loop_counter
  @ singleton(
      [@implicit_arity]
      Ast.Block(
        [],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        singleton(
          [@implicit_arity]
          Ast.Loop(
            [],
            Concatlist.mapped_list_of_t(add_dummy_loc) @@
            get_loop_counter
            @ compiled_num_elts
            +@ [
              Ast.Compare(Values.I32(Ast.IntOp.GeS)),
              Ast.BrIf(add_dummy_loc @@ Int32.of_int(1)),
            ]
            @ get_arr_addr
            @ get_loop_counter
            +@ [
              /* Since the counter is tagged, only need to multiply by 2 */
              Ast.Const(const_int32(2)),
              Ast.Binary(Values.I32(Ast.IntOp.Mul)),
              Ast.Binary(Values.I32(Ast.IntOp.Add)),
            ]
            @ elt
            +@ [store(~offset=8, ())]
            @ get_loop_counter
            +@ [
              /* Add 2 to keep tagged counter */
              Ast.Const(const_int32(2)),
              Ast.Binary(Values.I32(Ast.IntOp.Add)),
            ]
            @ set_loop_counter
            +@ [Ast.Br(add_dummy_loc @@ Int32.of_int(0))],
          ),
        ),
      ),
    )
  @ get_arr_addr
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(ArrayType))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

let allocate_array_init = (env, num_elts, init_f) => {
  let get_arr_addr = get_swap(env, 0);
  let get_loop_counter = get_swap(env, 1);
  let set_loop_counter = set_swap(env, 1);

  let compiled_num_elts = compile_imm(env, num_elts);

  let compiled_func = compile_imm(env, init_f);
  let ftype = add_dummy_loc @@ Int32.of_int(get_arity_func_type_idx(env, 2));

  compiled_num_elts
  +@ [
    Ast.Const(const_int32(0)),
    Ast.Compare(Values.I32(Ast.IntOp.LtS)),
    error_if_true(env, InvalidArgument, [num_elts]),
  ]
  @ heap_allocate_imm(~additional_words=2, env, num_elts)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(ArrayType))),
    store(~offset=0, ()),
  ]
  @ get_arr_addr
  @ compiled_num_elts
  +@ [
    Ast.Const(const_int32(1)),
    Ast.Binary(Values.I32(Ast.IntOp.ShrS)),
    store(~offset=4, ()),
    Ast.Const(const_int32(0)),
  ]
  @ set_loop_counter
  @ singleton(
      [@implicit_arity]
      Ast.Block(
        [],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        singleton(
          [@implicit_arity]
          Ast.Loop(
            [],
            Concatlist.mapped_list_of_t(add_dummy_loc) @@
            get_loop_counter
            @ compiled_num_elts
            +@ [
              Ast.Compare(Values.I32(Ast.IntOp.GeS)),
              Ast.BrIf(add_dummy_loc @@ Int32.of_int(1)),
            ]
            @ get_arr_addr
            @ get_loop_counter
            +@ [
              /* Since the counter is tagged, only need to multiply by 2 */
              Ast.Const(const_int32(2)),
              Ast.Binary(Values.I32(Ast.IntOp.Mul)),
              Ast.Binary(Values.I32(Ast.IntOp.Add)),
            ]
            @ compiled_func
            @ untag(LambdaTagType)
            @ get_loop_counter
            @ compiled_func
            @ untag(LambdaTagType)
            +@ [
              load(~offset=4, ()),
              Ast.CallIndirect(ftype),
              store(~offset=8, ()),
            ]
            @ get_loop_counter
            +@ [
              /* Add 2 to keep tagged counter */
              Ast.Const(const_int32(2)),
              Ast.Binary(Values.I32(Ast.IntOp.Add)),
            ]
            @ set_loop_counter
            +@ [Ast.Br(add_dummy_loc @@ Int32.of_int(0))],
          ),
        ),
      ),
    )
  @ get_arr_addr
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(ArrayType))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

let allocate_record = (env, ttag, elts) => {
  let (_, elts) = List.split(elts);
  /* Heap memory layout of records:
      [ <value type tag>, <module_tag>, <type_tag>, <arity> ordered elts ... ]
     */
  let num_elts = List.length(elts);
  let get_swap = get_swap(env, 0);
  let compile_elt = (idx, elt) =>
    get_swap @ compile_imm(env, elt) +@ [store(~offset=4 * (idx + 4), ())];

  heap_allocate(env, num_elts + 4)
  @ tee_swap(~skip_incref=true, env, 0)
  +@ [
    Ast.Const(const_int32(tag_val_of_heap_tag_type(RecordType))),
    store(~offset=0, ()),
  ]
  @ get_swap
  +@ [
    Ast.GlobalGet(var_of_ext_global(env, runtime_mod, module_runtime_id)),
    /* Tag the runtime id */
    Ast.Const(const_int32(2)),
    Ast.Binary(Values.I32(Ast.IntOp.Mul)),
    store(~offset=4, ()),
  ]
  @ get_swap
  @ compile_imm(env, ttag)
  +@ [store(~offset=8, ())]
  @ get_swap
  +@ [Ast.Const(const_int32(num_elts)), store(~offset=12, ())]
  @ (Concatlist.flatten @@ List.mapi(compile_elt, elts))
  @ get_swap
  +@ [
    Ast.Const(
      const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(RecordType))),
    ),
    Ast.Binary(Values.I32(Ast.IntOp.Or)),
  ];
};

let compile_prim1 = (env, p1, arg): Concatlist.t(Wasm.Ast.instr') => {
  let compiled_arg = compile_imm(env, arg);
  let get_swap_i64 = get_swap(~ty=I64Type, env, 0);
  let tee_swap_i64 = tee_swap(~ty=I64Type, env, 0);
  let get_swap = get_swap(env, 0);
  let tee_swap = tee_swap(env, 0);
  /* TODO: Overflow checks? */
  switch (p1) {
  | Incr =>
    compiled_arg
    @+ [
      Ast.Const(encoded_const_int32(1)),
      Ast.Binary(Values.I32(Ast.IntOp.Add)),
    ]
  | Decr =>
    compiled_arg
    @+ [
      Ast.Const(encoded_const_int32(1)),
      Ast.Binary(Values.I32(Ast.IntOp.Sub)),
    ]
  | Not =>
    compiled_arg
    @+ [
      Ast.Const(const_int32(0x80000000)),
      Ast.Binary(Values.I32(Ast.IntOp.Xor)),
    ]
  | Ignore =>
    (compiled_arg @+ [call_decref_drop(env), Ast.Drop])
    @ singleton(Ast.Const(const_void))
  | ArrayLength => compile_array_op(env, arg, MArrayLength)
  | Assert =>
    compiled_arg
    @+ [
      Ast.Const(const_false),
      Ast.Compare(Values.I32(Ast.IntOp.Eq)),
      error_if_true(env, AssertionError, []),
      Ast.Const(const_void),
    ]
  | FailWith => call_error_handler(env, Failure, [arg])
  | Int64FromNumber =>
    heap_allocate(env, 3)
    @ tee_swap
    +@ [
      Ast.Const(const_int32(tag_val_of_heap_tag_type(Int64Type))),
      store(~offset=0, ()),
    ]
    @ get_swap
    @ compiled_arg
    @ untag_number
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      store(~ty=I64Type, ~offset=4, ()),
    ]
    @ get_swap
    +@ [
      Ast.Const(
        const_int32 @@ tag_val_of_tag_type(GenericHeapType(Some(Int64Type))),
      ),
      Ast.Binary(Values.I32(Ast.IntOp.Or)),
    ]
  | Int64ToNumber =>
    compiled_arg
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ tee_swap_i64
    +@ [
      Ast.Const(const_int64(grain_number_max)),
      Ast.Compare(Values.I64(Ast.IntOp.GtS)),
      error_if_true(env, OverflowError, []),
    ]
    @ get_swap_i64
    +@ [
      Ast.Const(const_int64(grain_number_min)),
      Ast.Compare(Values.I64(Ast.IntOp.LtS)),
      error_if_true(env, OverflowError, []),
    ]
    @ get_swap_i64
    +@ [
      Ast.Convert(Values.I32(Ast.IntOp.WrapI64)),
      Ast.Const(const_int32(1)),
      Ast.Binary(Values.I32(Ast.IntOp.Shl)),
    ]
  | Int64Lnot =>
    Concatlist.t_of_list([
      /* 2's complement */
      Ast.Const(const_int64(-1)),
    ])
    @ compiled_arg
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Binary(Values.I64(Ast.IntOp.Sub)),
    ]
    @ allocate_int64_imm(env)
  | Box => failwith("Unreachable case; should never get here: Box")
  | Unbox => failwith("Unreachable case; should never get here: Unbox")
  };
};

let compile_prim2 =
    (env: codegen_env, p2, arg1, arg2): Concatlist.t(Wasm.Ast.instr') => {
  let compiled_arg1 = compile_imm(env, arg1);
  let compiled_arg2 = compile_imm(env, arg2);
  let swap_get = get_swap(~ty=Types.I32Type, env, 0);
  let swap_set = set_swap(~ty=Types.I32Type, env, 0);
  let overflow_safe = instrs => {
    let compiled_swap_get = get_swap(~ty=Types.I64Type, env, 0);
    let compiled_swap_set = set_swap(~ty=Types.I64Type, env, 0);
    instrs
    @ compiled_swap_set
    @ compiled_swap_get
    @ compiled_swap_get
    @ check_overflow(env)
    @ compiled_swap_get
    +@ [Ast.Convert(Values.I32(Ast.IntOp.WrapI64))];
  };

  /* TODO: Overflow checks? */
  switch (p2) {
  | Plus =>
    overflow_safe @@
    compiled_arg1
    +@ [Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32))]
    @ compiled_arg2
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.Add)),
    ]
  | Minus =>
    overflow_safe @@
    compiled_arg1
    +@ [Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32))]
    @ compiled_arg2
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.Sub)),
    ]
  | Times =>
    /* Untag one of the numbers:
          ((a * 2) / 2) * (b * 2) = (a * b) * 2
       */
    overflow_safe @@
    compiled_arg1
    @ untag_number
    +@ [Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32))]
    @ compiled_arg2
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.Mul)),
    ]
  | Divide =>
    /* While (2a) / b = 2(a/b), we can't just untag b since b could be a multiple of 2 (yielding an odd result).
          Instead, perform the division and retag after:
          (2a / 2b) * 2 = (a / b) * 2
       */
    overflow_safe @@
    compiled_arg1
    +@ [Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32))]
    @ compiled_arg2
    +@ [
      Ast.Test(Values.I32(Ast.IntOp.Eqz)),
      error_if_true(env, DivisionByZeroError, []),
    ]
    @ compiled_arg2
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.DivS)),
      Ast.Const(const_int64(2)),
      Ast.Binary(Values.I64(Ast.IntOp.Mul)),
    ]
  | Mod =>
    /* Mod is not commutative, so untag everything and retag at the end */
    overflow_safe @@
    compiled_arg1
    @ untag_number
    +@ [Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32))]
    @ compiled_arg2
    +@ [
      Ast.Test(Values.I32(Ast.IntOp.Eqz)),
      error_if_true(env, ModuloByZeroError, []),
    ]
    @ compiled_arg2
    @ untag_number
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.RemS)),
      Ast.Const(const_int64(2)),
      Ast.Binary(Values.I64(Ast.IntOp.Mul)),
    ]
    @ tee_swap(~ty=Types.I64Type, env, 0)
    /* Convert remainder result into modulo result */
    @ compiled_arg1
    +@ [
      Ast.Const(const_int32(31)),
      Ast.Binary(Values.I32(Ast.IntOp.ShrU)),
    ]
    @ compiled_arg2
    +@ [
      Ast.Const(const_int32(31)),
      Ast.Binary(Values.I32(Ast.IntOp.ShrU)),
      Ast.Compare(Values.I32(Ast.IntOp.Eq)),
    ]
    @ get_swap(~ty=Types.I64Type, env, 0)
    +@ [
      Ast.Test(Values.I64(Ast.IntOp.Eqz)),
      Ast.Binary(Values.I32(Ast.IntOp.Or)),
      [@implicit_arity]
      Ast.If(
        [Types.I64Type],
        [add_dummy_loc @@ Ast.Const(const_int64(0))],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        compiled_arg2
        +@ [Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32))],
      ),
      Ast.Binary(Values.I64(Ast.IntOp.Add)),
    ]
  | And =>
    compiled_arg1
    @ swap_set
    @ swap_get
    @ decode_bool
    +@ [
      [@implicit_arity]
      Ast.If(
        [Types.I32Type],
        List.map(add_dummy_loc) @@ list_of_t(compiled_arg2),
        List.map(add_dummy_loc) @@ list_of_t(swap_get),
      ),
    ]
  | Or =>
    compiled_arg1
    @ swap_set
    @ swap_get
    @ decode_bool
    +@ [
      [@implicit_arity]
      Ast.If(
        [Types.I32Type],
        List.map(add_dummy_loc) @@ list_of_t(swap_get),
        List.map(add_dummy_loc) @@ list_of_t(compiled_arg2),
      ),
    ]
  | Greater =>
    compiled_arg1
    @ compiled_arg2
    +@ [Ast.Compare(Values.I32(Ast.IntOp.GtS))]
    @ encode_bool
  | GreaterEq =>
    compiled_arg1
    @ compiled_arg2
    +@ [Ast.Compare(Values.I32(Ast.IntOp.GeS))]
    @ encode_bool
  | Less =>
    compiled_arg1
    @ compiled_arg2
    +@ [Ast.Compare(Values.I32(Ast.IntOp.LtS))]
    @ encode_bool
  | LessEq =>
    compiled_arg1
    @ compiled_arg2
    +@ [Ast.Compare(Values.I32(Ast.IntOp.LeS))]
    @ encode_bool
  | Eq =>
    compiled_arg1
    @ compiled_arg2
    +@ [Ast.Compare(Values.I32(Ast.IntOp.Eq))]
    @ encode_bool
  | Int64Land =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Binary(Values.I64(Ast.IntOp.And)),
    ]
    @ allocate_int64_imm(env)
  | Int64Lor =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Binary(Values.I64(Ast.IntOp.Or)),
    ]
    @ allocate_int64_imm(env)
  | Int64Lxor =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Binary(Values.I64(Ast.IntOp.Xor)),
    ]
    @ allocate_int64_imm(env)
  | Int64Lsl =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag_number
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.Shl)),
    ]
    @ allocate_int64_imm(env)
  | Int64Lsr =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag_number
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.ShrU)),
    ]
    @ allocate_int64_imm(env)
  | Int64Asr =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag_number
    +@ [
      Ast.Convert(Values.I64(Ast.IntOp.ExtendSI32)),
      Ast.Binary(Values.I64(Ast.IntOp.ShrS)),
    ]
    @ allocate_int64_imm(env)
  | Int64Gt =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Compare(Values.I64(Ast.IntOp.GtS)),
    ]
    @ encode_bool
  | Int64Gte =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Compare(Values.I64(Ast.IntOp.GeS)),
    ]
    @ encode_bool
  | Int64Lt =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Compare(Values.I64(Ast.IntOp.LtS)),
    ]
    @ encode_bool
  | Int64Lte =>
    compiled_arg1
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [load(~ty=I64Type, ~offset=4, ())]
    @ compiled_arg2
    @ untag(GenericHeapType(Some(Int64Type)))
    +@ [
      load(~ty=I64Type, ~offset=4, ()),
      Ast.Compare(Values.I64(Ast.IntOp.LeS)),
    ]
    @ encode_bool
  | ArrayMake => allocate_array_n(env, arg1, arg2)
  | ArrayInit => allocate_array_init(env, arg1, arg2)
  };
};

let compile_allocation = (env, alloc_type) =>
  switch (alloc_type) {
  | MClosure(cdata) => allocate_closure(env, cdata)
  | MTuple(elts) => allocate_tuple(env, elts)
  | MBox(elt) => allocate_box(env, elt)
  | MArray(elts) => allocate_array(env, elts)
  | [@implicit_arity] MRecord(ttag, elts) => allocate_record(env, ttag, elts)
  | MString(str) => allocate_string(env, str)
  | [@implicit_arity] MADT(ttag, vtag, elts) =>
    allocate_adt(env, ttag, vtag, elts)
  | MInt32(i) => allocate_int32(env, i)
  | MInt64(i) => allocate_int64(env, i)
  };

let collect_backpatches = (env, f) => {
  let nested_backpatches = ref([]);
  let res = f({...env, backpatches: nested_backpatches});
  (res, nested_backpatches^);
};

let do_backpatches = (env, backpatches) => {
  let do_backpatch = ((lam, {variables})) => {
    let get_swap = get_swap(env, 0);
    let set_swap = set_swap(env, 0);
    let preamble =
      lam
      +@ [
        Ast.Const(const_int32 @@ tag_val_of_tag_type(LambdaTagType)),
        Ast.Binary(Values.I32(Ast.IntOp.Xor)),
      ]
      @ set_swap;
    let backpatch_var = (idx, var) =>
      get_swap
      @ compile_imm(env, var)
      +@ [call_incref_backpatch(env), store(~offset=4 * (idx + 3), ())];
    preamble @ Concatlist.flatten @@ List.mapi(backpatch_var, variables);
  };
  Concatlist.flatten @@ List.map(do_backpatch, backpatches);
};

let rec compile_store = (env, binds) => {
  let process_binds = env => {
    let process_bind = ((b, instr), acc) => {
      let store_bind = compile_bind(~action=BindSet, env, b);
      let store_bind_no_incref =
        compile_bind(~action=BindSet, ~skip_incref=true, env, b);
      let get_bind = compile_bind(~action=BindGet, env, b);
      let (compiled_instr, store_bind) =
        switch (instr) {
        | MAllocate(MClosure(cdata)) => (
            allocate_closure(env, ~lambda=get_bind, cdata),
            store_bind_no_incref,
          )
        /* HACK: We expect values returned from functions to have a refcount of 1, so we don't increment it when storing */
        /* | (MCallIndirect _) */
        | MCallKnown(_)
        | MAllocate(_) => (compile_instr(env, instr), store_bind_no_incref)
        /* [TODO] I think this is wrong? See commented out line above */
        | MCallIndirect(_)
        | _ => (compile_instr(env, instr), store_bind)
        };
      (compiled_instr @ store_bind) @ acc;
    };
    List.fold_right(process_bind, binds, empty);
  };
  let (instrs, backpatches) = collect_backpatches(env, process_binds);
  instrs @ do_backpatches(env, backpatches);
}

and compile_switch = (env, arg, branches, default) => {
  /* Constructs the jump table. Assumes that branch 0 is the default */
  let create_table = (~default=0, ~offset=0, stack) => {
    let max_label = List.fold_left(max, 0, stack);
    let label_blocks =
      List.mapi((i, l) => (i + offset, l), stack)
      |> List.sort(((_, l1), (_, l2)) => compare(l1, l2));
    let default_const = add_dummy_loc @@ Int32.of_int(default);
    let matching = (i, (block, lbl)) => lbl == i;
    let get_slot = i =>
      switch (List.find_opt(matching(i), label_blocks)) {
      | None => default_const
      | Some((b, _)) => add_dummy_loc @@ Int32.of_int(b)
      };
    BatList.init(max_label + 1, get_slot);
  };
  let rec process_branches = (count, stack, bs) =>
    switch (bs) {
    | [] =>
      let inner_block_body =
        singleton(Ast.Const(const_int32(0)))
        @ compile_imm(env, arg)
        /* Tag check elided */
        @ untag_number
        +@ [
          [@implicit_arity]
          Ast.BrTable(
            create_table(~offset=1, stack),
            add_dummy_loc(Int32.of_int(0)),
          ),
        ];
      let default_block_body = compile_block(env, default);
      Concatlist.t_of_list([
        [@implicit_arity]
        Ast.Block(
          [Types.I32Type],
          Concatlist.mapped_list_of_t(
            add_dummy_loc,
            singleton(
              [@implicit_arity]
              Ast.Block(
                [Types.I32Type],
                Concatlist.mapped_list_of_t(add_dummy_loc, inner_block_body),
              ),
            )
            @ default_block_body,
          ),
        ),
      ]);
    | [(lbl, hd), ...tl] =>
      Concatlist.t_of_list([
        [@implicit_arity]
        Ast.Block(
          [Types.I32Type],
          Concatlist.mapped_list_of_t(
            add_dummy_loc,
            process_branches(count + 1, [Int32.to_int(lbl), ...stack], tl)
            @ compile_block(env, hd)
            +@ [Ast.Br(add_dummy_loc @@ Int32.of_int(count))],
          ),
        ),
      ])
    };
  let processed = process_branches(0, [], branches);
  singleton(
    [@implicit_arity]
    Ast.Block(
      [Types.I32Type],
      Concatlist.mapped_list_of_t(add_dummy_loc, processed),
    ),
  );
}

and compile_block = (env, block) =>
  Concatlist.flatten(List.map(compile_instr(env), block))

and compile_instr = (env, instr) =>
  switch (instr) {
  | MDrop => singleton(Ast.Drop)
  | MTracepoint(x) => tracepoint(env, x)
  | MImmediate(imm) => compile_imm(env, imm)
  | MAllocate(alloc) => compile_allocation(env, alloc)
  | [@implicit_arity] MTupleOp(tuple_op, tup) =>
    compile_tuple_op(env, tup, tuple_op)
  | [@implicit_arity] MBoxOp(box_op, box) => compile_box_op(env, box, box_op)
  | [@implicit_arity] MArrayOp(array_op, ret) =>
    compile_array_op(env, ret, array_op)
  | [@implicit_arity] MAdtOp(adt_op, adt) => compile_adt_op(env, adt, adt_op)
  | [@implicit_arity] MRecordOp(record_op, record) =>
    compile_record_op(env, record, record_op)
  | [@implicit_arity] MPrim1(p1, arg) => compile_prim1(env, p1, arg)
  | [@implicit_arity] MPrim2(p2, arg1, arg2) =>
    compile_prim2(env, p2, arg1, arg2)
  | [@implicit_arity] MSwitch(arg, branches, default) =>
    compile_switch(env, arg, branches, default)
  | MStore(binds) => compile_store(env, binds)

  | [@implicit_arity] MCallIndirect(func, args) =>
    call_lambda(env, func, args)

  | [@implicit_arity] MIf(cond, thn, els) =>
    let compiled_cond = compile_imm(env, cond);
    let compiled_thn =
      Concatlist.mapped_list_of_t(add_dummy_loc, compile_block(env, thn));
    let compiled_els =
      Concatlist.mapped_list_of_t(add_dummy_loc, compile_block(env, els));
    compiled_cond
    @ decode_bool
    +@ [
      [@implicit_arity] Ast.If([Types.I32Type], compiled_thn, compiled_els),
    ];

  | [@implicit_arity] MWhile(cond, body) =>
    let compiled_cond = compile_block(env, cond);
    let compiled_body = compile_block(env, body);
    singleton(
      [@implicit_arity]
      Ast.Block(
        [Types.I32Type],
        Concatlist.mapped_list_of_t(add_dummy_loc) @@
        singleton(
          [@implicit_arity]
          Ast.Loop(
            [Types.I32Type],
            Concatlist.mapped_list_of_t(add_dummy_loc) @@
            singleton(Ast.Const(const_void))
            @ compiled_cond
            @ decode_bool
            +@ [
              Ast.Test(Values.I32(Ast.IntOp.Eqz)),
              Ast.BrIf(add_dummy_loc @@ Int32.of_int(1)),
            ]
            @ safe_drop(env)
            @ compiled_body
            +@ [Ast.Br(add_dummy_loc @@ Int32.of_int(0))],
          ),
        ),
      ),
    );

  | [@implicit_arity] MError(err, args) => call_error_handler(env, err, args)
  | [@implicit_arity] MCallKnown(func_idx, args) =>
    let compiled_args =
      Concatlist.flatten @@ List.map(compile_imm(env), args);
    compiled_args
    +@ [
      Ast.Call(
        add_dummy_loc(
          Int32.of_int(env.import_func_offset + Int32.to_int(func_idx)),
        ),
      ),
    ];
  | MArityOp(_) => failwith("NYI: (compile_instr): MArityOp")
  | MTagOp(_) => failwith("NYI: (compile_instr): MTagOp")
  };

let compile_function = (env, {index, arity, stack_size, body: body_instrs}) => {
  let arity_int = Int32.to_int(arity);
  let body_env = {...env, num_args: arity_int, stack_size};
  let body =
    Concatlist.mapped_list_of_t(add_dummy_loc) @@
    compile_block(body_env, body_instrs)
    @ cleanup_locals(body_env)
    +@ [Ast.Return];
  open Wasm.Ast;
  let ftype_idx = get_arity_func_type_idx(env, arity_int);
  let ftype = add_dummy_loc(Int32.(of_int(ftype_idx)));
  let locals =
    List.append(swap_slots) @@ BatList.init(stack_size, n => Types.I32Type);
  add_dummy_loc({ftype, locals, body});
};

let compute_table_size = (env, {imports, exports, functions}) =>
  List.length(functions)
  + (List.length(imports) - List.length(runtime_global_imports))
  + 1;

let compile_imports = (env, {imports} as prog) => {
  let compile_asm_type = t =>
    switch (t) {
    | I32Type => Types.I32Type
    | I64Type => Types.I64Type
    | F32Type => Types.F32Type
    | F64Type => Types.F64Type
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
    /* TODO: When user import become a thing, we'll need to worry about hygiene */
    let module_name =
      encode_string @@ compile_module_name(mimp_mod, mimp_kind);
    let item_name =
      encode_string @@ compile_import_name(mimp_name, mimp_kind);
    let idesc =
      switch (mimp_kind, mimp_type) {
      | (MImportGrain, MGlobalImport(typ)) =>
        let typ = compile_asm_type(typ);
        let func_type = [@implicit_arity] Types.FuncType([], [typ]);
        add_dummy_loc @@
        Ast.FuncImport(
          add_dummy_loc @@ Int32.of_int @@ get_func_type_idx(env, func_type),
        );
      | (_, [@implicit_arity] MFuncImport(args, ret)) =>
        let proc_list = List.map(compile_asm_type);
        let func_type =
          [@implicit_arity] Types.FuncType(proc_list(args), proc_list(ret));
        add_dummy_loc @@
        Ast.FuncImport(
          add_dummy_loc @@ Int32.of_int @@ get_func_type_idx(env, func_type),
        );
      | (_, MGlobalImport(typ)) =>
        let typ = compile_asm_type(typ);
        let imptyp = [@implicit_arity] Types.GlobalType(typ, Types.Immutable);
        add_dummy_loc @@ Ast.GlobalImport(imptyp);
      };

    Wasm.Ast.(add_dummy_loc({module_name, item_name, idesc}));
  };
  let table_size = compute_table_size(env, prog);
  let imports = List.map(compile_import, imports);
  List.append(
    imports,
    [
      add_dummy_loc({
        Ast.module_name: encode_string(Ident.name(runtime_mod)),
        Ast.item_name: encode_string("mem"),
        Ast.idesc:
          add_dummy_loc(
            Ast.MemoryImport(
              Types.MemoryType({Types.min: Int32.zero, Types.max: None}),
            ),
          ),
      }),
      add_dummy_loc({
        Ast.module_name: encode_string(Ident.name(runtime_mod)),
        Ast.item_name: encode_string("tbl"),
        Ast.idesc:
          add_dummy_loc(
            Ast.TableImport(
              [@implicit_arity]
              Types.TableType(
                {Types.min: Int32.of_int(table_size), Types.max: None},
                Types.FuncRefType,
              ),
            ),
          ),
      }),
    ],
  );
};

let compile_exports = (env, {functions, imports, exports, num_globals}) => {
  let compile_getter = (i, {ex_name, ex_global_index, ex_getter_index}) => {
    let exported_name = "GRAIN$EXPORT$GET$" ++ Ident.name(ex_name);
    let name = encode_string(exported_name);
    let fidx = Int32.to_int(ex_getter_index) + env.func_offset;
    let export =
      Wasm.Ast.(
        add_dummy_loc({
          name,
          edesc:
            add_dummy_loc(
              Ast.FuncExport(add_dummy_loc @@ Int32.of_int(fidx)),
            ),
        })
      );
    export;
  };

  let compile_lambda_export = (i, _) => {
    let name = encode_string("GRAIN$LAM_" ++ string_of_int(i));
    let edesc =
      add_dummy_loc(
        Ast.FuncExport(add_dummy_loc @@ Int32.of_int(i + env.func_offset)),
      );
    Wasm.Ast.(add_dummy_loc({name, edesc}));
  };
  let main_idx = env.func_offset + List.length(functions);
  let cleanup_globals_idx = main_idx + 1;
  let main_idx = add_dummy_loc @@ Int32.of_int(main_idx);
  let cleanup_globals_idx =
    add_dummy_loc @@ Int32.of_int(cleanup_globals_idx);
  let compiled_lambda_exports = List.mapi(compile_lambda_export, functions);
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
  let compiled_exports = List.mapi(compile_getter, exports);
  List.append(
    compiled_lambda_exports,
    List.append(
      compiled_exports,
      [
        add_dummy_loc({
          Ast.name: encode_string("_start"),
          Ast.edesc: add_dummy_loc(Ast.FuncExport(main_idx)),
        }),
        add_dummy_loc({
          Ast.name: encode_string("GRAIN$CLEANUP_GLOBALS"),
          Ast.edesc: add_dummy_loc(Ast.FuncExport(cleanup_globals_idx)),
        }),
        add_dummy_loc({
          Ast.name: encode_string(Ident.name(table_size)),
          Ast.edesc:
            add_dummy_loc(
              Ast.GlobalExport(
                add_dummy_loc @@
                Int32.of_int(
                  num_globals + 1 + List.length(runtime_global_imports),
                ),
              ),
            ),
        }),
        add_dummy_loc({
          Ast.name: encode_string("memory"),
          Ast.edesc:
            add_dummy_loc(
              Ast.MemoryExport(add_dummy_loc @@ Int32.of_int(0)),
            ),
        }),
      ],
    ),
  );
};

let compile_tables = (env, prog) =>
  /*let table_size = compute_table_size env prog in*/
  [];
    /*add_dummy_loc {
        Ast.ttype=Types.TableType({
            Types.min=Int32.of_int table_size;
            Types.max=None;
          }, Types.FuncRefType)
      };*/

let compile_elems = (env, prog) => {
  let table_size = compute_table_size(env, prog);
  Wasm.Ast.[
    add_dummy_loc({
      index: add_dummy_loc(Int32.zero),
      offset:
        add_dummy_loc([
          add_dummy_loc(Ast.GlobalGet(add_dummy_loc @@ Int32.of_int(0))),
        ]),
      init: BatList.init(table_size, n => add_dummy_loc(Int32.of_int(n))),
    }),
  ];
};

let compile_globals = (env, {num_globals} as prog) =>
  List.append(
    BatList.init(1 + num_globals, _ =>
      add_dummy_loc({
        Ast.gtype:
          [@implicit_arity] Types.GlobalType(Types.I32Type, Types.Mutable),
        Ast.value:
          add_dummy_loc([add_dummy_loc @@ Ast.Const(const_int32(0))]),
      })
    ),
    [
      add_dummy_loc({
        Ast.gtype:
          [@implicit_arity] Types.GlobalType(Types.I32Type, Types.Immutable),
        Ast.value:
          add_dummy_loc([
            add_dummy_loc @@
            Ast.Const(const_int32(compute_table_size(env, prog))),
          ]),
      }),
    ],
  );

let compile_main = (env, prog) => {
  let ret =
    compile_function(
      env,
      {
        index: Int32.of_int(-99),
        arity: Int32.zero,
        body: prog.main_body,
        stack_size: prog.main_body_stack_size,
      },
    );

  ret;
};

let compile_global_cleanup_function = (env, num_globals) => {
  let body =
    Concatlist.mapped_list_of_t(add_dummy_loc) @@
    List.fold_right(
      Concatlist.append,
      BatList.init(num_globals, n =>
        singleton(Ast.GlobalGet(add_dummy_loc @@ Int32.of_int(n)))
        +@ [call_decref_cleanup_globals(env), Ast.Drop]
      ),
      Concatlist.t_of_list([Ast.Const(wrap_int32(Int32.zero)), Ast.Return]),
    );
  open Wasm.Ast;
  let ftype_idx = get_arity_func_type_idx(env, 0);
  let ftype = add_dummy_loc(Int32.(of_int(ftype_idx)));
  add_dummy_loc({ftype, locals: [], body});
};

let compile_functions = (env, {functions, num_globals} as prog) => {
  let compiled_funcs = List.map(compile_function(env), functions);
  let main = compile_main(env, prog);
  let cleanup_globals = compile_global_cleanup_function(env, num_globals);
  List.append(compiled_funcs, [main, cleanup_globals]);
};

exception
  WasmRunnerError(
    option(string),
    Wasm.Source.region,
    string,
    Wasm.Ast.module_,
  );

let reparse_module = (module_: Wasm.Ast.module_) => {
  open Wasm.Source;
  let as_str = Wasm.Sexpr.to_string(80, Wasm.Arrange.module_(module_));
  let {it: script} = Wasm.Parse.string_to_module(as_str);
  switch (script) {
  | Wasm.Script.Textual(m) => m
  | Encoded(_) =>
    failwith(
      "Internal error: reparse_module: Returned Encoded (should be impossible)",
    )
  | Quoted(_) =>
    failwith(
      "Internal error: reparse_module: Returned Quoted (should be impossible)",
    )
  };
};

let validate_module = (~name=?, module_: Wasm.Ast.module_) =>
  try(Valid.check_module(module_)) {
  | [@implicit_arity] Wasm.Valid.Invalid(region, msg) =>
    /* Re-parse module in order to get actual locations */
    let reparsed = reparse_module(module_);
    try(Valid.check_module(reparsed)) {
    | [@implicit_arity] Wasm.Valid.Invalid(region, msg) =>
      raise([@implicit_arity] WasmRunnerError(name, region, msg, reparsed))
    };
    raise(
      [@implicit_arity]
      WasmRunnerError(
        name,
        region,
        Printf.sprintf("WARNING: Did not re-raise after reparse: %s", msg),
        module_,
      ),
    );
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
    let register = tbl => {
      let tbl =
        switch (Ident.find_same_opt(mimp_mod, tbl)) {
        | None => Ident.add(mimp_mod, Ident.empty, tbl)
        | Some(_) => tbl
        };
      Ident.add(
        mimp_mod,
        Ident.add(
          mimp_name,
          Int32.of_int(rt_idx),
          Ident.find_same(mimp_mod, tbl),
        ),
        tbl,
      );
    };

    let (imported_funcs, imported_globals) =
      switch (mimp_type) {
      | MFuncImport(_) => (
          register(acc_env.imported_funcs),
          acc_env.imported_globals,
        )
      | MGlobalImport(_) => (
          acc_env.imported_funcs,
          register(acc_env.imported_globals),
        )
      };
    {...acc_env, imported_funcs, imported_globals};
  };
  let import_offset = List.length(runtime_imports);
  let import_func_offset = List.length(runtime_function_imports);
  let import_global_offset = import_offset + List.length(imports);

  let new_imports = List.append(runtime_imports, imports);
  let new_env =
    BatList.fold_lefti(
      process_import(~is_runtime_import=true),
      env,
      runtime_global_imports,
    );
  let new_env =
    BatList.fold_lefti(
      process_import(~is_runtime_import=true),
      new_env,
      runtime_function_imports,
    );
  let new_env =
    BatList.fold_lefti(
      process_import(~dynamic_offset=import_global_offset),
      new_env,
      imports,
    );
  let global_offset = import_global_offset;
  let func_offset = global_offset - List.length(runtime_global_imports);
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
  open Wasm.Ast;
  let env =
    switch (env) {
    | None => init_codegen_env()
    | Some(e) => e
    };
  let (env, prog) = prepare(env, prog);
  let funcs = compile_functions(env, prog);
  let imports = compile_imports(env, prog);
  let exports = compile_exports(env, prog);
  let globals = compile_globals(env, prog);
  let tables = compile_tables(env, prog);
  let elems = compile_elems(env, prog);
  let types = List.map(add_dummy_loc, BatDeque.to_list(env.func_types^));
  let ret =
    add_dummy_loc({
      ...empty_module,
      funcs,
      imports,
      exports,
      globals,
      tables,
      elems,
      types,
      start: None,
    });
  validate_module(~name?, ret);
  ret;
};

let module_to_string = compiled_module =>
  /* Print module to string */
  Wasm.Sexpr.to_string(80) @@ Wasm.Arrange.module_(compiled_module);

let () =
  Printexc.register_printer(exc =>
    switch (exc) {
    | [@implicit_arity] WasmRunnerError(name, region, str, module_) =>
      let formatted_name =
        switch (name) {
        | None => "<unknown>"
        | Some(n) => n
        };
      let fmt_module = (_, m) =>
        Wasm.Sexpr.to_string(80, Wasm.Arrange.module_(m));
      let s =
        Printf.sprintf(
          "WASM Runner Exception at %s <%s>: '%s'\n%a\n",
          Wasm.Source.string_of_region(region),
          formatted_name,
          str,
          fmt_module,
          module_,
        );
      Some(s);
    | _ => None
    }
  );
