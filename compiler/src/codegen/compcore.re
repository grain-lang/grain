open Grain_typed;
open Mashtree;
open Linkedtree;
open Value_tags;
open Binaryen;
open Concatlist; /* NOTE: This import shadows (@) and introduces (@+) and (+@) */
open Grain_utils;
open Comp_utils;
open Comp_wasm_prim;

module StringSet = Set.Make(String);

let sources: ref(list((Expression.t, Grain_parsing.Location.t))) = ref([]);

/** Environment */

type codegen_env = {
  name: option(string),
  dep_id: int,
  func_debug_idx: int,
  num_args: int,
  num_closure_args: int,
  stack_size,
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Expression.t, closure_data))),
  required_imports: list(import),
  foreign_import_resolutions: ref(StringSet.t),
  global_import_resolutions: Hashtbl.t(string, string),
  func_import_resolutions: Hashtbl.t(string, string),
  compilation_mode: Config.compilation_mode,
  types: core_reftypes,
}

and core_reftypes = {
  grain_value: Type.t,
  grain_tuple: Type.t,
  grain_array: Type.t,
  grain_record: Type.t,
  grain_variant: Type.t,
  grain_closure: Type.t,
  grain_string: Type.t,
  grain_bytes: Type.t,
  grain_number: Type.t,
  grain_int64: Type.t,
  grain_float64: Type.t,
  grain_rational: Type.t,
  grain_big_int: Type.t,
  grain_int32: Type.t,
  grain_float32: Type.t,
  grain_uint32: Type.t,
  grain_uint64: Type.t,
};

let gensym_counter = ref(0);
let gensym_label = s => {
  gensym_counter := gensym_counter^ + 1;
  Printf.sprintf("%s.%d", s, gensym_counter^);
};
let reset_labels = () => gensym_counter := 0;

let data_segments: ref(list(Memory.segment)) = ref([]);
let push_data_segment = segment => {
  data_segments := [segment, ...data_segments^];
};
let reset_data_segments = () => data_segments := [];

/* Number of swap variables to allocate */
let swap_slots_ptr = [|ref_any(), ref_any(), ref_any()|];
let swap_slots_i32 = [|Type.int32, Type.int32, Type.int32|];
let swap_slots_i64 = [|Type.int64|];
let swap_slots_f32 = [|Type.float32|];
let swap_slots_f64 = [|Type.float64|];
let swap_ptr_offset = 0;
let swap_i32_offset = Array.length(swap_slots_ptr);
let swap_i64_offset = swap_i32_offset + Array.length(swap_slots_i32);
let swap_f32_offset = swap_i64_offset + Array.length(swap_slots_i64);
let swap_f64_offset = swap_f32_offset + Array.length(swap_slots_f32);
let swap_slots =
  Array.concat([
    swap_slots_ptr,
    swap_slots_i32,
    swap_slots_i64,
    swap_slots_f32,
    swap_slots_f64,
  ]);

/* The Grain environment */
let grain_env_mod = grain_env_name;
let runtime_heap_start_name =
  Ident.unique_name(Ident.create_persistent("runtimeHeapStart"));
let runtime_heap_next_ptr_name =
  Ident.unique_name(Ident.create_persistent("runtimeHeapNextPtr"));
let metadata_ptr_name =
  Ident.unique_name(Ident.create_persistent("metadataPtr"));

/* Memory allocation */
let malloc_name = Ident.unique_name(Ident.create_persistent("malloc"));

/* Garbage collection */
let incref_name = Ident.unique_name(Ident.create_persistent("incRef"));
let decref_name = Ident.unique_name(Ident.create_persistent("decRef"));

/* Exceptions */
let panic_with_exception_name =
  Ident.unique_name(Ident.create_persistent("panicWithException"));

/* Equality checking */
let equal_name = Ident.unique_name(Ident.create_persistent("equal"));

let build_func_type = (args, rets) => {
  let builder = Type_builder.make(1);
  Type_builder.set_signature_type(builder, 0, Type.create(args), rets);
  switch (Type_builder.build_and_dispose(builder)) {
  | Ok([ty]) => ty
  | _ => assert(false)
  };
};

let build_basic_func_type = arity => {
  build_func_type(Array.make(arity + 1, ref_any()), ref_any());
};

let build_array_type =
    (~packed_type=Packed_type.not_packed, ~mutable_=false, ty) => {
  let builder = Type_builder.make(1);
  Type_builder.set_array_type(builder, 0, ty, packed_type, mutable_);
  switch (Type_builder.build_and_dispose(builder)) {
  | Ok([ty]) => ty
  | _ => assert(false)
  };
};

let init_codegen_env =
    (~global_import_resolutions, ~func_import_resolutions, wasm_mod, name) => {
  let grain_value = {
    let builder = Type_builder.make(1);
    Type_builder.set_struct_type(
      builder,
      0,
      Type_builder.[
        {
          type_: Type.int32,
          packed_type: Packed_type.not_packed,
          mutable_: false,
        },
      ],
    );
    Type_builder.set_open(builder, 0);
    switch (Type_builder.build_and_dispose(builder)) {
    | Ok([ty]) => ty
    | _ => assert(false)
    };
  };
  let build_subtype = (~open_=false, ~supertype=grain_value, fields) => {
    let builder = Type_builder.make(1);
    Type_builder.set_struct_type(
      builder,
      0,
      Type_builder.[
        {
          type_: Type.int32,
          packed_type: Packed_type.not_packed,
          mutable_: false,
        },
        ...fields,
      ],
    );
    Type_builder.set_sub_type(builder, 0, supertype);
    if (open_) {
      Type_builder.set_open(builder, 0);
    };
    switch (Type_builder.build_and_dispose(builder)) {
    | Ok([ty]) => ty
    | _ => assert(false)
    };
  };
  let field = (~packed_type=Packed_type.not_packed, ~mutable_=false, type_) =>
    Type_builder.{type_, packed_type, mutable_};

  let grain_tuple =
    build_subtype([field(build_array_type(~mutable_=true, ref_any()))]);
  let grain_array =
    build_subtype([field(build_array_type(~mutable_=true, ref_any()))]);
  let grain_record =
    build_subtype([
      field(ref_i31()),
      field(ref_i31()),
      field(build_array_type(~mutable_=true, ref_any())),
    ]);
  let grain_variant =
    build_subtype([
      field(ref_i31()),
      field(ref_i31()),
      field(ref_i31()),
      field(build_array_type(~mutable_=true, ref_any())),
    ]);
  let grain_closure =
    build_subtype([
      field(Type.funcref),
      field(build_array_type(~mutable_=true, ref_any())),
    ]);
  let grain_string =
    build_subtype([
      field(
        build_array_type(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          Type.int32,
        ),
      ),
    ]);
  let grain_bytes =
    build_subtype([
      field(
        build_array_type(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          Type.int32,
        ),
      ),
    ]);
  let grain_number = build_subtype(~open_=true, [field(Type.int32)]);
  let grain_int64 =
    build_subtype(
      ~supertype=grain_number,
      [field(Type.int32), field(Type.int64)],
    );
  let grain_float64 =
    build_subtype(
      ~supertype=grain_number,
      [field(Type.int32), field(Type.float64)],
    );
  let grain_rational =
    build_subtype(
      ~supertype=grain_number,
      [field(Type.int32), field(ref_any()), field(ref_any())],
    );
  let grain_big_int =
    build_subtype(
      ~supertype=grain_number,
      [
        field(Type.int32),
        field(~mutable_=true, ~packed_type=Packed_type.int8, Type.int32),
        field(build_array_type(~mutable_=true, Type.int64)),
      ],
    );
  let grain_int32 = build_subtype([field(Type.int32)]);
  let grain_float32 = build_subtype([field(Type.float32)]);
  let grain_uint32 = build_subtype([field(Type.int32)]);
  let grain_uint64 = build_subtype([field(Type.int64)]);

  {
    name,
    dep_id: 0,
    func_debug_idx: 0,
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
    foreign_import_resolutions: ref(StringSet.empty),
    global_import_resolutions,
    func_import_resolutions,
    compilation_mode: Normal,
    types: {
      grain_value,
      grain_tuple,
      grain_array,
      grain_record,
      grain_variant,
      grain_closure,
      grain_string,
      grain_bytes,
      grain_number,
      grain_int64,
      grain_float64,
      grain_rational,
      grain_big_int,
      grain_int32,
      grain_float32,
      grain_uint32,
      grain_uint64,
    },
  };
};

// Generates the name for the linked symbol. Symbols are scoped using the
// dependency ID for that module.
let linked_name = (~env, id) => {
  Printf.sprintf("%s_%d", id, env.dep_id);
};

let rec resolve_global = (~env, name) => {
  switch (Hashtbl.find_opt(env.global_import_resolutions, name)) {
  | Some(resolution) => resolve_global(~env, resolution)
  | _ => name
  };
};

let rec resolve_func = (~env, name) => {
  switch (Hashtbl.find_opt(env.func_import_resolutions, name)) {
  | Some(resolution) => resolve_func(~env, resolution)
  | _ => name
  };
};

let reset = () => {
  reset_labels();
  reset_data_segments();
};

let get_runtime_heap_start = wasm_mod =>
  Expression.Global_get.make(wasm_mod, runtime_heap_start_name, Type.int32);

let get_metadata_ptr = wasm_mod =>
  Expression.Global_get.make(wasm_mod, metadata_ptr_name, Type.int32);

let get_grain_imported_name = (mod_, name) => Ident.unique_name(name);

let call_panic_handler = (wasm_mod, env, args) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      resolve_global(~env, panic_with_exception_name),
      ref_any(),
    ),
    ...args,
  ];
  Expression.Call.make(
    wasm_mod,
    resolve_func(~env, panic_with_exception_name),
    args,
    ref_any(),
  );
};

let call_malloc = (wasm_mod, env, args) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      resolve_global(~env, malloc_name),
      Type.int32,
    ),
    ...args,
  ];
  Expression.Call.make(
    wasm_mod,
    resolve_func(~env, malloc_name),
    args,
    Type.int32,
  );
};
let call_incref = (wasm_mod, env, arg) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      resolve_global(~env, incref_name),
      Type.int32,
    ),
    arg,
  ];
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.Call.make(
      wasm_mod,
      resolve_func(~env, incref_name),
      args,
      Type.int32,
    );
  };
};
let call_decref = (wasm_mod, env, arg) => {
  let args = [
    Expression.Global_get.make(
      wasm_mod,
      resolve_global(~env, decref_name),
      Type.int32,
    ),
    arg,
  ];
  if (Config.no_gc^) {
    arg;
  } else {
    Expression.Call.make(
      wasm_mod,
      resolve_func(~env, decref_name),
      args,
      Type.int32,
    );
  };
};
let call_equal = (wasm_mod, env, args) =>
  Expression.Call.make(
    wasm_mod,
    resolve_func(~env, equal_name),
    [
      call_incref(wasm_mod, env) @@
      Expression.Global_get.make(
        wasm_mod,
        resolve_global(~env, equal_name),
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
    Expression.I31.get(
      wasm_mod,
      Expression.Ref.cast(wasm_mod, value, ref_i31()),
      true,
    ),
    Expression.Const.make(wasm_mod, const_int32(1)),
  );

let tag_number = (wasm_mod, value) =>
  Expression.I31.make(
    wasm_mod,
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
    ),
  );

let encode_bool = (wasm_mod, value) =>
  Expression.I31.make(
    wasm_mod,
    Expression.Binary.make(
      wasm_mod,
      Op.or_int32,
      Expression.Binary.make(
        wasm_mod,
        Op.shl_int32,
        value,
        Expression.Const.make(wasm_mod, const_int32(30)),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(Int32.to_int(Mashtree.const_false)),
      ),
    ),
  );

let decode_bool = (wasm_mod, value) =>
  Expression.Binary.make(
    wasm_mod,
    Op.shr_u_int32,
    Expression.I31.get(
      wasm_mod,
      Expression.Ref.cast(wasm_mod, value, ref_i31()),
      false,
    ),
    Expression.Const.make(wasm_mod, const_int32(30)),
  );

let encoded_const_int32 = n => const_int32(encoded_int32(n));

type bind_action =
  | BindGet
  | BindSet({value: Expression.t})
  | BindTee({value: Expression.t});

let compile_bind =
    (~action, wasm_mod: Module.t, env: codegen_env, b: binding): Expression.t => {
  let get_slot = (slot, typ) => {
    Expression.Local_get.make(wasm_mod, slot, typ);
  };
  let set_slot = (slot, arg) => {
    Expression.Local_set.make(wasm_mod, slot, arg);
  };
  let tee_slot = (slot, typ, arg) => {
    Expression.Local_tee.make(wasm_mod, slot, arg, typ);
  };
  switch (b) {
  | MArgBind(i, alloc) =>
    /* No adjustments are needed for argument bindings */
    let typ = wasm_type(alloc);
    let slot = Int32.to_int(i);
    switch (action) {
    | BindGet => get_slot(slot, typ)
    | BindSet({value}) => set_slot(slot, value)
    | BindTee({value}) => tee_slot(slot, typ, value)
    };
  | MClosureBind(i) =>
    /* Closure bindings need to be offset to account for arguments */
    let slot = env.num_args + Int32.to_int(i);
    switch (action) {
    | BindGet => get_slot(slot, ref_any())
    | BindSet({value}) => set_slot(slot, value)
    | BindTee({value}) => tee_slot(slot, ref_any(), value)
    };
  | MLocalBind(i, alloc) =>
    /* Local bindings need to be offset to account for arguments, closure arguments, and swap variables */
    let typ = wasm_type(alloc);
    let slot =
      switch (alloc) {
      | Types.GrainValue(_)
      | Types.WasmValue(WasmRef) =>
        env.num_args
        + env.num_closure_args
        + Array.length(swap_slots)
        + Int32.to_int(i)
      | Types.WasmValue(WasmI32) =>
        env.num_args
        + env.num_closure_args
        + Array.length(swap_slots)
        + env.stack_size.stack_size_ptr
        + Int32.to_int(i)
      | Types.WasmValue(WasmI64) =>
        env.num_args
        + env.num_closure_args
        + Array.length(swap_slots)
        + env.stack_size.stack_size_ptr
        + env.stack_size.stack_size_i32
        + Int32.to_int(i)
      | Types.WasmValue(WasmF32) =>
        env.num_args
        + env.num_closure_args
        + Array.length(swap_slots)
        + env.stack_size.stack_size_ptr
        + env.stack_size.stack_size_i32
        + env.stack_size.stack_size_i64
        + Int32.to_int(i)
      | Types.WasmValue(WasmF64) =>
        env.num_args
        + env.num_closure_args
        + Array.length(swap_slots)
        + env.stack_size.stack_size_ptr
        + env.stack_size.stack_size_i32
        + env.stack_size.stack_size_i64
        + env.stack_size.stack_size_f32
        + Int32.to_int(i)
      };
    switch (action) {
    | BindGet => get_slot(slot, typ)
    | BindSet({value}) => set_slot(slot, value)
    | BindTee({value}) => tee_slot(slot, typ, value)
    };
  | MSwapBind(i, wasm_ty) =>
    /* Swap bindings need to be offset to account for arguments and closure arguments */
    let slot = env.num_args + env.num_closure_args + Int32.to_int(i);
    let typ = wasm_type(wasm_ty);
    switch (action) {
    | BindGet => get_slot(slot, typ)
    | BindSet({value}) => set_slot(slot, value)
    | BindTee({value}) => tee_slot(slot, typ, value)
    };
  | MGlobalBind(slot, wasm_ty) =>
    let slot = resolve_global(~env, linked_name(~env, slot));
    let typ = wasm_type(wasm_ty);
    switch (action) {
    | BindGet => Expression.Global_get.make(wasm_mod, slot, typ)
    | BindSet({value}) => Expression.Global_set.make(wasm_mod, slot, value)
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

let get_swap = (~ty as typ=Types.GrainValue(GrainAny), wasm_mod, env, idx) =>
  switch (typ) {
  | Types.GrainValue(_) =>
    if (idx > Array.length(swap_slots_ptr)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_ptr_offset), typ),
    );
  | Types.WasmValue(WasmI32) =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), typ),
    );
  | Types.WasmValue(WasmI64) =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), typ),
    );
  | Types.WasmValue(WasmF32) =>
    if (idx > Array.length(swap_slots_f32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f32_offset), typ),
    );
  | Types.WasmValue(WasmF64) =>
    if (idx > Array.length(swap_slots_f64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindGet,
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f64_offset), typ),
    );
  | Types.WasmValue(_) => failwith("NYI")
  };

let set_swap =
    (~ty as typ=Types.GrainValue(GrainAny), wasm_mod, env, idx, value) => {
  switch (typ) {
  | Types.GrainValue(_) =>
    if (idx > Array.length(swap_slots_ptr)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_ptr_offset), typ),
    );
  | Types.WasmValue(WasmI32) =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), typ),
    );
  | Types.WasmValue(WasmI64) =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), typ),
    );
  | Types.WasmValue(WasmF32) =>
    if (idx > Array.length(swap_slots_f32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f32_offset), typ),
    );
  | Types.WasmValue(WasmF64) =>
    if (idx > Array.length(swap_slots_f64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindSet({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f64_offset), typ),
    );
  | Types.WasmValue(_) => failwith("NYI")
  };
};

let tee_swap =
    (~ty as typ=Types.GrainValue(GrainAny), wasm_mod, env, idx, value) =>
  switch (typ) {
  | Types.GrainValue(_) =>
    if (idx > Array.length(swap_slots_ptr)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_ptr_offset), typ),
    );
  | Types.WasmValue(WasmI32) =>
    if (idx > Array.length(swap_slots_i32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i32_offset), typ),
    );
  | Types.WasmValue(WasmI64) =>
    if (idx > Array.length(swap_slots_i64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_i64_offset), typ),
    );
  | Types.WasmValue(WasmF32) =>
    if (idx > Array.length(swap_slots_f32)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f32_offset), typ),
    );
  | Types.WasmValue(WasmF64) =>
    if (idx > Array.length(swap_slots_f64)) {
      raise(Not_found);
    };
    compile_bind(
      ~action=BindTee({value: value}),
      wasm_mod,
      env,
      MSwapBind(Int32.of_int(idx + swap_f64_offset), typ),
    );
  | Types.WasmValue(_) => failwith("NYI")
  };

let rec compile_imm = (wasm_mod, env: codegen_env, i: immediate): Expression.t =>
  switch (i.immediate_desc) {
  | MImmConst(c) => compile_const(wasm_mod, c)
  | MImmBinding(b) => compile_bind(~action=BindGet, wasm_mod, env, b)
  | MImmTrap => Expression.Unreachable.make(wasm_mod)
  };

let round_to_8 = n => Int.logand(n + 7, Int.lognot(7));

/** Heap allocations. */

/** Rounds the given number of words to be aligned correctly */
let round_to_even = num_words =>
  if (num_words mod 2 == 0) {
    num_words;
  } else {
    num_words + 1;
  };

let heap_allocate = (wasm_mod, env, num_words: int) =>
  switch (env.compilation_mode) {
  | Runtime =>
    let addition =
      Expression.Binary.make(
        wasm_mod,
        Op.add_int32,
        Expression.Global_get.make(
          wasm_mod,
          runtime_heap_next_ptr_name,
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
                  runtime_heap_next_ptr_name,
                  Type.int32,
                ),
                // fake GC refcount of 2
                // this means objects (should) never drop to zero refcount
                Expression.Const.make(wasm_mod, const_int32(2)),
              ),
              Expression.Binary.make(
                wasm_mod,
                Op.add_int32,
                Expression.Global_get.make(
                  wasm_mod,
                  runtime_heap_next_ptr_name,
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
                runtime_heap_next_ptr_name,
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
  | Normal =>
    call_malloc(
      wasm_mod,
      env,
      [Expression.Const.make(wasm_mod, const_int32(4 * num_words))],
    )
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
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ADTType)),
      ),
      compile_imm(wasm_mod, env, type_hash),
      compile_imm(wasm_mod, env, ttag),
      compile_imm(wasm_mod, env, vtag),
      Expression.Array.new_fixed(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
        List.map(compile_imm(wasm_mod, env), elts),
      ),
    ]),
    Type.get_heap_type(env.types.grain_variant),
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
  let type_hash = imm(MImmConst(MConstWasmI32(0l)));
  let ty_id =
    imm(
      MImmConst(
        MConstWasmI32(
          Int32.of_int(Path.stamp(Builtin_types.path_exception)),
        ),
      ),
    );
  let cstr_id =
    imm(
      MImmConst(
        MConstWasmI32(
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
    Expression.Array.get(
      wasm_mod,
      Expression.Struct.get(
        wasm_mod,
        1,
        Expression.Ref.cast(wasm_mod, tup(), env.types.grain_tuple),
        env.types.grain_tuple,
        false,
      ),
      Expression.Const.make(wasm_mod, const_int32(idx_int)),
      ref_any(),
      false,
    );
  | MTupleSet(idx, imm) =>
    let idx_int = Int32.to_int(idx);
    Expression.Array.set(
      wasm_mod,
      Expression.Struct.get(
        wasm_mod,
        1,
        Expression.Ref.cast(wasm_mod, tup(), env.types.grain_tuple),
        env.types.grain_tuple,
        false,
      ),
      Expression.Const.make(wasm_mod, const_int32(idx_int)),
      compile_imm(wasm_mod, env, imm),
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
        const_void(wasm_mod),
      ],
    );
  };
};

let compile_adt_op = (wasm_mod, env, adt_imm, op) => {
  let adt = compile_imm(wasm_mod, env, adt_imm);
  switch (op) {
  | MAdtGet(idx) =>
    let idx_int = Int32.to_int(idx);
    Expression.Array.get(
      wasm_mod,
      Expression.Struct.get(
        wasm_mod,
        4,
        Expression.Ref.cast(wasm_mod, adt, env.types.grain_variant),
        env.types.grain_variant,
        false,
      ),
      Expression.Const.make(wasm_mod, const_int32(idx_int)),
      ref_any(),
      false,
    );
  | MAdtGetTag =>
    Expression.Struct.get(
      wasm_mod,
      2,
      Expression.Ref.cast(wasm_mod, adt, env.types.grain_variant),
      env.types.grain_variant,
      false,
    )
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
        const_void(wasm_mod),
      ],
    );
  };
};

let compile_closure_op = (wasm_mod, env, closure_imm, op) => {
  let closure = () => compile_imm(wasm_mod, env, closure_imm);
  switch (op) {
  | MClosureSetFuncRef(id, arity) =>
    Expression.Struct.set(
      wasm_mod,
      1,
      closure(),
      Expression.Ref.func(
        wasm_mod,
        linked_name(~env, Ident.unique_name(id)),
        build_basic_func_type(arity),
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
    let name = resolve_func(~env, linked_name(~env, name));
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
            (wasm_mod, ptr, args, ty) =>
              Expression.Return.make(
                wasm_mod,
                Expression.Call_ref.make(wasm_mod, ptr, args, ty),
              )
          );
        } else {
          Expression.Call_ref.make_return;
        };
      } else {
        Expression.Call_ref.make;
      };
    let get_func_swap = () => get_swap(wasm_mod, env, 0);
    let args = [get_func_swap(), ...compiled_args];

    let functype =
      build_func_type(
        Array.map(
          wasm_type,
          Array.of_list([Types.GrainValue(GrainClosure), ...argsty]),
        ),
        retty,
      );

    Expression.Block.make(
      wasm_mod,
      gensym_label("call_lambda"),
      [
        set_swap(wasm_mod, env, 0, compiled_func()),
        instr(
          wasm_mod,
          Expression.Ref.cast(
            wasm_mod,
            Expression.Struct.get(
              wasm_mod,
              1,
              Expression.Ref.cast(
                wasm_mod,
                get_func_swap(),
                env.types.grain_closure,
              ),
              env.types.grain_closure,
              false,
            ),
            functype,
          ),
          args,
          functype,
        ),
      ],
    );
  };
};

let allocate_byte_like_from_buffer = (wasm_mod, env, buf, tag, label) => {
  let grain_type =
    switch (tag) {
    | StringType => env.types.grain_string
    | BytesType => env.types.grain_bytes
    | _ => failwith("Non bytes-like type")
    };
  let array_mut_i8 =
    build_array_type(
      ~packed_type=Packed_type.int8,
      ~mutable_=true,
      Type.int32,
    );

  if (Config.bulk_memory^) {
    let segment_name = gensym_label(label);
    let data_size = Buffer.length(buf);
    Memory.(
      push_data_segment({
        name: segment_name,
        data: Buffer.to_bytes(buf),
        kind: Passive,
        size: data_size,
      })
    );

    Expression.Struct.new_(
      wasm_mod,
      Some([
        Expression.Const.make(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(tag)),
        ),
        Expression.Array.new_data(
          wasm_mod,
          Type.get_heap_type(array_mut_i8),
          segment_name,
          Expression.Const.make(wasm_mod, const_int32(0)),
          Expression.Const.make(wasm_mod, const_int32(data_size)),
        ),
      ]),
      Type.get_heap_type(grain_type),
    );
  } else {
    let data =
      List.init(Buffer.length(buf), idx =>
        Expression.Const.make(
          wasm_mod,
          const_int32(Char.code(Buffer.nth(buf, idx))),
        )
      );
    Expression.Struct.new_(
      wasm_mod,
      Some([
        Expression.Const.make(
          wasm_mod,
          const_int32(tag_val_of_heap_tag_type(tag)),
        ),
        Expression.Array.new_fixed(
          wasm_mod,
          Type.get_heap_type(array_mut_i8),
          data,
        ),
      ]),
      Type.get_heap_type(grain_type),
    );
  };
};

let allocate_byte_like_uninitialized = (wasm_mod, env, size, tag) => {
  let grain_type =
    switch (tag) {
    | StringType => env.types.grain_string
    | BytesType => env.types.grain_bytes
    | _ => failwith("Non bytes-like type")
    };
  let array_mut_i8 =
    build_array_type(
      ~packed_type=Packed_type.int8,
      ~mutable_=true,
      Type.int32,
    );
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(tag)),
      ),
      Expression.Array.new_(
        wasm_mod,
        Type.get_heap_type(array_mut_i8),
        compile_imm(wasm_mod, env, size),
        Expression.Const.make(wasm_mod, const_int32(0)),
      ),
    ]),
    Type.get_heap_type(grain_type),
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
  allocate_byte_like_uninitialized(wasm_mod, env, size, StringType);
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
  allocate_byte_like_uninitialized(wasm_mod, env, size, BytesType);
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
      {func_id, arity, variables} as closure_data,
    ) => {
  let num_free_vars = List.length(variables);
  let get_swap = () => get_swap(wasm_mod, env, 0);
  let funcref =
    switch (func_id) {
    | Some(id) =>
      Expression.Ref.func(
        wasm_mod,
        resolve_func(~env, linked_name(~env, Ident.unique_name(id))),
        build_basic_func_type(Int32.to_int(arity)),
      )
    | None => Expression.Ref.null(wasm_mod, Type.funcref)
    };
  if (skip_patching) {
    let access_lambda = Option.value(~default=get_swap(), lambda);
    env.backpatches := [(access_lambda, closure_data), ...env.backpatches^];

    tee_swap(
      wasm_mod,
      env,
      0,
      Expression.Struct.new_(
        wasm_mod,
        Some([
          Expression.Const.make(
            wasm_mod,
            const_int32(tag_val_of_heap_tag_type(LambdaType)),
          ),
          funcref,
          Expression.Array.new_(
            wasm_mod,
            Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
            Expression.Const.make(wasm_mod, const_int32(num_free_vars)),
            const_ref_0(wasm_mod),
          ),
        ]),
        Type.get_heap_type(env.types.grain_closure),
      ),
    );
  } else {
    tee_swap(
      wasm_mod,
      env,
      0,
      Expression.Struct.new_(
        wasm_mod,
        Some([
          Expression.Const.make(
            wasm_mod,
            const_int32(tag_val_of_heap_tag_type(LambdaType)),
          ),
          funcref,
          Expression.Array.new_fixed(
            wasm_mod,
            Heap_type.any(),
            List.map(compile_imm(wasm_mod, env), variables),
          ),
        ]),
        Type.get_heap_type(env.types.grain_closure),
      ),
    );
  };
};

let allocate_tuple = (wasm_mod, env, elts) => {
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(TupleType)),
      ),
      Expression.Array.new_fixed(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
        List.map(compile_imm(wasm_mod, env), elts),
      ),
    ]),
    Type.get_heap_type(env.types.grain_tuple),
  );
};

let allocate_uninitialized_tuple = (wasm_mod, env, num_elts) => {
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(TupleType)),
      ),
      Expression.Array.new_(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
        compile_imm(wasm_mod, env, num_elts),
        const_ref_0(wasm_mod),
      ),
    ]),
    Type.get_heap_type(env.types.grain_tuple),
  );
};

let allocate_box = (wasm_mod, env, elt) =>
  /* At the moment, we make no runtime distinction between boxes and tuples */
  allocate_tuple(wasm_mod, env, [elt]);

let allocate_uninitialized_array = (wasm_mod, env, num_elts) => {
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ArrayType)),
      ),
      Expression.Array.new_(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
        compile_imm(wasm_mod, env, num_elts),
        const_ref_0(wasm_mod),
      ),
    ]),
    Type.get_heap_type(env.types.grain_array),
  );
};

let allocate_array = (wasm_mod, env, elts) => {
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(ArrayType)),
      ),
      Expression.Array.new_fixed(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
        List.map(compile_imm(wasm_mod, env), elts),
      ),
    ]),
    Type.get_heap_type(env.types.grain_array),
  );
};

let allocate_record = (wasm_mod, env, type_hash, ttag, elts) => {
  let (_, elts) = List.split(elts);
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(RecordType)),
      ),
      compile_imm(wasm_mod, env, type_hash),
      compile_imm(wasm_mod, env, ttag),
      Expression.Array.new_fixed(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, ref_any())),
        List.map(compile_imm(wasm_mod, env), elts),
      ),
    ]),
    Type.get_heap_type(env.types.grain_record),
  );
};

type alloc_alt_num_type =
  | Int32(Expression.t)
  | Float32(Expression.t)
  | Uint32(Expression.t)
  | Uint64(Expression.t);

let allocate_alt_num = (wasm_mod, env, num_value) => {
  let (tag, value, grain_type) =
    switch (num_value) {
    | Int32(int32) => (Int32Type, int32, env.types.grain_int32)
    | Float32(float32) => (Float32Type, float32, env.types.grain_float32)
    | Uint32(uint32) => (Uint32Type, uint32, env.types.grain_uint32)
    | Uint64(uint64) => (Uint64Type, uint64, env.types.grain_uint64)
    };
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(tag)),
      ),
      value,
    ]),
    Type.get_heap_type(grain_type),
  );
};

type alloc_number_type =
  | Int64(Expression.t)
  | Float64(Expression.t)
  | Rational(Expression.t, Expression.t)
  | BigInt(Expression.t, list(Expression.t));

let allocate_number = (wasm_mod, env, number) => {
  let (number_tag, values, grain_type) =
    switch (number) {
    | Int64(int64) => (BoxedInt64, [int64], env.types.grain_int64)
    | Float64(float64) => (BoxedFloat64, [float64], env.types.grain_float64)
    | Rational(numerator, denominator) => (
        BoxedRational,
        [numerator, denominator],
        env.types.grain_rational,
      )
    | BigInt(flags, limbs) => (
        BoxedBigInt,
        [
          flags,
          Expression.Array.new_fixed(
            wasm_mod,
            Type.get_heap_type(build_array_type(~mutable_=true, Type.int64)),
            limbs,
          ),
        ],
        env.types.grain_big_int,
      )
    };

  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(BoxedNumberType)),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_boxed_number_tag_type(number_tag)),
      ),
      ...values,
    ]),
    Type.get_heap_type(grain_type),
  );
};

let allocate_bigint_uninitialized = (wasm_mod, env, limbs) => {
  Expression.Struct.new_(
    wasm_mod,
    Some([
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_heap_tag_type(BoxedNumberType)),
      ),
      Expression.Const.make(
        wasm_mod,
        const_int32(tag_val_of_boxed_number_tag_type(BoxedBigInt)),
      ),
      Expression.Const.make(wasm_mod, const_int32(0)),
      Expression.Array.new_(
        wasm_mod,
        Type.get_heap_type(build_array_type(~mutable_=true, Type.int64)),
        limbs,
        Expression.Const.make(wasm_mod, const_int64(0)),
      ),
    ]),
    Type.get_heap_type(env.types.grain_big_int),
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
  Expression.I31.make(
    wasm_mod,
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
    ),
  );
};

let compile_prim0 = (wasm_mod, env, p0): Expression.t => {
  switch (p0) {
  | WasmMemorySize =>
    Expression.Memory_size.make(wasm_mod, grain_memory, false)
  | Unreachable => Expression.Unreachable.make(wasm_mod)
  | HeapStart => get_runtime_heap_start(wasm_mod)
  | HeapTypeMetadata => get_metadata_ptr(wasm_mod)
  };
};

let compile_prim1 = (wasm_mod, env, p1, arg, loc): Expression.t => {
  let compiled_arg = compile_imm(wasm_mod, env, arg);
  switch (p1) {
  | AllocateArray => allocate_uninitialized_array(wasm_mod, env, arg)
  | AllocateTuple => allocate_uninitialized_tuple(wasm_mod, env, arg)
  | AllocateBytes => allocate_bytes_uninitialized(wasm_mod, env, arg)
  | AllocateString => allocate_string_uninitialized(wasm_mod, env, arg)
  | AllocateBigInt =>
    allocate_bigint_uninitialized(wasm_mod, env, compiled_arg)
  | NewInt32 => allocate_alt_num(wasm_mod, env, Int32(compiled_arg))
  | NewInt64 => allocate_number(wasm_mod, env, Int64(compiled_arg))
  | NewFloat32 => allocate_alt_num(wasm_mod, env, Float32(compiled_arg))
  | NewFloat64 => allocate_number(wasm_mod, env, Float64(compiled_arg))
  | NewUint32 => allocate_alt_num(wasm_mod, env, Uint32(compiled_arg))
  | NewUint64 => allocate_alt_num(wasm_mod, env, Uint64(compiled_arg))
  | LoadAdtVariant =>
    Expression.Struct.get(
      wasm_mod,
      2,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_variant),
      env.types.grain_variant,
      false,
    )
  | LoadValueTag =>
    Expression.Struct.get(
      wasm_mod,
      0,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_value),
      env.types.grain_value,
      false,
    )
  | StringSize =>
    Expression.Array.len(
      wasm_mod,
      Expression.Struct.get(
        wasm_mod,
        1,
        Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_string),
        env.types.grain_string,
        false,
      ),
    )
  | BytesSize =>
    Expression.Array.len(
      wasm_mod,
      Expression.Struct.get(
        wasm_mod,
        1,
        Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_bytes),
        env.types.grain_bytes,
        false,
      ),
    )
  | BigIntSize =>
    Expression.Array.len(
      wasm_mod,
      Expression.Struct.get(
        wasm_mod,
        3,
        Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_big_int),
        env.types.grain_big_int,
        false,
      ),
    )
  | BigIntFlags =>
    Expression.Struct.get(
      wasm_mod,
      2,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_big_int),
      env.types.grain_big_int,
      false,
    )
  | StringArrayRef =>
    Expression.Struct.get(
      wasm_mod,
      1,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_string),
      env.types.grain_string,
      false,
    )
  | BytesArrayRef =>
    Expression.Struct.get(
      wasm_mod,
      1,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_bytes),
      env.types.grain_bytes,
      false,
    )
  | BigIntArrayRef =>
    Expression.Struct.get(
      wasm_mod,
      3,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_big_int),
      env.types.grain_big_int,
      false,
    )
  | TagSimpleNumber =>
    Expression.I31.make(
      wasm_mod,
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
      ),
    )
  | UntagSimpleNumber =>
    Expression.Binary.make(
      wasm_mod,
      Op.shr_s_int32,
      Expression.I31.get(
        wasm_mod,
        Expression.Ref.cast(wasm_mod, compiled_arg, ref_i31()),
        true,
      ),
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
      Expression.I31.get(
        wasm_mod,
        Expression.Ref.cast(wasm_mod, compiled_arg, ref_i31()),
        true,
      ),
      Expression.Const.make(wasm_mod, const_int32(0x8)),
    )
  | BoxedNumberTag =>
    Expression.Struct.get(
      wasm_mod,
      1,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_number),
      env.types.grain_number,
      false,
    )
  | BoxedInt32Value =>
    Expression.Struct.get(
      wasm_mod,
      1,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_int32),
      env.types.grain_int32,
      false,
    )
  | BoxedUint32Value =>
    Expression.Struct.get(
      wasm_mod,
      1,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_uint32),
      env.types.grain_uint32,
      false,
    )
  | BoxedFloat32Value =>
    Expression.Struct.get(
      wasm_mod,
      1,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_float32),
      env.types.grain_float32,
      false,
    )
  | BoxedInt64Value =>
    Expression.Struct.get(
      wasm_mod,
      2,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_int64),
      env.types.grain_int64,
      false,
    )
  | BoxedFloat64Value =>
    Expression.Struct.get(
      wasm_mod,
      2,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_float64),
      env.types.grain_float64,
      false,
    )
  | BoxedRationalNumerator =>
    Expression.Struct.get(
      wasm_mod,
      2,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_rational),
      env.types.grain_rational,
      false,
    )
  | BoxedRationalDenominator =>
    Expression.Struct.get(
      wasm_mod,
      3,
      Expression.Ref.cast(wasm_mod, compiled_arg, env.types.grain_rational),
      env.types.grain_rational,
      false,
    )
  | IsRefI31 =>
    encode_bool(
      wasm_mod,
      Expression.Ref.test(wasm_mod, compiled_arg, ref_i31()),
    )
  | IsGrainHeapValue =>
    encode_bool(
      wasm_mod,
      Expression.Ref.test(wasm_mod, compiled_arg, env.types.grain_value),
    )
  | I31Get({signed}) =>
    Expression.I31.get(
      wasm_mod,
      Expression.Ref.cast(wasm_mod, compiled_arg, ref_i31()),
      signed,
    )
  | Not =>
    /* Flip the second bit */
    Expression.I31.make(
      wasm_mod,
      Expression.Binary.make(
        wasm_mod,
        Op.xor_int32,
        Expression.I31.get(
          wasm_mod,
          Expression.Ref.cast(wasm_mod, compiled_arg, ref_i31()),
          false,
        ),
        Expression.Const.make(wasm_mod, const_int32(0x40000000)),
      ),
    )
  | Ignore =>
    Expression.Block.make(
      wasm_mod,
      gensym_label("Ignore"),
      [Expression.Drop.make(wasm_mod, compiled_arg), const_void(wasm_mod)],
    )
  | ArrayLength => compile_array_op(wasm_mod, env, arg, MArrayLength)
  | Throw =>
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
  | WasmRefArrayLen =>
    Expression.Array.len(
      wasm_mod,
      Expression.Ref.cast(
        wasm_mod,
        compile_imm(wasm_mod, env, arg),
        Type.from_heap_type(Heap_type.array(), false),
      ),
    )
  };
};

let compile_wasm_load =
    (~sz=?, ~ty=?, ~signed=?, wasm_mod, compiled_arg1, compiled_arg2, offset) => {
  switch (offset.immediate_desc) {
  | MImmConst(MConstWasmI32(offset)) =>
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
  | MImmConst(MConstWasmI32(offset)) =>
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
        const_void(wasm_mod),
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
        const_void(wasm_mod),
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
      Expression.Ref.eq(
        wasm_mod,
        Expression.Ref.cast(
          wasm_mod,
          compiled_arg1(),
          Type.from_heap_type(Heap_type.eq(), false),
        ),
        Expression.Ref.cast(
          wasm_mod,
          compiled_arg2(),
          Type.from_heap_type(Heap_type.eq(), false),
        ),
      ),
    )
  | NewRational =>
    allocate_number(
      wasm_mod,
      env,
      Rational(compiled_arg1(), compiled_arg2()),
    )
  | BigIntSetFlags =>
    Expression.Block.make(
      wasm_mod,
      gensym_label("bigint_set_flags"),
      [
        Expression.Struct.set(
          wasm_mod,
          2,
          Expression.Ref.cast(
            wasm_mod,
            compiled_arg1(),
            env.types.grain_big_int,
          ),
          compiled_arg2(),
        ),
        const_void(wasm_mod),
      ],
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
  | WasmRefArrayGet({array_type, signed}) =>
    let array_type =
      switch (array_type) {
      | Wasm_packed_i8 =>
        build_array_type(~packed_type=Packed_type.int8, Type.int32)
      | Wasm_int64 => build_array_type(Type.int64)
      };
    Expression.Array.get(
      wasm_mod,
      Expression.Ref.cast(wasm_mod, compiled_arg1(), array_type),
      compiled_arg2(),
      array_type,
      signed,
    );
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
        const_void(wasm_mod),
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
        const_void(wasm_mod),
      ],
    )
  | WasmMemoryCompare =>
    let lbl = gensym_label("memory_compare");
    let loop_lbl = gensym_label("memory_compare_loop");
    let set_ptr1 = set_swap(~ty=WasmValue(WasmI32), wasm_mod, env, 0);
    let set_ptr2 = set_swap(~ty=WasmValue(WasmI32), wasm_mod, env, 1);
    let set_count = set_swap(~ty=WasmValue(WasmI32), wasm_mod, env, 2);
    let get_ptr1 = () => get_swap(~ty=WasmValue(WasmI32), wasm_mod, env, 0);
    let get_ptr2 = () => get_swap(~ty=WasmValue(WasmI32), wasm_mod, env, 1);
    let get_count = () => get_swap(~ty=WasmValue(WasmI32), wasm_mod, env, 2);
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
  | WasmRefArraySet({array_type}) =>
    let array_type =
      switch (array_type) {
      | Wasm_packed_i8 =>
        build_array_type(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          Type.int32,
        )
      | Wasm_int64 => build_array_type(~mutable_=true, Type.int64)
      };
    Expression.Block.make(
      wasm_mod,
      gensym_label("ref_array_i8_set"),
      [
        Expression.Array.set(
          wasm_mod,
          Expression.Ref.cast(
            wasm_mod,
            compile_imm(wasm_mod, env, List.nth(args, 0)),
            array_type,
          ),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
          compile_imm(wasm_mod, env, List.nth(args, 2)),
        ),
        const_void(wasm_mod),
      ],
    );
  | WasmRefArrayCopy({array_type}) =>
    let (dest_type, src_type) =
      switch (array_type) {
      | Wasm_packed_i8 => (
          build_array_type(
            ~mutable_=true,
            ~packed_type=Packed_type.int8,
            Type.int32,
          ),
          build_array_type(~packed_type=Packed_type.int8, Type.int32),
        )
      | Wasm_int64 => (
          build_array_type(~mutable_=true, Type.int64),
          build_array_type(Type.int64),
        )
      };
    Expression.Block.make(
      wasm_mod,
      gensym_label("ref_array_copy"),
      [
        Expression.Array.copy(
          wasm_mod,
          Expression.Ref.cast(
            wasm_mod,
            compile_imm(wasm_mod, env, List.nth(args, 0)),
            dest_type,
          ),
          compile_imm(wasm_mod, env, List.nth(args, 1)),
          Expression.Ref.cast(
            wasm_mod,
            compile_imm(wasm_mod, env, List.nth(args, 2)),
            src_type,
          ),
          compile_imm(wasm_mod, env, List.nth(args, 3)),
          compile_imm(wasm_mod, env, List.nth(args, 4)),
        ),
        const_void(wasm_mod),
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
      Expression.Const.make(
        wasm_mod,
        Literal.float32(Int64.float_of_bits(i)),
      ),
    )
  | MFloat64(i) =>
    allocate_float64(
      wasm_mod,
      env,
      Expression.Const.make(
        wasm_mod,
        Literal.float64(Int64.float_of_bits(i)),
      ),
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
      Expression.Array.set(
        wasm_mod,
        Expression.Struct.get(
          wasm_mod,
          2,
          Expression.Ref.cast(wasm_mod, get_swap(), env.types.grain_closure),
          env.types.grain_closure,
          false,
        ),
        Expression.Const.make(wasm_mod, const_int32(idx)),
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

let expression_of_instrs = (wasm_mod, instrs) => {
  switch (instrs) {
  | [instr] => instr
  | _ => Expression.Block.make(wasm_mod, gensym_label("block"), instrs)
  };
};

let rec compile_store = (wasm_mod, env, binds) => {
  let process_binds = env => {
    let process_bind = ((b, instr), acc) => {
      let store_bind = value =>
        compile_bind(~action=BindSet({value: value}), wasm_mod, env, b);
      let compiled_instrs =
        switch (instr.instr_desc) {
        // special logic here for letrec
        | MAllocate(MClosure(cdata)) =>
          let get_bind = compile_bind(~action=BindGet, wasm_mod, env, b);
          [
            allocate_closure(
              wasm_mod,
              env,
              ~lambda=get_bind,
              ~skip_patching=true,
              cdata,
            ),
          ];
        | _ => compile_instr(wasm_mod, env, instr)
        };
      [store_bind(expression_of_instrs(wasm_mod, compiled_instrs)), ...acc];
    };
    List.fold_right(process_bind, binds, []);
  };
  let (instrs, backpatches) = collect_backpatches(env, process_binds);
  List.append(instrs, [do_backpatches(wasm_mod, env, backpatches)]);
}

and compile_set = (wasm_mod, env, b, i) => {
  Expression.Block.make(
    wasm_mod,
    gensym_label("compile_set"),
    [
      compile_bind(
        ~action=
          BindSet({
            value:
              expression_of_instrs(
                wasm_mod,
                compile_instr(wasm_mod, env, i),
              ),
          }),
        wasm_mod,
        env,
        b,
      ),
      const_void(wasm_mod),
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
        | Types.GrainValue(_)
        | Types.WasmValue(WasmRef) => const_ref_0(wasm_mod)
        | Types.WasmValue(WasmI32) =>
          Expression.Const.make(wasm_mod, const_int32(0))
        | Types.WasmValue(WasmI64) =>
          Expression.Const.make(wasm_mod, const_int64(0))
        | Types.WasmValue(WasmF32) =>
          Expression.Const.make(wasm_mod, const_float32(0.))
        | Types.WasmValue(WasmF64) =>
          Expression.Const.make(wasm_mod, const_float64(0.))
        };
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
  let compiled_instrs = List.concat_map(compile_instr(wasm_mod, env), block);
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
and compile_instr = (wasm_mod, env, instr) => {
  let exp = expression_of_instrs(wasm_mod);
  switch (instr.instr_desc) {
  | MDrop(arg) => [
      Expression.Drop.make(
        wasm_mod,
        exp(compile_instr(wasm_mod, env, arg)),
      ),
    ]
  | MImmediate(imm) => [compile_imm(wasm_mod, env, imm)]
  | MAllocate(alloc) => [compile_allocation(wasm_mod, env, alloc)]
  | MTupleOp(tuple_op, tup) => [
      compile_tuple_op(wasm_mod, env, tup, tuple_op),
    ]
  | MBoxOp(box_op, box) => [compile_box_op(wasm_mod, env, box, box_op)]
  | MArrayOp(array_op, ret) => [
      compile_array_op(wasm_mod, env, ret, array_op),
    ]
  | MAdtOp(adt_op, adt) => [compile_adt_op(wasm_mod, env, adt, adt_op)]
  | MRecordOp(record_op, record) => [
      compile_record_op(wasm_mod, env, record, record_op),
    ]
  | MClosureOp(closure_op, closure) => [
      compile_closure_op(wasm_mod, env, closure, closure_op),
    ]
  | MPrim0(p0) => [compile_prim0(wasm_mod, env, p0)]
  | MPrim1(p1, arg) => [
      compile_prim1(wasm_mod, env, p1, arg, instr.instr_loc),
    ]
  | MPrim2(p2, arg1, arg2) => [compile_prim2(wasm_mod, env, p2, arg1, arg2)]
  | MPrimN(p, args) => [compile_primn(wasm_mod, env, p, args)]
  | MSwitch(arg, branches, default, ty) => [
      compile_switch(wasm_mod, env, arg, branches, default, ty),
    ]
  | MStore(binds) => compile_store(wasm_mod, env, binds)
  | MSet(b, i) => [compile_set(wasm_mod, env, b, i)]
  | MCallKnown({func, closure, func_type, args}) => [
      call_lambda(~known=func, wasm_mod, env, closure, func_type, args),
    ]
  | MReturnCallKnown({func, closure, func_type, args}) => [
      call_lambda(
        ~tail=true,
        ~known=func,
        wasm_mod,
        env,
        closure,
        func_type,
        args,
      ),
    ]
  | MCallIndirect({func, func_type, args}) => [
      call_lambda(wasm_mod, env, func, func_type, args),
    ]
  | MReturnCallIndirect({func, func_type, args}) => [
      call_lambda(~tail=true, wasm_mod, env, func, func_type, args),
    ]
  | MCallRaw({func, func_type: (_, retty), args}) =>
    let compiled_args = List.map(compile_imm(wasm_mod, env), args);
    let func_name = linked_name(~env, func);
    let resolved_name = resolve_func(~env, func_name);
    if (resolved_name == func_name) {
      [
        // Name not resolved; call normally
        Expression.Call.make(
          wasm_mod,
          func,
          compiled_args,
          Type.create(Array.of_list(List.map(wasm_type, retty))),
        ),
      ];
    } else if (StringSet.mem(func_name, env.foreign_import_resolutions^)) {
      [
        // Deduplicated imports; call resolved name directly
        Expression.Call.make(
          wasm_mod,
          resolved_name,
          compiled_args,
          Type.create(Array.of_list(List.map(wasm_type, retty))),
        ),
      ];
    } else {
      // Raw function resolved to Grain function; inject closure argument
      let closure_global = resolve_global(~env, func_name);
      let closure_arg =
        Expression.Global_get.make(wasm_mod, closure_global, Type.int32);
      let compiled_args = [closure_arg, ...compiled_args];
      [
        Expression.Call.make(
          wasm_mod,
          resolved_name,
          compiled_args,
          Type.create(Array.of_list(List.map(wasm_type, retty))),
        ),
      ];
    };

  | MIf(cond, thn, els) =>
    let compiled_cond = compile_imm(wasm_mod, env, cond);
    let compiled_thn = compile_block(wasm_mod, env, thn);
    let compiled_els = compile_block(wasm_mod, env, els);
    [
      Expression.If.make(
        wasm_mod,
        decode_bool(wasm_mod, compiled_cond),
        compiled_thn,
        compiled_els,
      ),
    ];

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
            const_void(wasm_mod),
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
    [
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
          const_void(wasm_mod),
        ],
      ),
    ];
  | MContinue =>
    let (continue_label, _) = List.hd(loop_stack^);
    [
      Expression.Break.make(
        wasm_mod,
        continue_label,
        Expression.Null.make(),
        Expression.Null.make(),
      ),
    ];
  | MBreak =>
    let (_, block_label) = List.hd(loop_stack^);
    [
      Expression.Break.make(
        wasm_mod,
        block_label,
        Expression.Null.make(),
        const_void(wasm_mod),
      ),
    ];
  | MError(err, args) => [call_error_handler(wasm_mod, env, err, args)]
  | MReturn(value) =>
    let value =
      Option.fold(
        ~none=const_void(wasm_mod),
        ~some=compile_imm(wasm_mod, env),
        value,
      );
    [Expression.Return.make(wasm_mod, value)];
  | MArityOp(_) => failwith("NYI: (compile_instr): MArityOp")
  | MTagOp(_) => failwith("NYI: (compile_instr): MTagOp")
  };
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
  let func_name = linked_name(~env, func_name);
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
                    Expression.Array.get(
                      wasm_mod,
                      Expression.Struct.get(
                        wasm_mod,
                        2,
                        Expression.Ref.cast(
                          wasm_mod,
                          Expression.Local_get.make(wasm_mod, 0, ref_any()),
                          env.types.grain_closure,
                        ),
                        env.types.grain_closure,
                        false,
                      ),
                      Expression.Const.make(
                        wasm_mod,
                        const_int32(closure_slot),
                      ),
                      build_array_type(~mutable_=true, ref_any()),
                      false,
                    ),
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
      Array.make(body_env.num_closure_args, ref_any()),
      swap_slots,
      Array.make(stack_size.stack_size_ptr, ref_any()),
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
      ((exp, loc)) =>
        Function.set_debug_location(
          func_ref,
          exp,
          env.func_debug_idx,
          loc.loc_start.pos_lnum,
          loc.loc_start.pos_cnum - loc.loc_start.pos_bol,
        ),
      sources^,
    );
    Function.set_debug_location(
      func_ref,
      body,
      env.func_debug_idx,
      func_loc.loc_start.pos_lnum,
      func_loc.loc_start.pos_cnum - func_loc.loc_start.pos_bol,
    );
  };
  func_ref;
};

let compile_imports = (wasm_mod, env, {imports}, import_map) => {
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
      | MImportWasm => Ident.unique_name(mimp_id)
      };
    let import_key = (module_name, item_name, mimp_kind, mimp_type);
    switch (Hashtbl.find_opt(import_map, import_key)) {
    | Some(name) when mimp_kind == MImportWasm =>
      // Deduplicate wasm imports by resolving them to the previously imported name
      let linked_name = linked_name(~env, internal_name);
      switch (mimp_type) {
      | MFuncImport(_, _) =>
        Hashtbl.add(env.func_import_resolutions, linked_name, name)
      | MGlobalImport(_, _) =>
        Hashtbl.add(env.global_import_resolutions, linked_name, name)
      };
      env.foreign_import_resolutions :=
        StringSet.add(linked_name, env.foreign_import_resolutions^);
    | _ =>
      Hashtbl.add(import_map, import_key, internal_name);
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
  };

  List.iter(compile_import, imports);
};

let compile_exports = (wasm_mod, env, {imports, exports, globals}) => {
  let compile_export = (i, export) => {
    switch (export) {
    | WasmGlobalExport({ex_global_internal_name, ex_global_name}) =>
      let ex_global_name = "GRAIN$EXPORT$" ++ ex_global_name;
      let internal_name = linked_name(~env, ex_global_internal_name);
      let resolved_name = resolve_global(~env, internal_name);
      ignore @@
      Export.add_global_export(wasm_mod, resolved_name, ex_global_name);
    | WasmFunctionExport({ex_function_internal_name, ex_function_name}) =>
      let internal_name = linked_name(~env, ex_function_internal_name);
      let resolved_name = resolve_func(~env, internal_name);
      ignore @@
      Export.add_function_export(wasm_mod, resolved_name, ex_function_name);
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
};

let compile_globals = (wasm_mod, env, {globals}) => {
  let initial_value =
    fun
    | Types.GrainValue(_)
    | Types.WasmValue(WasmRef) => const_ref_0(wasm_mod)
    | Types.WasmValue(WasmI32) =>
      Expression.Const.make(wasm_mod, const_int32(0))
    | Types.WasmValue(WasmI64) =>
      Expression.Const.make(wasm_mod, const_int64(0))
    | Types.WasmValue(WasmF32) =>
      Expression.Const.make(wasm_mod, const_float32(0.))
    | Types.WasmValue(WasmF64) =>
      Expression.Const.make(wasm_mod, const_float64(0.));
  List.iter(
    ({id, mutable_, allocation_type, initial_value: initial}) => {
      let name = linked_name(~env, Ident.unique_name(id));
      ignore @@
      Global.add_global(
        wasm_mod,
        name,
        wasm_type(allocation_type),
        mutable_,
        switch (initial) {
        | Some(initial) => compile_const(wasm_mod, initial)
        | None => initial_value(allocation_type)
        },
      );
    },
    globals,
  );
};

let compile_main = (wasm_mod, env, prog) => {
  let num_mains = List.length(prog.programs);
  List.iteri(
    (dep_id, {mash_code: prog}: mash_program) => {
      let env = {...env, compilation_mode: prog.compilation_mode, dep_id};
      let compile = () => {
        ignore @@
        compile_function(
          ~name=grain_main,
          wasm_mod,
          env,
          {
            id: Ident.create(grain_main),
            name: Some(grain_main),
            args: [],
            return_type: [Types.GrainValue(GrainAny)],
            closure: None,
            body: prog.main_body,
            stack_size: prog.main_body_stack_size,
            attrs: [],
            func_loc: prog.prog_loc,
          },
        );
      };
      switch (prog.compilation_mode) {
      | Runtime =>
        Config.preserve_config(() => {
          Config.no_gc := true;
          compile();
        })
      | Normal => compile()
      };
    },
    prog.programs,
  );
  let starts =
    List.init(num_mains, dep_id =>
      Expression.Drop.make(
        wasm_mod,
        Expression.Call.make(
          wasm_mod,
          Printf.sprintf("%s_%d", grain_main, dep_id),
          [],
          ref_any(),
        ),
      )
    );
  let start =
    Function.add_function(
      wasm_mod,
      grain_start,
      Type.none,
      Type.none,
      [||],
      Expression.Block.make(wasm_mod, grain_start, starts),
    );
  if (Grain_utils.Config.use_start_section^) {
    Function.set_start(wasm_mod, start);
  } else {
    ignore @@
    Export.add_function_export(wasm_mod, grain_start, Comp_utils.grain_start);
  };
};

let compile_functions = (wasm_mod, env, {functions, prog_loc}) => {
  let func_debug_idx =
    Module.add_debug_info_filename(wasm_mod, prog_loc.loc_start.pos_fname);
  let handle_attrs = ({attrs, func_loc} as func) => {
    let env = {...env, func_debug_idx};
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
  };
  ignore @@ List.map(handle_attrs, functions);
};

let compile_type_metadata = (wasm_mod, env, prog) => {
  let metadata_tbl_data =
    if (Config.elide_type_info^) {
      None;
    } else {
      let metadata =
        List.map(
          prog => prog.Mashtree.signature.cmi_type_metadata,
          prog.programs,
        );
      Some(Type_metadata.construct_type_metadata_table(metadata));
    };
  let runtime_heap_ptr =
    switch (Grain_utils.Config.memory_base^) {
    | Some(x) => round_to_8(x)
    | None => Grain_utils.Config.default_memory_base
    };
  let metadata_heap_loc = runtime_heap_ptr + 8;
  let metadata_size =
    round_to_8(
      Option.value(~default=0, Option.map(Bytes.length, metadata_tbl_data)),
    );
  let runtime_heap_start = metadata_heap_loc + metadata_size;

  ignore @@
  Global.add_global(
    wasm_mod,
    runtime_heap_next_ptr_name,
    Type.int32,
    true,
    Expression.Const.make(
      wasm_mod,
      Literal.int32(Int32.of_int(runtime_heap_start)),
    ),
  );

  ignore @@
  Global.add_global(
    wasm_mod,
    runtime_heap_start_name,
    Type.int32,
    false,
    Expression.Const.make(
      wasm_mod,
      Literal.int32(Int32.of_int(runtime_heap_start)),
    ),
  );

  ignore @@
  Global.add_global(
    wasm_mod,
    metadata_ptr_name,
    Type.int32,
    false,
    Expression.Const.make(
      wasm_mod,
      Literal.int32(Int32.of_int(metadata_heap_loc)),
    ),
  );

  switch (metadata_tbl_data) {
  | Some(data) =>
    push_data_segment(
      Memory.{
        name: "type_metadata",
        data,
        kind:
          Active({
            offset:
              Expression.Const.make(
                wasm_mod,
                Literal.int32(Int32.of_int(metadata_heap_loc)),
              ),
          }),
        size: Bytes.length(data),
      },
    )

  | None => ()
  };
};

exception WasmRunnerError(Module.t, option(string), string);

let validate_module = (~name=?, wasm_mod: Module.t) =>
  try(assert(Module.validate(wasm_mod) == 1)) {
  | Assert_failure(_) =>
    Module.print(wasm_mod);
    raise(WasmRunnerError(wasm_mod, name, "WARNING: Invalid module"));
  };

let compile_wasm_module =
    (
      ~name=?,
      {global_import_resolutions, func_import_resolutions} as prog: Linkedtree.linked_program,
    ) => {
  reset();
  let wasm_mod = Module.create();
  let env =
    init_codegen_env(
      ~global_import_resolutions,
      ~func_import_resolutions,
      wasm_mod,
      name,
    );

  let default_features = [
    Module.Feature.mvp,
    Module.Feature.multivalue,
    Module.Feature.tail_call,
    Module.Feature.sign_ext,
    Module.Feature.mutable_globals,
    Module.Feature.reference_types,
    Module.Feature.gc,
  ];
  let features =
    if (Config.bulk_memory^) {
      [
        Module.Feature.bulk_memory,
        Module.Feature.bulk_memory_opt,
        ...default_features,
      ];
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

  compile_type_metadata(wasm_mod, env, prog);

  let import_map = Hashtbl.create(10);

  let compile_one = (dep_id, prog: mash_code) => {
    let env = {...env, dep_id, compilation_mode: prog.compilation_mode};
    ignore @@ compile_imports(wasm_mod, env, prog, import_map);
    ignore @@ compile_globals(wasm_mod, env, prog);
    ignore @@ compile_functions(wasm_mod, env, prog);
    ignore @@ compile_exports(wasm_mod, env, prog);
  };

  List.iteri(
    (dep_id, {mash_code: prog}) => {
      switch (prog.compilation_mode) {
      | Runtime =>
        Config.preserve_config(() => {
          Config.no_gc := true;
          compile_one(dep_id, prog);
        })
      | Normal => compile_one(dep_id, prog)
      }
    },
    prog.programs,
  );

  ignore @@ compile_main(wasm_mod, env, prog);

  let (initial_memory, maximum_memory) =
    switch (Config.initial_memory_pages^, Config.maximum_memory_pages^) {
    | (initial_memory, Some(maximum_memory)) => (
        initial_memory,
        maximum_memory,
      )
    | (initial_memory, None) => (initial_memory, Memory.unlimited)
    };

  Memory.set_memory(
    wasm_mod,
    initial_memory,
    maximum_memory,
    "memory",
    data_segments^,
    false,
    false,
    Comp_utils.grain_memory,
  );
  if (Config.import_memory^) {
    Import.add_memory_import(
      wasm_mod,
      Comp_utils.grain_memory,
      "env",
      "memory",
      false,
    );
  };

  validate_module(~name?, wasm_mod);

  switch (Config.profile^) {
  | Some(Release) =>
    Module.optimize(wasm_mod);
    Module.optimize(wasm_mod);
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
