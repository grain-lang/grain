open Mashtree;
open Binaryen;
open Grain_typed;
open Grain_utils;
open Grain_parsing;

module StringSet = Set.Make(String);

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
  foreign_import_resolutions: ref(StringSet.t),
  global_import_resolutions: Hashtbl.t(string, string),
  func_import_resolutions: Hashtbl.t(string, string),
  compilation_mode: Config.compilation_mode,
  types: core_reftypes,
}

and core_reftypes = {
  grain_value: Type.t,
  grain_compound_value: Type.t,
  grain_tuple: Type.t,
  grain_array: Type.t,
  grain_record: Type.t,
  grain_variant: Type.t,
  grain_closure: Type.t,
  grain_closure_full: Heap_type.t => Heap_type.t,
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
  array_mut_any: Type.t,
  array_mut_i8: Type.t,
  array_mut_i64: Type.t,
};

type error =
  | InvalidStartExport;
exception Error(Location.t, error);

let grain_main = "_gmain";
let grain_start = "_start";

// TODO(#): Use a more descriptive name once we get fixes into Binaryen
let grain_memory = "0";

let page_size = 65536;

let ref_any = () => Type.from_heap_type(Heap_type.any(), false);
let ref_i31 = () => Type.from_heap_type(Heap_type.i31(), false);

let type_of_repr = repr => {
  Types.(
    switch (repr) {
    | WasmI32 => Type.int32
    | WasmI64 => Type.int64
    | WasmF32 => Type.float32
    | WasmF64 => Type.float64
    | WasmRef => ref_any()
    }
  );
};

let wasm_type =
  fun
  | Types.GrainValue => ref_any()
  | Types.WasmValue(v) => type_of_repr(v);

let const_int32 = n => Literal.int32(Int32.of_int(n));
let const_int64 = n => Literal.int64(Int64.of_int(n));
let const_float32 = n => Literal.float32(n);
let const_float64 = n => Literal.float64(n);

type int_type =
  | Int8Type
  | Int16Type
  | Uint8Type
  | Uint16Type;

/** Constant compilation */

let compile_const = (wasm_mod, c) => {
  let conv_simple_number = n => Int32.(add(mul(2l, n), 1l));
  let conv_char = char => {
    let uchar = List.hd @@ Utf8.decodeUtf8String(char);
    let uchar_int: int = Utf8__Uchar.toInt(uchar);
    Int32.of_int(uchar_int lsl 8 lor 0b010);
  };
  let conv_short_int = (int, int_type) => {
    let tag =
      switch (int_type) {
      | Int8Type => 1l
      | Int16Type => 2l
      | Uint8Type => 3l
      | Uint16Type => 4l
      };
    let (<<) = Int32.shift_left;
    let (||) = Int32.logor;
    let shifted_tag = tag << 3;
    int << 8 || shifted_tag || 0b010l;
  };
  switch (c) {
  | MConstTrue =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(wasm_mod, Literal.int32(const_true)),
    )
  | MConstFalse =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(wasm_mod, Literal.int32(const_false)),
    )
  | MConstVoid =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(wasm_mod, Literal.int32(const_void)),
    )
  | MConstSimpleNumber(n) =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(wasm_mod, Literal.int32(conv_simple_number(n))),
    )
  | MConstI8(n) =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(
        wasm_mod,
        Literal.int32(conv_short_int(n, Int8Type)),
      ),
    )
  | MConstI16(n) =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(
        wasm_mod,
        Literal.int32(conv_short_int(n, Int16Type)),
      ),
    )
  | MConstU8(n) =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(
        wasm_mod,
        Literal.int32(conv_short_int(n, Uint8Type)),
      ),
    )
  | MConstU16(n) =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(
        wasm_mod,
        Literal.int32(conv_short_int(n, Uint16Type)),
      ),
    )
  | MConstChar(c) =>
    Expression.I31.make(
      wasm_mod,
      Expression.Const.make(wasm_mod, Literal.int32(conv_char(c))),
    )
  | MConstWasmI32(n) => Expression.Const.make(wasm_mod, Literal.int32(n))
  | MConstWasmI64(n) => Expression.Const.make(wasm_mod, Literal.int64(n))
  | MConstWasmF32(n) =>
    Expression.Const.make(wasm_mod, Literal.float32(Int64.float_of_bits(n)))
  | MConstWasmF64(n) =>
    Expression.Const.make(wasm_mod, Literal.float64(Int64.float_of_bits(n)))
  };
};

/* Translate constants to Wasm */
let const_true = wasm_mod => compile_const(wasm_mod, MConstTrue);
let const_false = wasm_mod => compile_const(wasm_mod, MConstFalse);
let const_void = wasm_mod => compile_const(wasm_mod, MConstVoid);
let const_ref_0 = wasm_mod =>
  Expression.I31.make(
    wasm_mod,
    Expression.Const.make(wasm_mod, const_int32(0)),
  );

let default_value = (wasm_mod, ty) =>
  switch (ty) {
  | Types.GrainValue
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

/* WebAssembly helpers */

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
  | Ok([ty]) => Type.from_heap_type(ty, false)
  | _ => assert(false)
  };
};

let get_array_type = (~env: codegen_env, array_type) =>
  switch (array_type) {
  | Wasm_packed_i8 => env.types.array_mut_i8
  | Wasm_int64 => env.types.array_mut_i64
  | Wasm_any => env.types.array_mut_any
  };

/* These instructions get helpers due to their verbosity */
let store = (~ty=Type.int32, ~align=?, ~offset=0, ~sz=?, wasm_mod, ptr, arg) => {
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
  let align = Option.value(~default=sz, align);
  Expression.Store.make(
    wasm_mod,
    sz,
    offset,
    align,
    ptr,
    arg,
    ty,
    grain_memory,
  );
};

let load =
    (~ty=Type.int32, ~align=?, ~offset=0, ~sz=?, ~signed=false, wasm_mod, ptr) => {
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
  let align = Option.value(~default=sz, align);
  Expression.Load.make(
    ~signed,
    wasm_mod,
    sz,
    offset,
    align,
    ty,
    ptr,
    grain_memory,
  );
};

let write_universal_exports =
    (~env, wasm_mod, {Cmi_format.cmi_sign}, all_exports, resolve) => {
  open Types;
  open Type_utils;
  let export_map = Hashtbl.create(128);
  List.iter(
    ((dep_id, exports)) =>
      List.iter(
        e => {
          switch (e) {
          | WasmGlobalExport({ex_global_name, ex_global_internal_name}) =>
            Hashtbl.add(
              export_map,
              ex_global_name,
              resolve(dep_id, ex_global_internal_name),
            )
          // All functions have an associated global
          | WasmFunctionExport(_) => ()
          }
        },
        exports,
      ),
    all_exports,
  );
  List.iter(
    item => {
      switch (item) {
      | TSigValue(id, {val_repr: ReprFunction(args, rets, direct), val_loc}) =>
        let name = Ident.name(id);
        let internal_name = Hashtbl.find(export_map, name);

        let arguments =
          List.mapi(
            (i, arg) =>
              Expression.Local_get.make(wasm_mod, i, type_of_repr(arg)),
            args,
          );

        let call_result_types =
          Type.create(
            Array.of_list(
              List.map(type_of_repr, rets == [] ? [WasmRef] : rets),
            ),
          );
        let functype =
          build_func_type(
            Array.of_list([
              type_of_repr(WasmRef),
              ...List.map(type_of_repr, args),
            ]),
            call_result_types,
          );

        let get_closure = () =>
          Expression.Ref.cast(
            wasm_mod,
            Expression.Global_get.make(wasm_mod, internal_name, ref_any()),
            Type.from_heap_type(
              env.types.grain_closure_full(functype),
              false,
            ),
          );
        let arguments = [get_closure(), ...arguments];
        let function_call =
          switch (direct) {
          | Direct({name}) =>
            Expression.Call.make(
              wasm_mod,
              internal_name,
              arguments,
              call_result_types,
            )
          | Indirect =>
            let func_ptr =
              Expression.Struct.get(
                wasm_mod,
                3,
                get_closure(),
                Type.from_heap_type(
                  env.types.grain_closure_full(functype),
                  false,
                ),
                false,
              );
            Expression.Call_ref.make(
              wasm_mod,
              func_ptr,
              arguments,
              Type.from_heap_type(functype, false),
            );
          | Unknown => failwith("Impossible: Unknown function call type")
          };
        let function_body =
          switch (rets) {
          | [] => Expression.Drop.make(wasm_mod, function_call)
          | _ => function_call
          };
        let arg_types =
          Type.create(Array.of_list(List.map(type_of_repr, args)));
        let result_types =
          Type.create(Array.of_list(List.map(type_of_repr, rets)));
        ignore @@
        Function.add_function(
          wasm_mod,
          name,
          arg_types,
          result_types,
          [||],
          function_body,
        );
        if (! Config.use_start_section^ && name == grain_start) {
          raise(Error(val_loc, InvalidStartExport));
        } else {
          ignore @@ Export.add_function_export(wasm_mod, name, name);
        };
      | TSigValue(_)
      | TSigType(_)
      | TSigTypeExt(_)
      | TSigModule(_)
      | TSigModType(_) => ()
      }
    },
    cmi_sign,
  );
};

open Format;

let report_error = ppf =>
  fun
  | InvalidStartExport =>
    fprintf(
      ppf,
      "The export `_start` is only allowed when compiling with `--use-start-section`.",
    );

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );
