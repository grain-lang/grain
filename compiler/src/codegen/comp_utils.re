open Mashtree;
open Binaryen;
open Grain_typed;
open Grain_utils;

let grain_main = "_gmain";
let grain_start = "_start";
let grain_env_name = "_genv";
let grain_global_function_table = "tbl";
// TODO(#): Use a more descriptive name once we get fixes into Binaryen
let grain_memory = "0";

let wasm_type =
  fun
  | Types.Managed
  | Types.Unmanaged(WasmI32) => Type.int32
  | Types.Unmanaged(WasmI64) => Type.int64
  | Types.Unmanaged(WasmF32) => Type.float32
  | Types.Unmanaged(WasmF64) => Type.float64;

let encoded_int32 = n => n * 2 + 1;

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

type int_type =
  | Int8Type
  | Int16Type
  | Uint8Type
  | Uint16Type;

/** Constant compilation */

let rec compile_const = (c): Literal.t => {
  let identity: 'a. 'a => 'a = x => x;
  let conv_int32 = n => Int32.(add(mul(2l, n), 1l));
  let conv_int64 = n => Int64.(add(mul(2L, n), 1L));
  let conv_uint32 = n => Int32.(add(mul(2l, n), 1l));
  let conv_uint64 = n => Int64.(add(mul(2L, n), 1L));
  let conv_float32 = identity;
  let conv_float64 = identity;
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
  | MConstLiteral(MConstLiteral(_) as c) => compile_const(c)
  | MConstI8(n) => Literal.int32(conv_short_int(n, Int8Type))
  | MConstI16(n) => Literal.int32(conv_short_int(n, Int16Type))
  | MConstI32(n) => Literal.int32(conv_int32(n))
  | MConstI64(n) => Literal.int64(conv_int64(n))
  | MConstU8(n) => Literal.int32(conv_short_int(n, Uint8Type))
  | MConstU16(n) => Literal.int32(conv_short_int(n, Uint16Type))
  | MConstU32(n) => Literal.int32(conv_uint32(n))
  | MConstU64(n) => Literal.int64(conv_uint64(n))
  | MConstF32(n) => Literal.float32(conv_float32(n))
  | MConstF64(n) => Literal.float64(conv_float64(n))
  | MConstChar(c) => Literal.int32(conv_char(c))
  | MConstLiteral(MConstI8(n))
  | MConstLiteral(MConstI16(n))
  | MConstLiteral(MConstI32(n)) => Literal.int32(n)
  | MConstLiteral(MConstI64(n)) => Literal.int64(n)
  | MConstLiteral(MConstU8(n))
  | MConstLiteral(MConstU16(n))
  | MConstLiteral(MConstU32(n)) => Literal.int32(n)
  | MConstLiteral(MConstU64(n)) => Literal.int64(n)
  | MConstLiteral(MConstF32(n)) => Literal.float32(n)
  | MConstLiteral(MConstF64(n)) => Literal.float64(n)
  | MConstLiteral(MConstChar(c)) => Literal.int32(conv_char(c))
  };
};

/* Translate constants to WASM */
let const_true = () => compile_const(const_true);
let const_false = () => compile_const(const_false);
let const_void = () => compile_const(const_void);

/* WebAssembly helpers */

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

let is_grain_env = str => grain_env_name == str;

let get_exported_names = (~function_names=?, ~global_names=?, wasm_mod) => {
  let num_exports = Export.get_num_exports(wasm_mod);
  let exported_names: Hashtbl.t(string, string) = Hashtbl.create(10);
  for (i in 0 to num_exports - 1) {
    let export = Export.get_export_by_index(wasm_mod, i);
    let export_kind = Export.export_get_kind(export);
    let exported_name = Export.get_name(export);
    let internal_name = Export.get_value(export);

    if (export_kind == Export.external_function) {
      let new_internal_name =
        switch (function_names) {
        | Some(function_names) => Hashtbl.find(function_names, internal_name)
        | None => internal_name
        };
      Hashtbl.add(exported_names, exported_name, new_internal_name);
    } else if (export_kind == Export.external_global) {
      let new_internal_name =
        switch (global_names) {
        | Some(global_names) => Hashtbl.find(global_names, internal_name)
        | None => internal_name
        };
      Hashtbl.add(exported_names, exported_name, new_internal_name);
    };
  };
  exported_names;
};

let type_of_repr = repr => {
  Types.(
    switch (repr) {
    | WasmI32 => Type.int32
    | WasmI64 => Type.int64
    | WasmF32 => Type.float32
    | WasmF64 => Type.float64
    }
  );
};

let write_universal_exports =
    (wasm_mod, {Cmi_format.cmi_sign}, exported_names) => {
  Types.(
    Type_utils.(
      List.iter(
        item => {
          switch (item) {
          | TSigValue(
              id,
              {
                val_repr: ReprFunction(args, rets, direct),
                val_fullpath: path,
              },
            ) =>
            let name = Ident.name(id);
            let exported_name = "GRAIN$EXPORT$" ++ name;
            let internal_global_name =
              Hashtbl.find(exported_names, exported_name);
            let get_closure = () =>
              Expression.Global_get.make(
                wasm_mod,
                internal_global_name,
                Type.int32,
              );
            let arguments =
              List.mapi(
                (i, arg) =>
                  Expression.Local_get.make(wasm_mod, i, type_of_repr(arg)),
                args,
              );
            let arguments = [get_closure(), ...arguments];
            let call_result_types =
              Type.create(
                Array.of_list(
                  List.map(type_of_repr, rets == [] ? [WasmI32] : rets),
                ),
              );
            let function_call =
              switch (direct) {
              | Direct({name}) =>
                Expression.Call.make(
                  wasm_mod,
                  Hashtbl.find(exported_names, name),
                  arguments,
                  call_result_types,
                )
              | Indirect =>
                let call_arg_types =
                  Type.create(
                    Array.of_list(
                      List.map(type_of_repr, [WasmI32, ...args]),
                    ),
                  );
                let func_ptr =
                  Expression.Load.make(
                    wasm_mod,
                    4,
                    8,
                    2,
                    Type.int32,
                    get_closure(),
                    grain_memory,
                  );
                Expression.Call_indirect.make(
                  wasm_mod,
                  grain_global_function_table,
                  func_ptr,
                  arguments,
                  call_arg_types,
                  call_result_types,
                );
              | Unknown => failwith("Impossible: Unknown function call type")
              };
            let function_body =
              switch (rets) {
              | [] => Expression.Drop.make(wasm_mod, function_call)
              | _ => function_call
              };
            let function_body =
              Expression.Block.make(
                wasm_mod,
                "closure_incref",
                [
                  Expression.If.make(
                    wasm_mod,
                    Expression.Binary.make(
                      wasm_mod,
                      Op.ne_int32,
                      get_closure(),
                      Expression.Const.make(wasm_mod, Literal.int32(0l)),
                    ),
                    store(
                      wasm_mod,
                      Expression.Binary.make(
                        wasm_mod,
                        Op.sub_int32,
                        get_closure(),
                        Expression.Const.make(wasm_mod, Literal.int32(8l)),
                      ),
                      Expression.Binary.make(
                        wasm_mod,
                        Op.add_int32,
                        load(
                          wasm_mod,
                          Expression.Binary.make(
                            wasm_mod,
                            Op.sub_int32,
                            get_closure(),
                            Expression.Const.make(
                              wasm_mod,
                              Literal.int32(8l),
                            ),
                          ),
                        ),
                        Expression.Const.make(wasm_mod, Literal.int32(1l)),
                      ),
                    ),
                    Expression.Null.make(),
                  ),
                  function_body,
                ],
              );
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
            // Remove existing Grain export (if any)
            Export.remove_export(wasm_mod, name);
            ignore @@ Export.add_function_export(wasm_mod, name, name);
          | TSigValue(_)
          | TSigType(_)
          | TSigTypeExt(_)
          | TSigModule(_)
          | TSigModType(_) => ()
          }
        },
        cmi_sign,
      )
    )
  );
};

let compiling_wasi_polyfill = name =>
  Option.is_some(Config.wasi_polyfill^) && Config.wasi_polyfill^ == name;
