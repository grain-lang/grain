open Mashtree;
open Binaryen;

let wasm_type =
  fun
  | I32Type => Type.int32
  | I64Type => Type.int64
  | F32Type => Type.float32
  | F64Type => Type.float64;

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

/** Constant compilation */

let rec compile_const = (c): Literal.t => {
  let identity: 'a. 'a => 'a = x => x;
  let conv_int32 = n => Int32.(add(mul(2l, n), 1l));
  let conv_int64 = n => Int64.(add(mul(2L, n), 1L));
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
  Expression.store(wasm_mod, sz, offset, align, ptr, arg, ty);
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
  Expression.load(~signed, wasm_mod, sz, offset, align, ty, ptr);
};
