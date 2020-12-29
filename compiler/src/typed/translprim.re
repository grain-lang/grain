open Grain_parsing;
open Ast_helper;
open Typedtree;
open Parsetree;

type primitive =
  | Primitive1(prim1)
  | Primitive2(prim2)
  | PrimitiveN(primn);

module PrimMap =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let default_loc = default_loc_src^();

let mkident = name =>
  Exp.ident(Location.mkloc(Identifier.IdentName(name), default_loc));
let mkpatvar = name => {
  ppat_desc: PPatVar(Location.mkloc(name, default_loc)),
  ppat_loc: default_loc,
};

let id_a = mkident("a");
let id_b = mkident("b");
let id_c = mkident("c");
let pat_a = mkpatvar("a");
let pat_b = mkpatvar("b");
let pat_c = mkpatvar("c");

let prim_map =
  PrimMap.of_seq(
    List.to_seq([
      ("@incr", Primitive1(Incr)),
      ("@decr", Primitive1(Decr)),
      ("@not", Primitive1(Not)),
      ("@box", Primitive1(Box)),
      ("@unbox", Primitive1(Unbox)),
      ("@ignore", Primitive1(Ignore)),
      ("@assert", Primitive1(Assert)),
      ("@fail", Primitive1(FailWith)),
      ("@plus", Primitive2(Plus)),
      ("@minus", Primitive2(Minus)),
      ("@times", Primitive2(Times)),
      ("@divides", Primitive2(Divide)),
      ("@modulo", Primitive2(Mod)),
      ("@less", Primitive2(Less)),
      ("@greater", Primitive2(Greater)),
      ("@lesseq", Primitive2(LessEq)),
      ("@greatereq", Primitive2(GreaterEq)),
      ("@is", Primitive2(Is)),
      ("@eq", Primitive2(Eq)),
      ("@and", Primitive2(And)),
      ("@or", Primitive2(Or)),
      ("@array.length", Primitive1(ArrayLength)),
      ("@array.make", Primitive2(ArrayMake)),
      ("@array.init", Primitive2(ArrayInit)),
      ("@int64.fromNumber", Primitive1(Int64FromNumber)),
      ("@int64.toNumber", Primitive1(Int64ToNumber)),
      ("@int64.lnot", Primitive1(Int64Lnot)),
      ("@int64.land", Primitive2(Int64Land)),
      ("@int64.lor", Primitive2(Int64Lor)),
      ("@int64.lxor", Primitive2(Int64Lxor)),
      ("@int64.lsl", Primitive2(Int64Lsl)),
      ("@int64.lsr", Primitive2(Int64Lsr)),
      ("@int64.asr", Primitive2(Int64Asr)),
      ("@int64.gt", Primitive2(Int64Gt)),
      ("@int64.gte", Primitive2(Int64Gte)),
      ("@int64.lt", Primitive2(Int64Lt)),
      ("@int64.lte", Primitive2(Int64Lte)),
      ("@int32.toNumber", Primitive1(Int32ToNumber)),
      ("@float32.toNumber", Primitive1(Float32ToNumber)),
      ("@float64.toNumber", Primitive1(Float64ToNumber)),
      ("@wasm.load_i32", Primitive2(WasmLoadI32)),
      ("@wasm.store_i32", PrimitiveN(WasmStoreI32)),
      (
        "@wasm.clz_i32",
        Primitive1(WasmUnaryI32({op: "clz_i32", boolean: false})),
      ),
      (
        "@wasm.ctz_i32",
        Primitive1(WasmUnaryI32({op: "ctz_i32", boolean: false})),
      ),
      (
        "@wasm.popcnt_i32",
        Primitive1(WasmUnaryI32({op: "popcnt_i32", boolean: false})),
      ),
      (
        "@wasm.eq_z_i32",
        Primitive1(WasmUnaryI32({op: "eq_z_i32", boolean: true})),
      ),
      (
        "@wasm.add_i32",
        Primitive2(WasmBinaryI32({op: "add_i32", boolean: false})),
      ),
      (
        "@wasm.sub_i32",
        Primitive2(WasmBinaryI32({op: "sub_i32", boolean: false})),
      ),
      (
        "@wasm.mul_i32",
        Primitive2(WasmBinaryI32({op: "mul_i32", boolean: false})),
      ),
      (
        "@wasm.div_s_i32",
        Primitive2(WasmBinaryI32({op: "div_s_i32", boolean: false})),
      ),
      (
        "@wasm.div_u_i32",
        Primitive2(WasmBinaryI32({op: "div_u_i32", boolean: false})),
      ),
      (
        "@wasm.rem_s_i32",
        Primitive2(WasmBinaryI32({op: "rem_s_i32", boolean: false})),
      ),
      (
        "@wasm.rem_u_i32",
        Primitive2(WasmBinaryI32({op: "rem_u_i32", boolean: false})),
      ),
      (
        "@wasm.and_i32",
        Primitive2(WasmBinaryI32({op: "and_i32", boolean: false})),
      ),
      (
        "@wasm.or_i32",
        Primitive2(WasmBinaryI32({op: "or_i32", boolean: false})),
      ),
      (
        "@wasm.xor_i32",
        Primitive2(WasmBinaryI32({op: "xor_i32", boolean: false})),
      ),
      (
        "@wasm.shl_i32",
        Primitive2(WasmBinaryI32({op: "shl_i32", boolean: false})),
      ),
      (
        "@wasm.shr_u_i32",
        Primitive2(WasmBinaryI32({op: "shr_u_i32", boolean: false})),
      ),
      (
        "@wasm.shr_s_i32",
        Primitive2(WasmBinaryI32({op: "shr_s_i32", boolean: false})),
      ),
      (
        "@wasm.rot_l_i32",
        Primitive2(WasmBinaryI32({op: "rot_l_i32", boolean: false})),
      ),
      (
        "@wasm.rot_r_i32",
        Primitive2(WasmBinaryI32({op: "rot_r_i32", boolean: false})),
      ),
      (
        "@wasm.eq_i32",
        Primitive2(WasmBinaryI32({op: "eq_i32", boolean: true})),
      ),
      (
        "@wasm.ne_i32",
        Primitive2(WasmBinaryI32({op: "ne_i32", boolean: true})),
      ),
      (
        "@wasm.lt_s_i32",
        Primitive2(WasmBinaryI32({op: "lt_s_i32", boolean: true})),
      ),
      (
        "@wasm.lt_u_i32",
        Primitive2(WasmBinaryI32({op: "lt_u_i32", boolean: true})),
      ),
      (
        "@wasm.le_s_i32",
        Primitive2(WasmBinaryI32({op: "le_s_i32", boolean: true})),
      ),
      (
        "@wasm.le_u_i32",
        Primitive2(WasmBinaryI32({op: "le_u_i32", boolean: true})),
      ),
      (
        "@wasm.gt_s_i32",
        Primitive2(WasmBinaryI32({op: "gt_s_i32", boolean: true})),
      ),
      (
        "@wasm.gt_u_i32",
        Primitive2(WasmBinaryI32({op: "gt_u_i32", boolean: true})),
      ),
      (
        "@wasm.ge_s_i32",
        Primitive2(WasmBinaryI32({op: "ge_s_i32", boolean: true})),
      ),
      (
        "@wasm.ge_u_i32",
        Primitive2(WasmBinaryI32({op: "ge_u_i32", boolean: true})),
      ),
      ("@wasm.ofGrain", Primitive1(WasmOfGrain)),
      ("@wasm.toGrain", Primitive1(WasmToGrain)),
    ]),
  );

let transl_prim = (env, desc) => {
  let loc = desc.tvd_loc;

  let prim =
    try(PrimMap.find(prim_map, List.hd(desc.tvd_prim))) {
    | Not_found => failwith("This primitive does not exist.")
    };

  let diable_gc = [Location.mknoloc("disableGC")];

  let value =
    switch (prim) {
    | Primitive1(WasmUnaryI32(_) as p) =>
      Exp.lambda(
        ~loc,
        ~attributes=diable_gc,
        [pat_a],
        Exp.prim1(~loc, p, id_a),
      )
    | Primitive1(p) => Exp.lambda(~loc, [pat_a], Exp.prim1(~loc, p, id_a))
    | Primitive2((WasmBinaryI32(_) | WasmLoadI32) as p) =>
      Exp.lambda(
        ~loc,
        ~attributes=diable_gc,
        [pat_a, pat_b],
        Exp.prim2(~loc, p, id_a, id_b),
      )
    | Primitive2(p) =>
      Exp.lambda(~loc, [pat_a, pat_b], Exp.prim2(~loc, p, id_a, id_b))
    | PrimitiveN(WasmStoreI32 as p) =>
      Exp.lambda(
        ~loc,
        ~attributes=diable_gc,
        [pat_a, pat_b, pat_c],
        Exp.primn(~loc, p, [id_a, id_b, id_c]),
      )
    };

  let binds = [
    {
      pvb_pat: {
        ppat_desc: PPatVar(desc.tvd_name),
        ppat_loc: loc,
      },
      pvb_expr: value,
      pvb_loc: loc,
    },
  ];
  let mut_flag = desc.tvd_val.val_mutable ? Mutable : Immutable;
  Typecore.type_binding(env, Nonrecursive, mut_flag, binds, None);
};
