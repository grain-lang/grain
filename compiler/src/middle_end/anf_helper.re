open Grain_parsing;
open Grain_typed;
open Anftree;
open Types;

type str = loc(string);
type loc = Location.t;
type env = Env.t;
type ident = Ident.t;
type attributes = Typedtree.attributes;

let default_loc = Location.dummy_loc;
let default_env = Env.empty;
let default_attributes = [];
let default_allocation_type = Managed;

let or_default_loc = Option.value(~default=default_loc);
let or_default_env = Option.value(~default=default_env);
let or_default_attributes = Option.value(~default=default_attributes);
let or_default_allocation_type =
  Option.value(~default=default_allocation_type);

module Imm = {
  let mk = (~loc=?, ~env=?, d) => {
    imm_desc: d,
    imm_loc: or_default_loc(loc),
    imm_env: or_default_env(env),
    imm_analyses: ref([]),
  };
  let id = (~loc=?, ~env=?, id) => mk(~loc?, ~env?, ImmId(id));
  let const = (~loc=?, ~env=?, const) => mk(~loc?, ~env?, ImmConst(const));
  let trap = (~loc=?, ~env=?, ()) => mk(~loc?, ~env?, ImmTrap);
};

module Comp = {
  let mk = (~loc=?, ~attributes=?, ~allocation_type=?, ~env=?, d) => {
    comp_desc: d,
    comp_loc: or_default_loc(loc),
    comp_env: or_default_env(env),
    comp_attributes: or_default_attributes(attributes),
    comp_allocation_type: or_default_allocation_type(allocation_type),
    comp_analyses: ref([]),
  };
  let imm = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, imm) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CImmExpr(imm));
  let number = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CNumber(i));
  let int32 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CInt32(i));
  let int64 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CInt64(i));
  let uint32 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CUint32(i));
  let uint64 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CUint64(i));
  let float32 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CFloat32(i));
  let float64 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CFloat64(i));
  let prim0 = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, p0) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CPrim0(p0));
  let prim1 = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, p1, a) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CPrim1(p1, a));
  let prim2 = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, p2, a1, a2) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CPrim2(p2, a1, a2));
  let primn = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, p, args) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CPrimN(p, args));
  let box_assign = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, a1, a2) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CBoxAssign(a1, a2));
  let local_assign = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, a1, a2) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CLocalAssign(a1, a2));
  let assign = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, a1, a2) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CAssign(a1, a2));
  let tuple = (~loc=?, ~attributes=?, ~env=?, elts) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CTuple(elts));
  let array = (~loc=?, ~attributes=?, ~env=?, elts) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CArray(elts));
  let array_get = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, arr, i) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CArrayGet(arr, i));
  let array_set = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, arr, i, a) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CArraySet(arr, i, a));
  let record = (~loc=?, ~attributes=?, ~env=?, type_hash, ttag, elts) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Managed,
      ~env?,
      CRecord(type_hash, ttag, elts),
    );
  let adt = (~loc=?, ~attributes=?, ~env=?, type_hash, ttag, vtag, elts) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Managed,
      ~env?,
      CAdt(type_hash, ttag, vtag, elts),
    );
  let tuple_get = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, idx, tup) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type,
      ~env?,
      CGetTupleItem(idx, tup),
    );
  let tuple_set =
      (~loc=?, ~attributes=?, ~allocation_type, ~env=?, idx, tup, value) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type,
      ~env?,
      CSetTupleItem(idx, tup, value),
    );
  let adt_get = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, idx, value) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type,
      ~env?,
      CGetAdtItem(idx, value),
    );
  let adt_get_tag = (~loc=?, ~attributes=?, ~env=?, value) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Unmanaged(WasmI32),
      ~env?,
      CGetAdtTag(value),
    );
  let record_get =
      (~loc=?, ~attributes=?, ~allocation_type, ~env=?, idx, record) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type,
      ~env?,
      CGetRecordItem(idx, record),
    );
  let record_set =
      (~loc=?, ~attributes=?, ~allocation_type, ~env=?, idx, record, arg) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type,
      ~env?,
      CSetRecordItem(idx, record, arg),
    );
  let if_ = (~loc=?, ~attributes=?, ~allocation_type, ~env=?, cond, tru, fals) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CIf(cond, tru, fals));
  let for_ = (~loc=?, ~attributes=?, ~env=?, cond, inc, body) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Unmanaged(WasmI32),
      ~env?,
      CFor(cond, inc, body),
    );
  let continue = (~loc=?, ~attributes=?, ~env=?, ()) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Unmanaged(WasmI32),
      ~env?,
      CContinue,
    );
  let break = (~loc=?, ~attributes=?, ~env=?, ()) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Unmanaged(WasmI32),
      ~env?,
      CBreak,
    );
  let return = (~loc=?, ~attributes=?, ~env=?, ret) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Unmanaged(WasmI32),
      ~env?,
      CReturn(ret),
    );
  let switch_ =
      (
        ~loc=?,
        ~attributes=?,
        ~allocation_type,
        ~env=?,
        arg,
        branches,
        partial,
      ) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type,
      ~env?,
      CSwitch(arg, branches, partial),
    );
  let app =
      (
        ~loc=?,
        ~attributes=?,
        ~allocation_type,
        ~env=?,
        ~tail=false,
        func,
        args,
      ) =>
    mk(~loc?, ~attributes?, ~allocation_type, ~env?, CApp(func, args, tail));
  let lambda = (~loc=?, ~attributes=?, ~env=?, ~name=?, args, body) =>
    mk(
      ~loc?,
      ~attributes?,
      ~allocation_type=Managed,
      ~env?,
      CLambda(name, args, body, Uncomputed),
    );
  let bytes = (~loc=?, ~attributes=?, ~env=?, b) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CBytes(b));
  let string = (~loc=?, ~attributes=?, ~env=?, s) =>
    mk(~loc?, ~attributes?, ~allocation_type=Managed, ~env?, CString(s));
};

module AExp = {
  let mk = (~loc=?, ~env=?, ~alloc_type, d) => {
    anf_desc: d,
    anf_loc: or_default_loc(loc),
    anf_env: or_default_env(env),
    anf_analyses: ref([]),
    anf_allocation_type: alloc_type,
  };

  let rec alloc_type = a =>
    switch (a.anf_desc) {
    | AELet(_, _, _, _, b) => alloc_type(b)
    | AESeq(_, b) => alloc_type(b)
    | AEComp(e) => e.comp_allocation_type
    };

  let let_ =
      (
        ~loc=?,
        ~env=?,
        ~global=Nonglobal,
        ~mut_flag=Immutable,
        rec_flag,
        binds,
        body,
      ) =>
    mk(
      ~loc?,
      ~env?,
      ~alloc_type=alloc_type(body),
      AELet(global, rec_flag, mut_flag, binds, body),
    );
  let seq = (~loc=?, ~env=?, hd, tl) =>
    mk(~loc?, ~env?, ~alloc_type=alloc_type(tl), AESeq(hd, tl));
  let comp = (~loc=?, ~env=?, e) =>
    mk(~loc?, ~env?, ~alloc_type=e.comp_allocation_type, AEComp(e));
};

module IncludeDeclaration = {
  let mk = (use_id, d, s, e) => {
    imp_use_id: use_id,
    imp_desc: d,
    imp_shape: s,
    imp_exported: e,
    imp_analyses: ref([]),
  };
  let grain_value = (~global=Nonglobal, a, md, name, s) =>
    mk(a, GrainValue(md, name), s, global);
  let wasm_func = (~global=Nonglobal, a, md, name, s) =>
    mk(a, WasmFunction(md, name), s, global);
  let wasm_value = (~global=Nonglobal, a, md, name, s) =>
    mk(a, WasmValue(md, name), s, global);
};
