open Grain_parsing;
open Grain_typed;
open Anftree;

type str = loc(string);
type loc = Location.t;
type env = Env.t;
type ident = Ident.t;
type attributes = Asttypes.attributes;

let default_loc = Location.dummy_loc;
let default_env = Env.empty;
let default_attributes = [];

let or_default_loc = Option.value(~default=default_loc);
let or_default_env = Option.value(~default=default_env);
let or_default_attributes = Option.value(~default=default_attributes);

module Imm = {
  let mk = (~loc=?, ~env=?, d) => {
    imm_desc: d,
    imm_loc: or_default_loc(loc),
    imm_env: or_default_env(env),
    imm_analyses: ref([]),
  };
  let id = (~loc=?, ~env=?, id) => mk(~loc?, ~env?, ImmId(id));
  let const = (~loc=?, ~env=?, const) => mk(~loc?, ~env?, ImmConst(const));
};

module Comp = {
  let mk = (~loc=?, ~attributes=?, ~env=?, d) => {
    comp_desc: d,
    comp_loc: or_default_loc(loc),
    comp_env: or_default_env(env),
    comp_attributes: or_default_attributes(attributes),
    comp_analyses: ref([]),
  };
  let imm = (~loc=?, ~attributes=?, ~env=?, imm) =>
    mk(~loc?, ~attributes?, ~env?, CImmExpr(imm));
  let number = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~env?, CNumber(i));
  let int32 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~env?, CInt32(i));
  let int64 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~env?, CInt64(i));
  let float32 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~env?, CFloat32(i));
  let float64 = (~loc=?, ~attributes=?, ~env=?, i) =>
    mk(~loc?, ~attributes?, ~env?, CFloat64(i));
  let prim1 = (~loc=?, ~attributes=?, ~env=?, p1, a) =>
    mk(~loc?, ~attributes?, ~env?, CPrim1(p1, a));
  let prim2 = (~loc=?, ~attributes=?, ~env=?, p2, a1, a2) =>
    mk(~loc?, ~attributes?, ~env?, CPrim2(p2, a1, a2));
  let primn = (~loc=?, ~attributes=?, ~env=?, p, args) =>
    mk(~loc?, ~attributes?, ~env?, CPrimN(p, args));
  let box_assign = (~loc=?, ~attributes=?, ~env=?, a1, a2) =>
    mk(~loc?, ~attributes?, ~env?, CBoxAssign(a1, a2));
  let assign = (~loc=?, ~attributes=?, ~env=?, a1, a2) =>
    mk(~loc?, ~attributes?, ~env?, CAssign(a1, a2));
  let tuple = (~loc=?, ~attributes=?, ~env=?, elts) =>
    mk(~loc?, ~attributes?, ~env?, CTuple(elts));
  let array = (~loc=?, ~attributes=?, ~env=?, elts) =>
    mk(~loc?, ~attributes?, ~env?, CArray(elts));
  let array_get = (~loc=?, ~attributes=?, ~env=?, arr, i) =>
    mk(~loc?, ~attributes?, ~env?, CArrayGet(arr, i));
  let array_set = (~loc=?, ~attributes=?, ~env=?, arr, i, a) =>
    mk(~loc?, ~attributes?, ~env?, CArraySet(arr, i, a));
  let record = (~loc=?, ~attributes=?, ~env=?, ttag, elts) =>
    mk(~loc?, ~attributes?, ~env?, CRecord(ttag, elts));
  let adt = (~loc=?, ~attributes=?, ~env=?, ttag, vtag, elts) =>
    mk(~loc?, ~attributes?, ~env?, CAdt(ttag, vtag, elts));
  let tuple_get = (~loc=?, ~attributes=?, ~env=?, idx, tup) =>
    mk(~loc?, ~attributes?, ~env?, CGetTupleItem(idx, tup));
  let tuple_set = (~loc=?, ~attributes=?, ~env=?, idx, tup, value) =>
    mk(~loc?, ~attributes?, ~env?, CSetTupleItem(idx, tup, value));
  let adt_get = (~loc=?, ~attributes=?, ~env=?, idx, value) =>
    mk(~loc?, ~attributes?, ~env?, CGetAdtItem(idx, value));
  let adt_get_tag = (~loc=?, ~attributes=?, ~env=?, value) =>
    mk(~loc?, ~attributes?, ~env?, CGetAdtTag(value));
  let record_get = (~loc=?, ~attributes=?, ~env=?, idx, record) =>
    mk(~loc?, ~attributes?, ~env?, CGetRecordItem(idx, record));
  let record_set = (~loc=?, ~attributes=?, ~env=?, idx, record, arg) =>
    mk(~loc?, ~attributes?, ~env?, CSetRecordItem(idx, record, arg));
  let if_ = (~loc=?, ~attributes=?, ~env=?, cond, tru, fals) =>
    mk(~loc?, ~attributes?, ~env?, CIf(cond, tru, fals));
  let while_ = (~loc=?, ~attributes=?, ~env=?, cond, body) =>
    mk(~loc?, ~attributes?, ~env?, CWhile(cond, body));
  let switch_ = (~loc=?, ~attributes=?, ~env=?, arg, branches) =>
    mk(~loc?, ~attributes?, ~env?, CSwitch(arg, branches));
  let app = (~loc=?, ~attributes=?, ~env=?, func, args) =>
    mk(~loc?, ~attributes?, ~env?, CApp(func, args));
  let app_builtin = (~loc=?, ~attributes=?, ~env=?, modname, name, args) =>
    mk(~loc?, ~attributes?, ~env?, CAppBuiltin(modname, name, args));
  let lambda = (~loc=?, ~attributes=?, ~env=?, args, body) =>
    mk(~loc?, ~attributes?, ~env?, CLambda(args, body));
  let string = (~loc=?, ~attributes=?, ~env=?, s) =>
    mk(~loc?, ~attributes?, ~env?, CString(s));
  let char = (~loc=?, ~attributes=?, ~env=?, c) =>
    mk(~loc?, ~attributes?, ~env?, CChar(c));
};

module AExp = {
  let mk = (~loc=?, ~env=?, d) => {
    anf_desc: d,
    anf_loc: or_default_loc(loc),
    anf_env: or_default_env(env),
    anf_analyses: ref([]),
  };
  let let_ = (~loc=?, ~env=?, ~global=Nonglobal, rec_flag, binds, body) =>
    mk(~loc?, ~env?, AELet(global, rec_flag, binds, body));
  let seq = (~loc=?, ~env=?, hd, tl) => mk(~loc?, ~env?, AESeq(hd, tl));
  let comp = (~loc=?, ~env=?, e) => mk(~loc?, ~env?, AEComp(e));
};

module Imp = {
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
  let js_func = (~global=Nonglobal, a, md, name, s) =>
    mk(a, JSFunction(md, name), s, global);
};
