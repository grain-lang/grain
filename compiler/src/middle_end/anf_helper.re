open Grain_parsing;
open Grain_typed;
open Anftree;

type str = loc(string);
type loc = Location.t;
type env = Env.t;
type ident = Ident.t;

let default_loc = Location.dummy_loc;
let default_env = Env.empty;

let or_default_loc = Option.value(~default=default_loc);
let or_default_env = Option.value(~default=default_env);

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
  let mk = (~loc=?, ~env=?, d) => {
    comp_desc: d,
    comp_loc: or_default_loc(loc),
    comp_env: or_default_env(env),
    comp_analyses: ref([]),
  };
  let imm = (~loc=?, ~env=?, imm) => mk(~loc?, ~env?, CImmExpr(imm));
  let number = (~loc=?, ~env=?, i) => mk(~loc?, ~env?, CNumber(i));
  let int32 = (~loc=?, ~env=?, i) => mk(~loc?, ~env?, CInt32(i));
  let int64 = (~loc=?, ~env=?, i) => mk(~loc?, ~env?, CInt64(i));
  let float32 = (~loc=?, ~env=?, i) => mk(~loc?, ~env?, CFloat32(i));
  let float64 = (~loc=?, ~env=?, i) => mk(~loc?, ~env?, CFloat64(i));
  let prim1 = (~loc=?, ~env=?, p1, a) => mk(~loc?, ~env?, CPrim1(p1, a));
  let prim2 = (~loc=?, ~env=?, p2, a1, a2) =>
    mk(~loc?, ~env?, CPrim2(p2, a1, a2));
  let box_assign = (~loc=?, ~env=?, a1, a2) =>
    mk(~loc?, ~env?, CBoxAssign(a1, a2));
  let assign = (~loc=?, ~env=?, a1, a2) =>
    mk(~loc?, ~env?, CAssign(a1, a2));
  let tuple = (~loc=?, ~env=?, elts) => mk(~loc?, ~env?, CTuple(elts));
  let array = (~loc=?, ~env=?, elts) => mk(~loc?, ~env?, CArray(elts));
  let array_get = (~loc=?, ~env=?, arr, i) =>
    mk(~loc?, ~env?, CArrayGet(arr, i));
  let array_set = (~loc=?, ~env=?, arr, i, a) =>
    mk(~loc?, ~env?, CArraySet(arr, i, a));
  let record = (~loc=?, ~env=?, ttag, elts) =>
    mk(~loc?, ~env?, CRecord(ttag, elts));
  let adt = (~loc=?, ~env=?, ttag, vtag, elts) =>
    mk(~loc?, ~env?, CAdt(ttag, vtag, elts));
  let tuple_get = (~loc=?, ~env=?, idx, tup) =>
    mk(~loc?, ~env?, CGetTupleItem(idx, tup));
  let tuple_set = (~loc=?, ~env=?, idx, tup, value) =>
    mk(~loc?, ~env?, CSetTupleItem(idx, tup, value));
  let adt_get = (~loc=?, ~env=?, idx, value) =>
    mk(~loc?, ~env?, CGetAdtItem(idx, value));
  let adt_get_tag = (~loc=?, ~env=?, value) =>
    mk(~loc?, ~env?, CGetAdtTag(value));
  let record_get = (~loc=?, ~env=?, idx, record) =>
    mk(~loc?, ~env?, CGetRecordItem(idx, record));
  let record_set = (~loc=?, ~env=?, idx, record, arg) =>
    mk(~loc?, ~env?, CSetRecordItem(idx, record, arg));
  let if_ = (~loc=?, ~env=?, cond, tru, fals) =>
    mk(~loc?, ~env?, CIf(cond, tru, fals));
  let while_ = (~loc=?, ~env=?, cond, body) =>
    mk(~loc?, ~env?, CWhile(cond, body));
  let switch_ = (~loc=?, ~env=?, arg, branches) =>
    mk(~loc?, ~env?, CSwitch(arg, branches));
  let app = (~loc=?, ~env=?, ~tail=false, func, args) =>
    mk(~loc?, ~env?, CApp(func, args, tail));
  let app_builtin = (~loc=?, ~env=?, modname, name, args) =>
    mk(~loc?, ~env?, CAppBuiltin(modname, name, args));
  let lambda = (~loc=?, ~env=?, args, body) =>
    mk(~loc?, ~env?, CLambda(args, body));
  let string = (~loc=?, ~env=?, s) => mk(~loc?, ~env?, CString(s));
  let char = (~loc=?, ~env=?, c) => mk(~loc?, ~env?, CChar(c));
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
