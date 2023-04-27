open Anftree;
open Anf_iterator;
open Grain_typed;

type analysis +=
  | Pure(bool);

let rec get_purity = lst =>
  switch (lst) {
  | [] => None
  | [Pure(x), ..._] => Some(x)
  | [_, ...tl] => get_purity(tl)
  };

module StringHash =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let comp_expression_purity = ({comp_analyses}) =>
  get_purity(comp_analyses^);
let anf_expression_purity = ({anf_analyses}) => get_purity(anf_analyses^);

/* Quick accessors for known-existing values */
let comp_expression_purity_internal = c =>
  Option.get(comp_expression_purity(c));
let anf_expression_purity_internal = a =>
  Option.get(anf_expression_purity(a));

let push_purity = (lref, p) => lref := [Pure(p), ...lref^];

module PurityArg: Anf_iterator.IterArgument = {
  include Anf_iterator.DefaultIterArgument;

  let leave_comp_expression = ({comp_desc: desc, comp_analyses: analyses}) =>
    push_purity(analyses) @@
    (
      switch (desc) {
      | CImmExpr({imm_desc: ImmTrap}) => false
      | CImmExpr(_) => true
      | CPrim0(
          AllocateInt32 | AllocateInt64 | AllocateUint32 | AllocateUint64 |
          AllocateFloat32 |
          AllocateFloat64 |
          AllocateRational |
          HeapStart |
          HeapTypeMetadata,
        ) =>
        true
      | CPrim0(WasmMemorySize | Unreachable) => false
      | CPrim1(
          AllocateArray | AllocateTuple | AllocateBytes | AllocateString |
          BuiltinId |
          NewInt32 |
          NewInt64 |
          NewUint32 |
          NewUint64 |
          NewFloat32 |
          NewFloat64 |
          LoadAdtVariant |
          StringSize |
          BytesSize |
          TagSimpleNumber |
          UntagSimpleNumber |
          TagChar |
          UntagChar |
          TagInt8 |
          UntagInt8 |
          TagInt16 |
          UntagInt16 |
          TagUint8 |
          UntagUint8 |
          TagUint16 |
          UntagUint16 |
          Magic |
          Not |
          Box |
          Unbox |
          BoxBind |
          UnboxBind |
          ArrayLength |
          WasmFromGrain |
          WasmToGrain |
          WasmUnaryI32(_) |
          WasmUnaryI64(_) |
          WasmUnaryF32(_) |
          WasmUnaryF64(_) |
          WasmMemoryGrow,
          _,
        ) =>
        true
      // We consider Ignore to be impure to provide sane semantics around reference holding
      | CPrim1(Ignore | Assert | Throw | AllocateBigInt, _) => false
      | CPrim2(
          NewRational | Is | Eq | And | Or | WasmLoadI32(_) | WasmLoadI64(_) |
          WasmLoadF32 |
          WasmLoadF64 |
          WasmBinaryI32(_) |
          WasmBinaryI64(_) |
          WasmBinaryF32(_) |
          WasmBinaryF64(_),
          _,
          _,
        ) =>
        true
      | CPrimN(
          WasmStoreI32(_) | WasmStoreI64(_) | WasmStoreF32 | WasmStoreF64 |
          WasmMemoryCopy |
          WasmMemoryFill |
          WasmMemoryCompare,
          _,
        ) =>
        false
      | CArraySet(_)
      | CBoxAssign(_)
      | CAssign(_)
      | CLocalAssign(_) => false
      | CArrayGet(_)
      | CArray(_)
      | CTuple(_)
      | CAdt(_)
      | CRecord(_)
      | CGetTupleItem(_) => true
      | CSetTupleItem(_) => false
      | CGetAdtItem(_)
      | CGetAdtTag(_)
      | CGetRecordItem(_) => true
      | CSetRecordItem(_) => false
      | CIf(_, t, f) =>
        anf_expression_purity_internal(t)
        && anf_expression_purity_internal(f)
      | CFor(c, inc, body) =>
        Option.fold(~none=true, ~some=anf_expression_purity_internal, c)
        && Option.fold(~none=true, ~some=anf_expression_purity_internal, inc)
        && anf_expression_purity_internal(body)
      | CContinue
      | CBreak
      | CReturn(_) => false
      | CSwitch(_, branches, _) =>
        let branches_purities =
          List.map(
            ((_, b)) => {anf_expression_purity_internal(b)},
            branches,
          );
        List.for_all(x => x, branches_purities);
      | CApp(_) => false
      | CLambda(_)
      | CNumber(_)
      | CInt32(_)
      | CInt64(_)
      | CUint32(_)
      | CUint64(_)
      | CFloat32(_)
      | CFloat64(_)
      | CBytes(_)
      | CString(_) => true
      }
    );

  let leave_anf_expression = ({anf_desc: desc, anf_analyses: analyses}) =>
    push_purity(analyses) @@
    (
      switch (desc) {
      | AELet(_, _, _, binds, body) =>
        let process_bind = ((_, bind)) => {
          comp_expression_purity_internal(bind);
        };

        let bind_purity =
          List.for_all(x => x, List.map(process_bind, binds));
        anf_expression_purity_internal(body) && bind_purity;
      | AESeq(hd, tl) =>
        comp_expression_purity_internal(hd)
        && anf_expression_purity_internal(tl)
      | AEComp(c) => comp_expression_purity_internal(c)
      }
    );
};

module PurityIterator = Anf_iterator.MakeIter(PurityArg);

let analyze = anfprog => PurityIterator.iter_anf_program(anfprog);
