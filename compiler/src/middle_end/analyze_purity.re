open Anftree;
open Anf_iterator;
open Grain_typed;

/*
    NOTE: Not every ANF node is guaranteed to have a purity analysis,
    but every node is guaranteed to either have one or be the child
    of a node which does.
 */

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

let rec analyze_comp_expression =
        ({comp_desc: desc, comp_analyses: analyses}) => {
  let purity =
    switch (desc) {
    | CImmExpr({imm_desc: ImmTrap}) => false
    | CImmExpr(_) => true
    | CPrim0(
        AllocateChar | AllocateInt32 | AllocateInt64 | AllocateFloat32 |
        AllocateFloat64 |
        AllocateRational,
      ) =>
      true
    | CPrim1(
        AllocateArray | AllocateTuple | AllocateBytes | AllocateString |
        NewInt32 |
        NewInt64 |
        NewFloat32 |
        NewFloat64 |
        LoadAdtVariant |
        StringSize |
        BytesSize |
        TagSimpleNumber |
        UntagSimpleNumber |
        Not |
        Box |
        Unbox |
        BoxBind |
        UnboxBind |
        Ignore |
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
    | CPrim1(Assert | Throw, _) => false
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
        WasmMemorySize |
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
      analyze_anf_expression(t);
      analyze_anf_expression(f);
      anf_expression_purity_internal(t) && anf_expression_purity_internal(f);
    | CFor(c, inc, body) =>
      Option.iter(analyze_anf_expression, c);
      Option.iter(analyze_anf_expression, inc);
      analyze_anf_expression(body);
      Option.fold(~none=true, ~some=anf_expression_purity_internal, c)
      && Option.fold(~none=true, ~some=anf_expression_purity_internal, inc)
      && anf_expression_purity_internal(body);
    | CContinue
    | CBreak => false
    | CSwitch(_, branches, _) =>
      let branches_purities =
        List.map(
          ((t, b)) => {
            analyze_anf_expression(b);
            anf_expression_purity_internal(b);
          },
          branches,
        );
      List.for_all(x => x, branches_purities);
    | CApp(_) => false
    | CAppBuiltin(_) => false
    | CLambda(_, _, (body, _)) =>
      analyze_anf_expression(body);
      true;
    | CNumber(_)
    | CInt32(_)
    | CInt64(_)
    | CFloat32(_)
    | CFloat64(_)
    | CBytes(_)
    | CString(_)
    | CChar(_) => true
    };

  push_purity(analyses, purity);
}

and analyze_anf_expression = ({anf_desc: desc, anf_analyses: analyses}) =>
  switch (desc) {
  | AELet(g, Nonrecursive, _, binds, body) =>
    let process_bind = ((id, bind)) => {
      analyze_comp_expression(bind);
      comp_expression_purity_internal(bind);
    };

    let bind_purity = List.for_all(x => x, List.map(process_bind, binds));
    analyze_anf_expression(body);
    let purity = anf_expression_purity_internal(body) && bind_purity;
    push_purity(analyses, purity);
  | AELet(g, Recursive, _, binds, body) =>
    let process_bind = ((id, bind)) => {
      analyze_comp_expression(bind);
      comp_expression_purity_internal(bind);
    };

    let bind_purity = List.for_all(x => x, List.map(process_bind, binds));
    analyze_anf_expression(body);
    let purity = anf_expression_purity_internal(body) && bind_purity;
    push_purity(analyses, purity);
  | AESeq(hd, tl) =>
    analyze_comp_expression(hd);
    analyze_anf_expression(tl);
    let purity =
      comp_expression_purity_internal(hd)
      && anf_expression_purity_internal(tl);
    push_purity(analyses, purity);
  | AEComp(c) =>
    analyze_comp_expression(c);
    let purity = comp_expression_purity_internal(c);
    push_purity(analyses, purity);
  };

let analyze = ({imports, body, analyses}) => {
  analyze_anf_expression(body);
};
