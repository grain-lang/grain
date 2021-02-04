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
    | CImmExpr(_) => true
    | CPrim1(
        Incr | Decr | Not | Box | Unbox | Ignore | ArrayLength |
        Int64FromNumber |
        Int64ToNumber |
        Int32ToNumber |
        Float64ToNumber |
        Float32ToNumber |
        Int64Lnot |
        WasmFromGrain |
        WasmToGrain |
        WasmUnaryI32(_) |
        WasmUnaryI64(_) |
        WasmUnaryF32(_) |
        WasmUnaryF64(_),
        _,
      ) =>
      true
    | CPrim1(Assert | FailWith, _) => false
    | CPrim2(
        Plus | Minus | Times | Divide | Mod | Less | Greater | LessEq |
        GreaterEq |
        Is |
        Eq |
        And |
        Or |
        ArrayMake |
        Int64Land |
        Int64Lor |
        Int64Lxor |
        Int64Lsl |
        Int64Lsr |
        Int64Asr |
        Int64Gt |
        Int64Gte |
        Int64Lt |
        Int64Lte |
        WasmLoadI32(_) |
        WasmLoadI64(_) |
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
    | CPrim2(ArrayInit, _, _) => false // Array.init takes (and calls) a function which could have side effects
    | CPrimN(
        WasmStoreI32(_) | WasmStoreI64(_) | WasmStoreF32 | WasmStoreF64 |
        WasmMemoryCopy |
        WasmMemoryFill,
        _,
      ) =>
      false
    | CArraySet(_)
    | CBoxAssign(_)
    | CAssign(_) =>
      /* TODO: Would be nice if we could "scope" the purity analysis to local assignments */
      false
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
    | CWhile(c, body) =>
      analyze_anf_expression(c);
      analyze_anf_expression(body);
      anf_expression_purity_internal(c)
      && anf_expression_purity_internal(body);
    | CSwitch(_, branches) =>
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
    | CLambda(_, (body, _)) =>
      analyze_anf_expression(body);
      true;
    | CNumber(_)
    | CInt32(_)
    | CInt64(_)
    | CFloat32(_)
    | CFloat64(_)
    | CString(_)
    | CChar(_) => true
    };

  push_purity(analyses, purity);
}

and analyze_anf_expression = ({anf_desc: desc, anf_analyses: analyses}) =>
  switch (desc) {
  | AELet(g, Nonrecursive, binds, body) =>
    let process_bind = ((id, bind)) => {
      analyze_comp_expression(bind);
      comp_expression_purity_internal(bind);
    };

    let bind_purity = List.for_all(x => x, List.map(process_bind, binds));
    analyze_anf_expression(body);
    let purity = anf_expression_purity_internal(body) && bind_purity;
    push_purity(analyses, purity);
  | AELet(g, Recursive, binds, body) =>
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
