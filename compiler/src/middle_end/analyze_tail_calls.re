open Anftree;
open Anf_iterator;
open Grain_typed;

type analysis +=
  | TailCall(bool)
  | TailRecursive(bool);

let tail_callable_names = Ident_tbl.create(50);

let push_tail_callable_name = id =>
  Ident_tbl.add(tail_callable_names, id, true);

let pop_tail_callable_name = id => Ident_tbl.remove(tail_callable_names, id);

let is_tail_callable = id => Ident_tbl.mem(tail_callable_names, id);

let push_tail_call = analysis => analysis := [TailCall(true), ...analysis^];

let push_tail_recursive = analysis =>
  analysis := [TailRecursive(true), ...analysis^];

/* Indicate whether or not this expression contains a tail call that needs optimization */
let rec analyze_comp_expression =
        (is_tail, {comp_desc: desc, comp_analyses: analyses}) =>
  switch (desc) {
  | CIf(_, t, f) =>
    let t_branch = analyze_anf_expression(is_tail, t);
    let f_branch = analyze_anf_expression(is_tail, f);
    t_branch || f_branch;
  | CSwitch(_, branches, _) =>
    List.fold_left(
      (has_tail_call, (_, b)) =>
        analyze_anf_expression(is_tail, b) || has_tail_call,
      false,
      branches,
    )
  | CFor(_, _, body) =>
    /* While this loop itself is not in tail position, we still want to analyze the body. */
    ignore @@ analyze_anf_expression(false, body);
    false;
  | CLambda(_, args, (body, _), _) =>
    /* While this lambda itself is not in tail position, we still want to analyze the body. */
    ignore @@ analyze_anf_expression(true, body);
    false;
  | CApp(({imm_desc: ImmId(id)}, _), _, _) =>
    if (is_tail) {
      push_tail_call(analyses);
    };
    is_tail_callable(id);
  | CApp(_) =>
    if (is_tail) {
      push_tail_call(analyses);
    };
    false;
  | CReturn(_)
  | CBoxAssign(_)
  | CAssign(_)
  | CLocalAssign(_)
  | CTuple(_)
  | CArray(_)
  | CArrayGet(_)
  | CArraySet(_)
  | CRecord(_)
  | CAdt(_)
  | CSetTupleItem(_)
  | CGetTupleItem(_)
  | CGetAdtItem(_)
  | CGetAdtTag(_)
  | CGetRecordItem(_)
  | CSetRecordItem(_)
  | CContinue
  | CBreak
  | CBytes(_)
  | CString(_)
  | CNumber(_)
  | CInt32(_)
  | CInt64(_)
  | CUint32(_)
  | CUint64(_)
  | CFloat32(_)
  | CFloat64(_)
  | CPrim0(_)
  | CPrim1(_)
  | CPrim2(_)
  | CPrimN(_)
  | CImmExpr(_) => false
  }

/* Mark functions as tail-recursive */
and analyze_anf_expression =
    (is_tail, {anf_desc: desc, anf_analyses: analyses}) =>
  switch (desc) {
  | AELet(_, Nonrecursive, _, binds, body) =>
    /* None of these binds are in tail position */
    List.iter(
      ((_, exp)) => ignore @@ analyze_comp_expression(false, exp),
      binds,
    );
    analyze_anf_expression(is_tail, body);
  | AELet(_, Recursive, _, binds, body) =>
    List.iter(((id, _)) => push_tail_callable_name(id), binds);
    List.iter(
      ((_, {comp_desc, comp_analyses} as bind)) =>
        switch (comp_desc) {
        | CLambda(_, args, (body, _), _) =>
          if (analyze_anf_expression(true, body)) {
            push_tail_recursive(comp_analyses);
          }
        | _ => ignore @@ analyze_comp_expression(false, bind)
        },
      binds,
    );
    List.iter(((id, _)) => pop_tail_callable_name(id), binds);
    analyze_anf_expression(is_tail, body);
  | AESeq(hd, tl) =>
    /* Only the AEComp at the end of a sequence is in tail position */
    ignore @@ analyze_comp_expression(false, hd);
    analyze_anf_expression(is_tail, tl);
  | AEComp(c) => analyze_comp_expression(is_tail, c)
  };

let comp_is_tail_recursive = ({comp_analyses}) => {
  let rec is_tail_recursive = analyses =>
    switch (analyses) {
    | [TailRecursive(b), ..._] => Some(b)
    | [_, ...tl] => is_tail_recursive(tl)
    | [] => None
    };
  is_tail_recursive(comp_analyses^);
};

let comp_is_tail_call = ({comp_analyses}) => {
  let rec is_tail_call = analyses =>
    switch (analyses) {
    | [TailCall(b), ..._] => Some(b)
    | [_, ...tl] => is_tail_call(tl)
    | [] => None
    };
  is_tail_call(comp_analyses^);
};

let mark_not_tail_recursive = ({comp_analyses}) => {
  let rec process_analyses =
    fun
    | [TailRecursive(true), ...tl] => [TailRecursive(false), ...tl]
    | [hd, ...tl] => [hd, ...process_analyses(tl)]
    | [] => [];
  comp_analyses := process_analyses(comp_analyses^);
};

let analyze = ({body}) => ignore @@ analyze_anf_expression(true, body);
