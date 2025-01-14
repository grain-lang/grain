/* Modelled off of typetreeMap.ml; see note about OCaml copyright */
open Anftree;
open Grain_parsing;
open Grain_typed;
open Types;

/* A stack of ident's representing the parent bindings */
type contexts = list(Ident.t);

/*
  This module pushes work onto a stack and processes it in a loop to keep
  stack usage low. This is done instead of something simpler like CPS because
  of JSOO's limitation of tail call optimization.
 */

type stack_node =
  | AnfNode(contexts, anf_expression)
  | AnfMarker(contexts, anf_marker)
  | BindingsNode(contexts, list((Ident.t, comp_expression)))
  | BindingsMarker(contexts, list(Ident.t))
  | CompNode(contexts, comp_expression)
  | CompMarker(contexts, comp_marker)
  | BranchesNode(contexts, list((int, anf_expression)))
  | BranchesMarker(contexts, list(int))
  | OptNode(option(stack_node))
  | OptMarker

and anf_marker =
  | Let(
      (list((Ident.t, comp_expression)), anf_expression) => anf_expression,
    )
  | Seq((comp_expression, anf_expression) => anf_expression)
  | Comp(comp_expression => anf_expression)

and comp_marker =
  | If((anf_expression, anf_expression) => comp_expression)
  | For(
      (option(anf_expression), option(anf_expression), anf_expression) =>
      comp_expression,
    )
  | Switch(list((int, anf_expression)) => comp_expression)
  | Lambda(anf_expression => comp_expression);

let inputs = ref([]);
let outputs = ref([]);

let push_input = v => {
  inputs := [v, ...inputs^];
};

let push_output = v => {
  outputs := [v, ...outputs^];
};

let pop_input = () => {
  switch (inputs^) {
  | [hd, ...tl] =>
    inputs := tl;
    Some(hd);
  | _ => None
  };
};

let rec list_take = (i, list) =>
  if (i == 0) {
    [];
  } else {
    switch (list) {
    | [hd, ...tl] => [hd, ...list_take(i - 1, tl)]
    | [] => raise(Invalid_argument("Not enough elements to take"))
    };
  };

let rec list_drop = (i, list) =>
  if (i == 0) {
    list;
  } else {
    switch (list) {
    | [_, ...tl] => list_drop(i - 1, tl)
    | [] => raise(Invalid_argument("Not enough elements to drop"))
    };
  };

module type MapArgument = {
  let enter_imm_expression: (list(Ident.t), imm_expression) => imm_expression;
  let leave_imm_expression: imm_expression => imm_expression;

  let enter_comp_expression: comp_expression => comp_expression;
  let leave_comp_expression: comp_expression => comp_expression;

  let enter_anf_expression: anf_expression => anf_expression;
  let leave_anf_expression: anf_expression => anf_expression;

  let enter_anf_program: anf_program => anf_program;
  let leave_anf_program: anf_program => anf_program;
};

module DefaultMapArgument: MapArgument = {
  let enter_imm_expression = (c, e) => e;
  let leave_imm_expression = e => e;

  let enter_comp_expression = e => e;
  let leave_comp_expression = e => e;

  let enter_anf_expression = e => e;
  let leave_anf_expression = e => e;

  let enter_anf_program = p => p;
  let leave_anf_program = p => p;
};

module MakeMap = (Iter: MapArgument) => {
  let rec process_imm_expression = (contexts, i) =>
    Iter.leave_imm_expression(Iter.enter_imm_expression(contexts, i))

  and process_comp_expression = (contexts, c) => {
    let {comp_desc: desc} as c = Iter.enter_comp_expression(c);
    let leave_with = d =>
      push_output(
        CompNode(contexts, Iter.leave_comp_expression({...c, comp_desc: d})),
      );
    let process_imm_expression = process_imm_expression(contexts);
    switch (desc) {
    | CImmExpr(i) => leave_with(CImmExpr(process_imm_expression(i)))
    | CPrim0(p0) => leave_with(CPrim0(p0))
    | CPrim1(p1, arg) =>
      leave_with(CPrim1(p1, process_imm_expression(arg)))
    | CPrim2(p2, arg1, arg2) =>
      let arg1 = process_imm_expression(arg1);
      let arg2 = process_imm_expression(arg2);
      leave_with(CPrim2(p2, arg1, arg2));
    | CPrimN(p, args) =>
      leave_with(CPrimN(p, List.map(process_imm_expression, args)))
    | CBoxAssign(lhs, rhs) =>
      let lhs = process_imm_expression(lhs);
      let rhs = process_imm_expression(rhs);
      leave_with(CBoxAssign(lhs, rhs));
    | CAssign(lhs, rhs) =>
      let lhs = process_imm_expression(lhs);
      let rhs = process_imm_expression(rhs);
      leave_with(CAssign(lhs, rhs));
    | CLocalAssign(id, rhs) =>
      let rhs = process_imm_expression(rhs);
      leave_with(CLocalAssign(id, rhs));
    | CTuple(elts) =>
      leave_with(CTuple(List.map(process_imm_expression, elts)))
    | CArray(elts) =>
      leave_with(CArray(List.map(process_imm_expression, elts)))
    | CArrayGet(arg1, arg2) =>
      leave_with(
        CArrayGet(
          process_imm_expression(arg1),
          process_imm_expression(arg2),
        ),
      )
    | CArraySet(arg1, arg2, arg3) =>
      leave_with(
        CArraySet(
          process_imm_expression(arg1),
          process_imm_expression(arg2),
          process_imm_expression(arg3),
        ),
      )
    | CRecord(type_hash, ttag, elts) =>
      leave_with(
        CRecord(
          process_imm_expression(type_hash),
          process_imm_expression(ttag),
          List.map(
            ((name, elt)) => (name, process_imm_expression(elt)),
            elts,
          ),
        ),
      )
    | CAdt(type_hash, ttag, vtag, elts) =>
      leave_with(
        CAdt(
          process_imm_expression(type_hash),
          process_imm_expression(ttag),
          process_imm_expression(vtag),
          List.map(process_imm_expression, elts),
        ),
      )
    | CGetTupleItem(idx, tup) =>
      leave_with(CGetTupleItem(idx, process_imm_expression(tup)))
    | CSetTupleItem(idx, tup, value) =>
      let tup = process_imm_expression(tup);
      let value = process_imm_expression(value);
      leave_with(CSetTupleItem(idx, tup, value));
    | CGetAdtItem(idx, adt) =>
      leave_with(CGetAdtItem(idx, process_imm_expression(adt)))
    | CGetAdtTag(adt) => leave_with(CGetAdtTag(process_imm_expression(adt)))
    | CGetRecordItem(idx, record) =>
      leave_with(CGetRecordItem(idx, process_imm_expression(record)))
    | CSetRecordItem(idx, record, arg) =>
      leave_with(
        CSetRecordItem(
          idx,
          process_imm_expression(record),
          process_imm_expression(arg),
        ),
      )
    | CIf(cond, t, f) =>
      let cond = process_imm_expression(cond);
      push_input(
        CompMarker(
          contexts,
          If((t, f) => {...c, comp_desc: CIf(cond, t, f)}),
        ),
      );
      push_input(AnfNode(contexts, f));
      push_input(AnfNode(contexts, t));
    | CFor(cond, inc, body) =>
      push_input(
        CompMarker(
          contexts,
          For(
            (cond, inc, body) => {...c, comp_desc: CFor(cond, inc, body)},
          ),
        ),
      );
      push_input(AnfNode(contexts, body));
      push_input(OptNode(Option.map(inc => AnfNode(contexts, inc), inc)));
      push_input(
        OptNode(Option.map(cond => AnfNode(contexts, cond), cond)),
      );
    | CContinue => leave_with(CContinue)
    | CBreak => leave_with(CBreak)
    | CReturn(expr) =>
      leave_with(CReturn(Option.map(process_imm_expression, expr)))
    | CSwitch(cond, branches, partial) =>
      let cond = process_imm_expression(cond);
      push_input(
        CompMarker(
          contexts,
          Switch(
            branches => {...c, comp_desc: CSwitch(cond, branches, partial)},
          ),
        ),
      );
      push_input(BranchesNode(contexts, branches));
    | CApp((f, fty), args, tail) =>
      let f = process_imm_expression(f);
      let args = List.map(process_imm_expression, args);
      leave_with(CApp((f, fty), args, tail));
    | CLambda(name, idents, (expr, alloc_ty), closure_status) =>
      push_input(
        CompMarker(
          contexts,
          Lambda(
            expr =>
              {
                ...c,
                comp_desc:
                  CLambda(name, idents, (expr, alloc_ty), closure_status),
              },
          ),
        ),
      );
      push_input(AnfNode(contexts, expr));
    | CBytes(b) => leave_with(CBytes(b))
    | CString(s) => leave_with(CString(s))
    | CNumber(i) => leave_with(CNumber(i))
    | CInt32(i) => leave_with(CInt32(i))
    | CInt64(i) => leave_with(CInt64(i))
    | CUint32(i) => leave_with(CUint32(i))
    | CUint64(i) => leave_with(CUint64(i))
    | CFloat32(f) => leave_with(CFloat32(f))
    | CFloat64(f) => leave_with(CFloat64(f))
    };
  }

  and process_anf_expression = (contexts, anf) => {
    let {anf_desc: desc} as anf = Iter.enter_anf_expression(anf);
    switch (desc) {
    | AELet(g, r, m, bindings, body) =>
      push_input(
        AnfMarker(
          contexts,
          Let(
            (bindings, body) =>
              {...anf, anf_desc: AELet(g, r, m, bindings, body)},
          ),
        ),
      );
      push_input(AnfNode(contexts, body));
      push_input(BindingsNode(contexts, bindings));
    | AESeq(hd, tl) =>
      push_input(
        AnfMarker(
          contexts,
          Seq((hd, tl) => {...anf, anf_desc: AESeq(hd, tl)}),
        ),
      );
      push_input(AnfNode(contexts, tl));
      push_input(CompNode(contexts, hd));
    | AEComp(c) =>
      push_input(
        AnfMarker(contexts, Comp(c => {...anf, anf_desc: AEComp(c)})),
      );
      push_input(CompNode(contexts, c));
    };
  }

  and map_bindings = (contexts, bindings) => {
    let (names, _) = List.split(bindings);
    push_input(BindingsMarker(contexts, names));
    List.iter(
      ((name, bind)) => process_comp_expression([name, ...contexts], bind),
      bindings,
    );
  }

  and map_branches = (contexts, branches) => {
    let (tags, branches) = List.split(branches);
    push_input(BranchesMarker(contexts, tags));
    List.iter(process_anf_expression(contexts), branches);
  }

  and process_worklist = () => {
    switch (pop_input()) {
    | None => ()
    | Some(input) =>
      switch (input) {
      | OptNode(None) => push_output(OptNode(None))
      | OptNode(Some(node)) =>
        push_input(OptMarker);
        push_input(node);
      | OptMarker =>
        switch (outputs^) {
        | [hd, ...rest] => outputs := [OptNode(Some(hd)), ...rest]
        | [] => failwith("Impossible: invalid output stack")
        }
      | BindingsMarker(contexts, names) =>
        let count = List.length(names);
        let binds = list_take(count, outputs^);
        let binds =
          List.map(
            bind => {
              switch (bind) {
              | CompNode(contexts, bind) => bind
              | _ => failwith("Impossible: invalid output stack")
              }
            },
            binds,
          );
        let binds = List.combine(names, binds);
        outputs := list_drop(count, outputs^);
        outputs := [BindingsNode(contexts, binds), ...outputs^];
      | BranchesMarker(contexts, tags) =>
        let count = List.length(tags);
        let branches = list_take(count, outputs^);
        let branches =
          List.map(
            bind => {
              switch (bind) {
              | AnfNode(_, bind) => bind
              | _ => failwith("Impossible: invalid output stack")
              }
            },
            branches,
          );
        let branches = List.combine(tags, branches);
        outputs := list_drop(count, outputs^);
        outputs := [BranchesNode(contexts, branches), ...outputs^];
      | AnfNode(contexts, anf) => process_anf_expression(contexts, anf)
      | CompNode(contexts, comp) => process_comp_expression(contexts, comp)
      | BindingsNode(contexts, bindings) => map_bindings(contexts, bindings)
      | BranchesNode(contexts, branches) => map_branches(contexts, branches)
      | AnfMarker(contexts, Let(f)) =>
        switch (outputs^) {
        | [AnfNode(_, body), BindingsNode(_, bindings), ...rest] =>
          let node = Iter.leave_anf_expression(f(bindings, body));
          outputs := [AnfNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      | AnfMarker(contexts, Seq(f)) =>
        switch (outputs^) {
        | [AnfNode(_, body), CompNode(_, comp), ...rest] =>
          let node = Iter.leave_anf_expression(f(comp, body));
          outputs := [AnfNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      | AnfMarker(contexts, Comp(f)) =>
        switch (outputs^) {
        | [CompNode(_, comp), ...rest] =>
          let node = Iter.leave_anf_expression(f(comp));
          outputs := [AnfNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      | CompMarker(contexts, If(f)) =>
        switch (outputs^) {
        | [AnfNode(_, false_), AnfNode(_, true_), ...rest] =>
          let node = Iter.leave_comp_expression(f(true_, false_));
          outputs := [CompNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      | CompMarker(contexts, Switch(f)) =>
        switch (outputs^) {
        | [BranchesNode(_, branches), ...rest] =>
          let node = Iter.leave_comp_expression(f(branches));
          outputs := [CompNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      | CompMarker(contexts, Lambda(f)) =>
        switch (outputs^) {
        | [AnfNode(contexts, body), ...rest] =>
          let node = Iter.leave_comp_expression(f(body));
          outputs := [CompNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      | CompMarker(contexts, For(f)) =>
        switch (outputs^) {
        | [AnfNode(_, body), OptNode(inc), OptNode(cond), ...rest] =>
          let cond =
            switch (cond) {
            | Some(AnfNode(_, cond)) => Some(cond)
            | None => None
            | _ => failwith("Impossible: invalid output stack")
            };
          let inc =
            switch (inc) {
            | Some(AnfNode(_, inc)) => Some(inc)
            | None => None
            | _ => failwith("Impossible: invalid output stack")
            };
          let node = Iter.leave_comp_expression(f(cond, inc, body));
          outputs := [CompNode(contexts, node), ...rest];
        | _ => failwith("Impossible: invalid output stack")
        }
      };
      process_worklist();
    };
  }

  and map_anf_program = prog => {
    let {body} as prog = Iter.enter_anf_program(prog);
    push_input(AnfNode([], body));
    process_worklist();
    let body =
      switch (outputs^) {
      | [AnfNode(_, body)] =>
        outputs := [];
        body;
      | _ => failwith("Impossible: invalid output stack")
      };
    Iter.leave_anf_program({...prog, body});
  };
};
