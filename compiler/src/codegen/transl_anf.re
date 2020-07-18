open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_utils;

open Asttypes;
open Anftree;
open Mashtree;

type compilation_env = {
  ce_binds: Ident.tbl(Mashtree.binding),
  /* Useful due to us needing a second pass over exports (for mutual recursion) */
  ce_exported_globals: Ident.tbl(int32),
  ce_stack_idx: int,
  ce_arity: int,
};

let initial_compilation_env = {
  ce_binds: Ident.empty,
  ce_exported_globals: Ident.empty,
  ce_stack_idx: 0,
  ce_arity: 0,
};

type worklist_elt_body =
  | Anf(anf_expression)
  | Precompiled(block);

type worklist_elt = {
  body: worklist_elt_body,
  env: compilation_env,
  arity: int,
  idx: int, /* Lambda-lifted index */
  stack_size: int,
  loc: Location.t,
};

// The OCaml docs warn that this isn't thread-safe,
// but I don't think we are threading yet
let compilation_worklist: Queue.t(worklist_elt) = Queue.create();

/** Lambda-lifting index (function index) */

let lift_index = ref(0);

let reset_lift = () => lift_index := 0;

let next_lift = () => {
  let ret = lift_index^;
  lift_index := ret + 1;
  ret;
};

/** Global index (index of global variables) */

let global_table = ref(Ident.empty: Ident.tbl((int32, int32)));
let global_index = ref(0);

let global_exports = () => {
  let tbl = global_table^;
  Ident.fold_all(
    (ex_name, (ex_global_index, ex_getter_index), acc) =>
      [{ex_name, ex_global_index, ex_getter_index}, ...acc],
    tbl,
    [],
  );
};

let reset_global = () => {
  global_table := Ident.empty;
  global_index := 0;
};

let next_global = id =>
  /* RIP Hygiene (this behavior works as expected until we have more metaprogramming constructs) */
  switch (Ident.find_same_opt(id, global_table^)) {
  | Some((ret, ret_get)) => (Int32.to_int(ret), Int32.to_int(ret_get))
  | None =>
    let ret = global_index^;
    let ret_get = next_lift();
    global_table :=
      Ident.add(
        id,
        (Int32.of_int(ret), Int32.of_int(ret_get)),
        global_table^,
      );
    global_index := ret + 1;
    (ret, ret_get);
  };

let find_id = (id, env) => Ident.find_same(id, env.ce_binds);
let find_global = (id, env) => Ident.find_same(id, env.ce_exported_globals);

let worklist_reset = () => Queue.clear(compilation_worklist);
let worklist_enqueue = elt => Queue.add(elt, compilation_worklist);
let worklist_empty = () => Queue.is_empty(compilation_worklist);
let worklist_pop = () =>
  switch (Queue.take_opt(compilation_worklist)) {
  | None => raise(Not_found)
  | Some(hd) => hd
  };

module IntMap = Map.Make(Int);
module RegisterAllocation = {
  type t = (IntMap.t(int), list(int));
  let initialize = (num_locals: int): t => (
    IntMap.empty,
    List.init(num_locals, x => x),
  );
  /* Takes an id (a local from the original mashtree) and assigns a slot to it */
  let allocate = var_id =>
    fun
    | (_, []) => failwith("Should be impossible")
    | (allocs, [hd, ...tl]) => ((IntMap.add(var_id, hd, allocs), tl), hd);

  let release_var = (var_id: int): (t => t) =>
    fun
    | (allocs, tl) => {
        let hd = IntMap.find(var_id, allocs);
        (allocs, [hd, ...tl]);
      };

  let release_slot = (slot_id, (allocs, tl): t) => (
    allocs,
    [slot_id, ...tl],
  );

  let get_allocations = ((allocs, _): t) => allocs;

  let get_allocation = (var_id, (allocs, _): t) =>
    IntMap.find(var_id, allocs);

  let get_allocation_int32 = (var_id, allocs) =>
    Int32.of_int(get_allocation(var_id, allocs));

  let rec apply_allocations =
          (allocs: t, instr: Mashtree.instr): Mashtree.instr => {
    let apply_allocation_to_bind =
      fun
      | MLocalBind(n) =>
        MLocalBind(get_allocation_int32(Int32.to_int(n), allocs))
      | _ as b => b;

    let apply_allocation_to_imm =
      fun
      | MImmBinding(b) => MImmBinding(apply_allocation_to_bind(b))
      | _ as i => i;

    let apply_allocation_to_block = List.map(apply_allocations(allocs));
    let desc =
      switch (instr.instr_desc) {
      | MImmediate(i) => MImmediate(apply_allocation_to_imm(i))
      | [@implicit_arity] MTagOp(top, tt, i) =>
        [@implicit_arity] MTagOp(top, tt, apply_allocation_to_imm(i))
      | [@implicit_arity] MArityOp(aop, at, i) =>
        [@implicit_arity] MArityOp(aop, at, apply_allocation_to_imm(i))
      | [@implicit_arity] MPrim1(pop, i) =>
        [@implicit_arity] MPrim1(pop, apply_allocation_to_imm(i))
      | [@implicit_arity] MTupleOp(top, i) =>
        [@implicit_arity] MTupleOp(top, apply_allocation_to_imm(i))
      | [@implicit_arity] MBoxOp(bop, i) =>
        [@implicit_arity] MBoxOp(bop, apply_allocation_to_imm(i))
      | [@implicit_arity] MArrayOp(aop, i) =>
        [@implicit_arity] MArrayOp(aop, apply_allocation_to_imm(i))
      | [@implicit_arity] MRecordOp(rop, i) =>
        [@implicit_arity] MRecordOp(rop, apply_allocation_to_imm(i))
      | [@implicit_arity] MAdtOp(aop, i) =>
        [@implicit_arity] MAdtOp(aop, apply_allocation_to_imm(i))
      | [@implicit_arity] MCallKnown(i32, is) =>
        [@implicit_arity]
        MCallKnown(i32, List.map(apply_allocation_to_imm, is))
      | [@implicit_arity] MError(e, is) =>
        [@implicit_arity] MError(e, List.map(apply_allocation_to_imm, is))
      | [@implicit_arity] MCallIndirect(imm, is) =>
        [@implicit_arity]
        MCallIndirect(
          apply_allocation_to_imm(imm),
          List.map(apply_allocation_to_imm, is),
        )
      | [@implicit_arity] MIf(c, t, f) =>
        [@implicit_arity]
        MIf(
          apply_allocation_to_imm(c),
          apply_allocation_to_block(t),
          apply_allocation_to_block(f),
        )
      | [@implicit_arity] MWhile(b1, b2) =>
        [@implicit_arity]
        MWhile(apply_allocation_to_block(b1), apply_allocation_to_block(b2))
      | [@implicit_arity] MSwitch(v, bs, d) =>
        [@implicit_arity]
        MSwitch(
          apply_allocation_to_imm(v),
          List.map(((t, b)) => (t, apply_allocation_to_block(b)), bs),
          apply_allocation_to_block(d),
        )
      | [@implicit_arity] MPrim2(pop, i1, i2) =>
        [@implicit_arity]
        MPrim2(
          pop,
          apply_allocation_to_imm(i1),
          apply_allocation_to_imm(i2),
        )
      | MStore(bs) =>
        MStore(
          List.map(
            ((b, bk)) =>
              (apply_allocation_to_bind(b), apply_allocations(allocs, bk)),
            bs,
          ),
        )
      | MAllocate(x) => MAllocate(x)
      | MDrop(i) => MDrop(apply_allocations(allocs, i))
      | MTracepoint(x) => MTracepoint(x)
      };
    {...instr, instr_desc: desc};
  };
};

let run_register_allocation = (instrs: list(Mashtree.instr)) => {
  let uniq = l => {
    let rec tail_uniq = (a, l) =>
      switch (l) {
      | [] => a
      | [hd, ...tl] =>
        tail_uniq([hd, ...a], List.filter(x => x !== hd, tl))
      };
    tail_uniq([], l);
  };
  let bind_live_local =
    fun
    | MLocalBind(n) => [Int32.to_int(n)]
    | _ => [];

  let imm_live_local =
    fun
    | MImmBinding(MLocalBind(n)) => [Int32.to_int(n)]
    | _ => [];

  let rec live_locals = instr =>
    switch (instr.instr_desc) {
    | MDrop(i) => live_locals(i)
    | MImmediate(imm)
    | [@implicit_arity] MTagOp(_, _, imm)
    | [@implicit_arity] MArityOp(_, _, imm)
    | [@implicit_arity] MPrim1(_, imm)
    | [@implicit_arity] MTupleOp(_, imm)
    | [@implicit_arity] MBoxOp(_, imm)
    | [@implicit_arity] MArrayOp(_, imm)
    | [@implicit_arity] MRecordOp(_, imm)
    | [@implicit_arity] MAdtOp(_, imm) => imm_live_local(imm)
    | [@implicit_arity] MCallKnown(_, is)
    | [@implicit_arity] MError(_, is) =>
      List.concat(List.map(imm_live_local, is))
    | [@implicit_arity] MCallIndirect(imm, is) =>
      List.concat(List.map(imm_live_local, is)) @ imm_live_local(imm)
    | [@implicit_arity] MIf(c, t, f) =>
      imm_live_local(c) @ block_live_locals(t) @ block_live_locals(f)
    | [@implicit_arity] MWhile(b1, b2) =>
      block_live_locals(b1) @ block_live_locals(b2)
    | [@implicit_arity] MSwitch(v, bs, d) =>
      imm_live_local(v)
      @ List.concat(List.map(((_, b)) => block_live_locals(b), bs))
      @ block_live_locals(d)
    | [@implicit_arity] MPrim2(_, i1, i2) =>
      imm_live_local(i1) @ imm_live_local(i2)
    | MStore(bs) =>
      List.concat(
        List.map(((b, bk)) => bind_live_local(b) @ live_locals(bk), bs),
      )
    | MAllocate(_)
    | MTracepoint(_) => []
    }
  and block_live_locals = b => List.concat(List.map(live_locals, b));

  /*** Mapping from (instruction index) -> (used locals) */
  let instr_live_sets =
    List.mapi((i, instr) => (i, uniq @@ live_locals(instr)), instrs);
  let num_locals = {
    let rec help = acc =>
      fun
      | [] => acc
      | [hd, ...tl] when hd > acc => help(hd, tl)
      | [_, ...tl] => help(acc, tl);
    1 + help(0, List.map(((_, lst)) => help(0, lst), instr_live_sets));
  };
  /* Printf.eprintf "Live sets:\n";
     List.iter (fun (i, items) -> Printf.eprintf "%d -> [%s]\n" i (BatString.join ", " (List.map string_of_int items))) instr_live_sets; */
  if (num_locals < 2) {
    instrs;
  } else {
    /* flipped mapping of instr_live_sets; (used locals) -> (first_instruction_used, last_instruction_used) */
    let live_intervals = {
      let rec min =
        List.fold_left(
          (acc, cur) =>
            if (cur < acc) {
              cur;
            } else {
              acc;
            },
          List.length(instrs),
        );
      let rec max =
        List.fold_left(
          (acc, cur) =>
            if (cur > acc) {
              cur;
            } else {
              acc;
            },
          0,
        );
      let rec help = (acc: IntMap.t(list(int))) =>
        fun
        | [] => acc
        | [(idx, items), ...tl] => {
            let with_items: IntMap.t(list(int)) =
              List.fold_left(
                (acc, cur) => {
                  let existing_elts: list(int) =
                    Option.value(~default=[], IntMap.find_opt(cur, acc));
                  IntMap.add(cur, [idx, ...existing_elts], acc);
                },
                acc,
                items,
              );
            help(with_items, tl);
          };
      let interval_map = help(IntMap.empty, instr_live_sets);
      List.sort(((_, (min1, _)), (_, (min2, _))) =>
        if (min1 < min2) {
          (-1);
        } else if (min1 === min2) {
          0;
        } else {
          1;
        }
      ) @@
      List.map(
        ((binding, indices)) => (binding, (min(indices), max(indices))),
        IntMap.bindings(interval_map),
      );
    };
    /* Modified version of Linear-scan register allocation; we don't need to spill, since we are optimizing with an
       unbounded number of memory locations */
    /* Printf.eprintf "Intervals:\n";
       List.iter (fun (v, (si, ei)) -> Printf.eprintf "%d: [%d,%d]\n" v si ei) live_intervals; */
    let expire_old_intervals = (current_instr, allocs, active) => {
      let rec help = ((allocs, active)) =>
        fun
        | [] => (allocs, active)
        | [(var_id, (_, max)), ...tl] when max < current_instr =>
          help((RegisterAllocation.release_var(var_id, allocs), active), tl)
        | [hd, ...tl] => help((allocs, [hd, ...active]), tl);
      help((allocs, []), active);
    };
    let _ =
      List_utils.fold_lefti(
        ((allocs, active), i, (var_id, (start_inst, end_inst)) as cur) => {
          let (allocs, active) =
            expire_old_intervals(start_inst, allocs, active);
          let (allocs, _) = RegisterAllocation.allocate(var_id, allocs);
          let active = [cur, ...active];
          (allocs, active);
        },
        (RegisterAllocation.initialize(num_locals), []),
        live_intervals,
      );
    /* Printf.eprintf "Allocations:\n";
       List.iter (fun (var_id, reg_id) -> Printf.eprintf "%d --> %d\n" var_id reg_id) (IntMap.bindings (RegisterAllocation.get_allocations allocs)); */
    /* List.map (RegisterAllocation.apply_allocations allocs) instrs */
    instrs;
  };
};

let compile_const = (c: Asttypes.constant) =>
  switch (c) {
  | Const_int(i) => MConstI32(Int32.of_int(i))
  | Const_string(_) => failwith("NYI: compile_const string")
  | Const_float(f_str) => failwith("NYI: compile_const float")
  | Const_int32(i32) => MConstI32(i32)
  | Const_int64(i64) => MConstI64(i64)
  | Const_bool(b) when b == true => const_true
  | Const_bool(_) => const_false
  | Const_void => const_void
  };

let compile_imm = (env, i: imm_expression) =>
  switch (i.imm_desc) {
  | ImmConst(c) => MImmConst(compile_const(c))
  | ImmId(id) => MImmBinding(find_id(id, env))
  };

let compile_lambda = (env, args, body, loc): Mashtree.closure_data => {
  let used_var_set = Anf_utils.anf_free_vars(body);
  let free_var_set = Ident.Set.diff(used_var_set) @@ Ident.Set.of_list(args);
  let free_vars = Ident.Set.elements(free_var_set);
  /* Bind all non-arguments in the function body to
     their respective closure slots */
  let free_binds =
    List_utils.fold_lefti(
      (acc, closure_idx, var) =>
        Ident.add(var, MClosureBind(Int32.of_int(closure_idx)), acc),
      Ident.empty,
      free_vars,
    );
  let closure_arg = Ident.create("$self");
  let new_args = [closure_arg, ...args];
  let arg_binds =
    List_utils.fold_lefti(
      (acc, arg_idx, arg) =>
        Ident.add(arg, MArgBind(Int32.of_int(arg_idx)), acc),
      free_binds,
      new_args,
    );
  let idx = next_lift();
  let arity = List.length(new_args);
  let stack_size = Anf_utils.anf_count_vars(body);
  let lam_env = {
    ...env,
    ce_binds: arg_binds,
    ce_stack_idx: 0,
    ce_arity: arity,
  };
  let worklist_item = {
    body: Anf(body),
    env: lam_env,
    idx,
    arity,
    stack_size,
    loc,
  };
  worklist_enqueue(worklist_item);
  {
    func_idx: Int32.of_int(idx),
    arity: Int32.of_int(arity),
    /* These variables should be in scope when the lambda is constructed. */
    variables: List.map(id => MImmBinding(find_id(id, env)), free_vars),
  };
};

let compile_wrapper = (env, func_name, arity): Mashtree.closure_data => {
  let body = [
    {
      instr_desc:
        MCallKnown(
          func_name,
          List.init(arity, i => MImmBinding(MArgBind(Int32.of_int(i + 1)))),
        ),
      instr_loc: Location.dummy_loc,
    },
  ];
  let idx = next_lift();
  let lam_env = {
    ...env,
    ce_binds: Ident.empty,
    ce_stack_idx: 0,
    ce_arity: arity + 1,
  };
  let worklist_item = {
    body: Precompiled(body),
    env: lam_env,
    idx,
    arity: arity + 1,
    stack_size: 0,
    loc: Location.dummy_loc,
  };
  worklist_enqueue(worklist_item);
  {
    func_idx: Int32.of_int(idx),
    arity: Int32.of_int(arity + 1),
    variables: [],
  };
};

let next_global = id => {
  let (ret, idx) = next_global(id);
  if (ret != global_index^ - 1) {
    ret;
  } else {
    let body = [
      {
        instr_desc: MImmediate(MImmBinding(MGlobalBind(Int32.of_int(ret)))),
        instr_loc: Location.dummy_loc,
      },
    ];
    let worklist_item = {
      body: Precompiled(body),
      env: initial_compilation_env,
      idx,
      arity: 0, /* <- this function cannot be called by the user, so no self argument is needed. */
      stack_size: 0,
      loc: Location.dummy_loc,
    };
    worklist_enqueue(worklist_item);
    ret;
  };
};

let rec compile_comp = (env, c) => {
  let desc =
    switch (c.comp_desc) {
    | CSwitch(arg, branches) =>
      let compiled_arg = compile_imm(env, arg);
      MSwitch(
        compiled_arg,
        List.map(
          ((lbl, body)) =>
            (Int32.of_int(lbl), compile_anf_expr(env, body)),
          branches,
        ),
        [
          {
            instr_desc: MError(Runtime_errors.SwitchError, [compiled_arg]),
            instr_loc: c.comp_loc,
          },
        ],
      );
    | [@implicit_arity] CIf(cond, thn, els) =>
      [@implicit_arity]
      MIf(
        compile_imm(env, cond),
        compile_anf_expr(env, thn),
        compile_anf_expr(env, els),
      )
    | [@implicit_arity] CWhile(cond, body) =>
      [@implicit_arity]
      MWhile(compile_anf_expr(env, cond), compile_anf_expr(env, body))
    | [@implicit_arity] CPrim1(Box, arg) =>
      MAllocate(MBox(compile_imm(env, arg)))
    | [@implicit_arity] CPrim1(Unbox, arg) =>
      [@implicit_arity] MBoxOp(MBoxUnbox, compile_imm(env, arg))
    | [@implicit_arity] CPrim1(p1, arg) =>
      [@implicit_arity] MPrim1(p1, compile_imm(env, arg))
    | [@implicit_arity] CPrim2(p2, arg1, arg2) =>
      [@implicit_arity]
      MPrim2(p2, compile_imm(env, arg1), compile_imm(env, arg2))
    | [@implicit_arity] CAssign(arg1, arg2) =>
      [@implicit_arity]
      MBoxOp(MBoxUpdate(compile_imm(env, arg2)), compile_imm(env, arg1))
    | [@implicit_arity] CBoxAssign(arg1, arg2) =>
      [@implicit_arity]
      MBoxOp(MBoxUpdate(compile_imm(env, arg2)), compile_imm(env, arg1))
    | CTuple(args) => MAllocate(MTuple(List.map(compile_imm(env), args)))
    | CArray(args) => MAllocate(MArray(List.map(compile_imm(env), args)))
    | [@implicit_arity] CArrayGet(idx, arr) =>
      [@implicit_arity]
      MArrayOp(MArrayGet(compile_imm(env, idx)), compile_imm(env, arr))
    | [@implicit_arity] CArraySet(idx, arr, arg) =>
      [@implicit_arity]
      MArrayOp(
        [@implicit_arity]
        MArraySet(compile_imm(env, idx), compile_imm(env, arg)),
        compile_imm(env, arr),
      )
    | [@implicit_arity] CRecord(ttag, args) =>
      MAllocate(
        [@implicit_arity]
        MRecord(
          compile_imm(env, ttag),
          List.map(
            (({txt: name}, arg)) => (name, compile_imm(env, arg)),
            args,
          ),
        ),
      )
    | [@implicit_arity] CAdt(ttag, vtag, args) =>
      MAllocate(
        [@implicit_arity]
        MADT(
          compile_imm(env, ttag),
          compile_imm(env, vtag),
          List.map(compile_imm(env), args),
        ),
      )
    | CString(s) => MAllocate(MString(s))
    | CInt32(i) => MAllocate(MInt32(i))
    | CInt64(i) => MAllocate(MInt64(i))
    | [@implicit_arity] CGetTupleItem(idx, tup) =>
      [@implicit_arity] MTupleOp(MTupleGet(idx), compile_imm(env, tup))
    | [@implicit_arity] CSetTupleItem(idx, tup, value) =>
      [@implicit_arity]
      MTupleOp(
        [@implicit_arity] MTupleSet(idx, compile_imm(env, value)),
        compile_imm(env, tup),
      )
    | [@implicit_arity] CGetAdtItem(idx, adt) =>
      [@implicit_arity] MAdtOp(MAdtGet(idx), compile_imm(env, adt))
    | CGetAdtTag(adt) =>
      [@implicit_arity] MAdtOp(MAdtGetTag, compile_imm(env, adt))
    | [@implicit_arity] CGetRecordItem(idx, record) =>
      [@implicit_arity] MRecordOp(MRecordGet(idx), compile_imm(env, record))
    | [@implicit_arity] CSetRecordItem(idx, record, arg) =>
      [@implicit_arity]
      MRecordOp(
        MRecordSet(idx, compile_imm(env, arg)),
        compile_imm(env, record),
      )
    | [@implicit_arity] CLambda(args, body) =>
      MAllocate(MClosure(compile_lambda(env, args, body, c.comp_loc)))
    | [@implicit_arity] CApp(f, args) =>
      /* TODO: Utilize MCallKnown */
      [@implicit_arity]
      MCallIndirect(compile_imm(env, f), List.map(compile_imm(env), args))
    | [@implicit_arity] CAppBuiltin(modname, name, args) =>
      [@implicit_arity]
      MCallKnown("builtin", List.map(compile_imm(env), args))
    | CImmExpr(i) => MImmediate(compile_imm(env, i))
    };
  {instr_desc: desc, instr_loc: c.comp_loc};
}
and compile_anf_expr = (env, a) =>
  switch (a.anf_desc) {
  | AESeq(hd, tl) => [
      {instr_desc: MDrop(compile_comp(env, hd)), instr_loc: hd.comp_loc},
      ...compile_anf_expr(env, tl),
    ]
  | AELet(global, recflag, binds, body) =>
    let get_loc = (idx, (id, _)) =>
      switch (global) {
      | Global => MGlobalBind(Int32.of_int(next_global(id)))
      | Nonglobal => MLocalBind(Int32.of_int(env.ce_stack_idx + idx))
      };
    let locations = List.mapi(get_loc, binds);
    let new_env =
      List.fold_left2(
        (acc, new_loc, (id, _)) =>
          {
            ...acc,
            ce_binds: Ident.add(id, new_loc, acc.ce_binds),
            ce_stack_idx: acc.ce_stack_idx + 1,
          }, /* FIXME: Why is this not ce_stack_idx + (List.length binds)?? */
        env,
        locations,
        binds,
      );
    switch (recflag) {
    | Nonrecursive =>
      List.fold_right2(
        (loc, (_, rhs), acc) =>
          [
            {
              instr_desc: MStore([(loc, compile_comp(env, rhs))]),
              instr_loc: rhs.comp_loc,
            },
            ...acc,
          ],
        locations,
        binds,
        compile_anf_expr(new_env, body),
      )
    | Recursive =>
      let binds =
        List.fold_left2(
          (acc, loc, (_, rhs)) =>
            [(loc, compile_comp(new_env, rhs)), ...acc],
          [],
          locations,
          binds,
        );
      [
        {instr_desc: MStore(List.rev(binds)), instr_loc: a.anf_loc},
        ...compile_anf_expr(new_env, body),
      ];
    };
  | AEComp(c) => [compile_comp(env, c)]
  };

let compile_worklist_elt = ({body, env}: worklist_elt) =>
  switch (body) {
  | Anf(body) => compile_anf_expr(env, body)
  | Precompiled(block) => block
  };

let fold_left_pop = (f, base) => {
  let rec help = acc =>
    if (worklist_empty()) {
      acc;
    } else {
      help(f(acc, worklist_pop()));
    };
  help(base);
};

let compile_remaining_worklist = () => {
  let compile_one =
      (funcs, {idx: index, arity, stack_size, loc} as cur: worklist_elt) => {
    let body = compile_worklist_elt(cur);
    let func = {
      index: Int32.of_int(index),
      arity: Int32.of_int(arity),
      body,
      stack_size,
      func_loc: loc,
    };
    [func, ...funcs];
  };
  List.rev(fold_left_pop(compile_one, []));
};

let lift_imports = (env, imports) => {
  let process_shape =
    fun
    | GlobalShape => MGlobalImport(I32Type)
    | [@implicit_arity] FunctionShape(inputs, outputs) =>
      [@implicit_arity]
      MFuncImport(
        List.init(inputs, _ => I32Type),
        List.init(outputs, _ => I32Type),
      );

  let process_import =
      ((imports, setups, env), {imp_use_id, imp_desc, imp_shape}) => {
    let glob = next_global(imp_use_id);
    switch (imp_desc) {
    | [@implicit_arity] GrainValue(mod_, name) =>
      let new_mod = {
        mimp_mod: Ident.create(mod_),
        mimp_name: Ident.create(name),
        mimp_type: MGlobalImport(I32Type) /*process_shape imp_shape*/,
        mimp_kind: MImportGrain,
        mimp_setup: MCallGetter,
      };
      let func_name =
        Printf.sprintf(
          "import_%s_%s",
          Ident.unique_name(new_mod.mimp_mod),
          Ident.unique_name(new_mod.mimp_name),
        );
      (
        [new_mod, ...imports],
        [
          [
            {
              instr_desc:
                MStore([
                  (
                    MGlobalBind(Int32.of_int(glob)),
                    {
                      instr_desc: MCallKnown(func_name, []),
                      instr_loc: Location.dummy_loc,
                    },
                  ),
                ]),
              instr_loc: Location.dummy_loc,
            },
          ],
          ...setups,
        ],
        {
          ...env,
          ce_binds:
            Ident.add(
              imp_use_id,
              MGlobalBind(Int32.of_int(glob)),
              env.ce_binds,
            ),
        },
      );
    | [@implicit_arity] WasmFunction(mod_, name) =>
      let new_mod = {
        mimp_mod: Ident.create(mod_),
        mimp_name: Ident.create(name),
        mimp_type: process_shape(imp_shape),
        mimp_kind: MImportWasm,
        mimp_setup: MWrap(Int32.zero),
      };
      let func_name =
        Printf.sprintf(
          "import_%s_%s",
          Ident.unique_name(new_mod.mimp_mod),
          Ident.unique_name(new_mod.mimp_name),
        );
      (
        [new_mod, ...imports],
        [
          switch (imp_shape) {
          | GlobalShape => []
          | [@implicit_arity] FunctionShape(inputs, outputs) =>
            if (outputs > 1) {
              failwith("NYI: Multi-result wrapper");
            } else {
              [
                {
                  instr_desc:
                    MStore([
                      (
                        MGlobalBind(Int32.of_int(glob)),
                        {
                          instr_desc:
                            MAllocate(
                              MClosure(
                                compile_wrapper(env, func_name, inputs),
                              ),
                            ),
                          instr_loc: Location.dummy_loc,
                        },
                      ),
                    ]),
                  instr_loc: Location.dummy_loc,
                },
              ];
            }
          },
          ...setups,
        ],
        {
          ...env,
          ce_binds:
            Ident.add(
              imp_use_id,
              MGlobalBind(Int32.of_int(glob)),
              env.ce_binds,
            ),
        },
      );
    | JSFunction(_) => failwith("NYI: lift_imports JSFunction")
    };
  };

  let (imports, setups, env) =
    List.fold_left(process_import, ([], [], env), imports);
  let imports = List.rev(imports);
  let setups = List.flatten(List.rev(setups));
  (imports, setups, env);
};

let transl_anf_program =
    (anf_prog: Anftree.anf_program): Mashtree.mash_program => {
  reset_lift();
  reset_global();
  worklist_reset();

  let (imports, setups, env) =
    lift_imports(initial_compilation_env, anf_prog.imports);
  let main_body_stack_size = Anf_utils.anf_count_vars(anf_prog.body);
  let main_body =
    run_register_allocation @@ setups @ compile_anf_expr(env, anf_prog.body);
  let exports = global_exports();
  let functions =
    List.map(
      ({body} as f: Mashtree.mash_function) =>
        {...f, body: run_register_allocation(body)},
      compile_remaining_worklist(),
    );

  {
    functions,
    imports,
    exports,
    main_body,
    main_body_stack_size,
    num_globals: global_index^,
    signature: anf_prog.signature,
  };
};
