open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_utils;

open Asttypes;
open Anftree;
open Mashtree;

type compilation_env = {
  ce_binds: Ident.tbl(Mashtree.binding),
  ce_stack_idx_ptr: int,
  ce_stack_idx_i32: int,
  ce_stack_idx_i64: int,
  ce_stack_idx_f32: int,
  ce_stack_idx_f64: int,
  ce_arity: int,
};

let initial_compilation_env = {
  ce_binds: Ident.empty,
  ce_stack_idx_ptr: 0,
  ce_stack_idx_i32: 0,
  ce_stack_idx_i64: 0,
  ce_stack_idx_f32: 0,
  ce_stack_idx_f64: 0,
  ce_arity: 0,
};

type worklist_elt_body =
  | Anf(anf_expression)
  | Precompiled(block);

type worklist_elt = {
  body: worklist_elt_body,
  env: compilation_env,
  args: list(Types.allocation_type),
  return_type: list(Types.allocation_type),
  id: Ident.t,
  name: option(string),
  attrs: attributes,
  stack_size: Mashtree.stack_size,
  loc: Location.t,
};

// The OCaml docs warn that this isn't thread-safe,
// but I don't think we are threading yet
let compilation_worklist: Queue.t(worklist_elt) = Queue.create();

/** Function table indexes */

let function_table_index = ref(0);
let function_table_idents = ref([]);

let reset_function_table_info = () => {
  function_table_index := 0;
  function_table_idents := [];
};

type function_ident =
  | FuncId(Ident.t)
  | FuncName(string);

let next_function_table_index = ident => {
  let name =
    switch (ident) {
    | FuncId(id) => Ident.unique_name(id)
    | FuncName(name) => name
    };
  function_table_idents := [name, ...function_table_idents^];
  let ret = function_table_index^;
  function_table_index := ret + 1;
  ret;
};

let get_function_table_idents = () => {
  List.rev(function_table_idents^);
};

/** Imports which are always in scope */
let global_imports = ref(Ident.Set.empty);

let set_global_imports = imports => {
  global_imports :=
    Ident.Set.of_list(
      Ident.fold_all((id, _, acc) => [id, ...acc], imports, []),
    );
};

/** Global index (index of global variables) */

let global_table = ref(Ident.empty: Ident.tbl(Types.allocation_type));

let get_globals = () => {
  Ident.fold_all((id, ty, acc) => [(id, ty), ...acc], global_table^, []);
};

let reset_global = () => {
  global_table := Ident.empty;
};

let get_global = (id, ty: Types.allocation_type) =>
  switch (Ident.find_same_opt(id, global_table^)) {
  | Some(_) => id
  | None =>
    global_table := Ident.add(id, ty, global_table^);
    id;
  };

let global_name = id => Ident.unique_name(id);

let find_id = (id, env) =>
  try(Ident.find_same(id, env.ce_binds)) {
  | Not_found =>
    let alloc = Ident.find_same(id, global_table^);
    MGlobalBind(global_name(id), alloc);
  };

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

  let get_allocation = (var_id, (allocs, _)) => {
    Int32.of_int(IntMap.find(var_id, allocs));
  };

  let rec apply_allocations =
          (ty: Types.allocation_type, allocs: t, instr: Mashtree.instr)
          : Mashtree.instr => {
    let apply_allocation_to_bind =
      fun
      | MLocalBind(n, ity) when ity == ty =>
        MLocalBind(get_allocation(Int32.to_int(n), allocs), ty)
      | _ as b => b;

    let apply_allocation_to_imm =
      fun
      | MImmBinding(b) => MImmBinding(apply_allocation_to_bind(b))
      | _ as i => i;

    let apply_allocation_to_block = List.map(apply_allocations(ty, allocs));
    let desc =
      switch (instr.instr_desc) {
      | MImmediate(i) => MImmediate(apply_allocation_to_imm(i))
      | MTagOp(top, tt, i) => MTagOp(top, tt, apply_allocation_to_imm(i))
      | MArityOp(aop, at, i) =>
        MArityOp(aop, at, apply_allocation_to_imm(i))
      | MPrim0(pop) => MPrim0(pop)
      | MPrim1(pop, i) => MPrim1(pop, apply_allocation_to_imm(i))
      | MTupleOp(top, i) => MTupleOp(top, apply_allocation_to_imm(i))
      | MBoxOp(bop, i) => MBoxOp(bop, apply_allocation_to_imm(i))
      | MArrayOp(aop, i) => MArrayOp(aop, apply_allocation_to_imm(i))
      | MRecordOp(rop, i) => MRecordOp(rop, apply_allocation_to_imm(i))
      | MAdtOp(aop, i) => MAdtOp(aop, apply_allocation_to_imm(i))
      | MClosureOp(cop, i) => MClosureOp(cop, apply_allocation_to_imm(i))
      | MCallRaw({func, func_type, args}) =>
        MCallRaw({
          func,
          func_type,
          args: List.map(apply_allocation_to_imm, args),
        })
      | MCallKnown({func, closure, func_type, args}) =>
        MCallKnown({
          func,
          closure: apply_allocation_to_imm(closure),
          func_type,
          args: List.map(apply_allocation_to_imm, args),
        })
      | MReturnCallKnown({func, closure, func_type, args}) =>
        MReturnCallKnown({
          func,
          closure: apply_allocation_to_imm(closure),
          func_type,
          args: List.map(apply_allocation_to_imm, args),
        })
      | MError(e, is) => MError(e, List.map(apply_allocation_to_imm, is))
      | MCallIndirect({func, func_type, args}) =>
        MCallIndirect({
          func: apply_allocation_to_imm(func),
          func_type,
          args: List.map(apply_allocation_to_imm, args),
        })
      | MReturnCallIndirect({func, func_type, args}) =>
        MReturnCallIndirect({
          func: apply_allocation_to_imm(func),
          func_type,
          args: List.map(apply_allocation_to_imm, args),
        })
      | MIf(c, t, f) =>
        MIf(
          apply_allocation_to_imm(c),
          apply_allocation_to_block(t),
          apply_allocation_to_block(f),
        )
      | MFor(b1, b2, b3) =>
        MFor(
          Option.map(apply_allocation_to_block, b1),
          Option.map(apply_allocation_to_block, b2),
          apply_allocation_to_block(b3),
        )
      | MContinue => MContinue
      | MBreak => MBreak
      | MReturn(v) => MReturn(Option.map(apply_allocations(ty, allocs), v))
      | MSwitch(v, bs, d, ty) =>
        MSwitch(
          apply_allocation_to_imm(v),
          List.map(((t, b)) => (t, apply_allocation_to_block(b)), bs),
          apply_allocation_to_block(d),
          ty,
        )
      | MPrim2(pop, i1, i2) =>
        MPrim2(
          pop,
          apply_allocation_to_imm(i1),
          apply_allocation_to_imm(i2),
        )
      | MPrimN(pop, is) =>
        MPrimN(pop, List.map(apply_allocation_to_imm, is))
      | MStore(bs) =>
        MStore(
          List.map(
            ((b, bk)) =>
              (
                apply_allocation_to_bind(b),
                apply_allocations(ty, allocs, bk),
              ),
            bs,
          ),
        )
      | MSet(b, i) =>
        MSet(apply_allocation_to_bind(b), apply_allocations(ty, allocs, i))
      | MAllocate(x) => MAllocate(x)
      | MDrop(i, a) => MDrop(apply_allocations(ty, allocs, i), a)
      | MIncRef(i) => MIncRef(apply_allocations(ty, allocs, i))
      | MTracepoint(x) => MTracepoint(x)
      };
    {...instr, instr_desc: desc};
  };
};

type live_local = (Types.allocation_type, int);

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
    | MLocalBind(n, ty) => [(ty, Int32.to_int(n))]
    | _ => [];

  let imm_live_local =
    fun
    | MImmBinding(MLocalBind(n, ty)) => [(ty, Int32.to_int(n))]
    | _ => [];

  let rec live_locals = instr =>
    switch (instr.instr_desc) {
    | MIncRef(i)
    | MDrop(i, _) => live_locals(i)
    | MImmediate(imm)
    | MTagOp(_, _, imm)
    | MArityOp(_, _, imm)
    | MPrim1(_, imm)
    | MTupleOp(_, imm)
    | MBoxOp(_, imm)
    | MArrayOp(_, imm)
    | MRecordOp(_, imm)
    | MAdtOp(_, imm)
    | MClosureOp(_, imm) => imm_live_local(imm)
    | MCallRaw({args: is})
    | MError(_, is) => List.concat(List.map(imm_live_local, is))
    | MCallKnown({closure: imm, args: is})
    | MReturnCallKnown({closure: imm, args: is})
    | MCallIndirect({func: imm, args: is})
    | MReturnCallIndirect({func: imm, args: is}) =>
      List.concat(List.map(imm_live_local, is)) @ imm_live_local(imm)
    | MIf(c, t, f) =>
      imm_live_local(c) @ block_live_locals(t) @ block_live_locals(f)
    | MFor(b1, b2, b3) =>
      Option.fold(~none=[], ~some=block_live_locals, b1)
      @ Option.fold(~none=[], ~some=block_live_locals, b2)
      @ block_live_locals(b3)
    | MPrim0(_)
    | MContinue
    | MBreak => []
    | MReturn(v) => Option.fold(~none=[], ~some=live_locals, v)
    | MSwitch(v, bs, d, ty) =>
      imm_live_local(v)
      @ List.concat(List.map(((_, b)) => block_live_locals(b), bs))
      @ block_live_locals(d)
    | MPrim2(_, i1, i2) => imm_live_local(i1) @ imm_live_local(i2)
    | MPrimN(_, is) => List.concat(List.map(imm_live_local, is))
    | MStore(bs) =>
      List.concat(
        List.map(((b, bk)) => bind_live_local(b) @ live_locals(bk), bs),
      )
    | MSet(b, i) => bind_live_local(b) @ live_locals(i)
    | MAllocate(_)
    | MTracepoint(_) => []
    }
  and block_live_locals = b => List.concat(List.map(live_locals, b));

  /*** Mapping from (instruction index) -> (used locals) */
  let instr_live_sets =
    List.mapi((i, instr) => (i, uniq @@ live_locals(instr)), instrs);
  let (
    num_locals_ptr,
    num_locals_i32,
    num_locals_i64,
    num_locals_f32,
    num_locals_f64,
  ) = {
    let rec help = ((ty: Types.allocation_type, acc: int)) =>
      fun
      | [] => (ty, acc)
      | [(hd_ty, hd), ...tl] when hd_ty == ty && hd > acc =>
        help((ty, hd), tl)
      | [_, ...tl] => help((ty, acc), tl);
    let ptr =
      snd @@
      help(
        (Types.Managed, 0),
        List.map(
          ((_, lst)) => help((Types.Managed, 0), lst),
          instr_live_sets,
        ),
      );
    let i32 =
      snd @@
      help(
        (Types.Unmanaged(WasmI32), 0),
        List.map(
          ((_, lst)) => help((Types.Unmanaged(WasmI32), 0), lst),
          instr_live_sets,
        ),
      );
    let i64 =
      snd @@
      help(
        (Types.Unmanaged(WasmI64), 0),
        List.map(
          ((_, lst)) => help((Types.Unmanaged(WasmI64), 0), lst),
          instr_live_sets,
        ),
      );
    let f32 =
      snd @@
      help(
        (Types.Unmanaged(WasmF32), 0),
        List.map(
          ((_, lst)) => help((Types.Unmanaged(WasmF32), 0), lst),
          instr_live_sets,
        ),
      );
    let f64 =
      snd @@
      help(
        (Types.Unmanaged(WasmF64), 0),
        List.map(
          ((_, lst)) => help((Types.Unmanaged(WasmF64), 0), lst),
          instr_live_sets,
        ),
      );
    (ptr + 1, i32 + 1, i64 + 1, f32 + 1, f64 + 1);
  };
  /* Printf.eprintf "Live sets:\n";
     List.iter (fun (i, items) -> Printf.eprintf "%d -> [%s]\n" i (BatString.join ", " (List.map string_of_int items))) instr_live_sets; */
  let run = (ty: Types.allocation_type, instrs) => {
    let num_locals =
      switch (ty) {
      | Types.Managed => num_locals_ptr
      | Types.Unmanaged(WasmI32) => num_locals_i32
      | Types.Unmanaged(WasmI64) => num_locals_i64
      | Types.Unmanaged(WasmF32) => num_locals_f32
      | Types.Unmanaged(WasmF64) => num_locals_f64
      };
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
                  (acc, (item_ty, cur)) =>
                    if (item_ty == ty) {
                      let existing_elts: list(int) =
                        Option.value(~default=[], IntMap.find_opt(cur, acc));
                      IntMap.add(cur, [idx, ...existing_elts], acc);
                    } else {
                      acc;
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
          ((binding, indices)) =>
            (binding, (min(indices), max(indices))),
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
            help(
              (RegisterAllocation.release_var(var_id, allocs), active),
              tl,
            )
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
  run(Types.Managed, instrs)
  |> run(Types.Unmanaged(WasmI32))
  |> run(Types.Unmanaged(WasmI64))
  |> run(Types.Unmanaged(WasmF32))
  |> run(Types.Unmanaged(WasmF64));
};

let wasm_import_name = (mod_, name) =>
  Printf.sprintf("wimport_%s_%s", mod_, name);

let compile_const = (c: Asttypes.constant) =>
  switch (c) {
  | Const_number(Const_number_int(i)) => MConstI32(Int64.to_int32(i))
  | Const_number(_) =>
    failwith("compile_const: Const_number float/rational post-ANF")
  | Const_bytes(_) => failwith("compile_const: Const_bytes post-ANF")
  | Const_string(_) => failwith("compile_const: Const_string post-ANF")
  | Const_char(_) => failwith("compile_const: Const_char post-ANF")
  | Const_bigint(_) => failwith("compile_const: Const_bigint post-ANF")
  | Const_int32(i32) => MConstI32(i32)
  | Const_int64(i64) => MConstI64(i64)
  | Const_float32(f) => MConstF32(f)
  | Const_float64(f) => MConstF64(f)
  | Const_wasmi32(i32) => MConstLiteral(MConstI32(i32))
  | Const_wasmi64(i64) => MConstLiteral(MConstI64(i64))
  | Const_wasmf32(f32) => MConstLiteral(MConstF32(f32))
  | Const_wasmf64(f64) => MConstLiteral(MConstF64(f64))
  | Const_bool(b) when b == true => const_true
  | Const_bool(_) => const_false
  | Const_void => const_void
  };

let compile_imm = (env, i: imm_expression) =>
  switch (i.imm_desc) {
  | ImmConst(c) => MImmConst(compile_const(c))
  | ImmId(id) => MImmBinding(find_id(id, env))
  | ImmTrap => MImmTrap
  };

type known_function =
  | Internal(Ident.t)
  | Imported(Ident.t, string);

let known_functions = Ident_tbl.create(50);
let register_function = func =>
  switch (func) {
  | Internal(id)
  | Imported(id, _) => Ident_tbl.add(known_functions, id, func)
  };
let clear_known_functions = () => Ident_tbl.clear(known_functions);
let known_function = f =>
  switch (f.imm_desc) {
  | ImmId(id) =>
    switch (Ident_tbl.find_opt(known_functions, id)) {
    | Some(Internal(id)) => Some(Ident.unique_name(id))
    | Some(Imported(id, name)) => Some(name)
    | None => None
    }
  | ImmConst(_)
  | ImmTrap => failwith("Impossible: function application of non-function")
  };

let compile_lambda =
    (~name=?, id, env, args, body, attrs, loc): Mashtree.closure_data => {
  register_function(Internal(id));

  let (body, return_type) = body;
  // NOTE: we special-case `id`, since we want to
  //       have simply-recursive uses of identifiers use
  //       argument 0 ("$self") rather than do the self-reference
  //       via a closure variable (this enables a large class
  //       of recursive functions to be garbage-collectible, since
  //       Grain's garbage collector does not currently collect
  //       cyclic reference chains)
  let used_var_set =
    Ident.Set.remove(id, Analyze_free_vars.anf_free_vars(body));
  let arg_vars = List.map(((arg, _)) => arg, args);
  let global_vars =
    Ident.fold_all((id, _, acc) => [id, ...acc], global_table^, []);
  let accessible_var_set =
    Ident.Set.union(
      global_imports^,
      Ident.Set.of_list(arg_vars @ global_vars),
    );
  let free_var_set = Ident.Set.diff(used_var_set, accessible_var_set);
  let free_vars = Ident.Set.elements(free_var_set);
  /* Bind all non-arguments in the function body to
     their respective closure slots */
  let free_binds =
    List_utils.fold_lefti(
      (acc, closure_idx, var) =>
        Ident.add(var, MClosureBind(Int32.of_int(closure_idx)), acc),
      env.ce_binds,
      free_vars,
    );
  let closure_arg = (Ident.create("$self"), Types.Managed);
  let new_args = [closure_arg, ...args];
  let arg_binds =
    List_utils.fold_lefti(
      (acc, arg_idx, (arg, arg_type)) => {
        Ident.add(arg, MArgBind(Int32.of_int(arg_idx), arg_type), acc)
      },
      free_binds,
      new_args,
    )
    |> Ident.add(id, MArgBind(Int32.of_int(0), Types.Managed));
  let func_idx =
    if (Analyze_function_calls.has_indirect_call(id)) {
      Some(Int32.of_int(next_function_table_index(FuncId(id))));
    } else {
      None;
    };
  let args = List.map(((_, ty)) => ty, new_args);
  let arity = List.length(args);
  let {
    stack_size_ptr,
    stack_size_i32,
    stack_size_i64,
    stack_size_f32,
    stack_size_f64,
  }: Anf_utils.stack_size =
    Anf_utils.anf_count_vars(body);
  let lam_env = {
    ce_binds: arg_binds,
    ce_stack_idx_ptr: 0,
    ce_stack_idx_i32: 0,
    ce_stack_idx_i64: 0,
    ce_stack_idx_f32: 0,
    ce_stack_idx_f64: 0,
    ce_arity: arity,
  };
  let worklist_item = {
    body: Anf(body),
    env: lam_env,
    id,
    name,
    args,
    return_type,
    attrs,
    stack_size: {
      stack_size_ptr,
      stack_size_i32,
      stack_size_i64,
      stack_size_f32,
      stack_size_f64,
    },
    loc,
  };
  worklist_enqueue(worklist_item);
  {
    func_idx,
    arity: Int32.of_int(arity),
    /* These variables should be in scope when the lambda is constructed. */
    variables: List.map(id => MImmBinding(find_id(id, env)), free_vars),
  };
};

let compile_wrapper =
    (~name=?, id, env, func_name, args, rets): Mashtree.closure_data => {
  register_function(Internal(id));

  let body = [
    {
      instr_desc:
        MCallRaw({
          func: func_name,
          func_type: (args, rets),
          args:
            List.mapi(
              (i, arg) => MImmBinding(MArgBind(Int32.of_int(i + 1), arg)),
              args,
            ),
        }),
      instr_loc: Location.dummy_loc,
    },
  ];
  let (body, return_type) =
    switch (rets) {
    | [] => (
        List.append(
          body,
          [
            {
              instr_desc: MImmediate(MImmConst(const_void)),
              instr_loc: Location.dummy_loc,
            },
          ],
        ),
        [Types.Unmanaged(Types.WasmI32)],
      )
    | _ => (body, rets)
    };
  let func_idx =
    if (Analyze_function_calls.has_indirect_call(id)) {
      Some(Int32.of_int(next_function_table_index(FuncId(id))));
    } else {
      None;
    };
  let arity = List.length(args);
  let lam_env = {
    ce_binds: Ident.empty,
    ce_stack_idx_ptr: 0,
    ce_stack_idx_i32: 0,
    ce_stack_idx_i64: 0,
    ce_stack_idx_f32: 0,
    ce_stack_idx_f64: 0,
    ce_arity: arity + 1,
  };
  let worklist_item = {
    body: Precompiled(body),
    env: lam_env,
    id,
    name,
    args: [Types.Managed, ...args],
    return_type,
    stack_size: {
      stack_size_ptr: 0,
      stack_size_i32: 0,
      stack_size_i64: 0,
      stack_size_f32: 0,
      stack_size_f64: 0,
    },
    attrs: [Disable_gc],
    loc: Location.dummy_loc,
  };
  worklist_enqueue(worklist_item);
  {func_idx, arity: Int32.of_int(arity + 1), variables: []};
};

let get_global = (id, ty) => {
  let ret = get_global(id, ty);
  global_name(ret);
};

let rec compile_comp = (~id=?, env, c) => {
  let desc =
    switch (c.comp_desc) {
    | CSwitch(arg, branches, partial) =>
      let compiled_arg = compile_imm(env, arg);
      let switch_type =
        Option.fold(
          ~none=Types.Unmanaged(WasmI32),
          ~some=((_, exp)) => exp.anf_allocation_type,
          List.nth_opt(branches, 0),
        );
      let default =
        switch (partial) {
        | Partial => [
            {
              instr_desc: MError(Runtime_errors.MatchFailure, []),
              instr_loc: c.comp_loc,
            },
          ]
        | Total => [
            {instr_desc: MImmediate(MImmTrap), instr_loc: c.comp_loc},
          ]
        };
      MSwitch(
        compiled_arg,
        List.map(
          ((lbl, body)) =>
            (Int32.of_int(lbl), compile_anf_expr(env, body)),
          branches,
        ),
        default,
        switch_type,
      );
    | CIf(cond, thn, els) =>
      MIf(
        compile_imm(env, cond),
        compile_anf_expr(env, thn),
        compile_anf_expr(env, els),
      )
    | CFor(cond, inc, body) =>
      MFor(
        Option.map(compile_anf_expr(env), cond),
        Option.map(compile_anf_expr(env), inc),
        compile_anf_expr(env, body),
      )
    | CContinue => MContinue
    | CBreak => MBreak
    | CReturn(e) => MReturn(Option.map(compile_comp(env), e))
    | CPrim0(p0) => MPrim0(p0)
    | CPrim1(Box, arg)
    | CPrim1(BoxBind, arg) => MAllocate(MBox(compile_imm(env, arg)))
    | CPrim1(Unbox, arg)
    | CPrim1(UnboxBind, arg) => MBoxOp(MBoxUnbox, compile_imm(env, arg))
    | CPrim1(p1, arg) => MPrim1(p1, compile_imm(env, arg))
    | CPrim2(p2, arg1, arg2) =>
      MPrim2(p2, compile_imm(env, arg1), compile_imm(env, arg2))
    | CPrimN(p, args) => MPrimN(p, List.map(compile_imm(env), args))
    | CAssign(arg1, arg2) =>
      MBoxOp(MBoxUpdate(compile_imm(env, arg2)), compile_imm(env, arg1))
    | CBoxAssign(arg1, arg2) =>
      MBoxOp(MBoxUpdate(compile_imm(env, arg2)), compile_imm(env, arg1))
    | CLocalAssign(arg1, arg2) =>
      MSet(
        find_id(arg1, env),
        {
          instr_desc: MImmediate(compile_imm(env, arg2)),
          instr_loc: arg2.imm_loc,
        },
      )
    | CTuple(args) => MAllocate(MTuple(List.map(compile_imm(env), args)))
    | CArray(args) => MAllocate(MArray(List.map(compile_imm(env), args)))
    | CArrayGet(idx, arr) =>
      MArrayOp(MArrayGet(compile_imm(env, idx)), compile_imm(env, arr))
    | CArraySet(idx, arr, arg) =>
      MArrayOp(
        MArraySet(compile_imm(env, idx), compile_imm(env, arg)),
        compile_imm(env, arr),
      )
    | CRecord(ttag, args) =>
      MAllocate(
        MRecord(
          compile_imm(env, ttag),
          List.map(
            ((name, arg)) =>
              (
                Option.map(({txt: name}) => name, name),
                compile_imm(env, arg),
              ),
            args,
          ),
        ),
      )
    | CAdt(ttag, vtag, args) =>
      MAllocate(
        MADT(
          compile_imm(env, ttag),
          compile_imm(env, vtag),
          List.map(compile_imm(env), args),
        ),
      )
    | CBytes(b) => MAllocate(MBytes(b))
    | CString(s) => MAllocate(MString(s))
    | CChar(c) => MAllocate(MChar(c))
    | CNumber(Const_number_int(n))
        when n <= Literals.simple_number_max && n >= Literals.simple_number_min =>
      MImmediate(MImmConst(MConstI32(Int64.to_int32(n))))
    | CNumber(Const_number_int(n))
        when
          n <= Int64.of_int32(Int32.max_int)
          && n >= Int64.of_int32(Int32.min_int) =>
      MAllocate(MInt32(Int64.to_int32(n)))
    | CNumber(Const_number_int(n)) => MAllocate(MInt64(n))
    | CNumber(Const_number_float(f)) => MAllocate(MFloat64(f))
    | CNumber(
        Const_number_rational({
          rational_negative,
          rational_num_limbs,
          rational_den_limbs,
          _,
        }),
      ) =>
      MAllocate(
        MRational({
          numerator_flags:
            if (rational_negative) {
              [Bigint_flags.BigIntNegative];
            } else {
              [];
            },
          numerator_limbs: rational_num_limbs,
          denominator_flags: [],
          denominator_limbs: rational_den_limbs,
        }),
      )
    | CNumber(Const_number_bigint({bigint_negative, bigint_limbs, _})) =>
      MAllocate(
        MBigInt({
          flags:
            if (bigint_negative) {
              [Bigint_flags.BigIntNegative];
            } else {
              [];
            },
          limbs: bigint_limbs,
        }),
      )
    | CInt32(i) => MAllocate(MInt32(i))
    | CInt64(i) => MAllocate(MInt64(i))
    | CFloat32(f) => MAllocate(MFloat32(f))
    | CFloat64(f) => MAllocate(MFloat64(f))
    | CGetTupleItem(idx, tup) =>
      MTupleOp(MTupleGet(idx), compile_imm(env, tup))
    | CSetTupleItem(idx, tup, value) =>
      MTupleOp(
        MTupleSet(idx, compile_imm(env, value)),
        compile_imm(env, tup),
      )
    | CGetAdtItem(idx, adt) => MAdtOp(MAdtGet(idx), compile_imm(env, adt))
    | CGetAdtTag(adt) => MAdtOp(MAdtGetTag, compile_imm(env, adt))
    | CGetRecordItem(idx, record) =>
      MRecordOp(MRecordGet(idx), compile_imm(env, record))
    | CSetRecordItem(idx, record, arg) =>
      MRecordOp(
        MRecordSet(idx, compile_imm(env, arg)),
        compile_imm(env, record),
      )
    | CLambda(name, args, body) =>
      let (body, return_type) = body;
      let body = (body, [return_type]);
      // Functions typically have an identifier associated with them, though
      // a first-class function returned directly from a block/function does
      // not. As a function returned this way will always be called indirectly,
      // we can generate a fresh identifier for it.
      let id =
        switch (id) {
        | Some(id) => id
        | None => Ident.create("func")
        };
      MAllocate(
        MClosure(
          compile_lambda(
            ~name?,
            id,
            env,
            args,
            body,
            c.comp_attributes,
            c.comp_loc,
          ),
        ),
      );
    | CApp((f, (argsty, retty)), args, true) =>
      let func_type = (argsty, [retty]);
      let closure = compile_imm(env, f);
      let args = List.map(compile_imm(env), args);
      switch (known_function(f)) {
      | Some(func) => MReturnCallKnown({func, closure, func_type, args})
      | None => MReturnCallIndirect({func: closure, func_type, args})
      };
    | CApp((f, (argsty, retty)), args, _) =>
      let func_type = (argsty, [retty]);
      let closure = compile_imm(env, f);
      let args = List.map(compile_imm(env), args);
      switch (known_function(f)) {
      | Some(func) => MCallKnown({func, closure, func_type, args})
      | None => MCallIndirect({func: closure, func_type, args})
      };
    | CAppBuiltin(modname, name, args) =>
      MCallRaw({
        func: "builtin",
        func_type: (
          List.map(i => Types.Unmanaged(WasmI32), args),
          [Types.Unmanaged(WasmI32)],
        ),
        args: List.map(compile_imm(env), args),
      })
    | CImmExpr(i) => MImmediate(compile_imm(env, i))
    };
  {instr_desc: desc, instr_loc: c.comp_loc};
}
and compile_anf_expr = (env, a) =>
  switch (a.anf_desc) {
  | AESeq(hd, tl) => [
      {
        instr_desc: MDrop(compile_comp(env, hd), hd.comp_allocation_type),
        instr_loc: hd.comp_loc,
      },
      ...compile_anf_expr(env, tl),
    ]
  | AELet(global, recflag, mutflag, binds, body) =>
    let rec get_locs = (env, binds) => {
      switch (binds) {
      | [(id, {comp_allocation_type}), ...rest] =>
        let (alloc, stack_idx, next_env) =
          switch (comp_allocation_type) {
          | Managed => (
              Types.Managed,
              env.ce_stack_idx_ptr,
              {...env, ce_stack_idx_ptr: env.ce_stack_idx_ptr + 1},
            )
          | Unmanaged(WasmI32) => (
              Types.Unmanaged(WasmI32),
              env.ce_stack_idx_i32,
              {...env, ce_stack_idx_i32: env.ce_stack_idx_i32 + 1},
            )
          | Unmanaged(WasmI64) => (
              Types.Unmanaged(WasmI64),
              env.ce_stack_idx_i64,
              {...env, ce_stack_idx_i64: env.ce_stack_idx_i64 + 1},
            )
          | Unmanaged(WasmF32) => (
              Types.Unmanaged(WasmF32),
              env.ce_stack_idx_f32,
              {...env, ce_stack_idx_f32: env.ce_stack_idx_f32 + 1},
            )
          | Unmanaged(WasmF64) => (
              Types.Unmanaged(WasmF64),
              env.ce_stack_idx_f64,
              {...env, ce_stack_idx_f64: env.ce_stack_idx_f64 + 1},
            )
          };
        let (env, loc) =
          switch (global) {
          | Global => (env, MGlobalBind(get_global(id, alloc), alloc))
          | Nonglobal => (
              next_env,
              MLocalBind(Int32.of_int(stack_idx), alloc),
            )
          };
        let env = {...env, ce_binds: Ident.add(id, loc, env.ce_binds)};
        let (new_env, locs) = get_locs(env, rest);
        (new_env, [loc, ...locs]);
      | [] => (env, [])
      };
    };
    let (new_env, locations) = get_locs(env, binds);
    switch (recflag) {
    | Nonrecursive =>
      let instrs =
        List.fold_right2(
          (loc, (id, rhs), acc) =>
            [
              {
                instr_desc: MStore([(loc, compile_comp(~id, env, rhs))]),
                instr_loc: rhs.comp_loc,
              },
              ...acc,
            ],
          locations,
          binds,
          [],
        );
      instrs @ compile_anf_expr(new_env, body);
    | Recursive =>
      let binds =
        List.fold_left2(
          (acc, loc, (id, rhs)) =>
            [(loc, compile_comp(~id, new_env, rhs)), ...acc],
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
      (
        funcs,
        {id, name, args, return_type, stack_size, attrs, loc} as cur: worklist_elt,
      ) => {
    let body = compile_worklist_elt(cur);
    let func = {
      id,
      name,
      args,
      return_type,
      body,
      stack_size,
      attrs,
      func_loc: loc,
    };
    [func, ...funcs];
  };
  List.rev(fold_left_pop(compile_one, []));
};

let lift_imports = (env, imports) => {
  let process_shape = (mut, shape) =>
    switch (shape) {
    | GlobalShape(alloc) => MGlobalImport(alloc, mut)
    | FunctionShape(inputs, outputs) => MFuncImport(inputs, outputs)
    };

  let process_import =
      (
        (imports, setups, env),
        {imp_use_id, imp_desc, imp_shape, imp_exported},
      ) => {
    switch (imp_desc) {
    | GrainValue(mimp_mod, mimp_name) =>
      let (alloc, mods, closure_setups) =
        switch (imp_shape) {
        | GlobalShape(alloc) => (
            alloc,
            [
              {
                mimp_id: imp_use_id,
                mimp_mod,
                mimp_name,
                mimp_type: process_shape(true, imp_shape),
                mimp_kind: MImportGrain,
                mimp_setup: MCallGetter,
                mimp_used: true,
              },
            ],
            [],
          )
        | FunctionShape(_) =>
          register_function(
            Imported(imp_use_id, Ident.unique_name(imp_use_id)),
          );
          let closure_setups =
            if (Analyze_function_calls.has_indirect_call(imp_use_id)) {
              let idx =
                next_function_table_index(
                  FuncName(Ident.unique_name(imp_use_id)),
                );
              [
                {
                  instr_desc:
                    MClosureOp(
                      MClosureSetPtr(Int32.of_int(idx)),
                      MImmBinding(
                        MGlobalBind(Ident.unique_name(imp_use_id), Managed),
                      ),
                    ),
                  instr_loc: Location.dummy_loc,
                },
              ];
            } else {
              [];
            };
          (
            Managed,
            [
              {
                mimp_id: imp_use_id,
                mimp_mod,
                mimp_name,
                mimp_type: process_shape(true, GlobalShape(Managed)),
                mimp_kind: MImportGrain,
                mimp_setup: MCallGetter,
                mimp_used: true,
              },
              {
                mimp_id: imp_use_id,
                mimp_mod,
                mimp_name,
                mimp_type: process_shape(true, imp_shape),
                mimp_kind: MImportGrain,
                mimp_setup: MSetupNone,
                mimp_used: true,
              },
            ],
            closure_setups,
          );
        };
      (
        mods @ imports,
        [closure_setups, ...setups],
        {
          ...env,
          ce_binds:
            Ident.add(
              imp_use_id,
              MGlobalBind(Ident.unique_name(imp_use_id), alloc),
              env.ce_binds,
            ),
        },
      );
    | WasmValue(mimp_mod, mimp_name) =>
      let alloc =
        switch (imp_shape) {
        | GlobalShape(alloc) => alloc
        | FunctionShape(_) =>
          failwith("internal: WasmValue had FunctionShape")
        };
      let new_mod = {
        mimp_id: imp_use_id,
        mimp_mod,
        mimp_name,
        mimp_type: process_shape(false, imp_shape),
        mimp_kind: MImportWasm,
        mimp_setup: MWrap(Int32.zero),
        mimp_used: true,
      };
      (
        [new_mod, ...imports],
        setups,
        {
          ...env,
          ce_binds:
            Ident.add(
              imp_use_id,
              MGlobalBind(Ident.unique_name(imp_use_id), alloc),
              env.ce_binds,
            ),
        },
      );
    | WasmFunction(mod_, name) =>
      let glob = get_global(imp_use_id, Types.Unmanaged(WasmI32));
      let mimp_id = Ident.create(wasm_import_name(mod_, name));
      let new_mod = {
        mimp_id,
        mimp_mod: mod_,
        mimp_name: name,
        mimp_type: process_shape(false, imp_shape),
        mimp_kind: MImportWasm,
        mimp_setup: MWrap(Int32.zero),
        mimp_used: true,
      };
      (
        [new_mod, ...imports],
        [
          switch (imp_shape) {
          | GlobalShape(_) => []
          | FunctionShape(inputs, outputs) =>
            if (List.length(outputs) > 1) {
              failwith("NYI: Multi-result wrapper");
            } else {
              [
                {
                  instr_desc:
                    MStore([
                      (
                        MGlobalBind(glob, Types.Managed),
                        {
                          instr_desc:
                            MAllocate(
                              MClosure(
                                compile_wrapper(
                                  ~name,
                                  imp_use_id,
                                  env,
                                  Ident.unique_name(mimp_id),
                                  inputs,
                                  outputs,
                                ),
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
            Ident.add(imp_use_id, MGlobalBind(glob, Managed), env.ce_binds),
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

let transl_signature = (~functions, ~imports, signature) => {
  open Types;

  let exports = ref([]);

  // At this point in compilation, we know which functions can be called
  // directly/indirectly at the wasm level. We add this information to the
  // module signature.
  let func_map = Ident_tbl.create(30);
  List.iter(
    (func: mash_function) =>
      switch (func.name) {
      | Some(name) =>
        Ident_tbl.add(func_map, func.id, Ident.unique_name(func.id))
      | None => ()
      },
    functions,
  );
  List.iter(
    imp =>
      switch (imp.imp_shape) {
      | FunctionShape(_) =>
        let internal_name = Ident.unique_name(imp.imp_use_id);
        Ident_tbl.add(func_map, imp.imp_use_id, internal_name);
      | _ => ()
      },
    imports.specs,
  );
  let sign =
    List.map(
      fun
      | TSigValue(vid, {val_repr, val_internalpath} as vd) => {
          let id =
            switch (val_internalpath) {
            | PIdent(id) => id
            | PExternal(_) =>
              switch (Path_tbl.find_opt(imports.path_map, val_internalpath)) {
              | Some(id) => id
              | None =>
                failwith(
                  "Impossible: path to import not found "
                  ++ Path.name(val_internalpath),
                )
              }
            };
          exports :=
            [
              GlobalExport({
                ex_global_name: Ident.name(vid),
                ex_global_internal_name: Ident.unique_name(id),
              }),
              ...exports^,
            ];
          switch (val_repr) {
          | ReprFunction(args, rets, _) =>
            switch (Ident_tbl.find_opt(func_map, id)) {
            | Some(internal_name) =>
              let external_name = Ident.name(vid);
              exports :=
                [
                  FunctionExport({
                    ex_function_name: external_name,
                    ex_function_internal_name: internal_name,
                  }),
                  ...exports^,
                ];
              TSigValue(
                vid,
                {
                  ...vd,
                  val_repr: ReprFunction(args, rets, Direct(external_name)),
                },
              );
            | _ =>
              TSigValue(
                vid,
                {...vd, val_repr: ReprFunction(args, rets, Indirect)},
              )
            }
          | ReprValue(_) => TSigValue(vid, vd)
          };
        }
      | _ as item => item,
      signature.Cmi_format.cmi_sign,
    );
  ({...signature, cmi_sign: sign}, exports^);
};

let transl_anf_program =
    (anf_prog: Anftree.anf_program): Mashtree.mash_program => {
  reset_function_table_info();
  reset_global();
  worklist_reset();
  clear_known_functions();

  Analyze_function_calls.analyze(anf_prog);

  let (imports, setups, env) =
    lift_imports(initial_compilation_env, anf_prog.imports.specs);

  set_global_imports(env.ce_binds);

  let {
    stack_size_ptr,
    stack_size_i32,
    stack_size_i64,
    stack_size_f32,
    stack_size_f64,
  }: Anf_utils.stack_size =
    Anf_utils.anf_count_vars(anf_prog.body);
  let main_body_stack_size = {
    stack_size_ptr,
    stack_size_i32,
    stack_size_i64,
    stack_size_f32,
    stack_size_f64,
  };
  let main_body =
    run_register_allocation @@ setups @ compile_anf_expr(env, anf_prog.body);
  let functions =
    List.map(
      ({body} as f: Mashtree.mash_function) =>
        {...f, body: run_register_allocation(body)},
      compile_remaining_worklist(),
    );
  let (signature, exports) =
    transl_signature(
      ~functions,
      ~imports=anf_prog.imports,
      anf_prog.signature,
    );
  let globals = get_globals();
  let function_table_elements = get_function_table_idents();

  {
    functions,
    imports,
    exports,
    main_body,
    main_body_stack_size,
    globals,
    function_table_elements,
    signature,
    type_metadata: anf_prog.type_metadata,
  };
};
