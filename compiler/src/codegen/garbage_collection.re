open Mashtree;

/*
 * This module takes a list of Mashtree instructions and incorporates
 * reference-counting instructions.
 */

let instr_produces_value = instr =>
  switch (instr.instr_desc) {
  | MImmediate(_)
  | MCallRaw({func_type: (_, [_, ..._])})
  | MCallKnown({func_type: (_, [_, ..._])})
  | MCallIndirect({func_type: (_, [_, ..._])}) => true
  | MReturnCallKnown({func_type: (_, [_, ..._])})
  | MReturnCallIndirect({func_type: (_, [_, ..._])}) => false
  | MCallRaw(_)
  | MCallKnown(_)
  | MCallIndirect(_)
  | MReturnCallKnown(_)
  | MReturnCallIndirect(_) => false
  | MError(_) => false
  | MAllocate(_) => true
  | MTagOp(_) => false
  | MArityOp(_) => true
  | MIf(_) => true
  | MFor(_) => true
  | MContinue
  | MBreak => false
  | MReturn(_) => false
  | MSwitch(_) => true
  | MPrim0(_) => true
  | MPrim1(_)
  | MPrim2(_)
  | MPrimN(_) => true
  | MTupleOp(_)
  | MBoxOp(_)
  | MArrayOp(_)
  | MAdtOp(_)
  | MRecordOp(_) => true
  | MClosureOp(MClosureSetPtr(_), _) => false
  | MStore(_) => false
  | MSet(_) => true
  | MDrop(_) => false
  | MCleanup(_) => failwith("Impossible: MCleanup before GC")
  };

module BindMap =
  Map.Make({
    type t = binding;
    let compare = Stdlib.compare;
  });

module BindSet =
  Set.Make({
    type t = binding;
    let compare = Stdlib.compare;
  });

let usage_map = ref(BindMap.empty);

let register_slot = slot => {
  usage_map := BindMap.add(slot, None, usage_map^);
};

let finalize = () => {
  BindMap.iter(
    (_, imm) => {
      switch (imm) {
      // This use of the immediate was the last seen, so mark it as such.
      | Some(imm) => imm.immediate_analyses.last_usage = Last
      | None => ()
      }
    },
    usage_map^,
  );
};

let register_usage = (slot, imm) => {
  // Here we could mark the old immediate explicitly as NotLast, but this isn't
  // information we use at this time, so we save a few cycles.
  usage_map := BindMap.update(slot, _ => Some(Some(imm)), usage_map^);
};

let rec analyze_usage = instrs => {
  let process_imm = imm => {
    switch (imm.immediate_desc) {
    | MImmBinding(binding) => register_usage(binding, imm)
    | MImmConst(_)
    | MImmTrap
    | MIncRef(_) => ()
    };
  };

  let process_tail_call_arg = imm => {
    switch (imm.immediate_desc) {
    | MImmBinding(binding) =>
      switch (BindMap.find(binding, usage_map^)) {
      | Some(imm) => imm.immediate_analyses.last_usage = TailCallLast
      | None => ()
      }
    | MImmConst(_)
    | MImmTrap
    | MIncRef(_) => ()
    };
  };

  let rec process_instr = instr => {
    switch (instr.instr_desc) {
    | MImmediate(imm) => process_imm(imm)
    | MCallRaw({args}) => List.iter(process_imm, args)
    | MCallKnown({closure: func, args})
    | MCallIndirect({func, args}) =>
      process_imm(func);
      List.iter(process_imm, args);
    | MReturnCallKnown({closure: func, args})
    | MReturnCallIndirect({func, args}) =>
      process_imm(func);
      List.iter(process_imm, args);
      process_tail_call_arg(func);
      List.iter(process_tail_call_arg, args);
    | MError(e, args) => List.iter(process_imm, args)
    | MAllocate(alloc) =>
      switch (alloc) {
      | MClosure({variables: args})
      | MTuple(args)
      | MArray(args) => List.iter(process_imm, args)
      | MBox(imm) => process_imm(imm)
      | MRecord(_, tag, fields) =>
        List.iter(((_, value)) => process_imm(value), fields)
      | MADT(_, ttag, vtag, args) =>
        process_imm(ttag);
        process_imm(vtag);
        List.iter(process_imm, args);
      | MBytes(_)
      | MString(_)
      | MInt32(_)
      | MInt64(_)
      | MUint32(_)
      | MUint64(_)
      | MFloat32(_)
      | MFloat64(_)
      | MRational(_)
      | MBigInt(_) => ()
      }
    | MTagOp(_, _, imm)
    | MArityOp(_, _, imm) => process_imm(imm)
    | MIf(imm, thn, els) =>
      process_imm(imm);
      analyze_usage(thn);
      analyze_usage(els);
    | MFor(cond, inc, body) =>
      Option.iter(analyze_usage, cond);
      Option.iter(analyze_usage, inc);
      analyze_usage(body);
    | MContinue
    | MBreak => ()
    | MReturn(ret) => Option.iter(process_imm, ret)
    | MSwitch(value, branches, default, _) =>
      process_imm(value);
      List.iter(((_, branch)) => analyze_usage(branch), branches);
      analyze_usage(default);
    | MPrim0(_) => ()
    | MPrim1(prim1, imm) => process_imm(imm)
    | MPrim2(prim2, imm1, imm2) =>
      process_imm(imm1);
      process_imm(imm2);
    | MPrimN(primn, args) => List.iter(process_imm, args)
    | MTupleOp(tuple_op, imm) =>
      switch (tuple_op) {
      | MTupleGet(_) => ()
      | MTupleSet(_, imm) => process_imm(imm)
      };
      process_imm(imm);
    | MBoxOp(box_op, imm) =>
      switch (box_op) {
      | MBoxUnbox => ()
      | MBoxUpdate(imm) => process_imm(imm)
      };
      process_imm(imm);
    | MArrayOp(array_op, imm) =>
      switch (array_op) {
      | MArrayLength => ()
      | MArrayGet(idx) => process_imm(idx)
      | MArraySet(idx, imm) =>
        process_imm(idx);
        process_imm(imm);
      };
      process_imm(imm);
    | MAdtOp(adt_op, imm) => process_imm(imm)
    | MRecordOp(record_op, imm) =>
      switch (record_op) {
      | MRecordGet(_) => ()
      | MRecordSet(_, imm) => process_imm(imm)
      };
      process_imm(imm);
    | MClosureOp(closure_op, imm) => process_imm(imm)
    | MStore(binds) =>
      List.iter(
        ((bind, instr)) => {
          // The instruction is executed before the slot is available, so we're
          // careful to maintain order here.
          process_instr(instr);
          register_slot(bind);
        },
        binds,
      )
    | MSet(binding, instr) =>
      // special case: consider this a use of the binding. This prevents a
      // potential double-decref, as MSet always decrefs the old value.
      usage_map := BindMap.update(binding, _ => Some(None), usage_map^);
      process_instr(instr);
    | MDrop(instr) => process_instr(instr)
    | MCleanup(_) => failwith("Impossible: MCleanup before GC")
    };
  };

  List.iter(process_instr, instrs);
};

let analyze_usage = instrs => {
  analyze_usage(instrs);
  finalize();
};

let is_last_usage = imm =>
  switch (imm.immediate_analyses.last_usage) {
  | Last => true
  | _ => false
  };

let is_last_tail_call_usage = imm =>
  switch (imm.immediate_analyses.last_usage) {
  | TailCallLast => true
  | _ => false
  };

type bind_state =
  | Alive
  | Dead;

type cleanup_mode =
  // Standard cleanup of dead bindings on the current level.
  | Normal
  // Cleanup of all bindings on the current level, including unused bindings.
  | CleanUpLevel
  // Cleanup of all bindings created within the current loop, used with `break` and `continue`.
  | CleanUpLoop;

let live_bindings = ref(BindMap.empty);

let rec apply_gc = (~level, ~loop_context, ~implicit_return=false, instrs) => {
  let apply_gc = (~loop_context=loop_context) =>
    apply_gc(~loop_context, ~level=level + 1);

  let get_level_dead_bindings = () => {
    BindMap.fold(
      (slot, (lv, state), acc) =>
        switch (state) {
        | Dead when lv == level => [slot, ...acc]
        | _ => acc
        },
      live_bindings^,
      [],
    );
  };

  let get_level_bindings = () => {
    BindMap.fold(
      (slot, (lv, _), acc) =>
        if (lv == level) {
          [slot, ...acc];
        } else {
          acc;
        },
      live_bindings^,
      [],
    );
  };

  let get_loop_bindings = () => {
    let loop_level = List.hd(loop_context);
    BindMap.fold(
      (slot, (lv, _), acc) =>
        if (lv > loop_level && lv <= level) {
          [slot, ...acc];
        } else {
          acc;
        },
      live_bindings^,
      [],
    );
  };

  let incref = imm => {
    immediate_desc: MIncRef(imm),
    immediate_analyses: {
      last_usage: Unknown,
    },
  };

  let handle_imm =
      (~non_gc_instr=false, ~is_return=false, ~is_tail=false, imm) => {
    switch (imm.immediate_desc) {
    | MImmBinding((MArgBind(_) | MLocalBind(_) | MClosureBind(_)) as binding) =>
      let alloc =
        switch (binding) {
        | MArgBind(_, alloc)
        | MLocalBind(_, alloc)
        | MGlobalBind(_, alloc)
        | MSwapBind(_, alloc) => alloc
        | MClosureBind(_) => Managed
        };
      if (is_last_usage(imm)) {
        let bind_level =
          switch (BindMap.find_opt(binding, live_bindings^)) {
          | Some((level, _)) => level
          | None => failwith("Impossible: Unknown binding")
          };
        if (level == bind_level) {
          if (non_gc_instr) {
            // Still need this to be cleaned up.
            live_bindings :=
              BindMap.update(
                binding,
                _ => Some((bind_level, Dead)),
                live_bindings^,
              );
          } else {
            live_bindings := BindMap.remove(binding, live_bindings^);
          };
          imm;
        } else {
          live_bindings :=
            BindMap.update(
              binding,
              _ => Some((bind_level, Dead)),
              live_bindings^,
            );
          switch (alloc) {
          | Unmanaged(_) => imm
          | Managed when non_gc_instr || is_return || is_tail => imm
          | Managed => incref(imm)
          };
        };
      } else if (is_last_tail_call_usage(imm)) {
        imm;
      } else {
        switch (alloc) {
        | Managed when non_gc_instr || is_return => imm
        | Unmanaged(_) => imm
        | Managed => incref(imm)
        };
      };
    | MImmBinding(_) when non_gc_instr => imm
    // Return statements will still incref non-owned values.
    | MImmBinding(MGlobalBind(_, Managed) | MSwapBind(_, Managed)) =>
      incref(imm)
    | MImmBinding(MGlobalBind(_) | MSwapBind(_))
    | MImmConst(_)
    | MImmTrap
    | MIncRef(_) => imm
    };
  };

  let rec process_instr = instr => {
    let instr_desc =
      switch (instr.instr_desc) {
      | MImmediate(imm) => MImmediate(handle_imm(imm))
      | MCallRaw({args} as data) =>
        MCallRaw({...data, args: List.map(handle_imm, args)})
      | MCallKnown({closure, args} as data) =>
        MCallKnown({
          ...data,
          closure: handle_imm(closure),
          args: List.map(handle_imm, args),
        })
      | MReturnCallKnown({closure, args} as data) =>
        // tail calls will use arguments directly without the need to incref
        MReturnCallKnown({
          ...data,
          closure: handle_imm(~is_tail=true, closure),
          args: List.map(handle_imm(~is_tail=true), args),
        })
      | MCallIndirect({func, args} as data) =>
        MCallIndirect({
          ...data,
          func: handle_imm(func),
          args: List.map(handle_imm, args),
        })
      | MReturnCallIndirect({func, args} as data) =>
        // tail calls will use arguments directly without the need to incref
        MReturnCallIndirect({
          ...data,
          func: handle_imm(~is_tail=true, func),
          args: List.map(handle_imm(~is_tail=true), args),
        })
      | MError(e, args) => MError(e, List.map(handle_imm, args))
      | MAllocate(alloc) =>
        let alloc =
          switch (alloc) {
          | MClosure({variables} as cdata) =>
            MClosure({...cdata, variables: List.map(handle_imm, variables)})
          | MTuple(args) => MTuple(List.map(handle_imm, args))
          | MBox(imm) => MBox(handle_imm(imm))
          | MArray(args) => MArray(List.map(handle_imm, args))
          | MRecord(type_hash, tag, fields) =>
            MRecord(
              handle_imm(type_hash),
              handle_imm(tag),
              List.map(
                ((field, value)) => (field, handle_imm(value)),
                fields,
              ),
            )
          | MADT(type_hash, ttag, vtag, args) =>
            MADT(
              handle_imm(type_hash),
              handle_imm(ttag),
              handle_imm(vtag),
              List.map(handle_imm, args),
            )
          | MBytes(_)
          | MString(_)
          | MInt32(_)
          | MInt64(_)
          | MUint32(_)
          | MUint64(_)
          | MFloat32(_)
          | MFloat64(_)
          | MRational(_)
          | MBigInt(_) => alloc
          };
        MAllocate(alloc);
      | MTagOp(tag_op, tag_type, imm) =>
        MTagOp(tag_op, tag_type, handle_imm(~non_gc_instr=true, imm))
      | MArityOp(arity_operand, arity_op, imm) =>
        MArityOp(
          arity_operand,
          arity_op,
          handle_imm(~non_gc_instr=true, imm),
        )
      | MIf(imm, thn, els) =>
        MIf(
          handle_imm(~non_gc_instr=true, imm),
          apply_gc(thn),
          apply_gc(els),
        )
      | MFor(cond, inc, body) =>
        MFor(
          Option.map(apply_gc, cond),
          Option.map(apply_gc, inc),
          apply_gc(~loop_context=[level, ...loop_context], body),
        )
      | MContinue
      | MBreak => instr.instr_desc
      // A return will use the argument directly without the need to incref
      | MReturn(ret) =>
        MReturn(Option.map(handle_imm(~is_return=true), ret))
      | MSwitch(value, branches, default, alloc) =>
        MSwitch(
          handle_imm(value),
          List.map(
            ((branch_id, branch)) => (branch_id, apply_gc(branch)),
            branches,
          ),
          apply_gc(default),
          alloc,
        )
      | MPrim0(_) => instr.instr_desc
      | MPrim1(WasmFromGrain, imm) =>
        // special case: we delay cleanup of any `fromGrain` to the end of the function to
        // remove the headache of the value being GC'd before anything can be done with it
        switch (imm.immediate_desc) {
        | MImmBinding(
            (MArgBind(_, Managed) | MLocalBind(_, Managed)) as bind,
          ) =>
          let (_, dead) = BindMap.find(bind, live_bindings^);
          live_bindings :=
            BindMap.update(bind, _ => Some(((-1), dead)), live_bindings^);
        | _ => ()
        };
        MPrim1(WasmFromGrain, handle_imm(~non_gc_instr=true, imm));
      | MPrim1((Box | BoxBind | Throw | Magic) as prim1, imm) =>
        MPrim1(prim1, handle_imm(imm))
      | MPrim1(prim1, imm) =>
        MPrim1(prim1, handle_imm(~non_gc_instr=true, imm))
      | MPrim2(Eq, imm1, imm2) =>
        MPrim2(Eq, handle_imm(imm1), handle_imm(imm2))
      | MPrim2(prim2, imm1, imm2) =>
        MPrim2(
          prim2,
          handle_imm(~non_gc_instr=true, imm1),
          handle_imm(~non_gc_instr=true, imm2),
        )
      | MPrimN(primn, args) =>
        MPrimN(primn, List.map(handle_imm(~non_gc_instr=true), args))
      | MTupleOp(tuple_op, imm) =>
        let tuple_op =
          switch (tuple_op) {
          | MTupleGet(idx) => MTupleGet(idx)
          | MTupleSet(idx, imm) => MTupleSet(idx, handle_imm(imm))
          };
        MTupleOp(tuple_op, handle_imm(~non_gc_instr=true, imm));
      | MBoxOp(box_op, imm) =>
        let box_op =
          switch (box_op) {
          | MBoxUnbox => MBoxUnbox
          | MBoxUpdate(imm) => MBoxUpdate(handle_imm(imm))
          };
        MBoxOp(box_op, handle_imm(~non_gc_instr=true, imm));
      | MArrayOp(array_op, imm) =>
        let array_op =
          switch (array_op) {
          | MArrayLength => MArrayLength
          | MArrayGet(idx) => MArrayGet(handle_imm(~non_gc_instr=true, idx))
          | MArraySet(idx, imm) =>
            MArraySet(handle_imm(~non_gc_instr=true, idx), handle_imm(imm))
          };
        MArrayOp(array_op, handle_imm(~non_gc_instr=true, imm));
      | MAdtOp(adt_op, imm) =>
        MAdtOp(adt_op, handle_imm(~non_gc_instr=true, imm))
      | MRecordOp(record_op, imm) =>
        let record_op =
          switch (record_op) {
          | MRecordGet(idx) => MRecordGet(idx)
          | MRecordSet(idx, imm) => MRecordSet(idx, handle_imm(imm))
          };
        MRecordOp(record_op, handle_imm(~non_gc_instr=true, imm));
      | MClosureOp(closure_op, imm) =>
        MClosureOp(closure_op, handle_imm(~non_gc_instr=true, imm))
      | MStore(binds) =>
        MStore(
          List.map(
            ((bind, instr)) => {
              let instr = process_instr(instr);
              live_bindings :=
                BindMap.add(bind, (level, Alive), live_bindings^);
              (bind, instr);
            },
            binds,
          ),
        )
      | MSet(binding, instr) => MSet(binding, process_instr(instr))
      | MDrop(instr) => MDrop(process_instr(instr))
      | MCleanup(_) => failwith("Impossible: MCleanup before GC")
      };
    {...instr, instr_desc};
  };

  let do_full_cleanup = (~skip, instr) => {
    let binds =
      BindMap.fold(
        (bind, _, acc) =>
          if (BindSet.mem(bind, skip)) {
            acc;
          } else {
            switch (bind) {
            | MArgBind(_, Managed)
            | MLocalBind(_, Managed)
            | MClosureBind(_) => [
                {
                  immediate_desc: MImmBinding(bind),
                  immediate_analyses: {
                    last_usage: Unknown,
                  },
                },
                ...acc,
              ]
            | _ => acc
            };
          },
        live_bindings^,
        [],
      );
    live_bindings :=
      List.fold_left(
        (bind, map) => BindMap.remove(map, bind),
        live_bindings^,
        get_level_dead_bindings(),
      );
    switch (binds, instr) {
    | ([], Some(instr)) => [instr]
    | ([], None) => []
    | (_, Some(instr)) when instr_produces_value(instr) => [
        {
          instr_desc: MCleanup(Some(instr), binds),
          instr_loc: Grain_parsing.Location.dummy_loc,
        },
      ]
    | (_, Some(instr)) => [
        {
          instr_desc: MCleanup(None, binds),
          instr_loc: Grain_parsing.Location.dummy_loc,
        },
        instr,
      ]
    | (_, None) => [
        {
          instr_desc: MCleanup(None, binds),
          instr_loc: Grain_parsing.Location.dummy_loc,
        },
      ]
    };
  };

  let do_local_cleanup = (~skip, ~mode=Normal, instr) => {
    let (bindings, remove) =
      switch (mode) {
      | Normal => (get_level_dead_bindings(), true)
      | CleanUpLevel => (get_level_bindings(), true)
      | CleanUpLoop => (get_loop_bindings(), false)
      };
    let cleanup =
      List.filter_map(
        bind => {
          if (remove) {
            live_bindings := BindMap.remove(bind, live_bindings^);
          };
          switch (bind) {
          | _ when BindSet.mem(bind, skip) => None
          | MArgBind(_, Managed)
          | MLocalBind(_, Managed)
          | MGlobalBind(_, Managed)
          | MClosureBind(_)
          | MSwapBind(_, Managed) =>
            Some({
              immediate_desc: MImmBinding(bind),
              immediate_analyses: {
                last_usage: Unknown,
              },
            })
          | _ => None
          };
        },
        bindings,
      );
    switch (cleanup, mode) {
    | ([], _) => [instr]
    | (_, CleanUpLoop) => [
        {
          instr_desc: MCleanup(None, cleanup),
          instr_loc: Grain_parsing.Location.dummy_loc,
        },
        instr,
      ]
    | _ =>
      if (instr_produces_value(instr)) {
        [
          {
            instr_desc: MCleanup(Some(instr), cleanup),
            instr_loc: Grain_parsing.Location.dummy_loc,
          },
        ];
      } else {
        [
          instr,
          {
            instr_desc: MCleanup(None, cleanup),
            instr_loc: Grain_parsing.Location.dummy_loc,
          },
        ];
      }
    };
  };

  let is_return_instr = instr =>
    switch (instr.instr_desc) {
    | MReturn(_)
    | MReturnCallKnown(_)
    | MReturnCallIndirect(_) => true
    | _ => false
    };

  let rec do_gc = (acc, instrs) => {
    switch (instrs) {
    | [] => acc
    | [instr] when implicit_return && !is_return_instr(instr) =>
      // Last value of the function, implicitly returned. Don't clean yourself up!
      let (skip, instr) =
        switch (instr.instr_desc) {
        | MImmediate({immediate_desc: MImmBinding(self)} as imm) => (
            BindSet.singleton(self),
            {
              ...instr,
              instr_desc: MImmediate(handle_imm(~non_gc_instr=true, imm)),
            },
          )
        | _ => (BindSet.empty, process_instr(instr))
        };
      List.rev_append(do_full_cleanup(~skip, Some(instr)), acc);
    | [
        {instr_desc: MImmediate({immediate_desc: MImmBinding(self)})} as instr,
      ] =>
      // Last value of a block, the block's value. Don't clean yourself up!
      let instr = process_instr(instr);
      List.rev_append(
        do_local_cleanup(
          ~mode=CleanUpLevel,
          ~skip=BindSet.singleton(self),
          instr,
        ),
        acc,
      );
    | [instr, ...instrs] =>
      let instr = process_instr(instr);
      switch (instr.instr_desc) {
      | MReturn(Some(imm)) =>
        let skip =
          switch (imm.immediate_desc) {
          | MImmBinding(bind) =>
            // Don't clean up the thing we're about to return.
            BindSet.singleton(bind)
          | _ => BindSet.empty
          };
        do_gc(
          [instr, ...List.rev_append(do_full_cleanup(~skip, None), acc)],
          instrs,
        );
      | MReturn(None) =>
        do_gc(
          [
            instr,
            ...List.rev_append(
                 do_full_cleanup(~skip=BindSet.empty, None),
                 acc,
               ),
          ],
          instrs,
        )
      | MReturnCallIndirect({func, args})
      | MReturnCallKnown({closure: func, args}) =>
        // Don't clean up anything being sent to the next function.
        let skip =
          switch (func.immediate_desc) {
          | MImmBinding(bind) => BindSet.singleton(bind)
          | _ => BindSet.empty
          };
        let skip =
          BindSet.union(
            skip,
            BindSet.of_list(
              List.filter_map(
                arg => {
                  switch (arg.immediate_desc) {
                  // Don't clean up anything being sent to the next function.
                  | MImmBinding(bind) => Some(bind)
                  | _ => None
                  }
                },
                args,
              ),
            ),
          );
        do_gc(
          List.rev_append(do_full_cleanup(~skip, Some(instr)), acc),
          instrs,
        );
      | MDrop({instr_desc: MBreak | MContinue})
      | MBreak
      | MContinue =>
        do_gc(
          List.rev_append(
            do_local_cleanup(~mode=CleanUpLoop, ~skip=BindSet.empty, instr),
            acc,
          ),
          instrs,
        )
      | _ =>
        let mode =
          switch (instrs) {
          | [] => CleanUpLevel
          | _ => Normal
          };
        do_gc(
          List.rev_append(
            do_local_cleanup(~mode, ~skip=BindSet.empty, instr),
            acc,
          ),
          instrs,
        );
      };
    };
  };

  List.rev(do_gc([], instrs));
};

let apply_gc = (args, closure, block) => {
  usage_map := BindMap.empty;
  live_bindings := BindMap.empty;

  // Include function arguments as GC targets
  List.iteri(
    (i, arg) => {
      live_bindings :=
        BindMap.add(
          MArgBind(Int32.of_int(i), arg),
          (0, Alive),
          live_bindings^,
        )
    },
    args,
  );

  // Include closure arguments as GC targets
  switch (closure) {
  | Some(num_elements) =>
    ignore(
      List.init(num_elements, i => {
        live_bindings :=
          BindMap.add(
            MClosureBind(Int32.of_int(i)),
            (0, Alive),
            live_bindings^,
          )
      }),
    )
  | None => ()
  };

  analyze_usage(block);

  apply_gc(~loop_context=[], ~level=0, ~implicit_return=true, block);
};
