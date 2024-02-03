open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_utils;

open Asttypes;
open Anftree;
open Mashtree;

type compilation_env = {
  ce_binds: Ident.tbl(Mashtree.binding),
  ce_arity: int,
};

let initial_compilation_env = {ce_binds: Ident.empty, ce_arity: 0};

type worklist_elt_body =
  | Anf(anf_expression)
  | Precompiled(block, Mashtree.stack_size);

type worklist_elt = {
  body: worklist_elt_body,
  env: compilation_env,
  args: list(Types.allocation_type),
  return_type: list(Types.allocation_type),
  closure: option(int),
  id: Ident.t,
  name: option(string),
  attrs: attributes,
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

let local_ptr = ref(0);
let local_i32 = ref(0);
let local_i64 = ref(0);
let local_f32 = ref(0);
let local_f64 = ref(0);

let next_local = alloc => {
  let current =
    switch (alloc) {
    | Types.Managed => local_ptr
    | Types.Unmanaged(WasmI32) => local_i32
    | Types.Unmanaged(WasmI64) => local_i64
    | Types.Unmanaged(WasmF32) => local_f32
    | Types.Unmanaged(WasmF64) => local_f64
    };
  let slot = current^;
  incr(current);
  slot;
};

let reset_locals = () => {
  local_ptr := 0;
  local_i32 := 0;
  local_i64 := 0;
  local_f32 := 0;
  local_f64 := 0;
};

let get_stack_size = () => {
  {
    stack_size_ptr: local_ptr^,
    stack_size_i32: local_i32^,
    stack_size_i64: local_i64^,
    stack_size_f32: local_f32^,
    stack_size_f64: local_f64^,
  };
};

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

let wasm_import_name = (mod_, name) =>
  Printf.sprintf("wimport_%s_%s", mod_, name);

let compile_const = (c: Asttypes.constant) =>
  switch (c) {
  | Const_number(Const_number_int(i)) => MConstI32(Int64.to_int32(i))
  | Const_number(_) =>
    failwith("compile_const: Const_number float/rational post-ANF")
  | Const_bytes(_) => failwith("compile_const: Const_bytes post-ANF")
  | Const_string(_) => failwith("compile_const: Const_string post-ANF")
  | Const_bigint(_) => failwith("compile_const: Const_bigint post-ANF")
  | Const_rational(_) => failwith("compile_const: Const_rational post-ANF")
  | Const_int8(i8) => MConstI8(i8)
  | Const_int16(i16) => MConstI16(i16)
  | Const_int32(i32) => MConstI32(i32)
  | Const_int64(i64) => MConstI64(i64)
  | Const_uint8(u8) => MConstU8(u8)
  | Const_uint16(u16) => MConstU16(u16)
  | Const_uint32(u32) => MConstU32(u32)
  | Const_uint64(u64) => MConstU64(u64)
  | Const_float32(f) => MConstF32(f)
  | Const_float64(f) => MConstF64(f)
  | Const_wasmi32(i32) => MConstLiteral(MConstI32(i32))
  | Const_wasmi64(i64) => MConstLiteral(MConstI64(i64))
  | Const_wasmf32(f32) => MConstLiteral(MConstF32(f32))
  | Const_wasmf64(f64) => MConstLiteral(MConstF64(f64))
  | Const_char(c) => MConstChar(c)
  | Const_bool(b) when b == true => const_true
  | Const_bool(_) => const_false
  | Const_void => const_void
  };

let imm = i => {
  immediate_desc: i,
  immediate_analyses: {
    last_usage: Unknown,
  },
};

let compile_imm = (env, i: imm_expression) =>
  switch (i.imm_desc) {
  | ImmConst(c) => imm(MImmConst(compile_const(c)))
  | ImmId(id) => imm(MImmBinding(find_id(id, env)))
  | ImmTrap => imm(MImmTrap)
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
    (~name=?, ~closure_status, id, env, args, body, attrs, loc)
    : option(Mashtree.closure_data) => {
  register_function(Internal(id));

  let (body, return_type) = body;

  let (free_vars, closure) =
    switch (closure_status) {
    | Unnecessary => ([], None)
    | Precomputed(vars) => (vars, Some(List.length(vars)))
    | Uncomputed =>
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
      let elements = Ident.Set.elements(free_var_set);
      (elements, Some(List.length(elements)));
    };

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
  let lam_env = {ce_binds: arg_binds, ce_arity: arity};
  let worklist_item = {
    body: Anf(body),
    env: lam_env,
    id,
    name,
    args,
    return_type,
    closure,
    attrs,
    loc,
  };
  worklist_enqueue(worklist_item);
  if (Option.is_some(closure) || Analyze_function_calls.has_indirect_call(id)) {
    Some({
      func_idx,
      arity: Int32.of_int(arity),
      /* These variables should be in scope when the lambda is constructed. */
      variables:
        List.map(id => imm(MImmBinding(find_id(id, env))), free_vars),
    });
  } else {
    None;
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
              (i, arg) =>
                imm(MImmBinding(MArgBind(Int32.of_int(i + 1), arg))),
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
              instr_desc: MImmediate(imm(MImmConst(const_void))),
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
  let lam_env = {ce_binds: Ident.empty, ce_arity: arity + 1};
  let worklist_item = {
    body:
      Precompiled(
        body,
        {
          stack_size_ptr: 0,
          stack_size_i32: 0,
          stack_size_i64: 0,
          stack_size_f32: 0,
          stack_size_f64: 0,
        },
      ),
    env: lam_env,
    id,
    name,
    args: [Types.Managed, ...args],
    return_type,
    closure: Some(0),
    attrs: [Location.mknoloc(Typedtree.Disable_gc)],
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
            {instr_desc: MImmediate(imm(MImmTrap)), instr_loc: c.comp_loc},
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
    | CReturn(e) => MReturn(Option.map(compile_imm(env), e))
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
    | CRecord(type_hash, ttag, args) =>
      MAllocate(
        MRecord(
          compile_imm(env, type_hash),
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
    | CAdt(type_hash, ttag, vtag, args) =>
      MAllocate(
        MADT(
          compile_imm(env, type_hash),
          compile_imm(env, ttag),
          compile_imm(env, vtag),
          List.map(compile_imm(env), args),
        ),
      )
    | CBytes(b) => MAllocate(MBytes(b))
    | CString(s) => MAllocate(MString(s))
    | CNumber(Const_number_int(n))
        when n <= Literals.simple_number_max && n >= Literals.simple_number_min =>
      MImmediate(imm(MImmConst(MConstI32(Int64.to_int32(n)))))
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
    | CUint32(i) => MAllocate(MUint32(i))
    | CUint64(i) => MAllocate(MUint64(i))
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
    | CLambda(name, args, body, closure_status) =>
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
      let cdata =
        compile_lambda(
          ~name?,
          ~closure_status,
          id,
          env,
          args,
          body,
          c.comp_attributes,
          c.comp_loc,
        );
      switch (cdata) {
      | Some(cdata) => MAllocate(MClosure(cdata))
      | None => MImmediate(imm(MImmConst(MConstLiteral(MConstI32(0l)))))
      };
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
  | AELet(global, recflag, mutflag, binds, body) =>
    let rec get_locs = (env, binds) => {
      switch (binds) {
      | [(id, {comp_allocation_type: alloc}), ...rest] =>
        let (env, loc) =
          switch (global) {
          | Global => (env, MGlobalBind(get_global(id, alloc), alloc))
          | Nonglobal => (
              env,
              MLocalBind(Int32.of_int(next_local(alloc)), alloc),
            )
          };
        let env = {...env, ce_binds: Ident.add(id, loc, env.ce_binds)};
        let (new_env, locs) = get_locs(env, rest);
        (new_env, [loc, ...locs]);
      | [] => (env, [])
      };
    };
    let (new_env, locations) = get_locs(env, binds);
    let instrs =
      switch (recflag) {
      | Nonrecursive =>
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
        )
      | Recursive => [
          {
            instr_desc:
              MStore(
                List.fold_right2(
                  (loc, (id, rhs), acc) =>
                    [(loc, compile_comp(~id, new_env, rhs)), ...acc],
                  locations,
                  binds,
                  [],
                ),
              ),
            instr_loc: a.anf_loc,
          },
        ]
      };
    instrs @ compile_anf_expr(new_env, body);
  | AEComp(c) => [compile_comp(env, c)]
  };

let compile_function_body = (env, body) => {
  reset_locals();
  let body = compile_anf_expr(env, body);
  let stack_size = get_stack_size();
  (body, stack_size);
};

let compile_worklist_elt = ({body, env}: worklist_elt) =>
  switch (body) {
  | Anf(body) => compile_function_body(env, body)
  | Precompiled(block, stack_size) => (block, stack_size)
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
        {id, name, args, return_type, closure, attrs, loc} as cur: worklist_elt,
      ) => {
    let (body, stack_size) = compile_worklist_elt(cur);
    let func = {
      id,
      name,
      args,
      return_type,
      closure,
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
    | FunctionShape({args, returns}) => MFuncImport(args, returns)
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
        | FunctionShape({args, has_closure}) =>
          register_function(
            Imported(imp_use_id, Ident.unique_name(imp_use_id)),
          );
          let closure_setups =
            if (Analyze_function_calls.has_indirect_call(imp_use_id)) {
              let idx =
                next_function_table_index(
                  FuncName(Ident.unique_name(imp_use_id)),
                );
              if (has_closure) {
                [
                  {
                    instr_desc:
                      MClosureOp(
                        MClosureSetPtr(Int32.of_int(idx)),
                        imm(
                          MImmBinding(
                            MGlobalBind(
                              Ident.unique_name(imp_use_id),
                              Managed,
                            ),
                          ),
                        ),
                      ),
                    instr_loc: Location.dummy_loc,
                  },
                ];
              } else {
                [
                  {
                    instr_desc:
                      MStore([
                        (
                          MGlobalBind(
                            Ident.unique_name(imp_use_id),
                            Types.Managed,
                          ),
                          {
                            instr_desc:
                              MAllocate(
                                MClosure({
                                  func_idx: Some(Int32.of_int(idx)),
                                  arity: Int32.of_int(List.length(args)),
                                  variables: [],
                                }),
                              ),
                            instr_loc: Location.dummy_loc,
                          },
                        ),
                      ]),
                    instr_loc: Location.dummy_loc,
                  },
                ];
              };
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
          | FunctionShape({args, returns}) =>
            if (List.length(returns) > 1) {
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
                                  args,
                                  returns,
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
        Ident_tbl.add(
          func_map,
          func.id,
          (Ident.unique_name(func.id), Option.is_some(func.closure)),
        )
      | None => ()
      },
    functions,
  );
  List.iter(
    imp =>
      switch (imp.imp_shape) {
      | FunctionShape({has_closure}) =>
        let internal_name = Ident.unique_name(imp.imp_use_id);
        Ident_tbl.add(
          func_map,
          imp.imp_use_id,
          (internal_name, has_closure),
        );
      | _ => ()
      },
    imports.specs,
  );
  let rec process_item = (~prefix="", item) => {
    switch (item) {
    | TSigValue(vid, {val_repr, val_internalpath} as vd) =>
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
          WasmGlobalExport({
            ex_global_name: prefix ++ Ident.name(vid),
            ex_global_internal_name: Ident.unique_name(id),
          }),
          ...exports^,
        ];
      switch (val_repr) {
      | ReprFunction(args, rets, _) =>
        switch (Ident_tbl.find_opt(func_map, id)) {
        | Some((internal_name, closure)) =>
          let external_name = Ident.name(vid);
          exports :=
            [
              WasmFunctionExport({
                ex_function_name: prefix ++ external_name,
                ex_function_internal_name: internal_name,
              }),
              ...exports^,
            ];
          TSigValue(
            vid,
            {
              ...vd,
              val_repr:
                ReprFunction(
                  args,
                  rets,
                  Direct({name: external_name, closure}),
                ),
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
    | TSigModule(tid, decl, rs) =>
      let decl =
        switch (decl.md_type) {
        | TModIdent(_)
        | TModAlias(_) => decl
        | TModSignature(signature) => {
            ...decl,
            // This module will provide all exports for all submodules
            md_filepath: None,
            md_type:
              TModSignature(
                List.map(
                  process_item(
                    ~prefix=Printf.sprintf("%s%s.", prefix, Ident.name(tid)),
                  ),
                  signature,
                ),
              ),
          }
        };
      TSigModule(tid, decl, rs);
    | _ as item => item
    };
  };
  let sign = List.map(process_item, signature.Cmi_format.cmi_sign);
  ({...signature, cmi_sign: sign}, exports^);
};

let transl_anf_program =
    (anf_prog: Anftree.anf_program): Mashtree.mash_program => {
  reset_function_table_info();
  reset_global();
  worklist_reset();
  clear_known_functions();

  let anf_prog = Optimize_closures.optimize(anf_prog);

  let (imports, setups, env) =
    lift_imports(initial_compilation_env, anf_prog.imports.specs);

  set_global_imports(env.ce_binds);

  let (main_body, main_body_stack_size) =
    compile_function_body(env, anf_prog.body);
  let main_body = setups @ main_body;
  let main_body =
    if (Config.no_gc^) {
      main_body;
    } else {
      Garbage_collection.apply_gc([], None, main_body);
    };
  let functions =
    List.map(
      (func: mash_function) =>
        if (Config.no_gc^
            || List.exists(
                 ({Grain_parsing.Location.txt}) =>
                   txt == Typedtree.Disable_gc,
                 func.attrs,
               )) {
          func;
        } else {
          let body =
            Garbage_collection.apply_gc(func.args, func.closure, func.body);
          {...func, body};
        },
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
