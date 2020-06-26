open Grain_parsing
open Grain_typed
open Grain_middle_end

open Asttypes
open Anftree
open Mashtree


module StrMap = BatMap.String

type compilation_env = {
  ce_binds: Mashtree.binding Ident.tbl;
  (* Useful due to us needing a second pass over exports (for mutual recursion) *)
  ce_exported_globals: int32 Ident.tbl;
  ce_stack_idx: int;
  ce_arity: int;
}

let initial_compilation_env = {
  ce_binds = Ident.empty;
  ce_exported_globals = Ident.empty;
  ce_stack_idx = 0;
  ce_arity = 0;
}

type worklist_elt_body =
  | Anf of anf_expression
  | Precompiled of block

type worklist_elt = {
  body : worklist_elt_body;
  env : compilation_env;
  arity : int;
  idx : int; (* Lambda-lifted index *)
  stack_size : int;
}

let compilation_worklist = ref (BatDeque.empty : worklist_elt BatDeque.t)

(** Lambda-lifting index (function index) *)
let lift_index = ref 0

let reset_lift() =
  lift_index := 0

let next_lift() =
  let ret = !lift_index in
  lift_index := ret + 1;
  ret

(** Global index (index of global variables) *)
let global_table = ref (Ident.empty : (int32 * int32) Ident.tbl)
let global_index = ref 0

let global_exports() =
  let tbl = !global_table in
  Ident.fold_all (fun ex_name (ex_global_index, ex_getter_index) acc -> {ex_name; ex_global_index; ex_getter_index}::acc) tbl []

let reset_global() =
  global_table := Ident.empty;
  global_index := 0

let next_global id =
  (* RIP Hygiene (this behavior works as expected until we have more metaprogramming constructs) *)
  match Ident.find_same_opt id (!global_table) with
  | Some(ret, ret_get) -> Int32.to_int ret, Int32.to_int ret_get
  | None ->
    begin
      let ret = !global_index in
      let ret_get = next_lift() in
      global_table := Ident.add id ((Int32.of_int ret), (Int32.of_int ret_get)) !global_table;
      global_index := ret + 1;
      (ret, ret_get)
    end

let find_id id env = Ident.find_same id env.ce_binds
let find_global id env = Ident.find_same id env.ce_exported_globals


let worklist_reset () = compilation_worklist := BatDeque.empty
let worklist_enqueue elt = compilation_worklist := BatDeque.snoc !compilation_worklist elt
let worklist_empty () = BatDeque.is_empty !compilation_worklist
let worklist_pop () =
  match BatDeque.front !compilation_worklist with
  | None -> raise Not_found
  | Some(hd, tl) ->
    compilation_worklist := tl;
    hd

module IntMap = Map.Make(Int)
module RegisterAllocation = struct
  type t = (int IntMap.t) * (int list)
  let initialize (num_locals: int): t = (IntMap.empty, BatList.init num_locals (fun x -> x))
  (* Takes an id (a local from the original mashtree) and assigns a slot to it *)
  let allocate var_id = function
    | (_, []) -> failwith "Should be impossible"
    | (allocs, hd::tl) -> (IntMap.add var_id hd allocs, tl), hd

  let release_var (var_id: int): (t -> t) = function
    | (allocs, tl) ->
      let hd = IntMap.find var_id allocs in
      (allocs, hd::tl)

  let release_slot slot_id ((allocs, tl): t) = (allocs, slot_id::tl)

  let get_allocations ((allocs, _): t) = allocs

  let get_allocation var_id ((allocs, _): t) = IntMap.find var_id allocs

  let get_allocation_int32 var_id allocs =
    Int32.of_int (get_allocation var_id allocs)

  let rec apply_allocations (allocs: t) (instr: Mashtree.instr) : Mashtree.instr =
    let apply_allocation_to_bind = function
      | MLocalBind(n) -> MLocalBind(get_allocation_int32 (Int32.to_int n) allocs)
      | _ as b -> b
    in
    let apply_allocation_to_imm = function
      | MImmBinding(b) -> MImmBinding(apply_allocation_to_bind b)
      | _ as i -> i
    in
    let apply_allocation_to_block = List.map (apply_allocations allocs) in
    match instr with
      | MImmediate(i) -> MImmediate(apply_allocation_to_imm i)
      | MTagOp(top, tt, i) -> MTagOp(top, tt, apply_allocation_to_imm i)
      | MArityOp(aop, at, i) -> MArityOp(aop, at, apply_allocation_to_imm i)
      | MPrim1(pop, i) -> MPrim1(pop, apply_allocation_to_imm i)
      | MTupleOp(top, i) -> MTupleOp(top, apply_allocation_to_imm i)
      | MBoxOp(bop, i) -> MBoxOp(bop, apply_allocation_to_imm i)
      | MArrayOp(aop, i) -> MArrayOp(aop, apply_allocation_to_imm i)
      | MRecordOp(rop, i) -> MRecordOp(rop, apply_allocation_to_imm i)
      | MAdtOp(aop, i) -> MAdtOp(aop, apply_allocation_to_imm i)
      | MCallKnown(i32, is) -> MCallKnown(i32, List.map apply_allocation_to_imm is)
      | MError(e, is) -> MError(e, List.map apply_allocation_to_imm is)
      | MCallIndirect(imm, is) -> MCallIndirect(apply_allocation_to_imm imm, List.map apply_allocation_to_imm is)
      | MIf(c, t, f) -> MIf(apply_allocation_to_imm c, apply_allocation_to_block t, apply_allocation_to_block f)
      | MWhile(b1, b2) -> MWhile(apply_allocation_to_block b1, apply_allocation_to_block b2)
      | MSwitch(v, bs, d) -> MSwitch(apply_allocation_to_imm v, List.map (fun (t, b) -> (t, apply_allocation_to_block b)) bs, apply_allocation_to_block d)
      | MPrim2(pop, i1, i2) -> MPrim2(pop, apply_allocation_to_imm i1, apply_allocation_to_imm i2)
      | MStore(bs) -> MStore(List.map (fun (b, bk) -> (apply_allocation_to_bind b, apply_allocations allocs bk)) bs)
      | MAllocate(x) -> MAllocate(x)
      | MDrop -> MDrop
      | MTracepoint(x) -> MTracepoint(x)
end

let run_register_allocation (instrs: Mashtree.instr list) =
  let uniq l =
    let rec tail_uniq a l =
      match l with
        | [] -> a
        | hd::tl -> tail_uniq (hd::a) (List.filter (fun x -> x != hd) tl) in
    tail_uniq [] l in
  let bind_live_local = function
    | MLocalBind(n) -> [Int32.to_int n]
    | _ -> []
  in
  let imm_live_local = function
    | MImmBinding(MLocalBind(n)) -> [Int32.to_int n]
    | _ -> []
  in
  let rec live_locals = function
    | MImmediate(imm)
    | MTagOp(_, _, imm)
    | MArityOp(_, _, imm)
    | MPrim1(_, imm)
    | MTupleOp(_, imm)
    | MBoxOp(_, imm)
    | MArrayOp(_, imm)
    | MRecordOp(_, imm)
    | MAdtOp(_, imm) -> imm_live_local imm
    | MCallKnown(_, is)
    | MError(_, is) -> List.concat (List.map imm_live_local is)
    | MCallIndirect(imm, is) -> (List.concat (List.map imm_live_local is)) @ (imm_live_local imm)
    | MIf(c, t, f) -> (imm_live_local c) @ (block_live_locals t) @ (block_live_locals f)
    | MWhile(b1, b2) -> (block_live_locals b1) @ (block_live_locals b2)
    | MSwitch(v, bs, d) -> (imm_live_local v) @ (List.concat (List.map (fun (_, b) -> block_live_locals b) bs)) @ (block_live_locals d)
    | MPrim2(_, i1, i2) -> (imm_live_local i1) @ (imm_live_local i2)
    | MStore(bs) -> List.concat (List.map (fun (b, bk) -> (bind_live_local b) @ (live_locals bk)) bs)
    | MAllocate _
    | MTracepoint _
    | MDrop -> []
  and block_live_locals b = List.concat (List.map live_locals b)
  in
  (** Mapping from (instruction index) -> (used locals) *)
  let instr_live_sets = List.mapi (fun i instr -> (i, uniq @@ live_locals instr)) instrs in
  let num_locals =
    let rec help acc = function
      | [] -> acc
      | hd::tl when hd > acc -> help hd tl
      | _::tl -> help acc tl in
    1 + (help 0 (List.map (fun (_, lst) -> help 0 lst) instr_live_sets)) in
  (* Printf.eprintf "Live sets:\n";
  List.iter (fun (i, items) -> Printf.eprintf "%d -> [%s]\n" i (BatString.join ", " (List.map string_of_int items))) instr_live_sets; *)
  if num_locals < 2 then
    instrs
  else begin
    (* flipped mapping of instr_live_sets; (used locals) -> (first_instruction_used, last_instruction_used) *)
    let live_intervals =
      let rec min = List.fold_left (fun acc cur -> if cur < acc then cur else acc) (List.length instrs) in
      let rec max = List.fold_left (fun acc cur -> if cur > acc then cur else acc) 0 in
      let rec help (acc: (int list) IntMap.t) = function
        | [] -> acc
        | (idx, items)::tl ->
        let with_items: int list IntMap.t = List.fold_left (fun acc cur ->
          let existing_elts: int list = BatOption.default [] (IntMap.find_opt cur acc) in
          IntMap.add cur (idx::existing_elts) acc) acc items in
        help with_items tl in
      let interval_map = help IntMap.empty instr_live_sets in
      List.sort (fun (_, (min1, _)) (_, (min2, _)) -> if min1 < min2 then -1 else if min1 == min2 then 0 else 1) @@
      List.map (fun (binding, indices) -> (binding, (min indices, max indices))) (IntMap.bindings interval_map) in
    (* Modified version of Linear-scan register allocation; we don't need to spill, since we are optimizing with an
       unbounded number of memory locations *)
    (* Printf.eprintf "Intervals:\n";
    List.iter (fun (v, (si, ei)) -> Printf.eprintf "%d: [%d,%d]\n" v si ei) live_intervals; *)
    let expire_old_intervals current_instr allocs active =
      let rec help (allocs, active) = function
        | [] -> (allocs, active)
        | (var_id, (_, max))::tl when max < current_instr -> help (RegisterAllocation.release_var var_id allocs, active) tl
        | hd::tl -> help (allocs, (hd::active)) tl in
      help (allocs, []) active in
    let (allocs, _) = BatList.fold_lefti (fun (allocs, active) i ((var_id, (start_inst, end_inst)) as cur) ->
      let (allocs, active) = expire_old_intervals start_inst allocs active in
      let allocs, _ = RegisterAllocation.allocate var_id allocs in
      let active = cur::active in
      (allocs, active)) (RegisterAllocation.initialize num_locals, []) live_intervals in
    (* Printf.eprintf "Allocations:\n";
    List.iter (fun (var_id, reg_id) -> Printf.eprintf "%d --> %d\n" var_id reg_id) (IntMap.bindings (RegisterAllocation.get_allocations allocs)); *)
    (* List.map (RegisterAllocation.apply_allocations allocs) instrs *)
    instrs
  end


let compile_const (c : Asttypes.constant) =
  match c with
  | Const_int i -> MConstI32 (Int32.of_int i)
  | Const_string _ -> failwith "NYI: compile_const string"
  | Const_float f_str -> failwith "NYI: compile_const float"
  | Const_int32 i32 -> MConstI32 i32
  | Const_int64 i64 -> MConstI64 i64
  | Const_bool b when b = true -> const_true
  | Const_bool _ -> const_false
  | Const_void -> const_void

let compile_imm env (i : imm_expression) =
  match i.imm_desc with
  | ImmConst c -> MImmConst(compile_const c)
  | ImmId id -> MImmBinding(find_id id env)


let compile_lambda env args body : Mashtree.closure_data =
  let used_var_set = Anf_utils.anf_free_vars body in
  let free_var_set = Ident.Set.diff used_var_set @@ Ident.Set.of_list args in
  let free_vars = Ident.Set.elements free_var_set in
  (* Bind all non-arguments in the function body to
     their respective closure slots *)
  let free_binds = BatList.fold_lefti (fun acc closure_idx var ->
      Ident.add var (MClosureBind(Int32.of_int closure_idx)) acc)
      Ident.empty free_vars in
  let closure_arg = Ident.create "$self" in
  let new_args = closure_arg::args in
  let arg_binds = BatList.fold_lefti (fun acc arg_idx arg ->
      Ident.add arg (MArgBind(Int32.of_int arg_idx)) acc)
      free_binds new_args in
  let idx = next_lift() in
  let arity = List.length new_args in
  let stack_size = Anf_utils.anf_count_vars body in
  let lam_env = {
    env with
    ce_binds=arg_binds;
    ce_stack_idx=0;
    ce_arity=arity;
  } in
  let worklist_item = {
    body=Anf body;
    env=lam_env;
    idx;
    arity;
    stack_size;
  } in
  worklist_enqueue worklist_item;
  {
    func_idx=(Int32.of_int idx);
    arity=(Int32.of_int arity);
    (* These variables should be in scope when the lambda is constructed. *)
    variables=(List.map (fun id -> MImmBinding(find_id id env)) free_vars);
  }

let compile_wrapper env real_idx arity : Mashtree.closure_data =
  let body = [
    MCallKnown(Int32.of_int real_idx, BatList.init arity (fun i -> MImmBinding(MArgBind(Int32.of_int (i + 1)))));
  ] in
  let idx = next_lift() in
  let lam_env = {
    env with
    ce_binds=Ident.empty;
    ce_stack_idx=0;
    ce_arity=arity + 1;
  } in
  let worklist_item = {
    body=Precompiled body;
    env=lam_env;
    idx;
    arity=arity + 1;
    stack_size=0;
  } in
  worklist_enqueue worklist_item;
  {
    func_idx=(Int32.of_int idx);
    arity=(Int32.of_int (arity + 1));
    variables=[];
  }

let next_global id =
  let ret, idx = next_global id in
  if ret <> ((!global_index) - 1) then
    ret
  else begin
    let body = [
      MImmediate(MImmBinding(MGlobalBind (Int32.of_int ret)));
    ] in
    let worklist_item = {
      body=Precompiled body;
      env=initial_compilation_env;
      idx;
      arity=0; (* <- this function cannot be called by the user, so no self argument is needed. *)
      stack_size=0;
    } in
    worklist_enqueue worklist_item;
    ret
  end

let rec compile_comp env c =
  match c.comp_desc with
  | CSwitch(arg, branches) ->
    let compiled_arg = compile_imm env arg in
    MSwitch(compiled_arg,
            List.map (fun (lbl, body) ->
                (Int32.of_int lbl, compile_anf_expr env body)) branches,
            [MError(Runtime_errors.SwitchError, [compiled_arg])])
  | CIf(cond, thn, els) ->
    MIf(compile_imm env cond, compile_anf_expr env thn, compile_anf_expr env els)
  | CWhile(cond, body) ->
    MWhile(compile_anf_expr env cond, compile_anf_expr env body)
  | CPrim1(Box, arg) ->
    MAllocate(MBox(compile_imm env arg))
  | CPrim1(Unbox, arg) ->
    MBoxOp(MBoxUnbox, compile_imm env arg)
  | CPrim1(p1, arg) ->
    MPrim1(p1, compile_imm env arg)
  | CPrim2(p2, arg1, arg2) ->
    MPrim2(p2, compile_imm env arg1, compile_imm env arg2)
  | CAssign(arg1, arg2) ->
    MBoxOp(MBoxUpdate(compile_imm env arg2), compile_imm env arg1)
  | CTuple(args) ->
    MAllocate(MTuple (List.map (compile_imm env) args))
  | CArray(args) ->
    MAllocate(MArray (List.map (compile_imm env) args))
  | CArrayGet(idx, arr) ->
    MArrayOp(MArrayGet(compile_imm env idx), compile_imm env arr)
  | CArraySet(idx, arr, arg) ->
    MArrayOp(MArraySet(compile_imm env idx, compile_imm env arg), compile_imm env arr)
  | CRecord(ttag, args) ->
    MAllocate(MRecord (compile_imm env ttag, List.map (fun ({txt=name}, arg) -> name, (compile_imm env arg)) args))
  | CAdt(ttag, vtag, args) ->
    MAllocate(MADT(compile_imm env ttag, compile_imm env vtag, List.map (compile_imm env) args))
  | CString(s) ->
    MAllocate(MString s)
  | CInt32(i) ->
    MAllocate(MInt32 i)
  | CInt64(i) ->
    MAllocate(MInt64 i)
  | CGetTupleItem(idx, tup) ->
    MTupleOp(MTupleGet(idx), compile_imm env tup)
  | CSetTupleItem(idx, tup, value) ->
    MTupleOp(MTupleSet(idx, compile_imm env value), compile_imm env tup)
  | CGetAdtItem(idx, adt) ->
    MAdtOp(MAdtGet(idx), compile_imm env adt)
  | CGetAdtTag(adt) ->
    MAdtOp(MAdtGetTag, compile_imm env adt)
  | CGetRecordItem(idx, record) ->
    MRecordOp(MRecordGet(idx), compile_imm env record)
  | CLambda(args, body) ->
    MAllocate(MClosure(compile_lambda env args body))
  | CApp(f, args) ->
    (* TODO: Utilize MCallKnown *)
    MCallIndirect(compile_imm env f, List.map (compile_imm env) args)
  | CAppBuiltin(modname, name, args) ->
    let builtin_idx = Int32.zero in
    MCallKnown(builtin_idx, List.map (compile_imm env) args)
  | CImmExpr(i) -> MImmediate(compile_imm env i)

and compile_anf_expr env a =
  match a.anf_desc with
  | AESeq(hd, tl) -> (compile_comp env hd)::MDrop::(compile_anf_expr env tl)
  | AELet(global, recflag, binds, body) ->
    let get_loc idx (id, _) =
      match global with
      | Global -> MGlobalBind(Int32.of_int (next_global id))
      | Nonglobal -> MLocalBind(Int32.of_int (env.ce_stack_idx + idx)) in
    let locations = List.mapi get_loc binds in
    let new_env = BatList.fold_left2 (fun acc new_loc (id, _) ->
        {acc with ce_binds=Ident.add id new_loc acc.ce_binds; ce_stack_idx=acc.ce_stack_idx + 1}) (* FIXME: Why is this not ce_stack_idx + (List.length binds)?? *)
        env locations binds in
    begin match recflag with
      | Nonrecursive ->
        BatList.fold_right2 (fun loc (_, rhs) acc ->
            (MStore [loc, (compile_comp env rhs)]) :: acc)
          locations binds (compile_anf_expr new_env body)
      | Recursive ->
        let binds = BatList.fold_left2 (fun acc loc (_, rhs) ->
            (loc, (compile_comp new_env rhs)) :: acc)
            [] locations binds in
        MStore(List.rev binds) :: (compile_anf_expr new_env body)
    end
  | AEComp(c) -> [compile_comp env c]


let compile_worklist_elt ({body; env} : worklist_elt) =
  match body with
  | Anf body ->
    compile_anf_expr env body
  | Precompiled block -> block


let fold_left_pop f base =
  let rec help acc =
    if worklist_empty() then
      acc
    else
      help (f acc (worklist_pop())) in
  help base


let compile_remaining_worklist () =
  let compile_one funcs ((({idx=index; arity; stack_size}) as cur) : worklist_elt) =
    let body = compile_worklist_elt cur in
    let func = {
      index=Int32.of_int index;
      arity=Int32.of_int arity;
      body;
      stack_size;
    } in
    func::funcs in
  List.rev (fold_left_pop compile_one [])


let lift_imports env imports =
  let process_shape = function
    | GlobalShape -> MGlobalImport I32Type
    | FunctionShape(inputs, outputs) ->
      MFuncImport
        ((BatList.init inputs (fun _ -> I32Type)),
         (BatList.init outputs (fun _ -> I32Type)))
  in
  let import_idx = ref 0 in

  let process_import (imports, setups, env) {imp_use_id; imp_desc; imp_shape} =
    let glob = next_global imp_use_id in
    let import_idx = begin
      let i = !import_idx in
      import_idx := i + 1;
      i
      end in
    match imp_desc with
    | GrainValue(mod_, name) ->
      ({
        mimp_mod = Ident.create mod_;
        mimp_name = Ident.create name;
        mimp_type = MGlobalImport I32Type (*process_shape imp_shape*);
        mimp_kind = MImportGrain;
        mimp_setup = MCallGetter;
      }::imports),
      ([
        MStore([(MGlobalBind (Int32.of_int glob)),
                MCallKnown((Int32.of_int import_idx), [])]);
      ]::setups),
      ({env with ce_binds=Ident.add imp_use_id (MGlobalBind(Int32.of_int glob)) env.ce_binds})
    | WasmFunction(mod_, name) ->
      ({
        mimp_mod = Ident.create mod_;
        mimp_name = Ident.create name;
        mimp_type = process_shape imp_shape;
        mimp_kind = MImportWasm;
        mimp_setup = MWrap(Int32.zero);
      }::imports),
      (begin
        match imp_shape with
        | GlobalShape -> []
        | FunctionShape(inputs, outputs) ->
          if outputs > 1 then
            failwith "NYI: Multi-result wrapper"
          else
            [MStore([MGlobalBind(Int32.of_int glob),
                     MAllocate(MClosure (compile_wrapper env import_idx inputs))])]
      end::setups),
      ({env with ce_binds=Ident.add imp_use_id (MGlobalBind(Int32.of_int glob)) env.ce_binds})
    | JSFunction _ -> failwith "NYI: lift_imports JSFunction"
  in
  let imports, setups, env = List.fold_left process_import ([], [], env) imports in
  let imports = List.rev imports in
  let setups = List.flatten (List.rev setups) in
  imports, setups, env

let transl_anf_program (anf_prog : Anftree.anf_program) : Mashtree.mash_program =
  reset_lift();
  reset_global();
  worklist_reset();

  let imports, setups, env = lift_imports initial_compilation_env anf_prog.imports in
  let main_body_stack_size = Anf_utils.anf_count_vars anf_prog.body in
  let main_body = run_register_allocation @@ setups @ (compile_anf_expr env anf_prog.body) in
  let exports = global_exports() in
  let functions = List.map (fun (({body} as f): Mashtree.mash_function) -> {f with body=run_register_allocation body}) (compile_remaining_worklist()) in

  {
    functions;
    imports;
    exports;
    main_body;
    main_body_stack_size;
    num_globals=(!global_index);
    signature=anf_prog.signature;
  }
