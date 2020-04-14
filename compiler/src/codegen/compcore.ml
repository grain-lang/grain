open Grain_typed
open Mashtree
open Value_tags
open Wasm
open Computils
open Compprim
open Concatlist (* NOTE: This import shadows (@) and introduces (@+) and (+@) *)

let compile_tuple_op env tup_imm op =
  let tup = compile_imm env tup_imm in
  match op with
  | MTupleGet(idx) ->
    let idx_int = Int32.to_int idx in
    (* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. *)
    tup @ (untag TupleTagType) +@ [
        load ~offset:(4 * (idx_int + 1)) ();
      ]
  | MTupleSet(idx, imm) ->
    let idx_int = Int32.to_int idx in
    tup @ (untag TupleTagType) @ (compile_imm env imm) +@ [
        store ~offset:(4 * (idx_int + 1)) ();
      ] @ (compile_imm env imm)


let compile_adt_op env adt_imm op =
  let adt = compile_imm env adt_imm in
  match op with
  | MAdtGet(idx) ->
    let idx_int = Int32.to_int idx in 
    adt @ (untag (GenericHeapType (Some ADTType))) +@ [
      load ~offset:(4 * (idx_int + 5)) ();
    ]
  | MAdtGetModule ->
    adt @ (untag (GenericHeapType (Some ADTType))) +@ [
      load ~offset:4 ();
    ]
  | MAdtGetTag ->
    adt @ (untag (GenericHeapType (Some ADTType))) +@ [
      load ~offset:12 ();
    ]


let compile_record_op env rec_imm op =
  let record = compile_imm env rec_imm in
  match op with
  | MRecordGet(idx) ->
    let idx_int = Int32.to_int idx in
    record @ (untag (GenericHeapType (Some RecordType))) +@ [
        load ~offset:(4 * (idx_int + 4)) ();
      ]

let round_up (num : int) (multiple : int) : int =
  num - (num mod multiple) + multiple

let buf_to_ints (buf : Buffer.t) : int64 list =
  let num_bytes = Buffer.length buf in
  let num_ints = round_up num_bytes 8 in
  let out_ints = ref [] in

  let byte_buf = Bytes.create (num_ints * 8) in
  Buffer.blit buf 0 byte_buf 0 num_bytes;
  let bytes_to_int = if Sys.big_endian then
      Stdint.Uint64.of_bytes_big_endian
    else
      Stdint.Uint64.of_bytes_little_endian in
  for i = 0 to (num_ints - 1) do
    out_ints := (bytes_to_int byte_buf (i * 8))::!out_ints
  done;
  List.rev @@ List.map Stdint.Uint64.to_int64 !out_ints

let call_lambda env func args =
  let compiled_func = compile_imm env func in
  let compiled_args = Concatlist.flatten @@ List.map (compile_imm env) args in
  let ftype = add_dummy_loc @@ Int32.of_int (get_arity_func_type_idx env (1 + List.length args)) in
  compiled_func @
  untag LambdaTagType @
  compiled_args @
  compiled_func @
  untag LambdaTagType +@ [
    load ~offset:4 ();
    Ast.CallIndirect(ftype);
  ]

let allocate_string env str =
  let str_as_bytes = Bytes.of_string str in
  let num_bytes = Bytes.length str_as_bytes in
  let num_ints = round_up num_bytes 8 in
  let buf = Buffer.create num_ints in
  BatUTF8.iter (BatUTF8.Buf.add_char buf) str;

  let ints_to_push : int64 list = buf_to_ints buf in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let preamble = Concatlist.t_of_list [
      Ast.Const(const_int32 @@ (4 * (2 + (2 * (List.length ints_to_push)))));
      call_malloc env;
    ] @ tee_swap +@ [
      Ast.Const(const_int32 @@ String.length str);
    ] @ get_swap +@ [
      Ast.Const(const_int32 (tag_val_of_heap_tag_type StringType));
      store ~offset:0 ();
      store ~offset:4 ();
    ] in
  let elts = List.flatten @@ List.mapi (fun idx (i : int64) ->
      Concatlist.list_of_t
        (get_swap +@
         [
           Ast.Const(add_dummy_loc (Values.I64 i));
           store ~ty:Types.I64Type ~offset:(8 * (idx + 1)) ();
         ])) ints_to_push in
  preamble +@ elts @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType(Some StringType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_closure env ?lambda ({func_idx; arity; variables} as closure_data) =
  let num_free_vars = List.length variables in
  let closure_size = num_free_vars + 3 in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let access_lambda = Option.default (get_swap @+ [
      Ast.Const(const_int32 @@ 4 * round_allocation_size closure_size);
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]) lambda in
  env.backpatches := (access_lambda, closure_data)::!(env.backpatches);
  (heap_allocate env closure_size) @ tee_swap +@ [
    Ast.Const(const_int32 num_free_vars);
  ] @ get_swap +@ [
    Ast.GlobalGet(var_of_ext_global env runtime_mod reloc_base);
    Ast.Const(wrap_int32 (Int32.(add func_idx (of_int env.func_offset))));
    Ast.Binary(Values.I32 Ast.IntOp.Add);
  ] @ get_swap +@ [
    Ast.Const(add_dummy_loc (Values.I32Value.to_value arity));

    store ~offset:0 ();
    store ~offset:4 ();
    store ~offset:8 ();
  ] @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_adt env ttag vtag elts =
  (* Heap memory layout of ADT types:
    [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
   *)
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) +@ [
      store ~offset:(4 * (idx + 5)) ();
    ] in

  (heap_allocate env (num_elts + 5)) @ tee_swap +@ [
    Ast.Const(const_int32 (tag_val_of_heap_tag_type ADTType));
    store ~offset:0 ();
  ] @ get_swap +@ [
    Ast.GlobalGet(var_of_ext_global env runtime_mod module_runtime_id);
    (* Tag the runtime id *)
    Ast.Const(const_int32 2);
    Ast.Binary(Values.I32 Ast.IntOp.Mul);
    store ~offset:4 ();
  ] @ get_swap @ (compile_imm env ttag) +@ [
    store ~offset:8 ();
  ] @ get_swap @ (compile_imm env vtag) +@ [
    store ~offset:12 ();
  ] @ get_swap +@ [
    Ast.Const(const_int32 num_elts);
    store ~offset:16 ();
  ] @ (Concatlist.flatten @@ List.mapi compile_elt elts) @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType (Some ADTType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_tuple env elts =
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) +@ [
      store ~offset:(4 * (idx + 1)) ();
    ] in

  (heap_allocate env (num_elts + 1)) @ tee_swap +@ [
    Ast.Const(const_int32 num_elts);
    store ~offset:0 ();
  ] @ (Concatlist.flatten @@ List.mapi compile_elt elts) @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type TupleTagType);
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_record env ttag elts =
  let _, elts = List.split elts in
  (* Heap memory layout of records:
    [ <value type tag>, <module_tag>, <type_tag>, <arity> ordered elts ... ]
   *)
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) +@ [
      store ~offset:(4 * (idx + 4)) ();
    ] in

  (heap_allocate env (num_elts + 4)) @ tee_swap +@ [
    Ast.Const(const_int32 (tag_val_of_heap_tag_type RecordType));
    store ~offset:0 ();
  ] @ get_swap +@ [
    Ast.GlobalGet(var_of_ext_global env runtime_mod module_runtime_id);
    (* Tag the runtime id *)
    Ast.Const(const_int32 2);
    Ast.Binary(Values.I32 Ast.IntOp.Mul);
    store ~offset:4 ();
  ] @ get_swap @ (compile_imm env ttag) +@ [
    store ~offset:8 ();
  ] @ get_swap +@ [
    Ast.Const(const_int32 num_elts);
    store ~offset:12 ();
  ] @ (Concatlist.flatten @@ List.mapi compile_elt elts) @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType (Some RecordType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let compile_allocation env alloc_type =
  match alloc_type with
  | MClosure(cdata) -> allocate_closure env cdata
  | MTuple(elts) -> allocate_tuple env elts
  | MArray(elts) -> allocate_array env elts
  | MRecord(ttag, elts) -> allocate_record env ttag elts
  | MString(str) -> allocate_string env str
  | MADT(ttag, vtag, elts) -> allocate_adt env ttag vtag elts


let collect_backpatches env f =
  let nested_backpatches = ref [] in
  let res = f {env with backpatches=nested_backpatches} in
  res, !nested_backpatches

let do_backpatches env backpatches =
  let do_backpatch (lam, {variables}) =
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    let preamble = lam +@ [
        Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
        Ast.Binary(Values.I32 Ast.IntOp.Xor);
      ] @ set_swap in
    let backpatch_var idx var =
      get_swap @ (compile_imm env var) +@ [
          store ~offset:(4 * (idx + 3)) ();
        ] in
    preamble @
    (Concatlist.flatten @@ List.mapi backpatch_var variables) in
  (Concatlist.flatten @@ List.map do_backpatch backpatches)

let rec compile_store env binds =
  let process_binds env =
    let process_bind (b, instr) acc =
      let store_bind = compile_bind ~action:BindSet env b in
      let get_bind = compile_bind ~action:BindGet env b in
      let compiled_instr = match instr with
        | MAllocate(MClosure(cdata)) ->
          allocate_closure env ~lambda:get_bind cdata
        | _ -> compile_instr env instr in
      (compiled_instr @ store_bind) @ acc in
    List.fold_right process_bind binds empty in
  let instrs, backpatches = collect_backpatches env process_binds in
  instrs @ (do_backpatches env backpatches)

and compile_switch env arg branches default =
  (* Constructs the jump table. Assumes that branch 0 is the default *)
  let create_table ?default:(default=0) ?offset:(offset=0) stack =
    let max_label = List.fold_left max 0 stack in
    let label_blocks = (List.mapi (fun i l -> (i + offset, l)) stack
                        |> List.sort (fun (_, l1) (_, l2) -> compare l1 l2)) in
    let default_const = add_dummy_loc @@ Int32.of_int default in
    let matching i (block, lbl) = lbl = i in
    let get_slot i =
      match List.find_opt (matching i) label_blocks with
      | None ->
        default_const
      | Some((b, _)) ->
        add_dummy_loc @@ Int32.of_int b in
    BatList.init (max_label + 1) get_slot in
  let rec process_branches count stack bs =
    match bs with
    | [] ->
      let inner_block_body = (singleton (Ast.Const(const_int32 0))) @
                             (compile_imm env arg) @
                             (* Tag check elided *)
                             untag_number +@
                             [
                               Ast.BrTable((create_table ~offset:1 stack),
                                           (add_dummy_loc (Int32.of_int 0)));
                             ] in
      let default_block_body = compile_block env default in
      Concatlist.t_of_list
              [Ast.Block([Types.I32Type],
                         Concatlist.mapped_list_of_t add_dummy_loc (
                           (singleton (Ast.Block([Types.I32Type],
                                                 Concatlist.mapped_list_of_t add_dummy_loc
                                                   inner_block_body
                                                ))) @
                           default_block_body
                           ))]
    | (lbl, hd)::tl ->
      Concatlist.t_of_list
        [Ast.Block([Types.I32Type],
                   Concatlist.mapped_list_of_t add_dummy_loc
                     ((process_branches (count + 1) ((Int32.to_int lbl)::stack) tl) @
                      (compile_block env hd) +@
                      [
                        Ast.Br(add_dummy_loc @@ Int32.of_int count)
                      ]))] in
  let processed = process_branches 0 [] branches in
  singleton (Ast.Block([Types.I32Type],
                      Concatlist.mapped_list_of_t add_dummy_loc processed))


and compile_block env block =
  Concatlist.flatten (List.map (compile_instr env) block)

and compile_instr env instr =
  match instr with
  | MDrop -> singleton Ast.Drop
  | MImmediate(imm) -> compile_imm env imm
  | MAllocate(alloc) -> compile_allocation env alloc
  | MTupleOp(tuple_op, tup) -> compile_tuple_op env tup tuple_op
  | MArrayOp(array_op, ret) -> compile_array_op env ret array_op
  | MAdtOp(adt_op, adt) -> compile_adt_op env adt adt_op
  | MRecordOp(record_op, record) -> compile_record_op env record record_op
  | MPrim1(p1, arg) -> compile_prim1 env p1 arg
  | MPrim2(p2, arg1, arg2) -> compile_prim2 env p2 arg1 arg2
  | MSwitch(arg, branches, default) -> compile_switch env arg branches default
  | MStore(binds) -> compile_store env binds

  | MCallIndirect(func, args) ->
    call_lambda env func args

  | MIf(cond, thn, els) ->
    let compiled_cond = compile_imm env cond in
    let compiled_thn = Concatlist.mapped_list_of_t
        add_dummy_loc
        (compile_block env thn) in
    let compiled_els = Concatlist.mapped_list_of_t
        add_dummy_loc
        (compile_block env els) in
    compiled_cond @
    decode_bool +@ [
      Ast.If([Types.I32Type],
             compiled_thn,
             compiled_els);
    ]

  | MWhile(cond, body) ->
    let compiled_cond = (compile_block env cond) in
    let compiled_body = (compile_block env body) in
    singleton (Ast.Block([Types.I32Type],
               Concatlist.mapped_list_of_t add_dummy_loc @@ 
               singleton (Ast.Loop([Types.I32Type],
                          Concatlist.mapped_list_of_t add_dummy_loc @@
                          singleton (Ast.Const const_void) @
                          compiled_cond @
                          decode_bool +@
                          [Ast.Test(Values.I32 Ast.IntOp.Eqz); 
                           Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)] +@
                          [Ast.Drop] @
                          compiled_body +@
                          [Ast.Br (add_dummy_loc @@ Int32.of_int 0)]))))

  | MError(err, args) ->
    call_error_handler env err args
  | MCallKnown(func_idx, args) ->
    let compiled_args = Concatlist.flatten @@ List.map (compile_imm env) args in
    compiled_args +@ [
      Ast.Call(add_dummy_loc (Int32.of_int (env.import_func_offset + (Int32.to_int func_idx))));
    ]
  | MArityOp _ -> failwith "NYI: (compile_instr): MArityOp"
  | MTagOp _ -> failwith "NYI: (compile_instr): MTagOp"


let compile_function env {index; arity; stack_size; body=body_instrs} =
  let arity_int = Int32.to_int arity in
  let body_env = {env with num_args=arity_int} in
  let body = Concatlist.mapped_list_of_t add_dummy_loc @@
    (compile_block body_env body_instrs) +@ [Ast.Return] in
  let open Wasm.Ast in
  let ftype_idx = get_arity_func_type_idx env arity_int in
  let ftype = add_dummy_loc Int32.(of_int ftype_idx) in
  let locals = List.append swap_slots @@ BatList.init (stack_size) (fun n -> Types.I32Type) in
  add_dummy_loc {
    ftype;
    locals;
    body;
  }

let compute_table_size env {imports; exports; functions} =
  (List.length functions) + ((List.length imports) - (List.length runtime_global_imports)) + 2

let compile_imports env ({imports} as prog) =
  let compile_asm_type t =
    match t with
    | I32Type -> Types.I32Type
    | I64Type -> Types.I64Type
    | F32Type -> Types.F32Type
    | F64Type -> Types.F64Type
  in

  let compile_module_name name = function
    | MImportWasm -> Ident.name name
    | MImportGrain -> "GRAIN$MODULE$" ^ (Ident.name name)
  in

  let compile_import_name name = function
    | MImportWasm -> Ident.name name
    | MImportGrain -> "GRAIN$EXPORT$GET$" ^ (Ident.name name)
  in

  let compile_import {mimp_mod; mimp_name; mimp_type; mimp_kind} =
    (* TODO: When user import become a thing, we'll need to worry about hygiene *)
    let module_name = encode_string @@ compile_module_name mimp_mod mimp_kind in
    let item_name = encode_string @@ compile_import_name mimp_name mimp_kind in
    let idesc = match mimp_kind, mimp_type with
      | MImportGrain, MGlobalImport typ ->
        let typ = compile_asm_type typ in
        let func_type = Types.FuncType([], [typ]) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
      | _, MFuncImport(args, ret) ->
        let proc_list = List.map compile_asm_type in
        let func_type = Types.FuncType(proc_list args, proc_list ret) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
      | _, MGlobalImport typ ->
        let typ = compile_asm_type typ in
        let imptyp = Types.GlobalType(typ, Types.Immutable) in
        add_dummy_loc @@ Ast.GlobalImport(imptyp)
    in
    let open Wasm.Ast in
    add_dummy_loc {
      module_name;
      item_name;
      idesc;
    } in
  let table_size = compute_table_size env prog in
  let imports = List.map compile_import imports in
  (List.append
    imports
    [
      add_dummy_loc {
        Ast.module_name=encode_string (Ident.name runtime_mod);
        Ast.item_name=encode_string "mem";
        Ast.idesc=add_dummy_loc (Ast.MemoryImport (Types.MemoryType({
            Types.min=Int32.zero;
            Types.max=None;
          })));
      };
      add_dummy_loc {
        Ast.module_name=encode_string (Ident.name runtime_mod);
        Ast.item_name=encode_string "tbl";
        Ast.idesc=add_dummy_loc (Ast.TableImport (Types.TableType({
            Types.min=Int32.of_int table_size;
            Types.max=None;
          }, Types.FuncRefType)));
      };
    ])

let compile_exports env {functions; imports; exports; num_globals} =
  let compile_getter i {ex_name; ex_global_index; ex_getter_index} =
    let exported_name = "GRAIN$EXPORT$GET$" ^ (Ident.name ex_name) in
    let name = encode_string exported_name in
    let fidx = (Int32.to_int ex_getter_index) + env.func_offset in
    let export =
      let open Wasm.Ast in
      add_dummy_loc {
        name;
        edesc=add_dummy_loc (Ast.FuncExport (add_dummy_loc @@ Int32.of_int fidx));
      } in
    export
  in
  let compile_lambda_export i _ =
    let name = encode_string ("GRAIN$LAM_" ^ (string_of_int i)) in
    let edesc = add_dummy_loc (Ast.FuncExport(add_dummy_loc @@ Int32.of_int (i + env.func_offset))) in
    let open Wasm.Ast in
    add_dummy_loc { name; edesc } in
  let heap_adjust_idx = env.func_offset + (List.length functions) in
  let main_idx = heap_adjust_idx + 1 in
  let heap_adjust_idx = add_dummy_loc @@ Int32.of_int heap_adjust_idx in
  let main_idx = add_dummy_loc @@ Int32.of_int main_idx in
  let compiled_lambda_exports = List.mapi compile_lambda_export functions in
  let compiled_exports = List.mapi compile_getter exports in
  (List.append
     compiled_lambda_exports
     (List.append
        compiled_exports
        [
          add_dummy_loc {
            Ast.name=encode_string "GRAIN$HEAP_ADJUST";
            Ast.edesc=add_dummy_loc (Ast.FuncExport heap_adjust_idx);
          };
          add_dummy_loc {
            Ast.name=encode_string "GRAIN$MAIN";
            Ast.edesc=add_dummy_loc (Ast.FuncExport main_idx);
          };
          add_dummy_loc {
            Ast.name=encode_string (Ident.name table_size);
            (* We add one here because of heap top *)
            Ast.edesc=add_dummy_loc (Ast.GlobalExport (add_dummy_loc @@ Int32.of_int (num_globals + 1 + (List.length runtime_global_imports))));
          };
          add_dummy_loc {
            Ast.name=encode_string "memory";
            Ast.edesc=add_dummy_loc (Ast.MemoryExport (add_dummy_loc @@ Int32.of_int 0));
          };
        ]))

let compile_tables env prog =
  (*let table_size = compute_table_size env prog in*)
  [
    (*add_dummy_loc {
      Ast.ttype=Types.TableType({
          Types.min=Int32.of_int table_size;
          Types.max=None;
        }, Types.FuncRefType)
    };*)
  ]


let compile_elems env prog =
  let table_size = compute_table_size env prog in
  let open Wasm.Ast in
  [
    add_dummy_loc {
      index=add_dummy_loc (Int32.zero);
      offset=add_dummy_loc [
        add_dummy_loc (Ast.GlobalGet(add_dummy_loc @@ Int32.of_int 0));
      ];
      init=BatList.init table_size (fun n -> (add_dummy_loc (Int32.of_int n)));
    };
  ]

let compile_globals env ({num_globals} as prog) =
  List.append
    (BatList.init (1 + num_globals) (fun _ -> add_dummy_loc {
         Ast.gtype=Types.GlobalType(Types.I32Type, Types.Mutable);
         Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 0)]);
       }))
    [
      add_dummy_loc {
        Ast.gtype=Types.GlobalType(Types.I32Type, Types.Immutable);
        Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 (compute_table_size env prog))]);
      }
    ]


let heap_adjust env = add_dummy_loc {
  Ast.ftype = add_dummy_loc Int32.(of_int (get_func_type_idx env (Types.FuncType([Types.I32Type], [Types.I32Type]))));
  Ast.locals = [];
  Ast.body = List.map add_dummy_loc [
      (*Ast.LocalGet(add_dummy_loc @@ Int32.of_int 0);
      call_runtime_check_memory env;*)
      Ast.GlobalGet(env.heap_top);
      Ast.LocalGet(add_dummy_loc @@ Int32.of_int 0);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      Ast.GlobalSet(env.heap_top);
      Ast.GlobalGet(env.heap_top);
    ]
}

let compile_main env prog =
  compile_function env
    {
      index=Int32.of_int (-99);
      arity=Int32.zero;
      body=prog.main_body;
      stack_size=prog.main_body_stack_size;
    }

let compile_functions env ({functions} as prog) =
  let compiled_funcs = List.map (compile_function env) functions in
  let heap_adjust = heap_adjust env in
  let main = compile_main env prog in
  List.append
    compiled_funcs
    [
      heap_adjust;
      main;
    ]

let reparse_module (module_ : Wasm.Ast.module_) =
  let open Wasm.Source in
  let as_str = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ module_) in
  let {it=script} = Wasm.Parse.string_to_module as_str in
  match script with
  | Wasm.Script.Textual(m) -> m
  | Encoded _ -> failwith "Internal error: reparse_module: Returned Encoded (should be impossible)"
  | Quoted _ -> failwith "Internal error: reparse_module: Returned Quoted (should be impossible)"

let validate_module (module_ : Wasm.Ast.module_) =
  try
    Valid.check_module module_
  with
  | Wasm.Valid.Invalid(region, msg) ->
    (* Re-parse module in order to get actual locations *)
    let reparsed = reparse_module module_ in
    (try
       Valid.check_module reparsed
     with
     | Wasm.Valid.Invalid(region, msg) ->
       raise (WasmRunnerError(region, msg, reparsed)));
    raise (WasmRunnerError(region, Printf.sprintf "WARNING: Did not re-raise after reparse: %s" msg, module_))


let prepare env ({imports} as prog) =
  let process_import ?dynamic_offset:(dynamic_offset=0) ?is_runtime_import:(is_runtime_import=false)
      (acc_env) idx {mimp_mod; mimp_name; mimp_type; mimp_kind} =
    let rt_idx = if is_runtime_import then idx + dynamic_offset else idx in
    let register tbl =
      let tbl = begin
        match Ident.find_same_opt mimp_mod tbl with
        | None -> Ident.add mimp_mod Ident.empty tbl
        | Some _ -> tbl
      end in
      Ident.add mimp_mod (Ident.add mimp_name (Int32.of_int rt_idx) (Ident.find_same mimp_mod tbl)) tbl
    in
    let imported_funcs, imported_globals = begin
      match mimp_type with
      | MFuncImport _ ->
        (register acc_env.imported_funcs), acc_env.imported_globals
      | MGlobalImport _ ->
        acc_env.imported_funcs, (register acc_env.imported_globals)
    end in
    {acc_env with imported_funcs; imported_globals} in
  let import_offset = List.length runtime_imports in
  let import_func_offset = List.length runtime_function_imports in
  let import_global_offset = import_offset + (List.length imports) in

  let new_imports = List.append runtime_imports imports in
  let new_env = BatList.fold_lefti (process_import ~is_runtime_import:true) env runtime_global_imports in
  let new_env = BatList.fold_lefti (process_import ~is_runtime_import:true) new_env runtime_function_imports in
  let new_env = BatList.fold_lefti (process_import ~dynamic_offset:import_global_offset) new_env imports in
  let global_offset = import_global_offset in
  let func_offset = global_offset - (List.length runtime_global_imports) in
  {
    new_env with
    import_offset;
    import_func_offset;
    import_global_offset;
    global_offset;
    func_offset;
  }, {
    prog with
    imports=new_imports;
    num_globals=prog.num_globals + (List.length new_imports) + (List.length imports);
  }


let compile_wasm_module ?env prog =
  let open Wasm.Ast in
  let env = match env with
    | None -> init_codegen_env()
    | Some(e) -> e in
  let env, prog = prepare env prog in
  let funcs = compile_functions env prog in
  let imports = compile_imports env prog in
  let exports = compile_exports env prog in
  let globals = compile_globals env prog in
  let tables = compile_tables env prog in
  let elems = compile_elems env prog in
  let types = List.map add_dummy_loc (BatDeque.to_list !(env.func_types)) in
  let ret = add_dummy_loc {
    empty_module with
    funcs;
    imports;
    exports;
    globals;
    tables;
    elems;
    types;
    start=None;
  } in
  validate_module ret;
  ret

let module_to_string compiled_module =
  (* Print module to string *)
  Wasm.Sexpr.to_string 80 @@ Wasm.Arrange.module_ compiled_module


let () =
  Printexc.register_printer (fun exc ->
      match exc with
      | WasmRunnerError(region, str, module_) ->
        let fmt_module _ m = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m) in
        let s = Printf.sprintf "WASM Runner Exception at %s: '%s'\n%a\n"
          (Wasm.Source.string_of_region region) str
          fmt_module module_ in
        Some(s)
      | _ -> None)

