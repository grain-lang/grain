open Grain_typed
open Wasm
open Mashtree
open Value_tags
open Computils
open Concatlist (* NOTE: This import shadows (@) and introduces (@+) and (+@) *)

let compile_array_op env arr_imm op =
  let arr = compile_imm env arr_imm in
  match op with
  | MArrayGet(idx_imm) ->
    let idx = compile_imm env idx_imm in
    (* Check that the index is in bounds *)
    arr @ (untag (GenericHeapType(Some ArrayType))) +@ [load ~offset:4 ()] @
    arr @ (untag (GenericHeapType(Some ArrayType))) +@ [load ~offset:4 ()] +@ [
      Ast.Const(const_int32 (-2));
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
    ] @ idx +@ [
      Ast.Compare(Values.I32 Ast.IntOp.GtS);
      error_if_true env ArrayIndexOutOfBounds [];
      Ast.Const(const_int32 2);
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
    ] @ idx +@ [
      Ast.Compare(Values.I32 Ast.IntOp.LeS);
      error_if_true env ArrayIndexOutOfBounds [];
    (* Resolve a negative index *)
    ] @ idx +@ [
      Ast.Const(const_int32 0);
      Ast.Compare(Values.I32 Ast.IntOp.LtS);
      Ast.If([Types.I32Type],
        Concatlist.mapped_list_of_t add_dummy_loc @@
        idx +@ [
          Ast.Const(const_int32 1);
          Ast.Binary(Values.I32 Ast.IntOp.ShrS);
        ] @ arr @ (untag (GenericHeapType(Some ArrayType))) +@ [
          load ~offset:4 ();
          (Ast.Binary(Values.I32 Ast.IntOp.Add))
        ],
        Concatlist.mapped_list_of_t add_dummy_loc @@
        idx +@ [
          Ast.Const(const_int32 1);
          Ast.Binary(Values.I32 Ast.IntOp.ShrS);
        ]
      );
      (* Get the item *)
      Ast.Const(const_int32 4);
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
    ] @ arr @ (untag (GenericHeapType(Some ArrayType))) +@ [
      Ast.Binary(Values.I32 Ast.IntOp.Add);
      load ~offset:8 ();
    ]
  | MArrayLength ->
    arr @ (untag (GenericHeapType(Some ArrayType))) +@ [
      load ~offset:4 ();
      Ast.Const(const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Shl);
    ]
  | MArraySet(idx_imm, val_imm) ->
    let idx = compile_imm env idx_imm in
    let val_ = compile_imm env val_imm in
    (* Check that the index is in bounds *)
    arr @ (untag (GenericHeapType(Some ArrayType))) +@ [load ~offset:4 ()] @
    arr @ (untag (GenericHeapType(Some ArrayType))) +@ [load ~offset:4 ()] +@ [
      Ast.Const(const_int32 (-2));
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
    ] @ idx +@ [
      Ast.Compare(Values.I32 Ast.IntOp.GtS);
      error_if_true env ArrayIndexOutOfBounds [];
      Ast.Const(const_int32 2);
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
    ] @ idx +@ [
      Ast.Compare(Values.I32 Ast.IntOp.LeS);
      error_if_true env ArrayIndexOutOfBounds [];
    (* Resolve a negative index *)
    ] @ idx +@ [
      Ast.Const(const_int32 0);
      Ast.Compare(Values.I32 Ast.IntOp.LtS);
      Ast.If([Types.I32Type],
        Concatlist.mapped_list_of_t add_dummy_loc @@
        idx +@ [
          Ast.Const(const_int32 1);
          Ast.Binary(Values.I32 Ast.IntOp.ShrS);
        ] @ arr @ (untag (GenericHeapType(Some ArrayType))) +@ [
          load ~offset:4 ();
          (Ast.Binary(Values.I32 Ast.IntOp.Add))
        ],
        Concatlist.mapped_list_of_t add_dummy_loc @@
        idx +@ [
          Ast.Const(const_int32 1);
          Ast.Binary(Values.I32 Ast.IntOp.ShrS);
        ]
      );
      (* Store the item *)
      Ast.Const(const_int32 4);
      Ast.Binary(Values.I32 Ast.IntOp.Mul);
    ] @ arr @ (untag (GenericHeapType(Some ArrayType))) +@ [
      Ast.Binary(Values.I32 Ast.IntOp.Add);
    ] @ val_ +@ [
      store ~offset:8 ();
    ] @ val_

let allocate_array env elts =
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) +@ [
      store ~offset:(4 * (idx + 2)) ();
    ] in

  (heap_allocate env (num_elts + 2)) @ tee_swap +@ [
    Ast.Const(const_int32 (tag_val_of_heap_tag_type ArrayType));
    store ~offset:0 ();
  ] @ get_swap +@ [
    Ast.Const(const_int32 num_elts);
    store ~offset:4 ();
  ] @ (Concatlist.flatten @@ List.mapi compile_elt elts) @ get_swap +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType (Some ArrayType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_array_n env num_elts elt =
  let get_arr_addr = get_swap env 0 in
  let set_arr_addr = set_swap env 0 in
  let get_loop_counter = get_swap env 1 in
  let set_loop_counter = set_swap env 1 in

  let compiled_num_elts = compile_imm env num_elts in
  let elt = compile_imm env elt in

  compiled_num_elts +@ [
    Ast.Const(const_int32 0);
    Ast.Compare(Values.I32 Ast.IntOp.LtS);
    error_if_true env InvalidArgument [num_elts];
  ] @ (heap_allocate_imm ~additional_words:2 env num_elts) @ set_arr_addr @ get_arr_addr +@ [
    Ast.Const(const_int32 (tag_val_of_heap_tag_type ArrayType));
    store ~offset:0 ();
  ] @ get_arr_addr @ compiled_num_elts +@ [
    Ast.Const(const_int32 1);
    Ast.Binary(Values.I32 Ast.IntOp.ShrS);
    store ~offset:4 ();
    Ast.Const(const_int32 0);
  ] @ set_loop_counter @ singleton 
  (Ast.Block([], Concatlist.mapped_list_of_t add_dummy_loc @@ singleton 
    (Ast.Loop([], Concatlist.mapped_list_of_t add_dummy_loc @@
      get_loop_counter @ compiled_num_elts +@ [
        Ast.Compare(Values.I32 Ast.IntOp.GeS);
        Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)
      ] @ get_arr_addr @ get_loop_counter +@ [
        (* Since the counter is tagged, only need to multiply by 2 *)
        Ast.Const(const_int32 2);
        Ast.Binary(Values.I32 Ast.IntOp.Mul);
        Ast.Binary(Values.I32 Ast.IntOp.Add);
      ] @ elt +@ [
        store ~offset:8 ();
      ] @ get_loop_counter +@ [
        (* Add 2 to keep tagged counter *)
        Ast.Const(const_int32 2);
        Ast.Binary(Values.I32 Ast.IntOp.Add);
      ] @ set_loop_counter +@ [
        Ast.Br (add_dummy_loc @@ Int32.of_int 0);
      ])
    )
  )) @ get_arr_addr +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType (Some ArrayType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]

let allocate_array_init env num_elts init_f =
  let get_arr_addr = get_swap env 0 in
  let set_arr_addr = set_swap env 0 in
  let get_loop_counter = get_swap env 1 in
  let set_loop_counter = set_swap env 1 in

  let compiled_num_elts = compile_imm env num_elts in

  let compiled_func = compile_imm env init_f in
  let ftype = add_dummy_loc @@ Int32.of_int (get_arity_func_type_idx env (2)) in

  compiled_num_elts +@ [
    Ast.Const(const_int32 0);
    Ast.Compare(Values.I32 Ast.IntOp.LtS);
    error_if_true env InvalidArgument [num_elts];
  ] @ (heap_allocate_imm ~additional_words:2 env num_elts) @ set_arr_addr @ get_arr_addr +@ [
    Ast.Const(const_int32 (tag_val_of_heap_tag_type ArrayType));
    store ~offset:0 ();
  ] @ get_arr_addr @ compiled_num_elts +@ [
    Ast.Const(const_int32 1);
    Ast.Binary(Values.I32 Ast.IntOp.ShrS);
    store ~offset:4 ();
    Ast.Const(const_int32 0);
  ] @ set_loop_counter @ singleton 
  (Ast.Block([], Concatlist.mapped_list_of_t add_dummy_loc @@ singleton 
    (Ast.Loop([], Concatlist.mapped_list_of_t add_dummy_loc @@
      get_loop_counter @ compiled_num_elts +@ [
        Ast.Compare(Values.I32 Ast.IntOp.GeS);
        Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)
      ] @ get_arr_addr @ get_loop_counter +@ [
        (* Since the counter is tagged, only need to multiply by 2 *)
        Ast.Const(const_int32 2);
        Ast.Binary(Values.I32 Ast.IntOp.Mul);
        Ast.Binary(Values.I32 Ast.IntOp.Add);
      ] @ compiled_func @
      untag LambdaTagType @
      get_loop_counter @
      compiled_func @
      untag LambdaTagType +@ [
        load ~offset:4 ();
        Ast.CallIndirect(ftype);
        store ~offset:8 ();
      ] @ get_loop_counter +@ [
        (* Add 2 to keep tagged counter *)
        Ast.Const(const_int32 2);
        Ast.Binary(Values.I32 Ast.IntOp.Add);
      ] @ set_loop_counter +@ [
        Ast.Br (add_dummy_loc @@ Int32.of_int 0);
      ])
    )
  )) @ get_arr_addr +@ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType (Some ArrayType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]


let compile_prim1 env p1 arg : Wasm.Ast.instr' Concatlist.t =
  let compiled_arg = compile_imm env arg in
  (* TODO: Overflow checks? *)
  match p1 with
  | Incr -> compiled_arg @+ [
      Ast.Const(encoded_const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
    ]
  | Decr -> compiled_arg @+ [
      Ast.Const(encoded_const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]
  | Not -> compiled_arg @+ [
      Ast.Const(const_int32 0x80000000);
      Ast.Binary(Values.I32 Ast.IntOp.Xor);
    ]
  | Ignore -> singleton (Ast.Const const_void)
  | ArrayLength -> compile_array_op env arg MArrayLength
  | Assert -> compiled_arg @+ [
    Ast.Const(const_false);
    Ast.Compare(Values.I32 Ast.IntOp.Eq);
    error_if_true env AssertionError [];
    Ast.Const(const_void);
  ]
  | FailWith -> call_error_handler env Failure [arg]
  | Box -> failwith "Unreachable case; should never get here: Box"
  | Unbox -> failwith "Unreachable case; should never get here: Unbox"


let compile_prim2 (env : codegen_env) p2 arg1 arg2 : Wasm.Ast.instr' Concatlist.t =
  let compiled_arg1 = compile_imm env arg1 in
  let compiled_arg2 = compile_imm env arg2 in
  let swap_get = get_swap ~ty:Types.I32Type env 0 in
  let swap_set = set_swap ~ty:Types.I32Type env 0 in
  let overflow_safe instrs =
    let compiled_swap_get = get_swap ~ty:Types.I64Type env 0 in
    let compiled_swap_set = set_swap ~ty:Types.I64Type env 0 in
    instrs @
    compiled_swap_set @
    compiled_swap_get @
    compiled_swap_get @
    (check_overflow env) @
    compiled_swap_get +@ [
      Ast.Convert(Values.I32 Ast.IntOp.WrapI64);
    ] in
                                                                          
  (* TODO: Overflow checks? *)
  match p2 with
  | Plus ->
    overflow_safe @@
    compiled_arg1 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Add);
    ]
  | Minus ->
    overflow_safe @@
    compiled_arg1 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Sub);
    ]
  | Times ->
    (* Untag one of the numbers:
       ((a * 2) / 2) * (b * 2) = (a * b) * 2
    *)
    overflow_safe @@
    compiled_arg1 @
    untag_number +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Mul);
    ]
  | Divide ->
    (* While (2a) / b = 2(a/b), we can't just untag b since b could be a multiple of 2 (yielding an odd result).
       Instead, perform the division and retag after:
       (2a / 2b) * 2 = (a / b) * 2
    *)
    overflow_safe @@
    compiled_arg1 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Test(Values.I32 Ast.IntOp.Eqz);
      error_if_true env DivisionByZeroError [];
    ] @
    compiled_arg2 +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.DivS);
      Ast.Const(const_int64 2);
      Ast.Binary(Values.I64 Ast.IntOp.Mul);
    ]
  | Mod ->
    (* Mod is not commutative, so untag everything and retag at the end *)
    overflow_safe @@
    compiled_arg1 @
    untag_number +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 +@ [
      Ast.Test(Values.I32 Ast.IntOp.Eqz);
      error_if_true env ModuloByZeroError [];
    ] @
    compiled_arg2 @
    untag_number +@ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.RemS);
      Ast.Const(const_int64 2);
      Ast.Binary(Values.I64 Ast.IntOp.Mul);
    ] @
    (* Convert remainder result into modulo result *)
    compiled_arg1 +@ [
      Ast.Const(const_int32 31);
      Ast.Binary(Values.I32 Ast.IntOp.ShrU);
    ] @
    compiled_arg2 +@ [
      Ast.Const(const_int32 31);
      Ast.Binary(Values.I32 Ast.IntOp.ShrU);
      Ast.Compare(Values.I32 Ast.IntOp.Eq);
      Ast.If([Types.I64Type],
        [add_dummy_loc @@ Ast.Const(const_int64 0)],
        Concatlist.mapped_list_of_t add_dummy_loc @@
        compiled_arg2 +@
        [Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32)]
      );
      Ast.Binary(Values.I64 Ast.IntOp.Add);
    ]
  | And ->
    compiled_arg1 @
    swap_set @
    swap_get @
    decode_bool +@ [
      Ast.If([Types.I32Type],
             List.map add_dummy_loc @@ list_of_t compiled_arg2,
             List.map add_dummy_loc @@ list_of_t swap_get)
    ]
  | Or ->
    compiled_arg1 @
    swap_set @
    swap_get @
    decode_bool +@ [
      Ast.If([Types.I32Type],
             List.map add_dummy_loc @@ list_of_t swap_get,
             List.map add_dummy_loc @@ list_of_t compiled_arg2)
    ]
  | Greater ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.GtS)
    ] @ encode_bool
  | GreaterEq ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.GeS)
    ] @ encode_bool
  | Less ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.LtS)
    ] @ encode_bool
  | LessEq ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.LeS)
    ] @ encode_bool
  | Eq ->
    compiled_arg1 @ compiled_arg2 +@ [
      Ast.Compare(Values.I32 Ast.IntOp.Eq)
    ] @ encode_bool
  | ArrayMake ->
    allocate_array_n env arg1 arg2
  | ArrayInit ->
    allocate_array_init env arg1 arg2
