open Wasm

open Values
open Types
open Instance
open Errors

exception GrainRuntimeError of string

let unbox = Values.I32Value.of_value
let unbox64 = Values.I64Value.of_value

let in_fd, out_fd =
  let ifd, ofd = Unix.pipe() in
  ref ifd, ref ofd

let in_channel, out_channel =
  let ic, oc = (Unix.in_channel_of_descr !in_fd, Unix.out_channel_of_descr !out_fd) in
  Pervasives.set_binary_mode_in ic false;
  Pervasives.set_binary_mode_out oc false;
  (* The channels will be reset when the runner is called *)
  Unix.close !in_fd;
  Unix.close !out_fd;
  ref ic, ref oc

let reset_channels() =
  let ifd, ofd = Unix.pipe() in
  in_fd := ifd;
  out_fd := ofd;
  let ic, oc = (Unix.in_channel_of_descr !in_fd, Unix.out_channel_of_descr !out_fd) in
  Pervasives.set_binary_mode_in ic false;
  Pervasives.set_binary_mode_out oc false;
  in_channel := ic;
  out_channel := oc

let memory_internal = (Memory.alloc (MemoryType {Types.min=(Int32.of_int 1); Types.max=None}))
let memory = ExternMemory memory_internal

let load_word addr : int32 =
  Memory.load_value memory_internal
    (Int64.of_int addr) Int32.zero Types.I32Type
  |> unbox

let load_word64 addr : int64 =
  Memory.load_value memory_internal
    (Int64.of_int addr) Int32.zero Types.I64Type
  |> unbox64

let set_word addr (value : int32) =
  let to_set = Values.I32Value.to_value value in
  Memory.store_value memory_internal
    (Int64.of_int addr) Int32.zero to_set

let string_of_grain_heap_value (v : int32) =
  let open Value_tags in
  let v_int = Int32.to_int v in
  let tag = heap_tag_type_of_tag_val @@ Int32.to_int @@ load_word v_int in
  match tag with
  | StringType ->
    let string_length = Int32.to_int @@ load_word (v_int + 4) in
    let num_ints = ((string_length - 1) / 8) + 1 in
    let outbytes = Bytes.create (8 * num_ints) in
    let to_bytes = if Sys.big_endian then
        Stdint.Uint64.to_bytes_big_endian
      else
        Stdint.Uint64.to_bytes_little_endian in
    for i = 0 to num_ints - 1 do
      let word = load_word64 (v_int + 8 + (8 * i)) in
      let uword = Stdint.Uint64.of_int64 word in
      to_bytes uword outbytes (i * 8);
    done;
    let buf = Buffer.create (Bytes.length outbytes) in
    Buffer.add_bytes buf outbytes;
    let str = Buffer.sub buf 0 string_length in
    Printf.sprintf "\"%s\"" str



let rec string_of_grain_help (v : int32) tuple_counter =
  let open Printf in
  if ((Int32.logand v Int32.one) = Int32.zero) then
    sprintf "%ld" (Int32.div v (Int32.of_int 2))
  else if ((Int32.logand v Int32.(of_int 7)) = Int32.one) then
    let tuple_idx = Int32.to_int (Int32.logxor v Int32.one) in
    let tuple_length = load_word tuple_idx in
    if (Int32.logand tuple_length (Int32.of_int 0x80000000) <> Int32.zero) then
      sprintf "<cyclic tuple %ld>" (Int32.logand tuple_length (Int32.of_int 0x7FFFFFFF))
    else
      begin
        tuple_counter := !tuple_counter + 1;
        set_word tuple_idx (Int32.logor (Int32.of_int !tuple_counter) (Int32.of_int 0x80000000));
        let elts = ref [] in
        for idx = 0 to (Int32.to_int tuple_length) - 1 do
          elts := (string_of_grain_help (load_word (tuple_idx + (4 * (1 + idx)))) tuple_counter)::!elts
        done;
        if (List.length !elts) = 1 then
          elts := "\b"::!elts;
        set_word tuple_idx tuple_length;
        sprintf "(%s)" @@ BatString.join ", " (List.rev !elts)
      end
  else if ((Int32.logand v (Int32.of_int 7)) = (Int32.of_int 5)) then
    "<lambda>"
  else if ((Int32.logand v (Int32.of_int 7)) = (Int32.of_int 3)) then
    string_of_grain_heap_value (Int32.logxor v (Int32.of_int 3))
  else if (v = Int32.minus_one) then
    "true"
  else if (v = Int32.max_int) then
    "false"
  else
    sprintf "<Unknown value: %ld>" v

let string_of_grain x =
  string_of_grain_help x (ref 0)

let rec grain_equal_help x y cycles =
  if ((Int32.logand x Int32.(of_int 7)) = Int32.one) then
    if ((Int32.logand y Int32.(of_int 7)) <> Int32.one) then
      false
    else
      begin
        let tuple_idx_x = Int32.to_int (Int32.logxor x Int32.one) in
        let tuple_idx_y = Int32.to_int (Int32.logxor y Int32.one) in
        let tuple_length_x = load_word tuple_idx_x in
        let tuple_length_y = load_word tuple_idx_y in
        if (tuple_length_x <> tuple_length_y) then
          false
        else if (Int32.logand tuple_length_x (Int32.of_int 0x80000000) <> Int32.zero) then
          true
        else
          begin
            set_word tuple_idx_x (Int32.logor (Int32.of_int !cycles) (Int32.of_int 0x80000000));
            set_word tuple_idx_y (Int32.logor (Int32.of_int !cycles) (Int32.of_int 0x80000000));
            cycles := !cycles + 1;
            let res = ref true in
            for idx = 0 to (Int32.to_int tuple_length_x) - 1 do
              res := !res && (grain_equal_help
                                (load_word (tuple_idx_x + (4 * (1 + idx))))
                                (load_word (tuple_idx_y + (4 * (1 + idx))))
                                cycles)
            done;
            set_word tuple_idx_x tuple_length_x;
            set_word tuple_idx_y tuple_length_y;
            !res
          end
      end
  else
    x = y

let error_message err (value1 : int32) (value2 : int32) =
  let open Printf in
  let value1_as_string =
    try string_of_grain value1
    with
    | _ -> "<error printing value>" in
  match err with
  | ArithmeticError ->
    sprintf "arithmetic expected a number, got value: %s" value1_as_string
  | ComparisonError ->
    sprintf "comparison expected a number, got value: %s" value1_as_string
  | IfError ->
    sprintf "if expected a boolean, got value: %s" value1_as_string
  | LogicError ->
    sprintf "logic expected a boolean, got value: %s" value1_as_string
  | ArityMismatch ->
    sprintf "arity mismatch (expected %ld arguments, but got %ld)" value1 value2
  | CalledNonFunction ->
    sprintf "called non-function: %s" value1_as_string
  | GetItemNotTuple ->
    sprintf "tuple access expected tuple, got value: %s" value1_as_string
  | GetItemIndexNotNumber ->
    sprintf "tuple access expected number for index, got value: %s" value1_as_string
  | SetItemIndexTooSmall
  | GetItemIndexTooSmall ->
    sprintf "tuple index too small: %s (tuple arity: %ld)" value1_as_string value2
  | SetItemIndexTooLarge
  | GetItemIndexTooLarge ->
    sprintf "tuple index too large: %s (tuple arity: %ld)" value1_as_string value2
  | SetItemNotTuple ->
    sprintf "tuple assignment expected tuple, got value: %s" value1_as_string
  | SetItemIndexNotNumber ->
    sprintf "tuple assignment expected number for index, got value: %s" value1_as_string
  | GenericNumberError ->
    sprintf "expected a number, got value: %s" value1_as_string
  | OverflowError ->
    sprintf "overflow"

let console_log = function
  | [x] ->
    Printf.fprintf !out_channel "%s\n" (string_of_grain (unbox x));
    [x]
  | _ -> failwith "NYI: console_log"

let console_debug = function
  | [x] -> [x]
  | _ -> failwith "signature violation"

let console_print_closure = function
  | _ -> failwith "signature violation"

let js_throw_error = function
  | [errcode; v1; v2] ->
    let err = error_of_code (Int32.to_int @@ unbox errcode) in
    raise (GrainRuntimeError(error_message err (unbox v1) (unbox v2)))
  | _ -> failwith "signature violation"

let grain_print = function
  | [x] ->
    Printf.fprintf !out_channel "%s\n" (string_of_grain (unbox x));
    [x]
  | _ -> failwith "signature violation"

let grain_check_memory = function
  | [x] ->
    []
  | _ -> failwith "signature violation"

let grain_equal = function
  | [x; y] ->
    if grain_equal_help (unbox x) (unbox y) (ref 0) then
      [Values.I32Value.to_value (Int32.of_int 0xFFFFFFFF)]
    else
      [Values.I32Value.to_value (Int32.of_int 0x7FFFFFFF)]
  | _ -> failwith "NYI: grain_equal"

let grain_nyi name = function
  | _ -> failwith (Printf.sprintf "NYI: %s" name)

let console_lookup name t =
  match (Utf8.encode name), t with
  | "log", ExternFuncType t -> ExternFunc (Func.HostFunc (t, console_log))
  | "debug", ExternFuncType t -> ExternFunc (Func.HostFunc (t, console_debug))
  | "printClosure", ExternFuncType t -> ExternFunc (Func.HostFunc (t, console_print_closure))
  | _ -> raise Not_found

let js_lookup name t =
  match (Utf8.encode name), t with
  | "throwError", ExternFuncType t -> ExternFunc (Func.HostFunc (t, js_throw_error))
  | "mem", ExternMemoryType t -> memory
  | "checkMemory", ExternFuncType t -> ExternFunc (Func.HostFunc (t, grain_check_memory))
  | _ -> raise Not_found

let grain_builtin_lookup name t =
  match (Utf8.encode name), t with
  | "print", ExternFuncType t -> ExternFunc (Func.HostFunc (t, grain_print))
  | "equal", ExternFuncType t -> ExternFunc (Func.HostFunc (t, grain_equal))
  | _, ExternFuncType t -> ExternFunc (Func.HostFunc (t, grain_nyi (Utf8.encode name)))
  | _ -> raise Not_found

let configured = ref false

let configure_runner() =
  if not !configured then
    begin
      Import.register (Utf8.decode "grainBuiltins") grain_builtin_lookup;
      Import.register (Utf8.decode "console") console_lookup;
      Import.register (Utf8.decode "js") js_lookup;
      configured := true
    end

let run_wasm (module_ : Wasm.Ast.module_) =
  (* FIXME: Maybe we should be using something like our_code_starts_here instead of start? *)
  (* Hack to get result of start *)
  let open Wasm.Source in
  let open Wasm.Ast in
  configure_runner();
  reset_channels();
  Valid.check_module module_;
  let imports = Import.link module_ in
  let inst = Eval.init module_ imports in
  let start = Instance.export inst (Utf8.decode "GRAIN$MAIN") in
  match start with
  | None -> failwith "No start function found in module!"
  | Some(ExternFunc s) ->
    begin match Eval.invoke s [] with
    | [] ->
      flush !out_channel;
      Unix.close !out_fd;
      let str = BatStream.to_string (BatStream.of_channel !in_channel) in
      Unix.close !in_fd;
      str
    | x1::x2::_ -> failwith "Multiple values returned by start"
    | x::[] ->
      flush !out_channel;
      Unix.close !out_fd;
      let str = BatStream.to_string (BatStream.of_channel !in_channel) in
      Unix.close !in_fd;
      str ^ (string_of_grain (unbox x)) ^ "\n"
    end
  | _ -> failwith "Bad GRAIN$MAIN export"
  
