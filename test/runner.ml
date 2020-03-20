open Unix
open Filename
open Str
open Grain.Compile
open Printf
open OUnit2
open Lexing
open Grain_codegen
open Grain_utils

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let either_printer e =
  match e with
  | Left(v) -> sprintf "Error: %s\n" v
  | Right(v) -> v

(* Read a file into a string *)
let string_of_file file_name =
  let inchan = open_in file_name in
  let buf = Bytes.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  buf 


let string_of_position p =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;


let parse name lexbuf =
  let ret = Grain_parsing.Driver.parse ~name lexbuf in
  let open Grain_parsing in
  let open Location in
  assert (ret.Parsetree.prog_loc.loc_start.pos_fname = name);
  ret

let parse_string name s = 
  let lexbuf = Lexing.from_string s in
  parse name lexbuf

let parse_file name input_file = 
  let lexbuf = Lexing.from_channel input_file in
  parse name lexbuf

let extract_anf {cstate_desc} =
  match cstate_desc with
  | Linearized(anf)
  | Optimized(anf) -> anf
  | _ -> raise (Invalid_argument "Expected ANF-containing state")

let compile_string_to_anf name s =
  extract_anf (compile_string ~hook:stop_after_anf ~name s)

let compile_string_to_final_anf name s =
  extract_anf (compile_string ~hook:stop_after_optimization ~name s)

let make_tmpfiles name =
  let (null_stdin, _) = pipe() in
  let stdout_name = (temp_file ("stdout_" ^ name) ".out") in
  let stdin_name = (temp_file ("stderr_" ^ name) ".err") in
  (openfile stdout_name [O_RDWR] 0o600, stdout_name,
   openfile stdin_name [O_RDWR] 0o600, stdin_name,
   null_stdin)

let assemble_object_file asm_file debug object_name =
  let rec get_encoded m =
    match m with
    | Wasm.Script.Textual m -> Wasm.Encode.encode m
    | Wasm.Script.Encoded(_, bs) -> bs
    | Wasm.Script.Quoted(_, s) -> get_encoded ((Wasm.Parse.string_to_module s).Wasm.Source.it) in
  let ic = open_in asm_file in
  let lexbuf = Lexing.from_channel ic in
  let (_, contents) = Wasm.Parse.parse asm_file lexbuf Wasm.Parse.Module in
  close_in ic;
  let oc = open_out_bin object_name in
  output_string oc @@ get_encoded (contents.Wasm.Source.it);
  close_out oc;
  "", (fun() -> ())

let compile_assembly_to_binary asm debug outfile_name =
  let asm_tmp_filename = if debug then outfile_name ^ ".wast" else (temp_file (Filename.basename outfile_name) ".wast") in
  let asm_tmp = open_out asm_tmp_filename in
  output_string asm_tmp asm;
  close_out asm_tmp;
  let obj_asm, obj_asm_cleanup = assemble_object_file asm_tmp_filename debug outfile_name in
  obj_asm

type result = (string, string) either

let extract_wasm {cstate_desc} =
  match cstate_desc with
  | Compiled(compiled) -> compiled
  | _ -> raise (Invalid_argument "Expected WASM State")

let read_stream cstream =
  let buf = Bytes.create 2048 in
  let i = ref 0 in
  Stream.iter (fun c ->
    (* This stream doesn't seem to have an end and causes the runner to hang, so we have an arbitrary cap *)
    (if !i >= 2048 then failwith "Program output exceeds 2048 characters");
    Bytes.set buf !i c;
    incr i
  ) cstream;
  Bytes.to_string @@ Bytes.sub buf 0 !i


let run_output cstate test_ctxt =
  let program = extract_wasm cstate in
  let file = Filename.temp_file "test" ".gr.wasm" in
  Emitmod.emit_module program file;

  let stdlib = Option.get (Grain_utils.Config.stdlib_directory()) in
  let testlibs = Sys.getcwd () ^ "/test-libs" in
  let include_dirs = stdlib ^ "," ^ testlibs in
  let result = ref "" in
  assert_command 
    ~foutput:(fun stream -> result := read_stream stream)
    ~use_stderr:true
    ~ctxt:test_ctxt
    "grain"
    ["-wpg"; "-I"; include_dirs; file];
  !result

let run_anf p out =
  let cstate = {
    cstate_desc=Linearized(p);
    cstate_filename=Some(out);
    cstate_outfile=None
  } in
  run_output (compile_resume ~hook:stop_after_compiled cstate)

let test_run ?cmp program_str outfile expected test_ctxt =
  let result = Config.preserve_config (fun () ->
      Config.include_dirs := "test-libs"::!Config.include_dirs;
      let cstate = compile_string ~hook:stop_after_compiled ~name:outfile program_str in
      run_output cstate test_ctxt
    ) in
  assert_equal 
  ~printer:Batteries.identity 
  ~cmp:(Option.default (=) cmp)
  (expected ^ "\n") result

let test_run_file filename name expected test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let outfile = "output/" ^ name in
  let cstate = compile_file ~hook:stop_after_compiled ~outfile input_filename in
  let result = run_output cstate test_ctxt in
  assert_equal ~printer:Batteries.identity (expected ^ "\n") result

let test_optimizations_sound program_str name expected test_ctxt =
  let compile_and_run () =
    run_output (compile_string ~hook:stop_after_compiled ~name program_str) test_ctxt in
  let result_unoptimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := false;
      compile_and_run ()) in
  let result_optimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := true;
      compile_and_run ()) in

  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (expected ^ "\n") result_optimized

let test_file_optimizations_sound filename name expected test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in

  let compile_and_run outfile =
    run_output (compile_file ~hook:stop_after_compiled ~outfile input_filename) test_ctxt in
  let result_unoptimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := false;
      compile_and_run full_outfile_unoptimized) in
  let result_optimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := true;
      compile_and_run full_outfile_optimized) in

  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (expected ^ "\n") result_optimized

let test_run_anf program_anf outfile expected test_ctxt =
  let result = run_anf program_anf outfile test_ctxt in
  assert_equal (expected ^ "\n") result ~printer:Batteries.identity

let test_err program_str outfile errmsg test_ctxt =
  let result = try
      Config.preserve_config (fun () ->
        Config.include_dirs := "test-libs"::!Config.include_dirs;
        let cstate = compile_string ~hook:stop_after_compiled ~name:outfile program_str in
        run_output cstate test_ctxt
      )
    with exn -> Printexc.to_string exn
  in
  assert_equal
    errmsg
    result
    ~cmp: (fun check result -> Batteries.String.exists result check)
    ~printer:Batteries.identity

let test_run_file_err filename name errmsg test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let outfile = "output/" ^ name in
  let result = try
      let cstate = compile_file ~hook:stop_after_compiled ~outfile input_filename in
      run_output cstate test_ctxt
    with exn -> Printexc.to_string exn
  in
  assert_equal
    errmsg
    result
    ~cmp: (fun check result -> Batteries.String.exists result check)
    ~printer:Batteries.identity
