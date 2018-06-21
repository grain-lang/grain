open Unix
open Filename
open Str
open Compile
open Printf
open OUnit2
open Lexing
open Grain_codegen

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
  let buf = String.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  buf 


let string_of_position p =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;


let print_errors exns =
  let open Wasm_runner in
  List.map (fun e ->
      match e with
      | GrainRuntimeError(msg) -> msg
      | _ ->
         sprintf "%s" (Printexc.to_string e)
    ) exns
;;


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
  | Compiled({asm}) -> asm
  | _ -> raise (Invalid_argument "Expected WASM State")

let run_output cstate =
  Wasm_runner.run_wasm (extract_wasm cstate)

let run_anf p out =
  let cstate = {
    cstate_desc=Linearized(p);
    cstate_filename=Some(out);
    cstate_outfile=None
  } in
  run_output (compile_resume ~hook:stop_after_compiled cstate)

let test_run program_str outfile expected test_ctxt =
  Wasm_runner.cur_modname := outfile;
  let result = run_output (compile_string ~hook:stop_after_compiled ~name:outfile program_str) in
  assert_equal (expected ^ "\n") result ~printer:Batteries.identity

let test_run_anf program_anf outfile expected test_ctxt =
  Wasm_runner.cur_modname := outfile;
  let result = run_anf program_anf outfile in
  assert_equal (expected ^ "\n") result ~printer:Batteries.identity

let test_err program_str outfile errmsg test_ctxt =
  Wasm_runner.cur_modname := outfile;
  let result = try
      run_output (compile_string ~hook:stop_after_compiled ~name:outfile program_str)
    with exn -> Printexc.to_string exn
  in
  assert_equal
    errmsg
    result
    ~cmp: (fun check result -> Batteries.String.exists result check)
    ~printer:Batteries.identity
