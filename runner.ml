open Unix
open Filename
open Str
open Compile
open Printf
open OUnit2
open ExtLib
open Lexing
open Types
open Pretty
       
let either_printer e =
  match e with
  | Left(v) -> sprintf "Error: %s\n" v
  | Right(v) -> v

let string_of_position p =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;

let parse name lexbuf =
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
  with
  |  Failure "lexing: empty token" ->
      failwith (sprintf "lexical error at %s"
                        (string_of_position lexbuf.lex_curr_p))

let parse_string name s = 
  let lexbuf = Lexing.from_string s in
  parse name lexbuf

let parse_file name input_file = 
  let lexbuf = Lexing.from_channel input_file in
  parse name lexbuf

let compile_file_to_string name input_file =
  let input_program = parse_file name input_file in
  (compile_to_string input_program);;

let compile_string_to_string name s =
  let input_program = parse_string name s in
  (compile_to_string input_program);;

let make_tmpfiles name =
  let (null_stdin, _) = pipe() in
  let stdout_name = (temp_file ("stdout_" ^ name) ".out") in
  let stdin_name = (temp_file ("stderr_" ^ name) ".err") in
  (openfile stdout_name [O_RDWR] 0o600, stdout_name,
   openfile stdin_name [O_RDWR] 0o600, stdin_name,
   null_stdin)

(* Read a file into a string *)
let string_of_file file_name =
  let inchan = open_in file_name in
  let buf = String.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  buf 

type result = (string, string) either

let run_no_vg (program_name : string) args : result =
  let (rstdout, rstdout_name, rstderr, rstderr_name, rstdin) = make_tmpfiles "run" in
  let ran_pid = Unix.create_process (program_name ^ ".run") (Array.of_list ([""] @ args)) rstdin rstdout rstderr in
  let (_, status) = waitpid [] ran_pid in
  let result = match status with
    | WEXITED 0 -> Right(string_of_file rstdout_name)
    | WEXITED n -> Left(sprintf "Error %d: %s" n (string_of_file rstderr_name))
    | WSIGNALED n ->
       Left(sprintf "Signalled with %d while running %s." n program_name)
    | WSTOPPED n ->
       Left(sprintf "Stopped with signal %d while running %s." n program_name) in
  List.iter close [rstdout; rstderr; rstdin];
  List.iter unlink [rstdout_name; rstderr_name];
  result


let run_vg (program_name : string) args : result =
  let (rstdout, rstdout_name, rstderr, rstderr_name, rstdin) = make_tmpfiles "run" in
  let ran_pid = Unix.create_process "valgrind"  (Array.of_list ([""; (program_name ^ ".run")] @ args)) rstdin rstdout rstderr in
  let (_, status) = waitpid [] ran_pid in
  let vg_str = string_of_file rstderr_name in
  let vg_ok = String.exists vg_str "0 errors from 0 contexts" in
  let result = match (status, vg_ok) with
    | WEXITED 0, true -> Right(string_of_file rstdout_name)
    | WEXITED 0, false -> Left("Stdout: " ^ (string_of_file rstdout_name) ^ "\n" ^ "Valgrind: \n" ^ vg_str)
    | WEXITED n, _ -> Left(sprintf "Error %d: %s" n vg_str)
    | WSIGNALED n, _ ->
       Left(sprintf "Signalled with %d while running %s." n program_name)
    | WSTOPPED n, _ ->
       Left(sprintf "Stopped with signal %d while running %s." n program_name) in
  List.iter close [rstdout; rstderr; rstdin];
  List.iter unlink [rstdout_name; rstderr_name];
  result

let run_asm asm_string out (runner : string -> string list  -> result) args =
  let outfile = open_out (out ^ ".s") in
  fprintf outfile "%s" asm_string;
  close_out outfile;
  let (bstdout, bstdout_name, bstderr, bstderr_name, bstdin) = make_tmpfiles "build" in
  let built_pid = Unix.create_process "make" (Array.of_list [""; out ^ ".run"]) bstdin bstdout bstderr in
  let (_, status) = waitpid [] built_pid in

  let try_running = match status with
    | WEXITED 0 ->
       Right(string_of_file bstdout_name)
    | WEXITED n ->
       Left(sprintf "Finished with error while building %s:\n%s" out (string_of_file bstderr_name))
    | WSIGNALED n ->
       Left(sprintf "Signalled with %d while building %s." n out)
    | WSTOPPED n ->
       Left(sprintf "Stopped with signal %d while building %s." n out) in

  let result = match try_running with
    | Left(_) -> try_running
    | Right(msg) ->
       runner out args in

  List.iter close [bstdout; bstderr; bstdin];
  List.iter unlink [bstdout_name; bstderr_name];
  result


let print_errors exns =
  List.map (fun e ->
      match e with
      | UnboundId(x, loc) ->
         sprintf "The identifier %s, used at <%s>, is not in scope" x (string_of_pos loc)
      | UnboundFun(x, loc) ->
         sprintf "The function name %s, used at <%s>, is not in scope" x (string_of_pos loc)
      | ShadowId(x, loc, existing) ->
         sprintf "The identifier %s, defined at <%s>, shadows one defined at <%s>"
                 x (string_of_pos loc) (string_of_pos existing)
      | DuplicateId(x, loc, existing) ->
         sprintf "The identifier %s, redefined at <%s>, duplicates one at <%s>"
                 x (string_of_pos loc) (string_of_pos existing)
      | DuplicateFun(x, loc, existing) ->
         sprintf "The function name %s, redefined at <%s>, duplicates one at <%s>"
                 x (string_of_pos loc) (string_of_pos existing)
      | Overflow(num, loc) ->
         sprintf "The number literal %d, used at <%s>, is not supported in this language"
                 num (string_of_pos loc)
      | LetRecNonFunction(name, loc) ->
         sprintf "The let-rec binding of %s is not bound to a function at <%s>"
                 name (string_of_pos loc)
      | _ ->
         sprintf "%s" (Printexc.to_string e)
    ) exns
;;

  
let run p out runner args =
  let maybe_asm_string =
    try compile_to_string p with
    | Failure s -> Left([Failure("Compile error: " ^ s)])
    | err -> Left([Failure("Unexpected compile error: " ^ Printexc.to_string err)])
  in    
  match maybe_asm_string with
  | Left(errs) -> Left(ExtString.String.join "\n" (print_errors errs))
  | Right(asm_string) ->
     run_asm asm_string out runner args

let run_anf p out runner args =
  let maybe_asm_string =
    try Right(compile_prog p) with
    | Failure s -> Left([Failure("Compile error: " ^ s)])
    | err -> Left([Failure("Unexpected compile error: " ^ Printexc.to_string err)])
  in    
  match maybe_asm_string with
  | Left(errs) -> Left(ExtString.String.join "\n" (print_errors errs))
  | Right(asm_string) ->
     run_asm asm_string out runner args


let test_run args program_str outfile expected test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string outfile program_str in
  let result = run program full_outfile run_no_vg args in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer

let test_run_anf args program_anf outfile expected test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let result = run_anf program_anf full_outfile run_no_vg args in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer

let test_run_valgrind args program_str outfile expected test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string outfile program_str in
  let result = run program full_outfile run_vg args in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer

let test_err args program_str outfile errmsg test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string outfile program_str in
  let result = run program full_outfile run_no_vg args in
  assert_equal
    (Left(errmsg))
    result
    ~printer:either_printer
    ~cmp: (fun check result ->
      match check, result with
      | Left(expect_msg), Left(actual_message) ->
         String.exists actual_message expect_msg
      | _ -> false
    )
