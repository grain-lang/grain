open Grain_parsing
open Grain_typed
open Grain_middle_end
open Grain_codegen
open Optimize

type input_source =
  | InputString of string
  | InputFile of string

type compilation_state_desc =
  | Initial of input_source
  | Parsed of Parsetree.parsed_program
  | WellFormed of Parsetree.parsed_program
  | TypeChecked of Typedtree.typed_program
  | Linearized of Anftree.anf_program
  | Optimized of Anftree.anf_program
  | Mashed of Mashtree.mash_program
  | Compiled of Compmod.compiled_program
  | Assembled

type compilation_state = {
  cstate_desc: compilation_state_desc;
  cstate_filename: string option;
  cstate_outfile: string option;
}

type compilation_action =
  | Continue of compilation_state
  | Stop

let compile_prog p = Compcore.module_to_string @@ Compcore.compile_wasm_module p

let initial_funcs = [
  ("print", (Lexing.dummy_pos, Lexing.dummy_pos), false);
  ("equal", (Lexing.dummy_pos, Lexing.dummy_pos), true);
  ("toString", (Lexing.dummy_pos, Lexing.dummy_pos), true);
  ("input", (Lexing.dummy_pos, Lexing.dummy_pos), false);
  ("strcat", (Lexing.dummy_pos, Lexing.dummy_pos), true);
  ("strlen", (Lexing.dummy_pos, Lexing.dummy_pos), true);
  ("strslice", (Lexing.dummy_pos, Lexing.dummy_pos), true);
  ("DOM::query", (Lexing.dummy_pos, Lexing.dummy_pos), false);
  ("DOM::setText", (Lexing.dummy_pos, Lexing.dummy_pos), false);
  ("DOM::dangerouslySetInnerHTML", (Lexing.dummy_pos, Lexing.dummy_pos), false);
  ("DOM::addEventListener", (Lexing.dummy_pos, Lexing.dummy_pos), false);
]

(* Environment containing initial functions *)
(* Deprecated *)
let initial_load_env = List.map (fun (n, l, _) -> (n, l)) initial_funcs

(** List of standard libraries to load *)
let libs = ["lists"]

let log_state state =
  if !Grain_utils.Config.verbose then begin
    let prerr_sexp conv x = prerr_string (Sexplib.Sexp.to_string_hum (conv x)) in
    begin match state.cstate_desc with
      | Initial(src) ->
        begin match src with
          | InputString(str) ->
            prerr_string "\nInput string:\n";
            prerr_string ("'" ^ str ^ "'");
          | InputFile(fname) ->
            prerr_string ("\nInput from file: " ^ fname)
        end
      | Parsed(p) ->
        prerr_string "\nParsed program:\n";
        prerr_sexp Grain_parsing.Parsetree.sexp_of_parsed_program p;
      | WellFormed _ ->
        prerr_string "\nWell-Formedness passed";
      | TypeChecked(typed_mod) ->
        prerr_string "\nTyped program:\n";
        prerr_sexp Grain_typed.Typedtree.sexp_of_typed_program typed_mod;
      | Linearized(anfed) ->
        prerr_string "\nANFed program:\n";
        prerr_sexp Anftree.sexp_of_anf_program anfed;
      | Optimized(optimized) ->
        prerr_string "\nOptimized program:\n";
        prerr_sexp Anftree.sexp_of_anf_program optimized;
      | Mashed(mashed) ->
        prerr_string "\nMashed program:\n";
        prerr_sexp Mashtree.sexp_of_mash_program mashed;
      | Compiled(compiled) ->
        prerr_string "\nCompiled successfully";
      | Assembled ->
        prerr_string "\nAssembled successfully";
    end;
    prerr_string "\n\n"
  end

let next_state ({cstate_desc} as cs) =
  let cstate_desc = match cstate_desc with
    | Initial(input) ->
      let name, lexbuf, cleanup = match input with
        | InputString(str) ->
          cs.cstate_filename, (Lexing.from_string str), (fun () -> ())
        | InputFile(name) ->
          let ic = open_in name in
          Some(name), (Lexing.from_channel ic), (fun () -> close_in ic)
      in
      let parsed =
        try
          Driver.parse ?name lexbuf
        with
        | _ as e ->
          cleanup();
          raise e
      in
      cleanup();
      Parsed(parsed)
    | Parsed(p) ->
      Well_formedness.check_well_formedness p;
      WellFormed(p)
    | WellFormed(full_p) ->
      TypeChecked(Typemod.type_implementation full_p)
    | TypeChecked(typed_mod) ->
      Linearized(Linearize.transl_anf_module typed_mod)
    | Linearized(anfed) ->
      if !Grain_utils.Config.optimizations_enabled then
        Optimized(Optimize.optimize_program anfed)
      else
        Optimized(anfed)
    | Optimized(optimized) ->
      Mashed(Transl_anf.transl_anf_program optimized)
    | Mashed(mashed) ->
      Compiled(Compmod.compile_wasm_module mashed)
    | Compiled(compiled) ->
      if !Grain_utils.Config.output_enabled then begin
        match cs.cstate_outfile with
        | Some(outfile) ->
          Emitmod.emit_module compiled outfile
        | None -> ()
      end;
      Assembled
    | Assembled -> Assembled
  in
  let ret = {cs with cstate_desc} in
  log_state ret;
  ret


let rec compile_resume ?hook (s : compilation_state) =
  let next_state = next_state s in
  match hook with
  | Some(func) ->
    begin match func next_state with
      | Continue ({cstate_desc=Assembled} as s) -> s
      | Continue s -> compile_resume ?hook s
      | Stop -> next_state
    end
  | None ->
    begin match next_state.cstate_desc with
      | Assembled -> next_state
      | _ -> compile_resume ?hook next_state
    end


let compile_string ?hook ?name ?outfile str =
  let cstate = {
    cstate_desc=Initial(InputString(str));
    cstate_filename=name;
    cstate_outfile=outfile;
  } in
  compile_resume ?hook cstate

let compile_file ?hook ?outfile filename =
  let cstate = {
    cstate_desc=Initial(InputFile(filename));
    cstate_filename=Some(filename);
    cstate_outfile=outfile;
  } in
  compile_resume ?hook cstate


let stop_after_parse = function
  | {cstate_desc=Parsed(_)} -> Stop
  | s -> Continue s

let stop_after_well_formed = function
  | {cstate_desc=WellFormed(_)} -> Stop
  | s -> Continue s

let stop_after_typed = function
  | {cstate_desc=TypeChecked(_)} -> Stop
  | s -> Continue s

let stop_after_anf = function
  | {cstate_desc=Linearized(_)} -> Stop
  | s -> Continue s

let stop_after_optimization = function
  | {cstate_desc=Optimized(_)} -> Stop
  | s -> Continue s

let stop_after_mashed = function
  | {cstate_desc=Mashed(_)} -> Stop
  | s -> Continue s

let stop_after_compiled = function
  | {cstate_desc=Compiled(_)} -> Stop
  | s -> Continue s

let anf = Linearize.transl_anf_module

let free_vars anfed =
  Ident.Set.elements @@ Anf_utils.anf_free_vars anfed

let () =
  Env.compile_module_dependency := (fun input outfile -> ignore(compile_file ~outfile input))

