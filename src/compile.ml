open Expr
open Anf
open Codegen
open Legacy_types
open Resolve_scope
open Optimize
open Grain_parsing
open Grain_typed

type compile_options = {
  type_check: bool;
  verbose: bool;
  sound_optimizations: bool;
  optimizations_enabled: bool;
  include_dirs: string list;
  use_stdlib: bool;
}

let default_compile_options = {
  type_check = false;
  verbose = false;
  sound_optimizations = true;
  optimizations_enabled = true;
  include_dirs = [];
  use_stdlib = true;
}

let compile_prog p = Codegen.module_to_string @@ Codegen.compile_aprog p

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
let initial_env = List.map (fun (n, l, _) -> (n, l)) initial_funcs

(** List of standard libraries to load *)
let libs = ["lists"]

let opts_to_optimization_settings opts = {
  verbose = opts.verbose;
  sound = opts.sound_optimizations;
  initial_functions = initial_funcs;
}

let lib_include_dirs opts =
  (if opts.use_stdlib then Option.map_default (fun x -> [x]) [] (Grain_stdlib.stdlib_directory()) else []) @ opts.include_dirs

let compile_module (opts: compile_options) (p : Parsetree.parsed_program) =
  match Grain_stdlib.load_libraries initial_env (lib_include_dirs opts) p with
  | Left(errs) -> Left(errs)
  | Right(full_p) ->
    Printf.eprintf "Bouta tc\n";
    Well_formedness.check_well_formedness full_p;
    Printf.eprintf "I am well formed";
    let typed_mod, signature, env = Typemod.type_module Env.empty full_p in
    Printf.eprintf "did it\n";
    let anfed = atag @@ Anf.anf_typed typed_mod in
    let renamed = resolve_scope anfed initial_env in
    let optimized =
      if opts.optimizations_enabled then
        optimize renamed (opts_to_optimization_settings opts)
      else
        renamed in
    Right(compile_aprog optimized)

let compile_to_string opts p =
  match compile_module opts p with
  | Left(v) -> Left(v)
  | Right(m) -> Right(module_to_string m)

let compile_to_anf (opts : compile_options) (p : Parsetree.parsed_program) =
  match Grain_stdlib.load_libraries initial_env (lib_include_dirs opts) p with
  | Left(errs) -> Left(errs)
  | Right(full_p) ->
    Well_formedness.check_well_formedness full_p;
    Printf.eprintf "Bouta tc\n";
    let typed_mod, signature, env = Typemod.type_module Env.empty full_p in
    Printf.eprintf "did it\n";
    let anfed = atag @@ Anf.anf_typed typed_mod in
    Right(anfed)

(* like compile_to_anf, but performs scope resolution and optimization. *)
let compile_to_final_anf (opts : compile_options) (p : Parsetree.parsed_program) =
  match Grain_stdlib.load_libraries initial_env (lib_include_dirs opts) p with
  | Left(errs) -> Left(errs)
  | Right(full_p) ->
    Well_formedness.check_well_formedness full_p;
    Printf.eprintf "I am well formed";
    let typed_mod, signature, env = Typemod.type_module Env.empty full_p in
    Printf.eprintf "did it\n";
    let anfed = atag @@ Anf.anf_typed typed_mod in
    let renamed = resolve_scope anfed initial_env in
    let optimized =
      if opts.optimizations_enabled then
        optimize renamed (opts_to_optimization_settings opts)
      else
        renamed in
    Right(optimized)


let anf = Anf.anf_typed

let free_vars anfed =
  Ast_utils.BindingSet.elements @@ Ast_utils.free_vars anfed
