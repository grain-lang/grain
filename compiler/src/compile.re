open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_codegen;
open Optimize;

type input_source =
  | InputString(string)
  | InputFile(string);

type compilation_state_desc =
  | Initial(input_source)
  | Parsed(Parsetree.parsed_program)
  | WithLibraries(Parsetree.parsed_program)
  | WellFormed(Parsetree.parsed_program)
  | TypeChecked(Typedtree.typed_program)
  | Linearized(Anftree.anf_program)
  | Optimized(Anftree.anf_program)
  | Mashed(Mashtree.mash_program)
  | Compiled(Compmod.compiled_program)
  | Assembled;

type compilation_state = {
  cstate_desc: compilation_state_desc,
  cstate_filename: option(string),
  cstate_outfile: option(string),
};

type compilation_action =
  | Continue(compilation_state)
  | Stop;

let compile_prog = p =>
  Compcore.module_to_bytes @@ Compcore.compile_wasm_module(p);

let initial_funcs = [
  ("print", (Lexing.dummy_pos, Lexing.dummy_pos), false),
  ("equal", (Lexing.dummy_pos, Lexing.dummy_pos), true),
  ("toString", (Lexing.dummy_pos, Lexing.dummy_pos), true),
  ("input", (Lexing.dummy_pos, Lexing.dummy_pos), false),
  ("strcat", (Lexing.dummy_pos, Lexing.dummy_pos), true),
  ("strlen", (Lexing.dummy_pos, Lexing.dummy_pos), true),
  ("strslice", (Lexing.dummy_pos, Lexing.dummy_pos), true),
  ("DOM::query", (Lexing.dummy_pos, Lexing.dummy_pos), false),
  ("DOM::setText", (Lexing.dummy_pos, Lexing.dummy_pos), false),
  (
    "DOM::dangerouslySetInnerHTML",
    (Lexing.dummy_pos, Lexing.dummy_pos),
    false,
  ),
  ("DOM::addEventListener", (Lexing.dummy_pos, Lexing.dummy_pos), false),
];

/* Environment containing initial functions */
/* Deprecated */
let initial_load_env = List.map(((n, l, _)) => (n, l), initial_funcs);

let log_state = state =>
  if (Grain_utils.Config.verbose^) {
    let prerr_sexp = (conv, x) =>
      prerr_string(Sexplib.Sexp.to_string_hum(conv(x)));
    switch (state.cstate_desc) {
    | Initial(src) =>
      switch (src) {
      | InputString(str) =>
        prerr_string("\nInput string:\n");
        prerr_string("'" ++ str ++ "'");
      | InputFile(fname) => prerr_string("\nInput from file: " ++ fname)
      }
    | Parsed(p) =>
      prerr_string("\nParsed program:\n");
      prerr_sexp(Grain_parsing.Parsetree.sexp_of_parsed_program, p);
    | WithLibraries(full_p) =>
      prerr_string("\nwith libraries:\n");
      prerr_sexp(Grain_parsing.Parsetree.sexp_of_parsed_program, full_p);
    | WellFormed(_) => prerr_string("\nWell-Formedness passed")
    | TypeChecked(typed_mod) =>
      prerr_string("\nTyped program:\n");
      prerr_sexp(Grain_typed.Typedtree.sexp_of_typed_program, typed_mod);
    | Linearized(anfed) =>
      prerr_string("\nANFed program:\n");
      prerr_sexp(Anftree.sexp_of_anf_program, anfed);
    | Optimized(optimized) =>
      prerr_string("\nOptimized program:\n");
      prerr_sexp(Anftree.sexp_of_anf_program, optimized);
    | Mashed(mashed) =>
      prerr_string("\nMashed program:\n");
      prerr_sexp(Mashtree.sexp_of_mash_program, mashed);
    | Compiled(compiled) => prerr_string("\nCompiled successfully")
    | Assembled => prerr_string("\nAssembled successfully")
    };
    prerr_string("\n\n");
  };

let next_state = ({cstate_desc, cstate_filename} as cs) => {
  let cstate_desc =
    switch (cstate_desc) {
    | Initial(input) =>
      let (name, lexbuf, cleanup) =
        switch (input) {
        | InputString(str) => (
            cs.cstate_filename,
            Lexing.from_string(str),
            (() => ()),
          )
        | InputFile(name) =>
          let ic = open_in(name);
          (Some(name), Lexing.from_channel(ic), (() => close_in(ic)));
        };

      let parsed =
        try(Driver.parse(~name?, lexbuf)) {
        | _ as e =>
          cleanup();
          raise(e);
        };

      cleanup();
      Parsed(parsed);
    | Parsed(p) =>
      /*WithLibraries(Grain_stdlib.load_libraries p)*/
      WithLibraries(p)
    | WithLibraries(full_p) =>
      Well_formedness.check_well_formedness(full_p);
      WellFormed(full_p);
    | WellFormed(full_p) => TypeChecked(Typemod.type_implementation(full_p))
    | TypeChecked(typed_mod) =>
      Linearized(Linearize.transl_anf_module(typed_mod))
    | Linearized(anfed) =>
      if (Grain_utils.Config.optimizations_enabled^) {
        Optimized(Optimize.optimize_program(anfed));
      } else {
        Optimized(anfed);
      }
    | Optimized(optimized) =>
      Mashed(Transl_anf.transl_anf_program(optimized))
    | Mashed(mashed) =>
      Compiled(Compmod.compile_wasm_module(~name=?cstate_filename, mashed))
    | Compiled(compiled) =>
      switch (cs.cstate_outfile) {
      | Some(outfile) => Emitmod.emit_module(compiled, outfile)
      | None => ()
      };
      Binaryen.Module.dispose(compiled.asm);
      Assembled;
    | Assembled => Assembled
    };

  let ret = {...cs, cstate_desc};
  log_state(ret);
  ret;
};

let rec compile_resume = (~hook=?, s: compilation_state) => {
  let next_state = next_state(s);
  switch (hook) {
  | Some(func) =>
    switch (func(next_state)) {
    | Continue({cstate_desc: Assembled} as s) => s
    | Continue(s) => compile_resume(~hook?, s)
    | Stop => next_state
    }
  | None =>
    switch (next_state.cstate_desc) {
    | Assembled => next_state
    | _ => compile_resume(~hook?, next_state)
    }
  };
};

let compile_string = (~hook=?, ~name=?, ~outfile=?, ~reset=true, str) => {
  if (reset) {
    Env.clear_imports();
  };
  let cstate = {
    cstate_desc: Initial(InputString(str)),
    cstate_filename: name,
    cstate_outfile: outfile,
  };
  compile_resume(~hook?, cstate);
};

let compile_file = (~hook=?, ~outfile=?, ~reset=true, filename) => {
  if (reset) {
    Env.clear_imports();
  };
  let cstate = {
    cstate_desc: Initial(InputFile(filename)),
    cstate_filename: Some(filename),
    cstate_outfile: outfile,
  };
  compile_resume(~hook?, cstate);
};

let stop_after_parse =
  fun
  | {cstate_desc: Parsed(_)} => Stop
  | s => Continue(s);

let stop_after_libraries =
  fun
  | {cstate_desc: WithLibraries(_)} => Stop
  | s => Continue(s);

let stop_after_well_formed =
  fun
  | {cstate_desc: WellFormed(_)} => Stop
  | s => Continue(s);

let stop_after_typed =
  fun
  | {cstate_desc: TypeChecked(_)} => Stop
  | s => Continue(s);

let stop_after_anf =
  fun
  | {cstate_desc: Linearized(_)} => Stop
  | s => Continue(s);

let stop_after_optimization =
  fun
  | {cstate_desc: Optimized(_)} => Stop
  | s => Continue(s);

let stop_after_mashed =
  fun
  | {cstate_desc: Mashed(_)} => Stop
  | s => Continue(s);

let stop_after_compiled =
  fun
  | {cstate_desc: Compiled(_)} => Stop
  | s => Continue(s);

let anf = Linearize.transl_anf_module;

let save_mashed = (f, outfile) =>
  switch (compile_file(~hook=stop_after_mashed, f)) {
  | {cstate_desc: Mashed(mashed)} =>
    Grain_utils.Files.ensure_parent_directory_exists(outfile);
    let mash_string =
      Sexplib.Sexp.to_string_hum @@ Mashtree.sexp_of_mash_program(mashed);
    let oc = open_out(outfile);
    output_string(oc, mash_string);
    close_out(oc);
  | _ => failwith("Should be impossible")
  };

let free_vars = anfed => Ident.Set.elements @@ Anf_utils.anf_free_vars(anfed);

let () =
  Env.compile_module_dependency :=
    (
      (input, outfile) => ignore(compile_file(~outfile, ~reset=false, input))
    );
