open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_codegen;
open Grain_linking;
open Grain_utils;
open Optimize;

type input_source =
  | InputString(string)
  | InputFile(string);

type compilation_state_desc =
  | Initial(input_source)
  | Parsed(Parsetree.parsed_program)
  | WellFormed(Parsetree.parsed_program)
  | DependenciesCompiled(Parsetree.parsed_program)
  | TypeChecked(Typedtree.typed_program)
  | TypedWellFormed(Typedtree.typed_program)
  | Linearized(Anftree.anf_program)
  | Optimized(Anftree.anf_program)
  | Mashed(Mashtree.mash_program)
  | Compiled(Compmod.compiled_program)
  | ObjectFileEmitted(Compmod.compiled_program)
  | Linked(Compmod.compiled_program)
  | Assembled;

type compilation_state = {
  cstate_desc: compilation_state_desc,
  cstate_filename: option(string),
  cstate_outfile: option(string),
};

type compilation_action =
  | Continue(compilation_state)
  | Stop;

type error =
  | Cannot_parse_inline_flags(string)
  | Cannot_use_help_or_version;

exception InlineFlagsError(Location.t, error);

let default_output_filename = name => name ++ ".wasm";

let default_mashtree_filename = name =>
  Filepath.String.remove_extension(name) ++ ".mashtree";

let compile_prog = p =>
  Compcore.module_to_bytes @@ Compcore.compile_wasm_module(p);

let save_mashed = (mashed, outfile) => {
  switch (outfile) {
  | Some(outfile) =>
    let outfile = default_mashtree_filename(outfile);
    Grain_utils.Fs_access.ensure_parent_directory_exists(outfile);
    let mash_string =
      Sexplib.Sexp.to_string_hum @@ Mashtree.sexp_of_mash_program(mashed);
    let oc = open_out(outfile);
    output_string(oc, mash_string);
    close_out(oc);
  | None => ()
  };
};

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
    | WellFormed(_) => prerr_string("\nWell-Formedness passed")
    | DependenciesCompiled(_) => prerr_string("\nDependencies compiled")
    | TypeChecked(typed_mod) =>
      prerr_string("\nTyped program:\n");
      prerr_sexp(Grain_typed.Typedtree.sexp_of_typed_program, typed_mod);
    | TypedWellFormed(typed_mod) =>
      prerr_string("\nTyped well-formedness passed")
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
    | ObjectFileEmitted(compiled) => prerr_string("\nEmitted successfully")
    | Linked(linked) => prerr_string("\nLinked successfully")
    | Assembled => prerr_string("\nAssembled successfully")
    };
    prerr_string("\n\n");
  };

let next_state = (~is_root_file=false, {cstate_desc, cstate_filename} as cs) => {
  let cstate_desc =
    switch (cstate_desc) {
    | Initial(input) =>
      let (name, lexbuf, source, cleanup) =
        switch (input) {
        | InputString(str) => (
            cs.cstate_filename,
            Sedlexing.Utf8.from_string(str),
            (() => str),
            (() => ()),
          )
        | InputFile(name) =>
          let ic = open_in(name);
          let source = () => {
            let ic = open_in_bin(name);
            let source = really_input_string(ic, in_channel_length(ic));
            close_in(ic);
            source;
          };
          (
            Some(name),
            Sedlexing.Utf8.from_channel(ic),
            source,
            (() => close_in(ic)),
          );
        };

      let parsed =
        try(Driver.parse(~name?, lexbuf, source)) {
        | _ as e =>
          cleanup();
          raise(e);
        };

      cleanup();
      Parsed(parsed);
    | Parsed(p) =>
      let has_attr = name =>
        List.exists(
          attr => attr.Asttypes.attr_name.txt == name,
          p.attributes,
        );
      Grain_utils.Config.apply_attribute_flags(
        ~no_pervasives=has_attr("noPervasives"),
        ~runtime_mode=has_attr("runtimeMode"),
      );

      Well_formedness.check_well_formedness(p);
      WellFormed(p);
    | WellFormed(p) =>
      if (is_root_file) {
        let base_file = Option.value(~default="", cstate_filename);
        Module_resolution.compile_dependency_graph(
          ~base_file,
          Driver.read_imports(p),
        );
      };
      DependenciesCompiled(p);
    | DependenciesCompiled(p) => TypeChecked(Typemod.type_implementation(p))
    | TypeChecked(typed_mod) =>
      Typed_well_formedness.check_well_formedness(typed_mod);
      TypedWellFormed(typed_mod);
    | TypedWellFormed(typed_mod) =>
      Linearized(Linearize.transl_anf_module(typed_mod))
    | Linearized(anfed) => Optimized(Optimize.optimize_program(anfed))
    | Optimized(optimized) =>
      let mashed = Transl_anf.transl_anf_program(optimized);
      if (Config.debug^) {
        save_mashed(mashed, cs.cstate_outfile);
      };
      Mashed(mashed);
    | Mashed(mashed) =>
      Compiled(Compmod.compile_wasm_module(~name=?cstate_filename, mashed))
    | Compiled(compiled) =>
      switch (cs.cstate_outfile) {
      | Some(outfile) => Emitmod.emit_module(compiled, outfile)
      | None => ()
      };
      ObjectFileEmitted(compiled);
    | ObjectFileEmitted(compiled) =>
      Linked(Linkmod.statically_link_wasm_module(compiled))
    | Linked(linked) =>
      switch (cs.cstate_outfile) {
      | Some(outfile) => Emitmod.emit_module(linked, outfile)
      | None => ()
      };
      Assembled;
    | Assembled => Assembled
    };

  let ret = {...cs, cstate_desc};
  log_state(ret);
  ret;
};

let rec compile_resume = (~is_root_file=false, ~hook=?, s: compilation_state) => {
  let next_state = next_state(~is_root_file, s);
  switch (hook) {
  | Some(func) =>
    switch (func(next_state)) {
    | Continue({cstate_desc: Assembled} as s) => s
    | Continue(s) => compile_resume(~is_root_file, ~hook?, s)
    | Stop => next_state
    }
  | None =>
    switch (next_state.cstate_desc) {
    | Assembled => next_state
    | _ => compile_resume(~is_root_file, ~hook?, next_state)
    }
  };
};

let stop_after_parse =
  fun
  | {cstate_desc: Parsed(_)} => Stop
  | s => Continue(s);

let stop_after_well_formed =
  fun
  | {cstate_desc: WellFormed(_)} => Stop
  | s => Continue(s);

let stop_after_typed =
  fun
  | {cstate_desc: TypeChecked(_)} => Stop
  | s => Continue(s);

let stop_after_typed_well_formed =
  fun
  | {cstate_desc: TypedWellFormed(_)} => Stop
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
let stop_after_object_file_emitted =
  fun
  | {cstate_desc: ObjectFileEmitted(_)} => Stop
  | s => Continue(s);
let stop_after_linked =
  fun
  | {cstate_desc: Linked(_)} => Stop
  | s => Continue(s);
let stop_after_assembled =
  fun
  | {cstate_desc: Assembled} => Stop
  | s => Continue(s);

let compile_wasi_polyfill = () => {
  switch (Grain_utils.Config.wasi_polyfill^) {
  | Some(file) =>
    Grain_utils.Config.preserve_config(() => {
      Grain_utils.Config.compilation_mode := Grain_utils.Config.Runtime;
      let cstate = {
        cstate_desc: Initial(InputFile(file)),
        cstate_filename: Some(file),
        cstate_outfile: Some(default_output_filename(file)),
      };
      ignore(
        compile_resume(
          ~is_root_file=true,
          ~hook=stop_after_object_file_emitted,
          cstate,
        ),
      );
    })
  | None => ()
  };
};

let reset_compiler_state = () => {
  Driver.reset();
  Ident.setup();
  Ctype.reset_levels();
  Env.clear_imports();
  Module_resolution.clear_dependency_graph();
  Grain_utils.Fs_access.flush_all_cached_data();
  Grain_utils.Warnings.reset_warnings();
};

let compile_string =
    (~is_root_file=false, ~hook=?, ~name=?, ~outfile=?, ~reset=true, str) => {
  if (reset) {
    reset_compiler_state();
    compile_wasi_polyfill();
  };
  if (is_root_file) {
    Grain_utils.Config.set_root_config();
  };
  let cstate = {
    cstate_desc: Initial(InputString(str)),
    cstate_filename: name,
    cstate_outfile: outfile,
  };
  Grain_utils.Config.preserve_all_configs(() =>
    compile_resume(~is_root_file, ~hook?, cstate)
  );
};

let compile_file =
    (~is_root_file=false, ~hook=?, ~outfile=?, ~reset=true, filename) => {
  if (reset) {
    reset_compiler_state();
    compile_wasi_polyfill();
  };
  if (is_root_file) {
    Grain_utils.Config.set_root_config();
  };
  let cstate = {
    cstate_desc: Initial(InputFile(filename)),
    cstate_filename: Some(filename),
    cstate_outfile: outfile,
  };
  Grain_utils.Config.preserve_all_configs(() =>
    compile_resume(~is_root_file, ~hook?, cstate)
  );
};

let anf = Linearize.transl_anf_module;

let report_error = loc =>
  Location.(
    Printf.(
      fun
      | Cannot_parse_inline_flags(msg) =>
        errorf(~loc, "Failed to parse inline flags: %s", msg)
      | Cannot_use_help_or_version =>
        errorf(~loc, "The --help and --version flags cannot be set inline.")
    )
  );

let () =
  Location.register_error_of_exn(
    fun
    | InlineFlagsError(loc, err) => Some(report_error(loc, err))
    | _ => None,
  );

let () =
  Module_resolution.compile_module_dependency :=
    (
      (input, outfile) =>
        ignore(
          compile_file(
            ~is_root_file=false,
            ~outfile,
            ~reset=false,
            ~hook=stop_after_object_file_emitted,
            input,
          ),
        )
    );
