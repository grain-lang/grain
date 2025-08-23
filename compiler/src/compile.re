open Grain_parsing;
open Grain_typed;
open Grain_middle_end;
open Grain_codegen;
open Grain_utils;
open Optimize;

type input_source =
  | InputString(string)
  | InputFile(string);

type compilation_state_desc =
  | Initial(input_source)
  | Parsed(Parsetree.parsed_program)
  | WellFormed(Parsetree.parsed_program)
  | TypeChecked(Typedtree.typed_program)
  | TypedWellFormed(Typedtree.typed_program)
  | Linearized(Anftree.anf_program)
  | Optimized(Anftree.anf_program)
  | Mashed(Mashtree.mash_program)
  | ObjectEmitted;

type compilation_state = {
  cstate_desc: compilation_state_desc,
  cstate_filename: option(string),
  cstate_object_outfile: option(string),
  cstate_wasm_outfile: option(string),
};

type compilation_action =
  | Continue(compilation_state)
  | Stop;

type error =
  | Cannot_parse_inline_flags(string)
  | Cannot_use_help_or_version;

exception InlineFlagsError(Location.t, error);

let default_wasm_filename = name =>
  Filepath.String.replace_extension(name, "wasm");
let default_object_filename = name =>
  Filepath.String.replace_extension(name, "gro");
let default_mashtree_filename = name =>
  Filepath.String.replace_extension(name, "mashtree");

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
    | ObjectEmitted => prerr_string("\nObject emitted successfully")
    };
    prerr_string("\n\n");
  };

let next_state = ({cstate_desc, cstate_filename} as cs) => {
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
        ~no_exception_mod=has_attr("noExceptions"),
      );

      Well_formedness.check_well_formedness(p);
      WellFormed(p);
    | WellFormed(p) => TypeChecked(Typemod.type_implementation(p))
    | TypeChecked(typed_mod) =>
      Typed_well_formedness.check_well_formedness(typed_mod);
      TypedWellFormed(typed_mod);
    | TypedWellFormed(typed_mod) =>
      Linearized(Linearize.transl_anf_module(typed_mod))
    | Linearized(anfed) => Optimized(Optimize.optimize_program(anfed))
    | Optimized(optimized) =>
      let mashed = Transl_anf.transl_anf_program(optimized);
      if (Config.debug^) {
        save_mashed(mashed, cs.cstate_object_outfile);
      };
      Mashed(mashed);
    | Mashed(mashed) =>
      switch (cs.cstate_object_outfile) {
      | Some(outfile) => Emitmod.emit_object(mashed, outfile)
      | None => ()
      };
      ObjectEmitted;
    | ObjectEmitted => ObjectEmitted
    };

  let ret = {
    ...cs,
    cstate_desc,
  };
  log_state(ret);
  ret;
};

let rec compile_resume = (~hook=?, s: compilation_state) => {
  let next_state = next_state(s);
  switch (hook) {
  | Some(func) =>
    switch (func(next_state)) {
    | Continue({cstate_desc: ObjectEmitted} as s) => s
    | Continue(s) => compile_resume(~hook?, s)
    | Stop => next_state
    }
  | None =>
    switch (next_state.cstate_desc) {
    | ObjectEmitted => next_state
    | _ => compile_resume(~hook?, next_state)
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

let stop_after_object_emitted =
  fun
  | {cstate_desc: ObjectEmitted} => Stop
  | s => Continue(s);

let compile_wasi_polyfill = () => {
  switch (Grain_utils.Config.wasi_polyfill^) {
  | Some(file) =>
    Grain_utils.Config.preserve_config(() => {
      Grain_utils.Config.compilation_mode := Grain_utils.Config.Runtime;
      let cstate = {
        cstate_desc: Initial(InputFile(file)),
        cstate_filename: Some(file),
        cstate_wasm_outfile: Some(default_wasm_filename(file)),
        cstate_object_outfile: Some(default_object_filename(file)),
      };
      ignore(compile_resume(~hook=stop_after_object_emitted, cstate));
    })
  | None => ()
  };
};

let reset_compiler_state = () => {
  Driver.reset();
  Ident.setup();
  Ctype.reset_levels();
  Env.clear_persistent_structures();
  Module_resolution.clear_dependency_graph();
  Grain_utils.Fs_access.flush_all_cached_data();
  Grain_utils.Warnings.reset_warnings();
};

let compile_string = (~hook=?, ~name=?, ~outfile=?, str) => {
  Ident.setup();
  let cstate = {
    cstate_desc: Initial(InputString(str)),
    cstate_filename: name,
    cstate_wasm_outfile: outfile,
    cstate_object_outfile: Option.map(default_object_filename, outfile),
  };
  Grain_utils.Config.preserve_all_configs(() =>
    compile_resume(~hook?, cstate)
  );
};

let compile_file = (~hook=?, ~outfile=?, filename) => {
  Ident.setup();
  let cstate = {
    cstate_desc: Initial(InputFile(filename)),
    cstate_filename: Some(filename),
    cstate_wasm_outfile: outfile,
    cstate_object_outfile: Option.map(default_object_filename, outfile),
  };
  Grain_utils.Config.preserve_all_configs(() =>
    compile_resume(~hook?, cstate)
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
