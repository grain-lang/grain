type digestable =
  | Digestable
  | NotDigestable;

type config_opt =
  | Opt((ref('a), 'a, digestable)): config_opt;

type saved_config_opt =
  | SavedOpt((ref('a), 'a, digestable)): saved_config_opt;

type digestable_opt =
  | DigestableOpt('a): digestable_opt;

type config = list(saved_config_opt);

let empty: config = [];

/* Here we model the API provided by cmdliner without introducing
   an explicit dependency on it (that's left to grainc). */
type config_info = {
  /** Title of the man page section to place arg under */
  docs: option(string),
  /** Variable name for non-flags */
  docv: option(string),
  /** Man page info for argument (see cmdliner documentation) */
  doc: option(string),
  /** Name of environment variable used to get default value. */
  env: option(string),
  /** Names which the flag can be referred to by */
  names: list(string),
};

type config_spec =
  | Spec(Cmdliner.Arg.t('a), list(string), ref('a)): config_spec;

let opts: ref(list(config_opt)) = (ref([]): ref(list(config_opt)));
let specs: ref(list(config_spec)) = (ref([]): ref(list(config_spec)));

let internal_opt: 'a. ('a, digestable) => ref('a) =
  (v, digestable) => {
    let cur = ref(v);
    opts := [Opt((cur, v, digestable)), ...opts^];
    cur;
  };

let arg_info:
  (
    ~docs: string=?,
    ~docv: string=?,
    ~doc: string=?,
    ~env_docs: string=?,
    ~env_doc: string=?,
    ~env: string=?,
    list(string)
  ) =>
  Cmdliner.Arg.info = (
  (~docs=?, ~docv=?, ~doc=?, ~env_docs=?, ~env_doc=?, ~env=?, names) => {
    let env =
      Option.map(
        e => {
          let (doc, docs) = (env_doc, env_docs);
          Cmdliner.Cmd.Env.info(~docs?, ~doc?, e);
        },
        env,
      );
    Cmdliner.Arg.(info(~docs?, ~docv?, ~doc?, ~env?, names));
  }:
    (
      ~docs: string=?,
      ~docv: string=?,
      ~doc: string=?,
      ~env_docs: string=?,
      ~env_doc: string=?,
      ~env: string=?,
      list(string)
    ) =>
    Cmdliner.Arg.info
);

let opt:
  'a.
  (
    ~docs: string=?,
    ~docv: string=?,
    ~doc: string=?,
    ~env_docs: string=?,
    ~env_doc: string=?,
    ~env: string=?,
    ~names: list(string),
    ~conv: Cmdliner.Arg.conv('a),
    'a
  ) =>
  ref('a)
 =
  (
    ~docs=?,
    ~docv=?,
    ~doc=?,
    ~env_docs=?,
    ~env_doc=?,
    ~env=?,
    ~names,
    ~conv as c,
    v,
  ) => {
    let cur = internal_opt(v, Digestable);
    specs :=
      [
        Spec(
          Cmdliner.Arg.(
            opt(
              c,
              v,
              arg_info(
                ~docs?,
                ~docv?,
                ~doc?,
                ~env_docs?,
                ~env_doc?,
                ~env?,
                names,
              ),
            )
          ),
          names,
          cur,
        ),
        ...specs^,
      ];
    cur;
  };

let toggle_flag:
  (
    ~docs: string=?,
    ~docv: string=?,
    ~doc: string=?,
    ~env_docs: string=?,
    ~env_doc: string=?,
    ~env: string=?,
    ~names: list(string),
    bool
  ) =>
  ref(bool) = (
  (~docs=?, ~docv=?, ~doc=?, ~env_docs=?, ~env_doc=?, ~env=?, ~names, default) => {
    let cur = internal_opt(default, Digestable);
    specs :=
      [
        Spec(
          Cmdliner.Arg.(
            vflag(
              default,
              [
                (
                  !default,
                  arg_info(
                    ~docs?,
                    ~docv?,
                    ~doc?,
                    ~env_docs?,
                    ~env_doc?,
                    ~env?,
                    names,
                  ),
                ),
              ],
            )
          ),
          names,
          cur,
        ),
        ...specs^,
      ];
    cur;
  }:
    (
      ~docs: string=?,
      ~docv: string=?,
      ~doc: string=?,
      ~env_docs: string=?,
      ~env_doc: string=?,
      ~env: string=?,
      ~names: list(string),
      bool
    ) =>
    ref(bool)
);

let save_config = () => {
  let single_save =
    fun
    | Opt((cur, _, digestable)) => SavedOpt((cur, cur^, digestable));
  List.map(single_save, opts^);
};

let restore_config = {
  let single_restore =
    fun
    | SavedOpt((ptr, value, _)) => ptr := value;
  List.iter(single_restore);
};

let reset_config = () => {
  let single_reset =
    fun
    | Opt((cur, default, _)) => cur := default;
  List.iter(single_reset, opts^);
};

let root_config = ref([]);
let root_config_digest = ref(None);

let set_root_config = () => {
  root_config := save_config();
  root_config_digest := None;
};

let get_root_config_digest = () => {
  switch (root_config_digest^) {
  | Some(dgst) => dgst
  | None =>
    let config_opts =
      root_config^
      |> List.filter((SavedOpt((_, _, digestable))) =>
           switch (digestable) {
           | Digestable => true
           | NotDigestable => false
           }
         )
      |> List.map((SavedOpt((_, opt, _))) => DigestableOpt(opt));
    let config = Marshal.to_bytes(config_opts, []);
    let ret = Digest.to_hex(Digest.bytes(config));
    root_config_digest := Some(ret);
    ret;
  };
};

let with_root_config = (c, thunk) => {
  // for test suite
  let saved = root_config^;
  let saved_digest = root_config_digest^;
  try(
    {
      root_config := c;
      let r = thunk();
      root_config := saved;
      root_config_digest := saved_digest;
      r;
    }
  ) {
  | exn =>
    root_config := saved;
    root_config_digest := saved_digest;
    raise(exn);
  };
};

let preserve_root_config = thunk => {
  // for test suite
  let saved = root_config^;
  let saved_digest = root_config_digest^;
  try({
    let r = thunk();
    root_config := saved;
    root_config_digest := saved_digest;
    r;
  }) {
  | exn =>
    root_config := saved;
    root_config_digest := saved_digest;
    raise(exn);
  };
};

let with_config = (c, thunk) => {
  /* Possible optimization: Only save the delta */
  let saved = save_config();
  try(
    {
      restore_config(c);
      let r = thunk();
      restore_config(saved);
      r;
    }
  ) {
  | exn =>
    restore_config(saved);
    raise(exn);
  };
};

let preserve_config = thunk => {
  let saved = save_config();
  try({
    let r = thunk();
    restore_config(saved);
    r;
  }) {
  | exn =>
    restore_config(saved);
    raise(exn);
  };
};

let preserve_all_configs = thunk =>
  preserve_root_config(() => preserve_config(thunk));

let with_cli_options = (term: 'a): Cmdliner.Term.t('a) => {
  open Cmdliner;
  open Term;
  let process_option = acc =>
    fun
    | Spec(arg, names, box) =>
      const((a, b) => {
        box := a;
        b;
      })
      $ Arg.value(arg)
      $ acc;
  let folded = List.fold_left(process_option, const(term), specs^);
  folded;
};

let with_unapplied_cli_options = (term: 'a): Cmdliner.Term.t('a) => {
  open Cmdliner;
  open Term;
  let process_option = acc =>
    fun
    | Spec(arg, names, box) => const((a, b) => {b}) $ Arg.value(arg) $ acc;
  let folded = List.fold_left(process_option, const(term), specs^);
  folded;
};

let process_used_cli_options = term => {
  open Cmdliner;
  open Term;
  let process_option = acc =>
    fun
    | Spec(arg, names, box) => {
        const((a, (_, used), b) => {
          if (List.fold_left(
                (acc, name) =>
                  acc
                  || List.mem("--" ++ name, used)
                  || List.mem("-" ++ name, used),
                false,
                names,
              )) {
            box := a;
          };
          b;
        })
        $ Arg.value(arg)
        $ with_used_args(term)
        $ acc;
      };
  let folded = List.fold_left(process_option, term, specs^);
  folded;
};

let apply_inline_flags = (~err, flag_string) => {
  open Cmdliner;
  let cmd =
    Cmd.v(
      Cmd.info("grainc"),
      process_used_cli_options(with_unapplied_cli_options(Term.const())),
    );
  // Remove grainc-flags prefix
  let len = String.length(flag_string) - 12;
  let flag_string = String.sub(flag_string, 12, len);
  let argv = Array.of_list(String.split_on_char(' ', flag_string));
  Cmd.eval_value(~argv, ~err, cmd);
};

let option_conv = ((prsr, prntr)) => (
  x =>
    switch (prsr(x)) {
    | `Ok(a) => `Ok(Some(a))
    | `Error(a) => `Error(a)
    },
  ppf =>
    fun
    | None => Format.fprintf(ppf, "<not set>")
    | Some(x) => prntr(ppf, x),
);

type profile =
  | Release;

let profile =
  opt(
    ~doc="Set a compilation profile.",
    ~names=["profile"],
    ~conv=option_conv(Cmdliner.Arg.enum([("release", Release)])),
    None,
  );

let default_memory_base = 0x400;

let memory_base =
  opt(
    ~doc="Set the start address for the Grain runtime heap.",
    ~names=["memory-base"],
    ~conv=option_conv(Cmdliner.Arg.int),
    None,
  );

let include_dirs =
  opt(
    ~names=["I", "include-dirs"],
    ~conv=Cmdliner.Arg.(list(dir)),
    ~doc="Extra library include directories",
    ~docv="DIR",
    [],
  );

let stdlib_dir =
  opt(
    ~names=["stdlib"],
    ~conv=option_conv(Cmdliner.Arg.string),
    ~doc="Path to the standard library (stdlib) directory",
    ~env="GRAIN_STDLIB",
    None,
  );

let color_enabled =
  toggle_flag(~names=["no-color"], ~doc="Disable colored output", true);

// TODO(#612): Add compiler flag when feature is complete or remove entirely
let principal = ref(false);

let initial_memory_pages =
  opt(
    ~names=["initial-memory-pages"],
    ~conv=Cmdliner.Arg.int,
    ~doc="Initial number of WebAssembly memory pages",
    64,
  );

let maximum_memory_pages =
  opt(
    ~names=["maximum-memory-pages"],
    ~conv=option_conv(Cmdliner.Arg.int),
    ~doc="Maximum number of WebAssembly memory pages",
    None,
  );

let compilation_mode =
  opt(
    ~names=["compilation-mode"],
    ~conv=
      option_conv(
        Cmdliner.Arg.enum([("normal", "normal"), ("runtime", "runtime")]),
      ),
    ~doc="Compilation mode (advanced use only)",
    None,
  );

let statically_link =
  toggle_flag(~names=["no-link"], ~doc="Disable static linking", true);

let experimental_tail_call =
  toggle_flag(
    ~names=["experimental-wasm-tail-call"],
    ~doc="Enables tail-call optimization",
    false,
  );

// TODO(#612): Add compiler flag when feature is complete or remove entirely
let recursive_types = ref(false);

let strict_sequence =
  toggle_flag(
    ~names=["strict-sequence"],
    ~doc="Enable strict sequencing",
    false,
  );

/* For now, leave this as true */
let safe_string = ref(true);

let parser_debug_level =
  opt(
    ~names=["parser-debug-level"],
    ~conv=Cmdliner.Arg.int,
    ~doc="Debugging level for parser output",
    0,
  );

let debug =
  toggle_flag(
    ~names=["debug"],
    ~doc="Compile with debugging information",
    false,
  );

let wat =
  toggle_flag(
    ~names=["wat"],
    ~doc="Additionally produce a WebAssembly Text (.wat) file",
    false,
  );

let verbose =
  toggle_flag(
    ~names=["verbose"],
    ~doc="Print critical information at various stages of compilation",
    false,
  );

let sexp_locs_enabled =
  toggle_flag(
    ~names=["hide-locs"],
    ~doc=
      "Hide locations from intermediate trees. Only has an effect with `--verbose'.",
    true,
  );

let no_pervasives =
  toggle_flag(
    ~names=["no-pervasives"],
    ~doc="Don't automatically import the Grain Pervasives module.",
    false,
  );

let no_gc =
  toggle_flag(
    ~names=["no-gc"],
    ~doc="Turn off reference counting garbage collection.",
    false,
  );

let bulk_memory =
  toggle_flag(
    ~names=["no-bulk-memory"],
    ~doc="Turn off Bulk Memory operations",
    true,
  );

let wasi_polyfill =
  opt(
    ~names=["wasi-polyfill"],
    ~conv=option_conv(Cmdliner.Arg.string),
    ~doc="Custom WASI implementation",
    None,
  );

let use_start_section =
  toggle_flag(
    ~names=["use-start-section"],
    ~doc="Replace the _start export with a start section during linking.",
    false,
  );

let elide_type_info =
  toggle_flag(
    ~names=["elide-type-info"],
    ~doc="Don't include runtime type information used by toString/print",
    false,
  );

let source_map =
  toggle_flag(~names=["source-map"], ~doc="Generate source maps", false);

let print_warnings = internal_opt(true, NotDigestable);

let stdlib_directory = (): option(string) =>
  Option.map(
    path => Filepath.(to_string(String.derelativize(path))),
    stdlib_dir^,
  );

let wasi_polyfill_path = (): option(string) =>
  Option.map(
    path => Filepath.(to_string(String.derelativize(path))),
    wasi_polyfill^,
  );

let module_search_path = () => {
  switch (stdlib_directory()) {
  | Some(x) => include_dirs^ @ [x] /* stdlib goes last */
  | None => include_dirs^
  };
};

let apply_inline_flags = (~on_error, cmt_content) =>
  if (Str.string_match(Str.regexp_string("grainc-flags"), cmt_content, 0)) {
    let err_buf = Buffer.create(80);
    let err = Format.formatter_of_buffer(err_buf);
    let result = apply_inline_flags(~err, cmt_content);
    switch (result) {
    | Ok(`Ok(_)) => ()
    | Ok(`Version)
    | Ok(`Help) => on_error(`Help)
    | Error(_) =>
      Format.pp_print_flush(err, ());
      on_error(`Message(Buffer.contents(err_buf)));
    };
  };

let with_inline_flags = (~on_error, cmt_content, thunk) => {
  preserve_config(() => {
    apply_inline_flags(~on_error, cmt_content);
    thunk();
  });
};

type implicit_opens =
  | Pervasives_mod
  | Gc_mod;

let get_implicit_opens = () => {
  let ret =
    if (no_pervasives^) {
      [];
    } else {
      [Pervasives_mod];
    };
  if (compilation_mode^ == Some("runtime")) {
    [];
  } else {
    // Pervasives goes first, just for good measure.
    List.rev([
      Gc_mod,
      ...ret,
    ]);
  };
};
