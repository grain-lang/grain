type config_opt =
  | Opt((ref('a), 'a)): config_opt;

type saved_config_opt =
  | SavedOpt((ref('a), 'a)): saved_config_opt;

type config = list(saved_config_opt);

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

let internal_opt: 'a. 'a => ref('a) =
  v => {
    let cur = ref(v);
    opts := [Opt((cur, v)), ...opts^];
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
    ~names: list(string)
  ) =>
  Cmdliner.Arg.info = (
  (~docs=?, ~docv=?, ~doc=?, ~env_docs=?, ~env_doc=?, ~env=?, ~names) => {
    let env =
      Option.map(
        e => {
          let (doc, docs) = (env_doc, env_docs);
          Cmdliner.Arg.(env_var(~docs?, ~doc?, e));
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
      ~names: list(string)
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
    let cur = internal_opt(v);
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
                ~names,
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
    let cur = internal_opt(default);
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
                    ~names,
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
    | Opt((cur, _)) => SavedOpt((cur, cur^));
  List.map(single_save, opts^);
};

let restore_config = {
  let single_restore =
    fun
    | SavedOpt((ptr, value)) => ptr := value;
  List.iter(single_restore);
};

let reset_config = () => {
  let single_reset =
    fun
    | Opt((cur, default)) => cur := default;
  List.iter(single_reset, opts^);
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
  let cmd = (
    process_used_cli_options(with_unapplied_cli_options(Term.const())),
    Term.info("grainc"),
  );
  // Remove grainc-flags prefix
  let len = String.length(flag_string) - 12;
  let flag_string = String.sub(flag_string, 12, len);
  let argv = Array.of_list(String.split_on_char(' ', flag_string));
  Term.eval(~argv, ~err, cmd);
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

let optimizations_enabled =
  toggle_flag(~doc="Disable optimizations.", ~names=["O0"], true);

let include_dirs =
  opt(
    ~names=["I"],
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

let principal =
  toggle_flag(
    ~names=["principal-types"],
    ~doc="Enable principal types",
    false,
  );

let compilation_mode =
  opt(
    ~names=["compilation-mode"],
    ~conv=
      option_conv(
        Cmdliner.Arg.enum([
          ("normal", "normal"),
          ("managed-runtime", "managed-runtime"),
          ("runtime", "runtime"),
          ("malloc", "malloc"),
        ]),
      ),
    ~doc="Compilation mode (advanced use only)",
    None,
  );

let recursive_types =
  toggle_flag(
    ~names=["recursive-types"],
    ~doc="Enable recursive types",
    false,
  );

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
    ~names=["g"],
    ~doc="Compile with debugging information",
    false,
  );

let verbose =
  toggle_flag(
    ~names=["cdebug"],
    ~doc="Print internal debug messages",
    false,
  );

let sexp_locs_enabled =
  toggle_flag(
    ~names=["hide-locs"],
    ~doc=
      "Hide locations from intermediate trees. Only has an effect with `--cdebug'.",
    true,
  );

let no_gc =
  toggle_flag(
    ~names=["no-gc"],
    ~doc="Turn off reference counting garbage collection.",
    false,
  );

let unsound_optimizations =
  toggle_flag(
    ~names=["Ounsound"],
    ~doc="Compile with optimizations which may remove runtime errors",
    false,
  );

let source_map =
  toggle_flag(~names=["source-map"], ~doc="Generate source maps", false);

let lsp_mode =
  toggle_flag(
    ~names=["lsp"],
    ~doc="Generate lsp errors and warnings only",
    false,
  );

/* To be filled in by grainc */
let base_path = internal_opt("");

let stdlib_directory = (): option(string) =>
  Option.map(path => Files.derelativize(path), stdlib_dir^);

let module_search_path = () =>
  switch (stdlib_directory()) {
  | Some(x) => [base_path^, ...include_dirs^] @ [x] /* stdlib goes last */
  | None => [base_path^, ...include_dirs^]
  };
