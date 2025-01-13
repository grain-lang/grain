// Digestible configurations are ones that affect Grain object files.
// Any config options that don't change object files should be marked
// NotDigestible to avoid unnecessary recompiles
type digestible =
  | Digestible
  | NotDigestible;

type config_opt =
  | Opt((ref('a), 'a, digestible)): config_opt;

type saved_config_opt =
  | SavedOpt((ref('a), 'a, digestible)): saved_config_opt;

type digestible_opt =
  | DigestibleOpt('a): digestible_opt;

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

let internal_opt: 'a. ('a, digestible) => ref('a) =
  (v, digestible) => {
    let cur = ref(v);
    opts := [Opt((cur, v, digestible)), ...opts^];
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
    ~digestible: digestible,
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
    ~digestible,
    ~names,
    ~conv as c,
    v,
  ) => {
    let cur = internal_opt(v, digestible);
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
    ~digestible: digestible,
    ~names: list(string),
    bool
  ) =>
  ref(bool) =
  (
    ~docs=?,
    ~docv=?,
    ~doc=?,
    ~env_docs=?,
    ~env_doc=?,
    ~env=?,
    ~digestible,
    ~names,
    default,
  ) => {
    let cur = internal_opt(default, digestible);
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
  };

let save_config = () => {
  let single_save =
    fun
    | Opt((cur, _, digestible)) => SavedOpt((cur, cur^, digestible));
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
      |> List.filter((SavedOpt((_, _, digestible))) =>
           switch (digestible) {
           | Digestible => true
           | NotDigestible => false
           }
         )
      |> List.map((SavedOpt((_, opt, _))) => DigestibleOpt(opt));
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
    ~digestible=Digestible,
    None,
  );

let default_memory_base = 0x400;

let memory_base =
  opt(
    ~doc="Set the start address for the Grain runtime heap.",
    ~names=["memory-base"],
    ~conv=option_conv(Cmdliner.Arg.int),
    ~digestible=Digestible,
    None,
  );

let include_dirs =
  opt(
    ~names=["I", "include-dirs"],
    ~conv=Cmdliner.Arg.(list(dir)),
    ~doc="Extra library include directories",
    ~docv="DIR",
    ~digestible=NotDigestible,
    [],
  );

let stdlib_dir =
  opt(
    ~names=["stdlib"],
    ~conv=option_conv(Cmdliner.Arg.string),
    ~doc="Path to the standard library (stdlib) directory",
    ~env="GRAIN_STDLIB",
    ~digestible=NotDigestible,
    None,
  );

let color_enabled =
  toggle_flag(
    ~names=["no-color"],
    ~doc="Disable colored output",
    ~digestible=NotDigestible,
    true,
  );

let initial_memory_pages =
  opt(
    ~names=["initial-memory-pages"],
    ~conv=Cmdliner.Arg.int,
    ~doc="Initial number of WebAssembly memory pages",
    ~digestible=NotDigestible,
    64,
  );

let maximum_memory_pages =
  opt(
    ~names=["maximum-memory-pages"],
    ~conv=option_conv(Cmdliner.Arg.int),
    ~doc="Maximum number of WebAssembly memory pages",
    ~digestible=NotDigestible,
    None,
  );

let import_memory =
  toggle_flag(
    ~names=["import-memory"],
    ~doc="Import the memory from `env.memory`",
    ~digestible=NotDigestible,
    false,
  );

type ignore_warning =
  | IgnoreAll
  | LetRecNonFunction
  | AmbiguousName
  | StatementType
  | NonreturningStatement
  | AllClausesGuarded
  | PartialMatch
  | UnusedMatch
  | UnusedPat
  | NonClosedRecordPattern
  | UnreachableCase
  | ShadowConstructor
  | NoCmiFile
  | FuncWasmUnsafe
  | FromNumberLiteral
  | UselessRecordSpread
  | PrintUnsafe
  | ToStringUnsafe
  | ArrayIndexNonInteger;

let ignore_warnings =
  opt(
    ~names=["ignore-warnings"],
    ~conv=
      Cmdliner.Arg.(
        list(
          enum([
            ("all", IgnoreAll),
            ("letRecNonFunction", LetRecNonFunction),
            ("ambiguousName", AmbiguousName),
            ("statementType", StatementType),
            ("nonreturningStatement", NonreturningStatement),
            ("allClausesGuarded", AllClausesGuarded),
            ("partialMatch", PartialMatch),
            ("unusedMatch", UnusedMatch),
            ("unusedPat", UnusedPat),
            ("nonClosedRecordPattern", NonClosedRecordPattern),
            ("unreachableCase", UnreachableCase),
            ("shadowConstructor", ShadowConstructor),
            ("noCmiFile", NoCmiFile),
            ("funcWasmUnsafe", FuncWasmUnsafe),
            ("fromNumberLiteral", FromNumberLiteral),
            ("uselessRecordSpread", UselessRecordSpread),
            ("printUnsafe", PrintUnsafe),
            ("toStringUnsafe", ToStringUnsafe),
            ("arrayIndexNonInteger", ArrayIndexNonInteger),
          ]),
        )
      ),
    ~doc="Compiler warnings to ignore",
    ~digestible=NotDigestible,
    [],
  );

type compilation_mode =
  | Normal /* Standard compilation with regular bells and whistles */
  | Runtime /* GC doesn't exist yet, allocations happen in runtime heap */;

let compilation_mode = internal_opt(Normal, NotDigestible);

let statically_link =
  toggle_flag(
    ~names=["no-link"],
    ~doc="Disable static linking",
    ~digestible=NotDigestible,
    true,
  );

let no_tail_call =
  toggle_flag(
    ~names=["no-wasm-tail-call"],
    ~doc="Disables tail-call optimization",
    ~digestible=Digestible,
    false,
  );

let strict_sequence =
  toggle_flag(
    ~names=["strict-sequence"],
    ~doc="Enable strict sequencing",
    ~digestible=NotDigestible,
    false,
  );

/* For now, leave this as true */
let safe_string = ref(true);

let debug =
  toggle_flag(
    ~names=["debug"],
    ~doc="Compile with debugging information",
    ~digestible=NotDigestible,
    false,
  );

let wat =
  toggle_flag(
    ~names=["wat"],
    ~doc="Additionally produce a WebAssembly Text (.wat) file",
    ~digestible=NotDigestible,
    false,
  );

let verbose =
  toggle_flag(
    ~names=["verbose"],
    ~doc="Print critical information at various stages of compilation",
    ~digestible=NotDigestible,
    false,
  );

let sexp_locs_enabled =
  toggle_flag(
    ~names=["hide-locs"],
    ~doc=
      "Hide locations from intermediate trees. Only has an effect with `--verbose'.",
    ~digestible=NotDigestible,
    true,
  );

let no_pervasives =
  toggle_flag(
    ~names=["no-pervasives"],
    ~doc="Don't automatically import the Grain Pervasives module.",
    ~digestible=Digestible,
    false,
  );

let no_gc =
  toggle_flag(
    ~names=["no-gc"],
    ~doc="Turn off reference counting garbage collection.",
    ~digestible=Digestible,
    false,
  );

let bulk_memory =
  toggle_flag(
    ~names=["no-bulk-memory"],
    ~doc="Turn off Bulk Memory operations",
    ~digestible=Digestible,
    true,
  );

let wasi_polyfill =
  opt(
    ~names=["wasi-polyfill"],
    ~conv=option_conv(Cmdliner.Arg.string),
    ~doc="Custom WASI implementation",
    ~digestible=NotDigestible,
    None,
  );

let use_start_section =
  toggle_flag(
    ~names=["use-start-section"],
    ~doc="Replace the _start export with a start section during linking.",
    ~digestible=NotDigestible,
    false,
  );

let elide_type_info =
  toggle_flag(
    ~names=["elide-type-info"],
    ~doc="Don't include runtime type information used by toString/print",
    ~digestible=Digestible,
    false,
  );

let source_map =
  toggle_flag(
    ~names=["source-map"],
    ~doc="Generate source maps",
    ~digestible=NotDigestible,
    false,
  );

let print_warnings = internal_opt(true, NotDigestible);

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
  compilation_mode := Normal;
  folded;
};

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

let apply_attribute_flags = (~no_pervasives as np, ~runtime_mode as rm) => {
  // Only apply options if attributes were explicitly given so as to not
  // unintentionally override options set previously e.g. compiling a
  // wasi-polyfill file in non-runtime-mode if @runtimeMode is not specified
  if (np) {
    no_pervasives := true;
  };
  if (rm) {
    compilation_mode := Runtime;
  };
};

let with_attribute_flags = (~no_pervasives, ~runtime_mode, thunk) => {
  preserve_config(() => {
    apply_attribute_flags(~no_pervasives, ~runtime_mode);
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
  if (compilation_mode^ == Runtime) {
    [];
  } else {
    // Pervasives goes first, just for good measure.
    List.rev([
      Gc_mod,
      ...ret,
    ]);
  };
};
