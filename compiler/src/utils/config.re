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
  | Spec(Cmdliner.Arg.t('a), ref('a)): config_spec;

let opts: ref(list(config_opt)) = (ref([]): ref(list(config_opt)));
let specs: ref(list(config_spec)) = (ref([]): ref(list(config_spec)));

let internal_opt: 'a. 'a => ref('a) =
  v => {
    let cur = ref(v);
    opts := [[@implicit_arity] Opt(cur, v), ...opts^];
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
        [@implicit_arity]
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
          cur,
        ),
        ...specs^,
      ];
    cur;
  };

let flag:
  'a.
  (~names: list(('a, Cmdliner.Arg.info)), ~default: 'a) => ref('a)
 =
  (~names, ~default as v) => {
    let cur = internal_opt(v);
    specs :=
      [
        [@implicit_arity] Spec(Cmdliner.Arg.(vflag(v, names)), cur),
        ...specs^,
      ];
    cur;
  };

let bool_flag:
  (~true_info: Cmdliner.Arg.info=?, ~false_info: Cmdliner.Arg.info=?, 'a) =>
  ref('a) = (
  (~true_info=?, ~false_info=?, default) => {
    let names = ref([]);
    Option.iter(i => names := [(true, i), ...names^], true_info);
    Option.iter(i => names := [(false, i), ...names^], false_info);
    switch (names^) {
    | [] => failwith("Internal error: bool_flag called with no info")
    | names => flag(~names, ~default)
    };
  }:
    (~true_info: Cmdliner.Arg.info=?, ~false_info: Cmdliner.Arg.info=?, 'a) =>
    ref('a)
);

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
        [@implicit_arity]
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
    | [@implicit_arity] Opt(cur, _) => [@implicit_arity] SavedOpt(cur, cur^);
  List.map(single_save, opts^);
};

let restore_config = {
  let single_restore =
    fun
    | [@implicit_arity] SavedOpt(ptr, value) => ptr := value;
  List.iter(single_restore);
};

let reset_config = () => {
  let single_reset =
    fun
    | [@implicit_arity] Opt(cur, default) => cur := default;
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
    | [@implicit_arity] Spec(arg, box) =>
      const((a, b) => {
        box := a;
        b;
      })
      $ Arg.value(arg)
      $ acc;
  let folded = List.fold_left(process_option, const(term), specs^);
  folded;
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

let unsound_optimizations =
  toggle_flag(
    ~names=["Ounsound"],
    ~doc="Compile with optimizations which may remove runtime errors",
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
