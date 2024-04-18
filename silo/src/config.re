open Toml;

type config_load_error =
  | ConfigNotFound
  | ConfigParseError(string, Parser.location)
  | ConfigMissingRequiredField(string);

exception LoadError(config_load_error);

type t = {
  package: package_config,
  build: build_config,
  bin: bin_config,
}
and package_config = {name: string}
and build_config = {
  elide_type_info: option(bool),
  include_dirs: option(list(string)),
  import_memory: option(bool),
  initial_memory_pages: option(int),
  maximum_memory_pages: option(int),
  memory_base: option(int),
  gc: option(bool),
  stdlib: option(string),
  use_start_section: option(bool),
  wasi_polyfill: option(string),
  wasm_features,
}
and wasm_features = {
  bulk_memory: option(bool),
  tail_call: option(bool),
}
and bin_config = {
  name: option(string),
  path: option(string),
  wat: option(bool),
  source_map: option(bool),
};

let load = () => {
  switch (Parser.from_filename("silo.toml")) {
  | `Ok(config) =>
    open Lenses;

    let package =
      switch (get(config, field("package"))) {
      | Some(package) => package
      | None => raise(LoadError(ConfigMissingRequiredField("[package]")))
      };

    let package_config = {
      name:
        switch (get(package, key("name") |-- string)) {
        | Some(name) => name
        | None =>
          raise(LoadError(ConfigMissingRequiredField("name of [package]")))
        },
    };

    let build = field("build");

    let wasm_features = build |-- field("wasm-features");
    let wasm_features = {
      bulk_memory:
        get(config, wasm_features |-- key("bulk-memory") |-- bool),
      tail_call: get(config, wasm_features |-- key("tail-call") |-- bool),
    };

    let build_config = {
      elide_type_info:
        get(config, build |-- key("elide-type-info") |-- bool),
      include_dirs:
        get(config, build |-- key("include-dirs") |-- array |-- strings),
      import_memory: get(config, build |-- key("import-memory") |-- bool),
      initial_memory_pages:
        get(config, build |-- key("initial-memory-pages") |-- int),
      maximum_memory_pages:
        get(config, build |-- key("maximum-memory-pages") |-- int),
      memory_base: get(config, build |-- key("memory-base") |-- int),
      gc: get(config, build |-- key("gc") |-- bool),
      stdlib: get(config, build |-- key("stdlib") |-- string),
      use_start_section:
        get(config, build |-- key("use-start-section") |-- bool),
      wasi_polyfill: get(config, build |-- key("wasi-polyfill") |-- string),
      wasm_features,
    };

    let bin = field("bin");
    let bin_config = {
      name: get(config, bin |-- key("name") |-- string),
      path: get(config, bin |-- key("path") |-- string),
      wat: get(config, bin |-- key("wat") |-- bool),
      source_map: get(config, bin |-- key("source-map") |-- bool),
    };

    {package: package_config, build: build_config, bin: bin_config};
  | `Error(msg, loc) => raise(LoadError(ConfigParseError(msg, loc)))
  | exception (Sys_error(_)) => raise(LoadError(ConfigNotFound))
  };
};

let _ =
  Printexc.register_printer(exn => {
    switch (exn) {
    | LoadError(error) =>
      switch (error) {
      | ConfigNotFound =>
        Some("no `silo.toml` found in the current directory")
      | ConfigParseError(msg, loc) =>
        Some(
          Printf.sprintf(
            "silo.toml parse error on line %d, column %d",
            loc.line,
            loc.column,
          ),
        )
      | ConfigMissingRequiredField(field) =>
        Some("silo.toml missing required field " ++ field)
      }

    | _ => None
    }
  });
