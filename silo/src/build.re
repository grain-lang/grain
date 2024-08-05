open Ansi;
open Toml;

type profile =
  | Dev
  | Release;

let grainc_command = (~profile, config) => {
  open Config;

  // TODO: replace with where the binary will be
  let cmd = ref(["grainc"]);

  // build flags

  switch (config.build.elide_type_info) {
  | Some(true) => cmd := ["--elide-type-info", ...cmd^]
  | _ => ()
  };

  switch (config.build.include_dirs) {
  | Some(dirs) =>
    cmd := List.fold_left((cmd, dir) => [dir, "-I", ...cmd], cmd^, dirs)
  | _ => ()
  };

  switch (config.build.import_memory) {
  | Some(true) => cmd := ["--import-memory", ...cmd^]
  | _ => ()
  };

  switch (config.build.initial_memory_pages) {
  | Some(pages) =>
    cmd := [string_of_int(pages), "--initial-memory-pages", ...cmd^]
  | _ => ()
  };

  switch (config.build.maximum_memory_pages) {
  | Some(pages) =>
    cmd := [string_of_int(pages), "--maximum-memory-pages", ...cmd^]
  | _ => ()
  };

  switch (config.build.memory_base) {
  | Some(pages) => cmd := [string_of_int(pages), "--memory-base", ...cmd^]
  | _ => ()
  };

  switch (config.build.gc) {
  | Some(false) => cmd := ["--no-gc", ...cmd^]
  | _ => ()
  };

  switch (config.build.stdlib) {
  | Some(stdlib) => cmd := [stdlib, "--stdlib", ...cmd^]
  | _ => ()
  };

  switch (config.build.use_start_section) {
  | Some(true) => cmd := ["--use-start-section", ...cmd^]
  | _ => ()
  };

  switch (config.build.wasi_polyfill) {
  | Some(wasi_polyfill) => cmd := [wasi_polyfill, "--wasi-polyfill", ...cmd^]
  | _ => ()
  };

  // bin flags

  switch (config.bin.wat) {
  | Some(true) => cmd := ["--wat", ...cmd^]
  | _ => ()
  };

  switch (config.bin.source_map) {
  | Some(true) => cmd := ["--source-map", ...cmd^]
  | _ => ()
  };

  // output name
  switch (config.bin.name) {
  | Some(name) => cmd := [name, "-o", ...cmd^]
  | _ => cmd := [config.package.name ++ ".wasm", "-o", ...cmd^]
  };

  // profile
  switch (profile) {
  | Release => cmd := ["release", "--profile", ...cmd^]
  | Dev => ()
  };

  // path to entrypoint
  switch (config.bin.path) {
  | Some(path) => cmd := [path, ...cmd^]
  | _ => cmd := ["src/main.gr", ...cmd^]
  };

  Array.of_list(List.rev(cmd^));
};

let exec = args => {
  let stdin = Unix.open_process_args_out(args[0], args);
  let status = Unix.close_process_out(stdin);

  switch (status) {
  | Unix.WEXITED(code) => code
  | _ => failwith("process did not exit properly")
  };
};

let time = f => {
  let start_time = Unix.gettimeofday();
  let status = f();
  let end_time = Unix.gettimeofday();
  if (status == 0) {
    Printf.printf(
      "%s build in %.2fs\n",
      ansi(~bold=true, ~color=GreenBright, "Finished"),
      end_time -. start_time,
    );
  };
  status;
};

let build = profile => {
  let config = Config.load();
  let status = time(() => exec(grainc_command(~profile, config)));
  exit(status);
};
