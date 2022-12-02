open TestFramework;
open WarningExtensions;
open BinaryFileExtensions;
open Grain.Compile;
open Grain_utils;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;

type customMatchers = {
  warning: warningExtensions,
  binaryFile: string => binaryFileExtensions,
};

let customMatchers = createMatcher => {
  warning: warningExtensions(createMatcher),
  binaryFile: str => binaryFileExtensions(str, createMatcher),
};

let grainfile = name =>
  Filepath.to_string(Fp.At.(test_input_dir / (name ++ ".gr")));
let stdlibfile = name =>
  Filepath.to_string(Fp.At.(test_stdlib_dir / (name ++ ".gr")));
let wasmfile = name =>
  Filepath.to_string(Fp.At.(test_output_dir / (name ++ ".gr.wasm")));
let watfile = name =>
  Filepath.to_string(Fp.At.(test_output_dir / (name ++ ".gr.wat")));

let formatter_out_file = name =>
  Filepath.to_string(Fp.At.(test_formatter_out_dir / (name ++ ".gr")));

let formatter_in_file = name =>
  Filepath.to_string(Fp.At.(test_formatter_in_dir / (name ++ ".gr")));

let read_channel = channel => {
  let buf = Buffer.create(2048);
  try(
    while (true) {
      Buffer.add_channel(buf, channel, 2048);
    }
  ) {
  | End_of_file => ()
  };
  Buffer.contents(buf);
};

let compile = (~num_pages=?, ~config_fn=?, ~hook=?, name, prog) => {
  Config.preserve_all_configs(() => {
    Config.with_config(
      Config.empty,
      () => {
        switch (config_fn) {
        | Some(fn) => fn()
        | None => ()
        };
        switch (num_pages) {
        | Some(pages) =>
          Config.initial_memory_pages := pages;
          Config.maximum_memory_pages := Some(pages);
        | None => ()
        };
        Config.include_dirs :=
          [Filepath.to_string(test_libs_dir), ...Config.include_dirs^];
        let outfile = wasmfile(name);
        compile_string(~is_root_file=true, ~hook?, ~name, ~outfile, prog);
      },
    )
  });
};

let compile_file = (~num_pages=?, ~config_fn=?, ~hook=?, filename, outfile) => {
  Config.preserve_all_configs(() => {
    Config.with_config(
      Config.empty,
      () => {
        switch (config_fn) {
        | Some(fn) => fn()
        | None => ()
        };
        switch (num_pages) {
        | Some(pages) =>
          Config.initial_memory_pages := pages;
          Config.maximum_memory_pages := Some(pages);
        | None => ()
        };
        Config.include_dirs :=
          [Filepath.to_string(test_libs_dir), ...Config.include_dirs^];
        compile_file(~is_root_file=true, ~hook?, ~outfile, filename);
      },
    )
  });
};

let extract_anf = ({cstate_desc}) =>
  switch (cstate_desc) {
  | Linearized(anf)
  | Optimized(anf) => anf
  | _ => raise(Invalid_argument("Expected ANF-containing state"))
  };

let compile_string_to_final_anf = (name, s) =>
  extract_anf(compile_string(~hook=stop_after_optimization, ~name, s));

let open_process = args => {
  // We need to run the tests in powershell on Windows to have the correct environment
  let program = Sys.win32 ? "powershell.exe" : "/usr/bin/env";

  // This differs based on the shell we are using
  let pre_command = [|Sys.win32 ? "-command" : "-c"|];

  // Powershell doesn't exit with the script's exit code so we need to do this
  let exit = Sys.win32 ? [|";", "exit", "$LastExitCode"|] : [||];

  let (stdout, stdin, stderr) =
    Unix.open_process_args_full(
      program,
      Array.concat([pre_command, args, exit]),
      Unix.environment(),
    );

  let pid = Unix.process_full_pid((stdout, stdin, stderr));
  let (status, timed_out) =
    try({
      let (_, status) = Test_utils.waitpid_timeout(15., pid);
      (status, false);
    }) {
    | Test_utils.Timeout =>
      // Windows only supports the `sigkill` signal
      Unix.kill(pid, Sys.sigkill);
      (Unix.WEXITED(-1), true);
    };

  let out = read_channel(stdout);
  let err = read_channel(stderr);

  close_in(stdout);
  close_in(stderr);
  close_out(stdin);

  let code =
    switch (status) {
    | Unix.WEXITED(code) => code
    | _ => failwith("process did not exit properly")
    };

  let out =
    if (timed_out) {
      "Timed out!\n" ++ out;
    } else {
      out;
    };

  (code, out, err);
};

let run = (~num_pages=?, file) => {
  let mem_flags =
    switch (num_pages) {
    | Some(x) => [|
        "--initial-memory-pages",
        string_of_int(x),
        "--maximum-memory-pages",
        string_of_int(x),
      |]
    | None => [||]
    };

  let stdlib = Option.get(Grain_utils.Config.stdlib_dir^);

  let cmd =
    Array.concat([
      [|"grain", "run"|],
      mem_flags,
      [|"-S", stdlib, "-I", Filepath.to_string(test_libs_dir)|],
      [|file|],
    ]);

  let (code, out, err) = open_process(cmd);

  (out ++ err, code);
};

let format = file => {
  let cmd = [|"grain", "format", file|];

  let (code, out, err) = open_process(cmd);

  (out ++ err, code);
};

let makeSnapshotRunner = (~config_fn=?, test, name, prog) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@
      compile(~hook=stop_after_object_file_emitted, ~config_fn?, name, prog);
      expect.file(watfile(name)).toMatchSnapshot();
    })
  });
};

let makeSnapshotFileRunner = (test, name, filename) => {
  test(
    name,
    ({expect}) => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      ignore @@
      compile_file(~hook=stop_after_object_file_emitted, infile, outfile);
      let file = watfile(name);
      expect.file(file).toMatchSnapshot();
    },
  );
};

let makeCompileErrorRunner = (test, name, prog, msg) => {
  test(
    name,
    ({expect}) => {
      let error =
        try(
          {
            ignore @@ compile(name, prog);
            "";
          }
        ) {
        | exn => Printexc.to_string(exn)
        };
      expect.string(error).toMatch(msg);
    },
  );
};

let makeWarningRunner = (test, name, prog, warning) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      Config.print_warnings := false;
      ignore @@ compile(name, prog);
      expect.ext.warning.toHaveTriggered(warning);
    })
  });
};

let makeNoWarningRunner = (test, name, prog) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      Config.print_warnings := false;
      ignore @@ compile(name, prog);
      expect.ext.warning.toHaveTriggeredNoWarnings();
    })
  });
};

let makeRunner = (test, ~num_pages=?, ~config_fn=?, name, prog, expected) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@ compile(~num_pages?, ~config_fn?, name, prog);
      let (result, _) = run(~num_pages?, wasmfile(name));
      expect.string(result).toEqual(expected);
    })
  });
};

let makeErrorRunner =
    (
      test,
      ~check_exists=true,
      ~num_pages=?,
      ~config_fn=?,
      name,
      prog,
      expected,
    ) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@ compile(~num_pages?, ~config_fn?, name, prog);
      let (result, _) = run(~num_pages?, wasmfile(name));
      if (check_exists) {
        expect.string(result).toMatch(expected);
      } else {
        expect.string(result).not.toMatch(expected);
      };
    })
  });
};

let makeFileRunner =
    (test, ~num_pages=?, ~config_fn=?, name, filename, expected) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      ignore @@ compile_file(~num_pages?, ~config_fn?, infile, outfile);
      let (result, _) = run(outfile);
      expect.string(result).toEqual(expected);
    })
  });
};

let makeFileCompileErrorRunner = (test, name, filename, expected) => {
  test(
    name,
    ({expect}) => {
      let error =
        try({
          let infile = grainfile(filename);
          let outfile = wasmfile(name);
          ignore @@ compile_file(infile, outfile);
          "";
        }) {
        | exn => Printexc.to_string(exn)
        };
      expect.string(error).toMatch(expected);
    },
  );
};

let makeFileErrorRunner = (test, name, filename, expected) => {
  test(
    name,
    ({expect}) => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      ignore @@ compile_file(infile, outfile);
      let (result, _) = run(outfile);
      expect.string(result).toMatch(expected);
    },
  );
};

let makeStdlibRunner = (test, ~code=0, name) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      // Run stdlib suites in release mode
      Config.profile := Some(Release);
      let infile = stdlibfile(name);
      let outfile = wasmfile(name);
      ignore @@ compile_file(infile, outfile);
      let (result, exit_code) = run(outfile);
      expect.int(exit_code).toBe(code);
      expect.string(result).toEqual("");
    })
  });
};

let parse = (name, lexbuf, source) => {
  let ret = Grain_parsing.Driver.parse(~name, lexbuf, source);
  open Grain_parsing;
  open Location;
  assert(ret.Parsetree.prog_loc.loc_start.pos_fname == name);
  ret;
};

let parseString = (name, s) => {
  let lexbuf = Sedlexing.Utf8.from_string(s);
  let source = () => s;
  parse(name, lexbuf, source);
};

let makeParseRunner =
    (
      ~keep_locs=false,
      test,
      name,
      input,
      expected: Grain_parsing.Parsetree.parsed_program,
    ) => {
  test(
    name,
    ({expect}) => {
      open Grain_parsing;
      let location_stripper = {
        ...Ast_mapper.default_mapper,
        location: (_, _) => Location.dummy_loc,
      };
      let comment_loc_stripper: Parsetree.comment => Parsetree.comment =
        comment => {
          switch (comment) {
          | Line(desc) => Line({...desc, cmt_loc: Location.dummy_loc})
          | Shebang(desc) => Shebang({...desc, cmt_loc: Location.dummy_loc})
          | Block(desc) => Block({...desc, cmt_loc: Location.dummy_loc})
          | Doc(desc) => Doc({...desc, cmt_loc: Location.dummy_loc})
          };
        };
      let strip_locs = ({statements, comments}: Parsetree.parsed_program) =>
        Parsetree.{
          statements:
            List.map(
              location_stripper.toplevel(location_stripper),
              statements,
            ),
          comments: List.map(comment_loc_stripper, comments),
          prog_loc: Location.dummy_loc,
        };
      let parsed =
        if (keep_locs) {
          parseString(name, input);
        } else {
          strip_locs @@ parseString(name, input);
        };
      let conv = p =>
        Sexplib.Sexp.to_string_hum @@
        Grain_parsing.Parsetree.sexp_of_parsed_program(p);
      expect.string(conv(parsed)).toEqual(conv(expected));
    },
  );
};

let makeFormatterRunner = (test, name, filename) => {
  test(
    name,
    ({expect}) => {
      let infile = formatter_in_file(filename);
      let (result, _) = format(infile);

      // we need do a binary content comparison to ensure the EOL is correct

      expect.ext.binaryFile(result).toBinaryMatch(formatter_out_file(name));
    },
  );
};
