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
let runtimefile = name =>
  Filepath.to_string(Fp.At.(test_runtime_dir / (name ++ ".gr")));
let wasmfile = name =>
  Filepath.to_string(Fp.At.(test_output_dir / (name ++ ".gr.wasm")));
let watfile = name =>
  Filepath.to_string(Fp.At.(test_output_dir / (name ++ ".gr.wat")));

let grainfmt_out_file = name =>
  Filepath.to_string(Fp.At.(test_grainfmt_dir / (name ++ ".expected.gr")));

let grainfmt_in_file = name =>
  Filepath.to_string(Fp.At.(test_grainfmt_dir / (name ++ ".input.gr")));

let graindoc_out_file = name =>
  Filepath.to_string(Fp.At.(test_gaindoc_dir / (name ++ ".expected.md")));

let gaindoc_in_file = name =>
  Filepath.to_string(Fp.At.(test_gaindoc_dir / (name ++ ".input.gr")));

let compile = (~num_pages=?, ~max_pages=?, ~config_fn=?, ~hook=?, name, prog) => {
  Config.preserve_all_configs(() => {
    Config.with_config(
      Config.empty,
      () => {
        switch (config_fn) {
        | Some(fn) => fn()
        | None => ()
        };
        switch (num_pages) {
        | Some(pages) => Config.initial_memory_pages := pages
        | None => ()
        };
        Config.maximum_memory_pages := max_pages;
        Config.include_dirs :=
          [Filepath.to_string(test_libs_dir), ...Config.include_dirs^];
        let outfile = wasmfile(name);
        compile_string(~is_root_file=true, ~hook?, ~name, ~outfile, prog);
      },
    )
  });
};

let compile_file =
    (~num_pages=?, ~max_pages=?, ~config_fn=?, ~hook=?, filename, outfile) => {
  Config.preserve_all_configs(() => {
    Config.with_config(
      Config.empty,
      () => {
        switch (config_fn) {
        | Some(fn) => fn()
        | None => ()
        };
        switch (num_pages) {
        | Some(pages) => Config.initial_memory_pages := pages
        | None => ()
        };
        Config.maximum_memory_pages := max_pages;
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

let open_process = (~stdin_input=?, args) => {
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

  switch (stdin_input) {
  | None => ()
  | Some(input) =>
    output_string(stdin, input);
    flush(stdin);
  };

  let current_time = Unix.time();

  let out_eof = ref(false);
  let err_eof = ref(false);

  let out_buf = Buffer.create(1024);
  let err_buf = Buffer.create(1024);

  // Windows buffers output, so read channels as the subprocess is running
  while ((! out_eof^ || ! err_eof^) && Unix.time() < current_time +. 15.) {
    try(Buffer.add_channel(out_buf, stdout, 1024)) {
    | End_of_file => out_eof := true
    };
    try(Buffer.add_channel(err_buf, stderr, 1024)) {
    | End_of_file => err_eof := true
    };
  };

  let timed_out = Unix.time() > current_time +. 15.;

  let status = Unix.close_process_full((stdout, stdin, stderr));
  let out = Buffer.contents(out_buf);
  let err = Buffer.contents(err_buf);

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

let run = (~extra_args=[||], file) => {
  let stdlib = Option.get(Grain_utils.Config.stdlib_dir^);

  let preopen =
    Printf.sprintf(
      "--dir=%s=%s",
      "/test/test-data",
      Filepath.to_string(test_data_dir),
    );

  let extra_args =
    Array.map(
      arg =>
        switch (Sys.win32, arg) {
        | (true, "--") => "`--"
        | _ => arg
        },
      extra_args,
    );
  let cmd =
    Array.concat([
      [|"grain", "run"|],
      [|"-S", stdlib, "-I", Filepath.to_string(test_libs_dir), preopen|],
      [|file|],
      extra_args,
    ]);

  let (code, out, err) = open_process(cmd);

  (out ++ err, code);
};

let format = file => {
  let cmd = [|"grain", "format", file|];

  let (code, out, err) = open_process(cmd);

  (out ++ err, code);
};

let doc = (file, arguments) => {
  let cmd = Array.concat([[|"grain", "doc", file|], arguments]);

  let (code, out, err) = open_process(cmd);

  (out ++ err, code);
};

let lsp = stdin_input => {
  let cmd = [|"grain", "lsp"|];

  let (code, out, err) = open_process(~stdin_input, cmd);

  (out ++ err, code);
};

let module_header = "module Test; ";

let makeSnapshotRunner =
    (~config_fn=?, test, ~module_header=module_header, name, prog) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@
      compile(
        ~hook=stop_after_object_file_emitted,
        ~config_fn?,
        name,
        module_header ++ prog,
      );
      expect.file(watfile(name)).toMatchSnapshot();
    })
  });
};

let makeFilesizeRunner =
    (test, ~config_fn=?, ~module_header=module_header, name, prog, size) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@ compile(~config_fn?, name, module_header ++ prog);
      let ic = open_in_bin(wasmfile(name));
      let filesize = in_channel_length(ic);
      close_in(ic);
      expect.int(filesize).toBe(size);
    })
  });
};

let makeSnapshotFileRunner = (test, ~config_fn=?, name, filename) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      ignore @@
      compile_file(
        ~hook=stop_after_object_file_emitted,
        ~config_fn?,
        infile,
        outfile,
      );
      let file = watfile(name);
      expect.file(file).toMatchSnapshot();
    })
  });
};

let makeCompileErrorRunner =
    (test, ~module_header=module_header, name, prog, msg) => {
  test(
    name,
    ({expect}) => {
      let error =
        try(
          {
            ignore @@ compile(name, module_header ++ prog);
            "";
          }
        ) {
        | exn => Printexc.to_string(exn)
        };
      expect.string(error).toMatch(msg);
    },
  );
};

let makeWarningRunner =
    (test, ~module_header=module_header, name, prog, warning) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      Config.print_warnings := false;
      ignore @@ compile(name, module_header ++ prog);
      expect.ext.warning.toHaveTriggered(warning);
    })
  });
};

let makeNoWarningRunner = (test, ~module_header=module_header, name, prog) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      Config.print_warnings := false;
      ignore @@ compile(name, module_header ++ prog);
      expect.ext.warning.toHaveTriggeredNoWarnings();
    })
  });
};

let makeRunner =
    (
      test,
      ~num_pages=?,
      ~max_pages=?,
      ~config_fn=?,
      ~extra_args=?,
      ~module_header=module_header,
      name,
      prog,
      expected,
    ) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@
      compile(
        ~num_pages?,
        ~max_pages?,
        ~config_fn?,
        name,
        module_header ++ prog,
      );
      let (result, _) = run(~extra_args?, wasmfile(name));
      expect.string(result).toEqual(expected);
    })
  });
};

let makeErrorRunner =
    (
      test,
      ~check_exists=true,
      ~num_pages=?,
      ~max_pages=?,
      ~config_fn=?,
      ~module_header=module_header,
      name,
      prog,
      expected,
    ) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      ignore @@
      compile(
        ~num_pages?,
        ~max_pages?,
        ~config_fn?,
        name,
        module_header ++ prog,
      );
      let (result, _) = run(wasmfile(name));
      if (check_exists) {
        expect.string(result).toMatch(expected);
      } else {
        expect.string(result).not.toMatch(expected);
      };
    })
  });
};

let makeFileRunner =
    (test, ~num_pages=?, ~max_pages=?, ~config_fn=?, name, filename, expected) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      ignore @@
      compile_file(~num_pages?, ~max_pages?, ~config_fn?, infile, outfile);
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

let makeRuntimeRunner = (test, ~code=0, name) => {
  test(name, ({expect}) => {
    Config.preserve_all_configs(() => {
      // Run stdlib suites in release mode
      Config.profile := Some(Release);
      let infile = runtimefile(name);
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
      let strip_locs =
          (
            {attributes, module_name, statements, comments}: Parsetree.parsed_program,
          ) =>
        Parsetree.{
          attributes,
          module_name: {
            ...module_name,
            loc: Location.dummy_loc,
          },
          statements:
            List.map(
              location_stripper.toplevel(location_stripper),
              statements,
            ),
          comments: List.map(comment_loc_stripper, comments),
          prog_loc: Location.dummy_loc,
          prog_core_loc: Location.dummy_loc,
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
      let infile = grainfmt_in_file(filename);
      let (result, _) = format(infile);

      // we need do a binary content comparison to ensure the EOL is correct

      expect.ext.binaryFile(result).toBinaryMatch(grainfmt_out_file(name));
    },
  );
};

let makeGrainDocRunner = (test, name, filename, arguments) => {
  test(
    name,
    ({expect}) => {
      let infile = gaindoc_in_file(filename);
      let (result, _) = doc(infile, arguments);

      // we need do a binary content comparison to ensure the EOL is correct

      expect.ext.binaryFile(result).toBinaryMatch(graindoc_out_file(name));
    },
  );
};

let makeGrainDocErrorRunner = (test, name, filename, expected, arguments) => {
  test(
    name,
    ({expect}) => {
      let infile = gaindoc_in_file(filename);
      let (result, _) = doc(infile, arguments);
      expect.string(result).toMatch(expected);
    },
  );
};

let lsp_request = json => {
  let str = Yojson.Safe.to_string(json);
  let request_len = String.length(str);
  "Content-Length: " ++ string_of_int(request_len) ++ "\r\n" ++ str ++ "\r\n";
};

let lsp_expected_response = json => {
  let str = Yojson.Safe.to_string(json);
  let request_len = String.length(str);
  "Content-Length: " ++ string_of_int(request_len) ++ "\r\n\r\n" ++ str;
};

let lsp_input = (method, params) => {
  `Assoc([
    ("jsonrpc", `String("2.0")),
    ("id", `Int(1)),
    ("method", `String(method)),
    ("params", params),
  ]);
};

let lsp_notification = (method, params) => {
  `Assoc([
    ("jsonrpc", `String("2.0")),
    ("method", `String(method)),
    ("params", params),
  ]);
};

let lsp_success_response = result => {
  `Assoc([
    ("jsonrpc", `String("2.0")),
    ("id", `Int(1)),
    ("result", result),
    ("error", `Null),
  ]);
};

let lsp_setup_teardown_requests = (code_uri, code) => {
  let init_request =
    lsp_input(
      "initialize",
      Yojson.Safe.from_string(
        {|{"processId":1,"clientInfo":null,"locale":null,"rootUri":null,"trace":"off"}|},
      ),
    );

  let open_request =
    lsp_input(
      "textDocument/didOpen",
      `Assoc([
        (
          "textDocument",
          `Assoc([
            ("uri", `String(code_uri)),
            ("languageId", `String("grain")),
            ("version", `Int(1)),
            ("text", `String(code)),
          ]),
        ),
      ]),
    );

  let shutdown_request =
    Yojson.Safe.from_string({|{"jsonrpc":"2.0","id":1,"method":"shutdown"}|});

  let exit_request =
    Yojson.Safe.from_string({|{"jsonrpc":"2.0","id":1,"method":"exit"}|});

  (
    lsp_request(init_request) ++ lsp_request(open_request),
    lsp_request(shutdown_request) ++ lsp_request(exit_request),
  );
};

let assert_lsp_responses =
    (expect, expected_open_diagnostics, ~expected_output=?, result) => {
  let len = String.length(result);

  let expected_init_response =
    lsp_expected_response(
      lsp_success_response(
        Yojson.Safe.from_string(
          {|{"capabilities":{"documentFormattingProvider":true,"textDocumentSync":1,"hoverProvider":true,"definitionProvider":{"linkSupport":true},"typeDefinitionProvider":true,"referencesProvider":false,"documentSymbolProvider":false,"codeActionProvider":true,"codeLensProvider":{"resolveProvider":true},"documentHighlightProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false,"inlayHintProvider":{"resolveProvider":false}}}|},
        ),
      ),
    );
  let expected_open_response =
    lsp_expected_response(
      lsp_notification(
        "textDocument/publishDiagnostics",
        expected_open_diagnostics,
      ),
    );
  let expected_begin_response =
    expected_init_response ++ expected_open_response;
  let expected_begin_response_len = String.length(expected_begin_response);

  let actual_begin_response =
    String.sub(result, 0, expected_begin_response_len);
  expect.string(actual_begin_response).toEqual(expected_begin_response);

  let expected_exit_response =
    lsp_expected_response(lsp_success_response(`Null));
  let expected_exit_response_len = String.length(expected_exit_response);
  let actual_exit_response =
    String.sub(
      result,
      len - expected_exit_response_len,
      expected_exit_response_len,
    );
  expect.string(actual_exit_response).toEqual(expected_exit_response);

  switch (expected_output) {
  | None => ()
  | Some(expected_output) =>
    let expected_response =
      lsp_expected_response(lsp_success_response(expected_output));
    let actual_response =
      String.sub(
        result,
        expected_begin_response_len,
        len - expected_begin_response_len - expected_exit_response_len,
      );
    expect.string(actual_response).toEqual(expected_response);
  };
};

let makeLspRunner =
    (test, name, code_uri, code, request_params, expected_output) => {
  test(
    name,
    ({expect}) => {
      let (setup_request, teardown_request) =
        lsp_setup_teardown_requests(code_uri, code);

      let (result, code) =
        lsp(
          setup_request ++ lsp_request(request_params) ++ teardown_request,
        );

      assert_lsp_responses(
        expect,
        `Assoc([("uri", `String(code_uri)), ("diagnostics", `List([]))]),
        ~expected_output,
        result,
      );

      expect.int(code).toBe(0);
    },
  );
};

let makeLspDiagnosticsRunner =
    (test, name, code_uri, code, expected_diagnostics) => {
  test(
    name,
    ({expect}) => {
      let (init_request, close_request) =
        lsp_setup_teardown_requests(code_uri, code);

      let (result, code) = lsp(init_request ++ close_request);

      assert_lsp_responses(expect, expected_diagnostics, result);

      expect.int(code).toBe(0);
    },
  );
};
