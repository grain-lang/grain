open TestFramework;
open Grain.Compile;
open Grain_utils;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;

let test_dir = Filename.concat(Sys.getcwd(), "test");
let test_libs_dir = Filename.concat(test_dir, "test-libs");
let test_input_dir = Filename.concat(test_dir, "input");
let test_output_dir = Filename.concat(test_dir, "output");
let test_stdlib_dir = Filename.concat(test_dir, "stdlib");

let grainfile = name => Filename.concat(test_input_dir, name ++ ".gr");
let stdlibfile = name => Filename.concat(test_stdlib_dir, name ++ ".gr");
let wasmfile = name => Filename.concat(test_output_dir, name ++ ".gr.wasm");
let watfile = name => Filename.concat(test_output_dir, name ++ ".gr.wat");

let read_stream = cstream => {
  let buf = Bytes.create(2048);
  let i = ref(0);
  Stream.iter(
    c => {
      /* This stream doesn't seem to have an end and causes the runner to hang, so we have an arbitrary cap */
      if (i^ >= 2048) {
        failwith("Program output exceeds 2048 characters");
      };
      Bytes.set(buf, i^, c);
      incr(i);
    },
    cstream,
  );
  Bytes.to_string @@ Bytes.sub(buf, 0, i^);
};

let compile = (~hook=?, name, prog) => {
  Config.preserve_config(() => {
    Config.include_dirs := [test_libs_dir, ...Config.include_dirs^];
    let outfile = wasmfile(name);
    ignore @@ compile_string(~hook?, ~name, ~outfile, prog);
  });
};

let compile_file = (~hook=?, filename, outfile) => {
  Config.preserve_config(() => {
    Config.include_dirs := [test_libs_dir, ...Config.include_dirs^];
    ignore @@ compile_file(~hook?, ~outfile, filename);
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

let run = (~num_pages=?, file) => {
  let cli_flags = "-g";

  let stdlib = Option.get(Grain_utils.Config.stdlib_dir^);
  let env =
    switch (num_pages) {
    | Some(x) =>
      Array.append(Unix.environment()) @@
      [|
        Printf.sprintf("GRAIN_INIT_MEMORY_PAGES=%d", x),
        Printf.sprintf("GRAIN_MAX_MEMORY_PAGES=%d ", x),
      |]
    | None => Unix.environment()
    };

  let args = [
    "grain",
    cli_flags,
    "-S",
    stdlib,
    "-I",
    test_libs_dir,
    "run",
    file,
  ];
  let command = String.concat(" ", args);

  let (stdout, stdin, stderr) = Unix.open_process_full(command, env);

  let pid = Unix.process_full_pid((stdout, stdin, stderr));
  let (_, status) = Unix.waitpid([], pid);

  let out = read_stream(Stream.of_channel(stdout));
  let err = read_stream(Stream.of_channel(stderr));

  close_in(stdout);
  close_in(stderr);
  close_out(stdin);

  let code =
    switch (status) {
    | Unix.WEXITED(code) => code
    | _ => failwith("process did not exit properly")
    };

  (out ++ err, code);
};

let makeSnapshotRunner = (test, name, prog) => {
  test(
    name,
    ({expect}) => {
      compile(~hook=stop_after_object_file_emitted, name, prog);
      expect.file(watfile(name)).toMatchSnapshot();
    },
  );
};

let makeSnapshotFileRunner = (test, name, filename) => {
  test(
    name,
    ({expect}) => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
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
            compile(name, prog);
            "";
          }
        ) {
        | exn => Printexc.to_string(exn)
        };
      expect.string(error).toMatch(msg);
    },
  );
};

let makeRunner = (test, ~num_pages=?, name, prog, expected) => {
  test(
    name,
    ({expect}) => {
      let hook = Option.map(_ => stop_after_object_file_emitted, num_pages);
      compile(~hook?, name, prog);
      let (result, _) = run(~num_pages?, wasmfile(name));
      expect.string(result).toEqual(expected);
    },
  );
};

let makeErrorRunner =
    (test, ~check_exists=true, ~num_pages=?, name, prog, expected) => {
  test(
    name,
    ({expect}) => {
      let hook = Option.map(_ => stop_after_object_file_emitted, num_pages);
      compile(~hook?, name, prog);
      let (result, _) = run(~num_pages?, wasmfile(name));
      if (check_exists) {
        expect.string(result).toMatch(expected);
      } else {
        expect.string(result).not.toMatch(expected);
      };
    },
  );
};

let makeFileRunner = (test, name, filename, expected) => {
  test(
    name,
    ({expect}) => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      compile_file(infile, outfile);
      let (result, _) = run(outfile);
      expect.string(result).toEqual(expected);
    },
  );
};

let makeFileErrorRunner = (test, name, filename, expected) => {
  test(
    name,
    ({expect}) => {
      let infile = grainfile(filename);
      let outfile = wasmfile(name);
      compile_file(infile, outfile);
      let (result, _) = run(outfile);
      expect.string(result).toMatch(expected);
    },
  );
};

let makeStdlibRunner = (test, ~code=0, name) => {
  test(
    name,
    ({expect}) => {
      let infile = stdlibfile(name);
      let outfile = wasmfile(name);
      compile_file(infile, outfile);
      let (result, exit_code) = run(outfile);
      expect.int(exit_code).toBe(code);
      expect.string(result).toEqual("");
    },
  );
};

let parse = (name, lexbuf) => {
  let ret = Grain_parsing.Driver.parse(~name, lexbuf);
  open Grain_parsing;
  open Location;
  assert(ret.Parsetree.prog_loc.loc_start.pos_fname == name);
  ret;
};

let parseString = (name, s) => {
  let lexbuf = Lexing.from_string(s);
  parse(name, lexbuf);
};

let parseFile = (name, input_file) => {
  let lexbuf = Lexing.from_channel(input_file);
  parse(name, lexbuf);
};

let makeParseRunner =
    (test, name, input, expected: Grain_parsing.Parsetree.parsed_program) => {
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
      let parsed = strip_locs @@ parseString(name, input);
      let untagged = strip_locs @@ parsed;
      let conv = p =>
        Sexplib.Sexp.to_string_hum @@
        Grain_parsing.Parsetree.sexp_of_parsed_program(p);
      expect.string(conv(untagged)).toEqual(conv(expected));
    },
  );
};
