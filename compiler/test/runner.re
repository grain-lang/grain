open Unix;
open Filename;
open Str;
open Grain.Compile;
open Printf;
open OUnit2;
open Lexing;
open Grain_codegen;
open Grain_utils;

type either('a, 'b) =
  | Left('a)
  | Right('b);

let either_printer = e =>
  switch (e) {
  | Left(v) => sprintf("Error: %s\n", v)
  | Right(v) => v
  };

let exists = (check, result) =>
  try(Str.search_forward(Str.regexp_string(check), result, 0) >= 0) {
  | Not_found => false
  };

/* Read a file into a string */
let string_of_file = file_name => {
  let inchan = open_in(file_name);
  let buf = Bytes.create(in_channel_length(inchan));
  really_input(inchan, buf, 0, in_channel_length(inchan));
  buf;
};

let string_of_position = p =>
  sprintf(
    "%s:line %d, col %d",
    p.pos_fname,
    p.pos_lnum,
    p.pos_cnum - p.pos_bol,
  );

let parse = (name, lexbuf) => {
  let ret = Grain_parsing.Driver.parse(~name, lexbuf);
  open Grain_parsing;
  open Location;
  assert(ret.Parsetree.prog_loc.loc_start.pos_fname == name);
  ret;
};

let parse_string = (name, s) => {
  let lexbuf = Lexing.from_string(s);
  parse(name, lexbuf);
};

let parse_file = (name, input_file) => {
  let lexbuf = Lexing.from_channel(input_file);
  parse(name, lexbuf);
};

let extract_anf = ({cstate_desc}) =>
  switch (cstate_desc) {
  | Linearized(anf)
  | Optimized(anf) => anf
  | _ => raise(Invalid_argument("Expected ANF-containing state"))
  };

let compile_string_to_anf = (name, s) =>
  extract_anf(compile_string(~hook=stop_after_anf, ~name, s));

let compile_string_to_final_anf = (name, s) =>
  extract_anf(compile_string(~hook=stop_after_optimization, ~name, s));

let make_tmpfiles = name => {
  let (null_stdin, _) = pipe();
  let stdout_name = temp_file("stdout_" ++ name, ".out");
  let stdin_name = temp_file("stderr_" ++ name, ".err");
  (
    openfile(stdout_name, [O_RDWR], 0o600),
    stdout_name,
    openfile(stdin_name, [O_RDWR], 0o600),
    stdin_name,
    null_stdin,
  );
};

type result = either(string, string);

let extract_wasm = ({cstate_desc}) =>
  switch (cstate_desc) {
  | Compiled(compiled) => compiled
  | _ => raise(Invalid_argument("Expected WASM State"))
  };

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

let run_output = (~code=0, ~heap_size=?, cstate, test_ctxt) => {
  let program = extract_wasm(cstate);
  let file = Filename.temp_file("test", ".gr.wasm");
  Emitmod.emit_module(program, file);

  let stdlib = Option.get(Grain_utils.Config.stdlib_dir^);
  let testlibs = Sys.getcwd() ++ "/test-libs";
  let result = ref("");
  let heap_args =
    switch (heap_size) {
    | Some(x) => ["--limitMemory", string_of_int(x)]
    | None => []
    };
  assert_command(
    ~foutput=stream => result := read_stream(stream),
    ~exit_code=Unix.WEXITED(code),
    ~use_stderr=true,
    ~ctxt=test_ctxt,
    "grain",
    ["-pg", "-S", stdlib, "-I", testlibs] @ heap_args @ ["run", file],
  );
  result^;
};

let run_anf = (p, out) => {
  let cstate = {
    cstate_desc: Linearized(p),
    cstate_filename: Some(out),
    cstate_outfile: None,
  };
  run_output(compile_resume(~hook=stop_after_compiled, cstate));
};

let test_run =
    (~cmp=?, ~heap_size=?, program_str, outfile, expected, test_ctxt) => {
  let result =
    Config.preserve_config(() => {
      Config.include_dirs := ["test-libs", ...Config.include_dirs^];
      let cstate =
        compile_string(~hook=stop_after_compiled, ~name=outfile, program_str);
      run_output(~heap_size?, cstate, test_ctxt);
    });
  assert_equal(
    ~printer=Fun.id,
    ~cmp=Option.value(~default=(==), cmp),
    expected ++ "\n",
    result,
  );
};

let test_run_file = (~heap_size=?, filename, name, expected, test_ctxt) => {
  let input_filename = "input/" ++ filename ++ ".gr";
  let outfile = "output/" ++ name;
  let cstate =
    compile_file(~hook=stop_after_compiled, ~outfile, input_filename);
  let result = run_output(~heap_size?, cstate, test_ctxt);
  assert_equal(~printer=Fun.id, expected ++ "\n", result);
};

let test_run_stdlib = (~returns="void\n", ~code=?, filename, test_ctxt) => {
  let input_filename = "stdlib/" ++ filename ++ ".gr";
  let outfile = "stdlib_output/" ++ filename;
  let cstate =
    compile_file(~hook=stop_after_compiled, ~outfile, input_filename);
  let result = run_output(~code?, cstate, test_ctxt);
  assert_equal(~printer=Fun.id, returns, result);
};

let test_optimizations_sound = (program_str, name, expected, test_ctxt) => {
  let compile_and_run = () =>
    run_output(
      compile_string(~hook=stop_after_compiled, ~name, program_str),
      test_ctxt,
    );
  let result_unoptimized =
    Config.preserve_config(() => {
      Config.optimizations_enabled := false;
      compile_and_run();
    });
  let result_optimized =
    Config.preserve_config(() => {
      Config.optimizations_enabled := true;
      compile_and_run();
    });

  assert_equal(result_optimized, result_unoptimized);
  assert_equal(expected ++ "\n", result_optimized);
};

let test_file_optimizations_sound = (filename, name, expected, test_ctxt) => {
  let input_filename = "input/" ++ filename ++ ".gr";
  let full_outfile_unoptimized = "output/" ++ name ++ ".no-optimize";
  let full_outfile_optimized = "output/" ++ name ++ "optimize";

  let compile_and_run = outfile =>
    run_output(
      compile_file(~hook=stop_after_compiled, ~outfile, input_filename),
      test_ctxt,
    );
  let result_unoptimized =
    Config.preserve_config(() => {
      Config.optimizations_enabled := false;
      compile_and_run(full_outfile_unoptimized);
    });
  let result_optimized =
    Config.preserve_config(() => {
      Config.optimizations_enabled := true;
      compile_and_run(full_outfile_optimized);
    });

  assert_equal(result_optimized, result_unoptimized);
  assert_equal(expected ++ "\n", result_optimized);
};

let test_run_anf = (program_anf, outfile, expected, test_ctxt) => {
  let result = run_anf(program_anf, outfile, test_ctxt);
  assert_equal(expected ++ "\n", result, ~printer=Fun.id);
};

let test_err = (~heap_size=?, program_str, outfile, errmsg, test_ctxt) => {
  let result =
    try(
      Config.preserve_config(() => {
        Config.include_dirs := ["test-libs", ...Config.include_dirs^];
        let cstate =
          compile_string(
            ~hook=stop_after_compiled,
            ~name=outfile,
            program_str,
          );
        run_output(~heap_size?, cstate, test_ctxt);
      })
    ) {
    | exn => Printexc.to_string(exn)
    };

  assert_equal(errmsg, result, ~cmp=exists, ~printer=Fun.id);
};

let test_run_file_err = (filename, name, errmsg, test_ctxt) => {
  let input_filename = "input/" ++ filename ++ ".gr";
  let outfile = "output/" ++ name;
  let result =
    try({
      let cstate =
        compile_file(~hook=stop_after_compiled, ~outfile, input_filename);
      run_output(cstate, test_ctxt);
    }) {
    | exn => Printexc.to_string(exn)
    };

  assert_equal(errmsg, result, ~cmp=exists, ~printer=Fun.id);
};
