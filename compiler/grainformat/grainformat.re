open Cmdliner;
open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_formatting;
open Grain_utils.Filepath.Args;

[@deriving cmdliner]
type io_params = {
  /** Grain source file or directory of source files to format */
  [@pos 0] [@docv "FILE"]
  input: ExistingFileOrDirectory.t,
  /** Output file or directory */
  [@name "o"] [@docv "FILE"]
  output: option(MaybeExistingFileOrDirectory.t),
};

let get_program_string = filename => {
  let ic = open_in_bin(filename);
  let n = in_channel_length(ic);
  let source_buffer = Buffer.create(n);
  Buffer.add_channel(source_buffer, ic, n);
  close_in(ic);
  Buffer.contents(source_buffer);
};

let compile_parsed = filename => {
  let filename = Filepath.to_string(filename);
  let program_str = get_program_string(filename);
  switch (Fmt.parse_source(program_str)) {
  | Error(ParseError(exn)) =>
    let bt =
      if (Printexc.backtrace_status()) {
        Some(Printexc.get_backtrace());
      } else {
        None;
      };
    Grain_parsing.Location.report_exception(Stdlib.Format.err_formatter, exn);
    Option.iter(
      s =>
        if (Config.debug^) {
          prerr_string("Backtrace:\n");
          prerr_string(s);
          prerr_string("\n");
        },
      bt,
    );
    exit(2);
  | Ok((parsed_program, lines, eol)) => (parsed_program, lines, eol)
  | Error(InvalidCompilationState) => failwith("Invalid compilation state")
  };
};

let format_code =
    (
      ~eol,
      ~output=?,
      ~source: array(string),
      program: Parsetree.parsed_program,
    ) => {
  switch (output) {
  | Some(outfile) =>
    let outfile = Filepath.to_string(outfile);
    // TODO: This crashes if you do something weird like `-o stdout/map.gr/foo`
    // because `foo` doesn't exist so it tries to mkdir it and raises
    Fs_access.ensure_parent_directory_exists(outfile);
    let oc = Fs_access.open_file_for_writing(outfile);
    set_binary_mode_out(oc, true);
    Grain_formatting.Fmt.format(
      ~write=output_string(oc),
      ~source,
      ~eol,
      program,
    );
    close_out(oc);
  | None =>
    set_binary_mode_out(stdout, true);
    Grain_formatting.Fmt.format(~write=print_string, ~source, ~eol, program);
    flush(stdout);
  };
};

type run = {
  input_path: Fp.t(Fp.absolute),
  output_path: option(Fp.t(Fp.absolute)),
};

let enumerate_directory = (input_dir_path, output_dir_path) => {
  let all_files = Array.to_list(Fs_access.readdir(input_dir_path));
  let grain_files =
    List.filter(
      filepath => Filename.extension(Fp.toString(filepath)) == ".gr",
      all_files,
    );
  List.map(
    filepath => {
      // We relativize between the input directory and the full filepath
      // such that we can reconstruct the directory structure of the input directory
      let relative_path =
        Fp.relativizeExn(~source=input_dir_path, ~dest=filepath);
      let gr_basename = Option.get(Fp.baseName(relative_path));
      let dirname = Fp.dirName(relative_path);
      let md_relative_path = Fp.join(dirname, Fp.relativeExn(gr_basename));
      let output_path = Fp.join(output_dir_path, md_relative_path);
      {input_path: filepath, output_path: Some(output_path)};
    },
    grain_files,
  );
};

let enumerate_runs = opts =>
  switch (opts.input, opts.output) {
  | (File(input_file_path), None) =>
    `Ok([{input_path: input_file_path, output_path: None}])
  | (File(input_file_path), Some(Exists(File(output_file_path)))) =>
    `Ok([
      {input_path: input_file_path, output_path: Some(output_file_path)},
    ])
  | (File(input_file_path), Some(NotExists(output_file_path))) =>
    `Ok([
      {input_path: input_file_path, output_path: Some(output_file_path)},
    ])
  | (Directory(_), None) =>
    `Error((
      false,
      "Directory input must be used with `-o` flag to specify output directory",
    ))
  | (Directory(input_dir_path), Some(Exists(Directory(output_dir_path)))) =>
    `Ok(enumerate_directory(input_dir_path, output_dir_path))
  | (Directory(input_dir_path), Some(NotExists(output_dir_path))) =>
    `Ok(enumerate_directory(input_dir_path, output_dir_path))
  | (File(input_file_path), Some(Exists(Directory(output_dir_path)))) =>
    `Error((
      false,
      "Using a file as input cannot be combined with directory output",
    ))
  | (Directory(_), Some(Exists(File(_)))) =>
    `Error((
      false,
      "Using a directory as input cannot be written as a single file output",
    ))
  };

let grainformat = runs => {
  List.iter(
    ({input_path, output_path}) => {
      let (program, source, eol) = compile_parsed(input_path);
      try(format_code(~eol, ~output=?output_path, ~source, program)) {
      | exn =>
        Stdlib.Format.eprintf("@[%s@]@.", Printexc.to_string(exn));
        exit(2);
      };
    },
    runs,
  );
};

let cmd = {
  open Term;

  let doc = "Format Grain source";
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };

  Cmd.v(
    Cmd.info(Sys.argv[0], ~version, ~doc),
    Config.with_cli_options(grainformat)
    $ ret(const(enumerate_runs) $ io_params_cmdliner_term()),
  );
};

let () =
  switch (Cmd.eval_value(cmd)) {
  | Error(_) => exit(1)
  | _ => ()
  };
