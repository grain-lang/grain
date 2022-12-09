open Cmdliner;
open Grain;
open Compile;
open Grain_typed;
open Grain_utils;
open Grain_diagnostics;
open Grain_utils.Filepath.Args;

let () =
  Printexc.register_printer(exc =>
    switch (Grain_parsing.Location.error_of_exn(exc)) {
    | None => None
    | Some(`Already_displayed) => None
    | Some(`Ok(err)) =>
      let buf = Buffer.create(512);
      let formatter = Format.formatter_of_buffer(buf);
      Format.fprintf(
        formatter,
        "@[%a@]@.",
        Grain_parsing.Location.report_error,
        err,
      );
      Format.pp_print_flush(formatter, ());
      let s = Buffer.contents(buf);
      Buffer.reset(buf);
      Some(s);
    }
  );

[@deriving cmdliner]
type io_params = {
  /** Grain source file or directory of source files to document */
  [@pos 0] [@docv "FILE"]
  input: ExistingFileOrDirectory.t,
  /** Output file or directory */
  [@name "o"] [@docv "FILE"]
  output: option(MaybeExistingFileOrDirectory.t),
};

[@deriving cmdliner]
type params = {
  /**
    The version to use as current when generating markdown for `@since` and `@history` attributes.
    Any future versions will be replace with `next` in the output.
  */
  [@name "current-version"] [@docv "VERSION"]
  current_version: option(string),
};

let compile_typed = (input: Fp.t(Fp.absolute)) => {
  switch (
    Compile.compile_file(
      ~is_root_file=true,
      ~hook=stop_after_typed,
      Filepath.to_string(input),
    )
  ) {
  | exception exn =>
    let bt =
      if (Printexc.backtrace_status()) {
        Some(Printexc.get_backtrace());
      } else {
        None;
      };
    Grain_parsing.Location.report_exception(Format.err_formatter, exn);
    Option.iter(
      s =>
        if (Grain_utils.Config.debug^) {
          prerr_string("Backtrace:\n");
          prerr_string(s);
          prerr_string("\n");
        },
      bt,
    );
    exit(2);
  | {cstate_desc: TypeChecked(typed_program)} => typed_program
  | _ => failwith("Invalid compilation state")
  };
};

let generate_docs =
    (~current_version, ~output=?, program: Typedtree.typed_program) => {
  let comments = Comments.to_ordered(program.comments);

  let exports = Docblock.enumerate_exports(program.statements);

  let signature_items = program.signature.cmi_sign;

  let buf = Buffer.create(0);
  let module_comment = Comments.Doc.find_module(comments);
  let module_name = ref(None);
  switch (module_comment) {
  | Some((_, desc, attrs)) =>
    let deprecations =
      attrs
      |> List.filter(Comments.Attribute.is_deprecated)
      |> List.map((attr: Comments.Attribute.t) => {
           switch (attr) {
           | Deprecated({attr_desc}) => attr_desc
           | _ =>
             failwith(
               "Unreachable: Non-`deprecated` attribute can't exist here.",
             )
           }
         });

    // TODO(#787): Should we fail if more than one `@module` attribute?
    let module_attr = attrs |> List.find(Comments.Attribute.is_module);
    switch (module_attr) {
    | Module({attr_name, attr_desc}) =>
      module_name := Some(attr_name);
      Buffer.add_string(buf, Markdown.frontmatter([("title", attr_name)]));
      if (List.length(deprecations) > 0) {
        List.iter(
          msg =>
            Buffer.add_string(
              buf,
              Markdown.blockquote(
                Markdown.bold("Deprecated:") ++ " " ++ msg,
              ),
            ),
          deprecations,
        );
      };
      Buffer.add_string(buf, Markdown.paragraph(attr_desc));
      switch (desc) {
      // Guard isn't be needed because we turn an empty string into None during extraction
      | Some(desc) => Buffer.add_string(buf, Markdown.paragraph(desc))
      | None => ()
      };
    | _ => failwith("Unreachable: Non-`module` attribute can't exist here.")
    };

    // TODO(#787): Should we fail if more than one `@since` attribute?
    let since_attr =
      attrs
      |> List.find_opt(Comments.Attribute.is_since)
      |> Option.map((attr: Comments.Attribute.t) => {
           switch (attr) {
           | Since({attr_version}) =>
             Docblock.output_for_since(~current_version, attr_version)
           | _ =>
             failwith("Unreachable: Non-`since` attribute can't exist here.")
           }
         });
    let history_attrs =
      attrs
      |> List.filter(Comments.Attribute.is_history)
      |> List.map((attr: Comments.Attribute.t) => {
           switch (attr) {
           | History({attr_version, attr_desc}) =>
             Docblock.output_for_history(
               ~current_version,
               attr_version,
               attr_desc,
             )
           | _ =>
             failwith("Unreachable: Non-`since` attribute can't exist here.")
           }
         });
    if (Option.is_some(since_attr) || List.length(history_attrs) > 0) {
      let summary = Option.value(~default="History", since_attr);
      let disabled = List.length(history_attrs) == 0 ? true : false;
      let details =
        if (List.length(history_attrs) == 0) {
          "No other changes yet.";
        } else {
          Html.table(~headers=["version", "changes"], history_attrs);
        };
      Buffer.add_string(buf, Html.details(~disabled, ~summary, details));
    };

    let example_attrs = attrs |> List.filter(Comments.Attribute.is_example);
    if (List.length(example_attrs) > 0) {
      List.iter(
        (attr: Comments.Attribute.t) => {
          switch (attr) {
          | Example({attr_desc}) =>
            Buffer.add_string(buf, Markdown.code_block(attr_desc))
          | _ =>
            failwith("Unreachable: Non-`example` attribute can't exist here.")
          }
        },
        example_attrs,
      );
    };
  | None => ()
  };

  let module_name = module_name^;
  let add_docblock = sig_item => {
    let docblock =
      Docblock.for_signature_item(
        ~comments,
        ~exports,
        ~module_name?,
        sig_item,
      );
    switch (docblock) {
    | Some(docblock) =>
      Buffer.add_buffer(
        buf,
        Docblock.to_markdown(~current_version, docblock),
      )
    | None => ()
    };
  };

  let section_comments = Comments.Doc.find_sections(comments);
  if (List.length(section_comments) == 0) {
    List.iter(add_docblock, signature_items);
  } else {
    List.iteri(
      (idx, (comment, desc, attrs)) => {
        let next_section_start_line =
          Option.fold(
            ~none=max_int,
            ~some=((comment, _, _)) => Comments.start_line(comment),
            List.nth_opt(section_comments, idx + 1),
          );
        let range =
          Grain_utils.Range.Exclusive(
            Comments.end_line(comment),
            next_section_start_line,
          );
        List.iter(
          (attr: Comments.Attribute.t) => {
            switch (attr) {
            | Section({attr_name, attr_desc}) =>
              Buffer.add_string(buf, Markdown.heading(~level=2, attr_name));
              Buffer.add_string(buf, Markdown.paragraph(attr_desc));
            | _ => ()
            }
          },
          attrs,
        );
        List.iter(
          sig_item =>
            if (Docblock.signature_item_in_range(~exports, sig_item, range)) {
              add_docblock(sig_item);
            },
          signature_items,
        );
      },
      section_comments,
    );
  };

  let contents = Buffer.to_bytes(buf);
  switch (output) {
  | Some(outfile) =>
    let outfile = Filepath.to_string(outfile);
    // TODO: This crashes if you do something weird like `-o stdout/map.gr/foo`
    // because `foo` doesn't exist so it tries to mkdir it and raises
    Fs_access.ensure_parent_directory_exists(outfile);
    let oc = Fs_access.open_file_for_writing(outfile);
    output_bytes(oc, contents);
    close_out(oc);
  | None => print_bytes(contents)
  };

  ();
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
      let md_basename =
        Filepath.String.remove_extension(gr_basename) ++ ".md";
      let dirname = Fp.dirName(relative_path);
      let md_relative_path = Fp.join(dirname, Fp.relativeExn(md_basename));
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

let graindoc = (opts, runs) => {
  List.iter(
    ({input_path, output_path}) => {
      let program = compile_typed(input_path);
      try(
        generate_docs(
          ~current_version=opts.current_version,
          ~output=?output_path,
          program,
        )
      ) {
      | exn =>
        Format.eprintf("@[%s@]@.", Printexc.to_string(exn));
        exit(2);
      };
    },
    runs,
  );
};

let cmd = {
  open Term;

  let doc = "Extract documentation from Grain programs";
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };

  Cmd.v(
    Cmd.info(Sys.argv[0], ~version, ~doc),
    Grain_utils.Config.with_cli_options(graindoc)
    $ params_cmdliner_term()
    $ ret(const(enumerate_runs) $ io_params_cmdliner_term()),
  );
};

let () =
  switch (Cmd.eval_value(cmd)) {
  | Error(_) => exit(1)
  | _ => ()
  };
