open Cmdliner;
open Grain;
open Compile;
open Grain_typed;
open Grain_utils;
open Grain_diagnostics;
open Filename;

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

module Input = {
  type t = string;

  let (prsr, prntr) = Arg.non_dir_file;

  let cmdliner_converter = (
    filename => prsr(Grain_utils.Files.normalize_separators(filename)),
    prntr,
  );
};

module Output = {
  type t = string;

  /** Checks that the given output filename is valid */
  let prsr = s => {
    let s_dir = dirname(s);
    Sys.file_exists(s_dir)
      ? if (Sys.is_directory(s_dir)) {
          `Ok(s);
        } else {
          `Error(Format.sprintf("`%s' is not a directory", s_dir));
        }
      : `Error(Format.sprintf("no `%s' directory", s_dir));
  };

  let cmdliner_converter = (
    filename => prsr(Grain_utils.Files.normalize_separators(filename)),
    Format.pp_print_string,
  );
};

[@deriving cmdliner]
type params = {
  /** Grain source file for which to extract documentation */
  [@pos 0] [@docv "FILE"]
  input: Input.t,
  /** Output filename */
  [@name "o"] [@docv "FILE"]
  output: option(Output.t),
  /**
    The version to use as current when generating markdown for `@since` and `@history` attributes.
    Any future versions will be replace with `next` in the output.
  */
  [@name "current-version"] [@docv "VERSION"]
  current_version: option(string),
};

let compile_typed = (opts: params) => {
  Grain_utils.Config.base_path := dirname(opts.input);

  switch (Compile.compile_file(~hook=stop_after_typed, opts.input)) {
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
    ({current_version, output}: params, program: Typedtree.typed_program) => {
  Comments.setup_comments(program.comments);

  let env = program.env;
  let signature_items = program.signature.cmi_sign;

  let buf = Buffer.create(0);
  let module_comment = Comments.Doc.find_module();
  switch (module_comment) {
  | Some((_, desc, attrs)) =>
    // TODO: Should we fail if more than one `@module` attribute?
    let module_attr = attrs |> List.find(Comments.Attribute.is_module);
    switch (module_attr) {
    | Module({attr_name, attr_desc}) =>
      Buffer.add_string(buf, Markdown.frontmatter([("title", attr_name)]));
      Buffer.add_string(buf, Markdown.paragraph(attr_desc));
      switch (desc) {
      // Guard isn't be needed because we turn an empty string into None during extraction
      | Some(desc) => Buffer.add_string(buf, Markdown.paragraph(desc))
      | None => ()
      };
    | _ => failwith("Unreachable: Non-`module` attribute can't exist here.")
    };

    // TODO: Should we fail if more than one `@since` attribute?
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

  let add_docblock = sig_item => {
    let docblock = Docblock.for_signature_item(~env, sig_item);
    switch (docblock) {
    | Some(docblock) =>
      Buffer.add_buffer(
        buf,
        Docblock.to_markdown(~current_version, docblock),
      )
    | None => ()
    };
  };

  let section_comments = Comments.Doc.find_sections();
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
            if (Docblock.signature_item_in_range(~env, sig_item, range)) {
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
    let oc = Fs_access.open_file_for_writing(outfile);
    output_bytes(oc, contents);
    close_out(oc);
  | None => print_bytes(contents)
  };

  ();
};

let graindoc = opts => {
  let program = compile_typed(opts);
  try(generate_docs(opts, program)) {
  | exn =>
    Format.eprintf("@[%s@]@.", Printexc.to_string(exn));
    exit(2);
  };
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
    Grain_utils.Config.with_cli_options(graindoc) $ params_cmdliner_term(),
  );
};

let () =
  switch (Cmd.eval_value(cmd)) {
  | Error(_) => exit(1)
  | _ => ()
  };
