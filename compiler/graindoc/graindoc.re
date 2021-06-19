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

let compile_typed = filename => {
  Grain_utils.Config.base_path := dirname(filename);

  switch (Compile.compile_file(~hook=stop_after_typed, filename)) {
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
  | {cstate_desc: TypeChecked(typed_program)} => `Ok(typed_program)
  | _ => `Error((false, "Invalid compilation state"))
  };
};

let generate_docs = (outfile, program: Typedtree.typed_program) => {
  Comments.setup_comments(program.comments);

  let env = program.env;
  let signature_items = program.signature.cmi_sign;

  let buf = Buffer.create(0);
  let module_comment = Comments.Doc.find_module();
  switch (module_comment) {
  | Some((_, desc, attrs)) =>
    List.iter(
      (attr: Comments.Attribute.t) => {
        switch (attr) {
        | Module({attr_name, attr_desc}) =>
          Buffer.add_string(
            buf,
            Markdown.frontmatter([("title", attr_name)]),
          );
          Buffer.add_string(buf, Markdown.paragraph(attr_desc));
        | Example({attr_desc}) =>
          Buffer.add_string(buf, Markdown.code_block(attr_desc))
        | _ => ()
        }
      },
      attrs,
    )
  | None => ()
  };

  let add_docblock = sig_item => {
    let docblock = Docblock.for_signature_item(~env, sig_item);
    switch (docblock) {
    | Some(docblock) =>
      Buffer.add_buffer(buf, Docblock.to_markdown(docblock))
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
  switch (outfile) {
  | Some(outfile) =>
    let oc = Fs_access.open_file_for_writing(outfile);
    output_bytes(oc, contents);
    close_out(oc);
  | None => print_bytes(contents)
  };

  `Ok();
};

let graindoc = (outfile, program) =>
  try(generate_docs(outfile, program)) {
  | e => `Error((false, Printexc.to_string(e)))
  };

let input_file_conv = {
  open Arg;
  let (prsr, prntr) = non_dir_file;

  (
    filename => prsr(Grain_utils.Files.normalize_separators(filename)),
    prntr,
  );
};

let input_filename = {
  let doc = "Grain source file for which to extract documentation";
  let docv = "FILE";
  Arg.(
    required
    & pos(~rev=true, 0, some(input_file_conv), None)
    & info([], ~docv, ~doc)
  );
};

/** Converter which checks that the given output filename is valid */
let output_file_conv = {
  let parse = s => {
    let s_dir = dirname(s);
    Sys.file_exists(s_dir)
      ? if (Sys.is_directory(s_dir)) {
          `Ok(s);
        } else {
          `Error(Format.sprintf("`%s' is not a directory", s_dir));
        }
      : `Error(Format.sprintf("no `%s' directory", s_dir));
  };
  (parse, Format.pp_print_string);
};

let output_filename = {
  let doc = "Output filename";
  let docv = "FILE";
  Arg.(
    value & opt(some(output_file_conv), None) & info(["o"], ~docv, ~doc)
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

  (
    Term.(
      ret(
        const(graindoc)
        $ output_filename
        $ ret(
            Grain_utils.Config.with_cli_options(compile_typed)
            $ input_filename,
          ),
      )
    ),
    Term.info(Sys.argv[0], ~version, ~doc),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
