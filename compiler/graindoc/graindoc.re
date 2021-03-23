open Cmdliner;
open Grain;
open Compile;
open Grain_typed;
open Filename;

type documentation = {
  type_sig: string,
  docblock: option(string),
};

let input_filename = {
  let doc = "Grain source file to compile";
  let docv = "FILE";
  Arg.(
    required
    & pos(~rev=true, 0, some(non_dir_file), None)
    & info([], ~docv, ~doc)
  );
};

let getTypedProgram = filename => {
  // TODO: Fix the stdlib location
  Grain_utils.Config.stdlib_dir := Some(Sys.getcwd() ++ "/stdlib");
  Grain_utils.Config.base_path := dirname(filename);
  switch (Compile.compile_file(~hook=stop_after_typed, filename)) {
  | exception exn =>
    let error = Grain_diagnostics.Output.exn_to_lsp_error(exn);
    let errors =
      switch (error) {
      | None => []
      | Some(err) => [err]
      };
    let json = Grain_diagnostics.Output.result_to_json(~errors, ~values=[]);
    `Error((false, json));
  | {cstate_desc: TypeChecked(typed_program)} => `Ok(typed_program)
  | _ => `Error((false, "Invalid compilation state"))
  };
};

let generateDocs = (_, i: Types.signature_item) => {
  Format.(
    switch (i) {
    | TSigValue(ident, valueDescription, docblock) =>
      fprintf(
        str_formatter,
        "%a",
        Printtyp.value_description(ident),
        valueDescription,
      );
      Some({type_sig: flush_str_formatter(), docblock});
    | TSigType(ident, typeDeclaration, _, docblock) =>
      fprintf(
        str_formatter,
        "%a",
        Printtyp.type_declaration(ident),
        typeDeclaration,
      );
      Some({type_sig: flush_str_formatter(), docblock});
    | _ => None
    }
  );
};

let extractComments = (typed_program: Typedtree.typed_program) => {
  Printtyp.print_items(
    generateDocs,
    typed_program.env,
    typed_program.signature.cmi_sign,
  )
  |> List.map(snd)
  |> List.iter(
       fun
       | Some({type_sig, docblock}) => {
           // TODO: This will be able to output to file or stdout
           print_string(type_sig);
           print_newline();
           Option.iter(docblock => print_string(docblock), docblock);
           print_newline();
           print_newline();
         }
       | None => (),
     );

  `Ok();
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
    const(extractComments) $ ret(const(getTypedProgram) $ input_filename),
    info(Sys.argv[0], ~version, ~doc),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
