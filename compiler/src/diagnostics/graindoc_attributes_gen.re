// This file takes the graindoc parser and looks for [@graindoc_attributes] to tell completions what graindoc attributes exists
// See folders coresponding dune file for how the file is generated
module Grammar =
  MenhirSdk.Cmly_read.Read({
    let filename = "graindoc_parser.cmly";
  });

let is_graindoc_attribute = terminal =>
  List.exists(
    attr => Grammar.Attribute.label(attr) == "graindoc_attribute",
    Grammar.Terminal.attributes(terminal),
  );

let attributes =
  Grammar.Terminal.fold(
    (terminal, acc) =>
      is_graindoc_attribute(terminal)
        ? [
          "@" ++ Grammar.Terminal.name(terminal) |> String.lowercase_ascii,
          ...acc,
        ]
        : acc,
    [],
  )
  |> List.sort_uniq(compare);

let () = {
  print_string("let all = [");
  List.iter(attr => print_string("\"" ++ attr ++ "\", "), attributes);
  print_string("];");
};
