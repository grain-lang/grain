// This file takes the parser and looks for [@keyword] to tell completions what keyword slots exists and what context they should appear in
// See folders coresponding dune file for how the file is generated
module Grammar =
  MenhirSdk.Cmly_read.Read({
    let filename = "parser.cmly";
  });

let keyword_attributes = terminal =>
  List.filter(
    attr => Grammar.Attribute.label(attr) == "keyword",
    Grammar.Terminal.attributes(terminal),
  );

let slots_of = terminal =>
  keyword_attributes(terminal)
  |> List.map(attr => String.trim(Grammar.Attribute.payload(attr)))
  |> List.filter(payload => payload != "")
  |> List.sort_uniq(compare);

let keywords =
  Grammar.Terminal.fold(
    (terminal, acc) => {
      let name = Grammar.Terminal.name(terminal) |> String.lowercase_ascii;
      if (keyword_attributes(terminal) != []) {
        [(name, slots_of(terminal)), ...acc];
      } else {
        acc;
      };
    },
    [],
  )
  |> List.sort_uniq((a, b) => compare(fst(a), fst(b)));

let () = {
  print_string("open Keyword_slot;\n\n");
  print_string("let all = [");
  List.iter(((name, _)) => print_string("\"" ++ name ++ "\", "), keywords);
  print_string("];\n");
  print_string("let slots = [");
  List.iter(
    ((name, slots)) => {
      print_string("(\"" ++ name ++ "\", [");
      List.iter(slot => print_string(slot ++ ", "), slots);
      print_string("]), ");
    },
    keywords,
  );
  print_string("];\n");
  print_string(
    "let slots_for = label => try(List.assoc(label, slots)) { | Not_found => [] };\n",
  );
};
