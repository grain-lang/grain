// Referencing https://github.com/marceline-cramer/tree-sitter-grain/blob/main/src/grammar.json to compare output

// MenhirSdk API: https://gitlab.inria.fr/fpottier/menhir/-/blob/master/sdk/cmly_api.ml
module Grammar =
  MenhirSdk.Cmly_read.Read({
    let filename = "parsing/parser.cmly";
  });

// https://github.com/tree-sitter/tree-sitter/blob/20924fa4cdeb10d82ac308481e39bf8519334e55/cli/src/generate/parse_grammar.rs#L10
// Also useful: https://tree-sitter.github.io/tree-sitter/creating-parsers#the-grammar-dsl
type tree_sitter_node =
  | String({value: string})
  | Pattern({value: string});

// Needs to map to JSONSchema defined at https://github.com/tree-sitter/tree-sitter/blob/20924fa4cdeb10d82ac308481e39bf8519334e55/cli/src/generate/grammar-schema.json
let yojson_of_tree_sitter_node = (node: tree_sitter_node) => {
  switch (node) {
  | String({value}) =>
    `Assoc([("type", `String("STRING")), ("value", `String(value))])
  | Pattern({value}) =>
    `Assoc([
      ("type", `String("PATTERN")),
      ("value", `String(value)),
      ("flags", `String("v")),
    ])
  };
};

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

let yojson_of_stringmap = (m: StringMap.t(tree_sitter_node)) => {
  let a =
    StringMap.bindings(m)
    |> List.map(((key, node)) => (key, yojson_of_tree_sitter_node(node)));
  `Assoc(a);
};

// https://github.com/tree-sitter/tree-sitter/blob/20924fa4cdeb10d82ac308481e39bf8519334e55/cli/src/generate/parse_grammar.rs#L75
[@deriving to_yojson]
type grammar_json = {
  name: string,
  [@to_yojson yojson_of_stringmap]
  rules: StringMap.t(tree_sitter_node),
  // #[serde(default)]
  // precedences: Vec<Vec<RuleJSON>>,
  // #[serde(default)]
  // conflicts: Vec<Vec<String>>,
  // #[serde(default)]
  // externals: Vec<RuleJSON>,
  // #[serde(default)]
  // extras: Vec<RuleJSON>,
  // #[serde(default)]
  // inline: Vec<String>,
  // #[serde(default)]
  // supertypes: Vec<String>,
  // word: Option<String>,
};

let _ = {
  // List.iter(
  //   ((a, b, c)) => {
  //     // hold
  //     print_endline(Grammar.Nonterminal.name(a));
  //     print_endline(Grammar.Nonterminal.name(Grammar.Production.lhs(b)));
  //     Array.iter(
  //       ((sym, id, attrs)) => {print_endline(id)},
  //       Grammar.Production.rhs(b),
  //     );
  //   },
  //   Grammar.Grammar.entry_points,
  // );
  // Grammar.Nonterminal.iter(nt => {
  //   print_endline(Grammar.Nonterminal.name(nt))
  // });

  let grammar: grammar_json =
    Grammar.Terminal.fold(
      (t, acc) => {
        switch (Grammar.Terminal.kind(t)) {
        | `REGULAR =>
          let name = Grammar.Terminal.name(t);
          let node =
            List.find_map(
              attr => {
                switch (Grammar.Attribute.label(attr)) {
                | "pattern" =>
                  let pattern = Grammar.Attribute.payload(attr);
                  let pattern =
                    Grain_utils.String_utils.slice(
                      ~first=1,
                      ~last=-1,
                      pattern,
                    );
                  let pattern = Scanf.unescaped(pattern);
                  Some(Pattern({value: pattern}));
                | _ => None
                }
              },
              Grammar.Terminal.attributes(t),
            );
          switch (node) {
          | Some(node) => {
              ...acc,
              rules: StringMap.add(name, node, acc.rules),
            }
          // TODO: Throw in the future
          | None => acc
          };
        | `ERROR => acc
        | `PSEUDO => acc
        | `EOF => acc
        }
      },
      {name: "grain", rules: StringMap.empty},
    );
  print_endline(
    Yojson.Safe.pretty_to_string(grammar_json_to_yojson(grammar)),
  );
};
