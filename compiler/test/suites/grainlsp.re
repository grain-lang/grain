open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_utils;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

let lsp_position = (line, char) => {
  `Assoc([("line", `Int(line)), ("character", `Int(char))]);
};

let lsp_text_document_position = (uri, line, char) => {
  `Assoc([
    ("textDocument", `Assoc([("uri", `String(uri))])),
    ("position", lsp_position(line, char)),
  ]);
};

let lsp_range = (start_position, end_position) => {
  let (start_line, start_char) = start_position;
  let (end_line, end_char) = end_position;
  `Assoc([
    ("start", lsp_position(start_line, start_char)),
    ("end", lsp_position(end_line, end_char)),
  ]);
};

let lsp_text_document_edit = (uri, edits) => {
  `Assoc([
    (
      "documentChanges",
      `List([
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(uri)), ("version", `Null)]),
          ),
          (
            "edits",
            `List(
              List.map(
                ((range, new_text)) =>
                  `Assoc([
                    ("range", range),
                    ("newText", `String(new_text)),
                  ]),
                edits,
              ),
            ),
          ),
        ]),
      ]),
    ),
  ]);
};

let lsp_location_link = (uri, origin_selection_range, target_selection_range) => {
  let (origin_start_range, origin_end_range) = origin_selection_range;
  let (target_start_range, target_end_range) = target_selection_range;
  `Assoc([
    (
      "originSelectionRange",
      lsp_range(origin_start_range, origin_end_range),
    ),
    ("targetUri", `String(uri)),
    ("targetRange", lsp_range(target_start_range, target_end_range)),
    (
      "targetSelectionRange",
      lsp_range(target_start_range, target_end_range),
    ),
  ]);
};

let make_test_utils_uri = filename => {
  let filename = Filepath.to_string(Fp.At.(test_libs_dir / filename));
  let uri = Uri.make(~scheme="file", ~host="", ~path=filename, ());
  Uri.to_string(uri);
};

describe("grainlsp", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertLspOutput = makeLspRunner(test_or_skip);
  let assertLspDiagnostics = makeLspDiagnosticsRunner(test_or_skip);

  assertLspOutput(
    "goto_definition1",
    "file:///a.gr",
    {|module A
let func = x => x
func(1)
|},
    lsp_input(
      "textDocument/definition",
      lsp_text_document_position("file:///a.gr", 2, 0),
    ),
    lsp_location_link(
      "file:///a.gr",
      ((2, 0), (2, 4)),
      ((1, 4), (1, 8)),
    ),
  );

  assertLspOutput(
    "goto_definition2",
    "file:///a.gr",
    {|module A
let func = x => x
func(1)
|},
    lsp_input(
      "textDocument/definition",
      lsp_text_document_position("file:///a.gr", 2, 4),
    ),
    lsp_location_link(
      "file:///a.gr",
      ((2, 0), (2, 4)),
      ((1, 4), (1, 8)),
    ),
  );

  assertLspOutput(
    "goto_definition3",
    make_test_utils_uri("a.gr"),
    {|module A
from "./provideAll.gr" include ProvideAll
ProvideAll.y(1)
|},
    lsp_input(
      "textDocument/definition",
      lsp_text_document_position(make_test_utils_uri("a.gr"), 2, 11),
    ),
    lsp_location_link(
      make_test_utils_uri("provideAll.gr"),
      ((2, 0), (2, 12)),
      ((3, 12), (3, 13)),
    ),
  );

  assertLspOutput(
    "goto_type_definition",
    "file:///a.gr",
    {|module A
record T {
  x: Number
}
let a = { x: 1 }
let b = a
|},
    lsp_input(
      "textDocument/typeDefinition",
      lsp_text_document_position("file:///a.gr", 5, 8),
    ),
    lsp_location_link(
      "file:///a.gr",
      ((5, 8), (5, 9)),
      ((1, 0), (3, 1)),
    ),
  );

  assertLspOutput(
    "formatting",
    "file:///a.gr",
    {|module A
record T { x: Number }
let a = {   x: 1 }; let b = a
|},
    lsp_input(
      "textDocument/formatting",
      Yojson.Safe.from_string(
        {|{"textDocument":{"uri":"file:///a.gr"},"options":{"tabSize":2,"insertSpaces":false,"trimTrailingWhitespace":false,"insertFinalNewLine":false,"trimFinalNewlines":false}}|},
      ),
    ),
    `List([
      `Assoc([
        ("range", lsp_range((0, 0), (2147483647, 2147483647))),
        (
          "newText",
          `String(
            {|module A

record T {
  x: Number,
}
let a = { x: 1, }
let b = a
|},
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_type_annotation1",
    "file:///a.gr",
    {|module A
record T { x: Number }
let abc = { x: 1 }
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 4), (2, 6))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Annotate type")),
        ("kind", `String("annotate-type")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [(lsp_range((2, 7), (2, 7)), ": T")],
          ),
        ),
      ]),
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (1, 4611686018427387871),
                  (1, 4611686018427387871),
                ),
                "\n/**\n *\n *\n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_type_annotation2",
    "file:///a.gr",
    {|module A
record T { x: Number }
let f = val => {
  print(val)
  val.x
}
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 8), (2, 9))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Annotate type")),
        ("kind", `String("annotate-type")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [(lsp_range((2, 11), (2, 11)), ": T")],
          ),
        ),
      ]),
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (1, 4611686018427387871),
                  (1, 4611686018427387871),
                ),
                "\n/**\n *\n *\n *\n * @param val:\n * @returns \n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_type_annotation3",
    "file:///a.gr",
    {|module A
record T { x: Number }
let abc: T = { x: 1 }
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 4), (2, 6))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (1, 4611686018427387871),
                  (1, 4611686018427387871),
                ),
                "\n/**\n *\n *\n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_label1",
    "file:///a.gr",
    {|module A
let f = (x, y, z) => x ++ y ++ z
f("y", x="x", "z")
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 2), (2, 4))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Use argument label")),
        ("kind", `String("use-argument-label")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [(lsp_range((2, 2), (2, 2)), "y=")],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_label2",
    "file:///a.gr",
    {|module A
let f = (x, y, z) => x ++ y ++ z
f("y", x="x", "z")
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 14), (2, 16))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Use argument label")),
        ("kind", `String("use-argument-label")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [(lsp_range((2, 14), (2, 14)), "z=")],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_label3",
    "file:///a.gr",
    {|module A
let f = (x) => x
f(x="x")
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 4), (2, 6))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `Null,
  );

  assertLspOutput(
    "code_action_add_function_label_pattern",
    "file:///a.gr",
    {|module A
let f = ((a, b)) => a + b
f((1, 2))
    |},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 2), (2, 2))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `Null,
  );

  assertLspOutput(
    "code_action_add_function_inferred",
    "file:///a.gr",
    {|module A
a => {a(1); void}
    |},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((1, 8), (1, 8))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `Null,
  );

  assertLspOutput(
    "code_action_remove_function_block_braces_1",
    "file:///a.gr",
    {|module A
let f = (x) => {
  x
}
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 2), (2, 3))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Remove block braces")),
        ("kind", `String("remove-block-braces")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (lsp_range((1, 15), (2, 2)), ""),
              (lsp_range((2, 3), (3, 1)), ""),
            ],
          ),
        ),
      ]),
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387894),
                  (0, 4611686018427387894),
                ),
                "\n/**\n *\n *\n *\n * @param x:\n * @returns \n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_remove_function_block_braces_2",
    "file:///a.gr",
    {|module A
let f = (x) => {
  print(x)
  x
}
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 2), (2, 3))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387894),
                  (0, 4611686018427387894),
                ),
                "\n/**\n *\n *\n *\n * @param x:\n * @returns \n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_block_braces_1",
    "file:///a.gr",
    {|module A
let f = (x) => print(x)
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((1, 15), (1, 16))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add block braces")),
        ("kind", `String("add-block-braces")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (lsp_range((1, 15), (1, 15)), "{ "),
              (lsp_range((1, 23), (1, 23)), " }"),
            ],
          ),
        ),
      ]),
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387894),
                  (0, 4611686018427387894),
                ),
                "\n/**\n *\n *\n *\n * @param x:\n * @returns \n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_block_braces_2",
    "file:///a.gr",
    {|module A
let f = () => () => print(1)
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((1, 20), (1, 21))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add block braces")),
        ("kind", `String("add-block-braces")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (lsp_range((1, 20), (1, 20)), "{ "),
              (lsp_range((1, 28), (1, 28)), " }"),
            ],
          ),
        ),
      ]),
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387894),
                  (0, 4611686018427387894),
                ),
                "\n/**\n *\n *\n *\n * @returns \n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_graindoc_module",
    "file:///a.gr",
    {|module Main
module Test {
  let f = 0
}|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((1, 10), (1, 11))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387891),
                  (0, 4611686018427387891),
                ),
                "\n/**\n *\n *\n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_graindoc_function",
    "file:///a.gr",
    {|module A
let f = (x0, (x1, x2), x3, (x4, x5)) => 1
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((1, 6), (1, 7))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387894),
                  (0, 4611686018427387894),
                ),
                "\n/**\n *\n *\n *\n * @param x0:\n * @param 1:\n * @param x3:\n * @param 3:\n * @returns \n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_graindoc_value",
    "file:///a.gr",
    {|module A
let f = 0
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((1, 6), (1, 7))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `List([
      `Assoc([
        ("title", `String("Add Graindoc")),
        ("kind", `String("add-graindoc")),
        (
          "edit",
          lsp_text_document_edit(
            "file:///a.gr",
            [
              (
                lsp_range(
                  (0, 4611686018427387894),
                  (0, 4611686018427387894),
                ),
                "\n/**\n *\n *\n * @example\n *\n * @since\n */",
              ),
            ],
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_graindoc_existing",
    "file:///a.gr",
    {|module A
/** Existing graindoc */
let f = 0
|},
    lsp_input(
      "textDocument/codeAction",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((2, 6), (2, 7))),
        ("context", `Assoc([("diagnostics", `List([]))])),
      ]),
    ),
    `Null,
  );

  assertLspOutput(
    "hover_pattern",
    "file:///a.gr",
    {|module A
record T { x: Number }
let a = { x: 1 }
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position("file:///a.gr", 2, 4),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          (
            "value",
            `String(
              "```grain\nrecord T {\n  x: Number,\n}\n```\n\n\n---\n<br><br>\n```grain-type\nT\n```\n\n",
            ),
          ),
        ]),
      ),
      ("range", lsp_range((2, 4), (2, 5))),
    ]),
  );

  assertLspOutput(
    "hover_val",
    "file:///a.gr",
    {|module A
let a = 1
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position("file:///a.gr", 1, 8),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          ("value", `String("```grain-type\nNumber\n```\n\n")),
        ]),
      ),
      ("range", lsp_range((1, 8), (1, 9))),
    ]),
  );

  assertLspOutput(
    "hover_type_definition",
    "file:///a.gr",
    {|module A
record T { x: Number }
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position("file:///a.gr", 1, 0),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          (
            "value",
            `String("```grain-type\nrecord T {\n  x: Number,\n}\n```\n\n"),
          ),
        ]),
      ),
      ("range", lsp_range((1, 0), (1, 22))),
    ]),
  );

  assertLspOutput(
    "hover_type",
    "file:///a.gr",
    {|module A
record T { x: Number }
let a: T = { x: 1 }
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position("file:///a.gr", 2, 7),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          ("value", `String("```grain-type\nT\n```\n\n")),
        ]),
      ),
      ("range", lsp_range((2, 7), (2, 8))),
    ]),
  );

  assertLspOutput(
    "hover_module",
    "file:///a.gr",
    {|module A
module B {
  provide let a = 1
  provide let b = 2
  let c = 3
}
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position("file:///a.gr", 1, 0),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          (
            "value",
            `String("```grain\nlet a: Number\nlet b: Number\n```\n\n"),
          ),
        ]),
      ),
      ("range", lsp_range((1, 0), (5, 1))),
    ]),
  );

  assertLspOutput(
    "hover_submodule",
    "file:///a.gr",
    {|module A
module B {
  provide let a = 1
  provide let b = 2
  let c = 3
  provide module C {
    provide let d = 4
  }
}
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position("file:///a.gr", 1, 0),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          (
            "value",
            `String(
              "```grain\nlet a: Number\nlet b: Number\nmodule C\n```\n\n",
            ),
          ),
        ]),
      ),
      ("range", lsp_range((1, 0), (8, 1))),
    ]),
  );

  assertLspOutput(
    "hover_include",
    make_test_utils_uri("a.gr"),
    {|module A
from "./provideAll.gr" include ProvideAll
|},
    lsp_input(
      "textDocument/hover",
      lsp_text_document_position(make_test_utils_uri("a.gr"), 1, 0),
    ),
    `Assoc([
      (
        "contents",
        `Assoc([
          ("kind", `String("markdown")),
          (
            "value",
            `String(
              "```grain\nmodule ProvideAll\n```\n\n\n---\n<br><br>\n```grain\nlet x: Number\nlet y: (x: a) => a\nlet z: String\n```\n\n",
            ),
          ),
        ]),
      ),
      ("range", lsp_range((1, 0), (1, 41))),
    ]),
  );

  assertLspOutput(
    "code_lens",
    "file:///a.gr",
    {|module A
record T { x: Number }
let a = 1
let b = 2 and c = 3
"abc"
|},
    lsp_input(
      "textDocument/codeLens",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
      ]),
    ),
    `List([
      `Assoc([
        ("range", lsp_range((1, 1), (1, 1))),
        (
          "command",
          `Assoc([
            ("title", `String("record T {\n  x: Number,\n}")),
            ("command", `String("")),
          ]),
        ),
      ]),
      `Assoc([
        ("range", lsp_range((2, 1), (2, 1))),
        (
          "command",
          `Assoc([
            ("title", `String("Number")),
            ("command", `String("")),
          ]),
        ),
      ]),
      `Assoc([
        ("range", lsp_range((3, 1), (3, 1))),
        (
          "command",
          `Assoc([
            ("title", `String("Number, Number")),
            ("command", `String("")),
          ]),
        ),
      ]),
      `Assoc([
        ("range", lsp_range((4, 1), (4, 1))),
        (
          "command",
          `Assoc([
            ("title", `String("String")),
            ("command", `String("")),
          ]),
        ),
      ]),
    ]),
  );

  assertLspDiagnostics(
    "compile_error1",
    "file:///a.gr",
    {|module A
let a = 123
let b = "a" + a
|},
    `Assoc([
      ("uri", `String("file:///a.gr")),
      (
        "diagnostics",
        `List([
          `Assoc([
            ("range", lsp_range((2, 8), (2, 11))),
            ("severity", `Int(1)),
            (
              "message",
              `String(
                "This expression has type String but an expression was expected of type\n         Number",
              ),
            ),
            ("relatedInformation", `List([])),
          ]),
        ]),
      ),
    ]),
  );

  assertLspDiagnostics(
    "compile_error2",
    make_test_utils_uri("a.gr"),
    {|module A
from "./compileError.gr" include CompileError
|},
    `Assoc([
      ("uri", `String(make_test_utils_uri("a.gr"))),
      (
        "diagnostics",
        `List([
          `Assoc([
            ("range", lsp_range((0, 0), (0, 1))),
            ("severity", `Int(1)),
            (
              "message",
              `String(
                "Failed to compile "
                ++ Filepath.to_string(
                     Fp.At.(test_libs_dir / "compileError.gr"),
                   ),
              ),
            ),
            (
              "relatedInformation",
              `List([
                `Assoc([
                  (
                    "location",
                    `Assoc([
                      (
                        "uri",
                        `String(make_test_utils_uri("compileError.gr")),
                      ),
                      ("range", lsp_range((2, 14), (2, 18))),
                    ]),
                  ),
                  (
                    "message",
                    `String(
                      "This expression has type Bool but an expression was expected of type\n         Number",
                    ),
                  ),
                ]),
              ]),
            ),
          ]),
        ]),
      ),
    ]),
  );

  assertLspDiagnostics(
    "compile_warning",
    "file:///a.gr",
    {|module A
let Some(a) = Some(1)
|},
    `Assoc([
      ("uri", `String("file:///a.gr")),
      (
        "diagnostics",
        `List([
          `Assoc([
            ("range", lsp_range((1, 4), (1, 11))),
            ("severity", `Int(2)),
            (
              "message",
              `String(
                "this pattern-matching is not exhaustive.\nHere is an example of a case that is not matched:\nNone",
              ),
            ),
            ("relatedInformation", `List([])),
          ]),
        ]),
      ),
    ]),
  );

  assertLspOutput(
    "document_symbol",
    "file:///a.gr",
    {|module Test
    provide record A { x: Number }
    abstract type B = Number
    enum C { A, B }
    let a = 1
    let a = () => 1
    let _ = [1, 2]
    let z = (x, y) => x + y
    provide module Sub {
      provide let sub = 1
    }
|},
    lsp_input(
      "textDocument/documentSymbol",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
      ]),
    ),
    `List([
      `Assoc([
        ("name", `String("Test")),
        ("kind", `Int(2)),
        ("range", lsp_range((0, 0), (11, 0))),
        ("selectionRange", lsp_range((0, 0), (11, 0))),
        (
          "children",
          `List([
            `Assoc([
              ("name", `String("A")),
              ("kind", `Int(23)),
              ("range", lsp_range((1, 12), (1, 34))),
              ("selectionRange", lsp_range((1, 12), (1, 34))),
            ]),
            `Assoc([
              ("name", `String("C")),
              ("kind", `Int(10)),
              ("range", lsp_range((3, 4), (3, 19))),
              ("selectionRange", lsp_range((3, 4), (3, 19))),
            ]),
            `Assoc([
              ("name", `String("a")),
              ("detail", `String("Number")),
              ("kind", `Int(13)),
              ("range", lsp_range((4, 8), (4, 13))),
              ("selectionRange", lsp_range((4, 8), (4, 13))),
            ]),
            `Assoc([
              ("name", `String("a")),
              ("detail", `String("() => Number")),
              ("kind", `Int(12)),
              ("range", lsp_range((5, 8), (5, 19))),
              ("selectionRange", lsp_range((5, 8), (5, 19))),
            ]),
            `Assoc([
              ("name", `String("z")),
              ("detail", `String("(x: Number, y: Number) => Number")),
              ("kind", `Int(12)),
              ("range", lsp_range((7, 8), (7, 27))),
              ("selectionRange", lsp_range((7, 8), (7, 27))),
            ]),
            `Assoc([
              ("name", `String("Sub")),
              ("kind", `Int(2)),
              ("range", lsp_range((8, 4), (10, 5))),
              ("selectionRange", lsp_range((8, 4), (10, 5))),
              (
                "children",
                `List([
                  `Assoc([
                    ("name", `String("sub")),
                    ("detail", `String("Number")),
                    ("kind", `Int(13)),
                    ("range", lsp_range((9, 18), (9, 25))),
                    ("selectionRange", lsp_range((9, 18), (9, 25))),
                  ]),
                ]),
              ),
            ]),
          ]),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "inlay_hints",
    "file:///a.gr",
    {|
    module Main

    // Top Level Values
    let a = 1
    and b = 2

    let c = true
    let d = []
    // Patterns
    enum A {
      S(Number),
    }
    let S(t) = S(1)
    // Functions
    let e = (a, b) => a + b
    let e = (a, b) => {
      return (a, b) => a + b
    }
    let test = (a, b, c: Number) => {
      let x = 1
      let S(t) = S(1)
      return a + b + c
    }
    // Record
    record Nested {
      a: Number,
      b: String,
    }
    record Test {
      test: String,
      test2: Nested,
    }
    let test = { test: "test", test2: { a: 1, b: "test" } }
    {
      let test = { test: "test", test2: { a: 1, b: "test" } }
    }
    |},
    lsp_input(
      "textDocument/inlayHint",
      `Assoc([
        ("textDocument", `Assoc([("uri", `String("file:///a.gr"))])),
        ("range", lsp_range((0, 0), (37, 0))),
      ]),
    ),
    `List(
      List.map(
        ((label, (line, char))) => {
          `Assoc([
            ("label", `String(label)),
            ("position", lsp_position(line, char)),
          ])
        },
        [
          // (label, position)
          (": Test", (35, 14)),
          (": Number", (20, 11)),
          (": Number", (19, 20)),
          (": Number", (19, 17)),
          (": Number", (17, 18)),
          (": Number", (17, 15)),
          (": a", (16, 17)),
          (": a", (16, 14)),
          (": Number", (15, 17)),
          (": Number", (15, 14)),
        ],
      ),
    ),
  );
});
