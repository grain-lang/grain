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

let lsp_text_document_edit = (uri, range, new_text) => {
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
            `List([
              `Assoc([("range", range), ("newText", `String(new_text))]),
            ]),
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
let func = x => print(x)
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
let func = x => print(x)
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
ignore(ProvideAll.y(1))
|},
    lsp_input(
      "textDocument/definition",
      lsp_text_document_position(make_test_utils_uri("a.gr"), 2, 18),
    ),
    lsp_location_link(
      make_test_utils_uri("provideAll.gr"),
      ((2, 7), (2, 19)),
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
            lsp_range((2, 7), (2, 7)),
            ": T",
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
let f = val => val.x
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
            lsp_range((2, 11), (2, 11)),
            ": T",
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
    `Null,
  );

  assertLspOutput(
    "code_action_add_function_label1",
    "file:///a.gr",
    {|module A
let f = (x, y, z) => print(x ++ y ++ z)
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
            lsp_range((2, 2), (2, 2)),
            "y=",
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_label2",
    "file:///a.gr",
    {|module A
let f = (x, y, z) => print(x ++ y ++ z)
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
            lsp_range((2, 14), (2, 14)),
            "z=",
          ),
        ),
      ]),
    ]),
  );

  assertLspOutput(
    "code_action_add_function_label3",
    "file:///a.gr",
    {|module A
let f = (x) => print(x)
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
            `String("```grain\nlet a : Number\nlet b : Number\n```\n\n"),
          ),
        ]),
      ),
      ("range", lsp_range((1, 0), (5, 1))),
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
              "```grain\nmodule ProvideAll\n```\n\n\n---\n<br><br>\n```grain\nlet x : Number\nlet y : (x: a) => a\nlet z : String\n```\n\n",
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
print("abc")
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
          `Assoc([("title", `String("Void")), ("command", `String(""))]),
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
});
