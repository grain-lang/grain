open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_tests_utils.Lsp_test_utils;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

let string_contains = (haystack, needle) =>
  try(
    {
      ignore(Str.search_forward(Str.regexp_string(needle), haystack, 0));
      true;
    }
  ) {
  | Not_found => false
  };

let string_occurs_before = (haystack, first, second) =>
  try({
    let first_index =
      Str.search_forward(Str.regexp_string(first), haystack, 0);
    let second_index =
      Str.search_forward(Str.regexp_string(second), haystack, 0);
    first_index < second_index;
  }) {
  | Not_found => false
  };

let string_occurrence_count = (haystack, needle) => {
  let regexp = Str.regexp_string(needle);
  let needle_len = String.length(needle);
  let rec loop = (offset, count) =>
    try({
      let index = Str.search_forward(regexp, haystack, offset);
      loop(index + needle_len, count + 1);
    }) {
    | Not_found => count
    };
  needle_len == 0 ? 0 : loop(0, 0);
};

describe("grainlsp_completions", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  test_or_skip("completion_local_value", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let apple = 1
let banana = app
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"apple"|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_excludes_current_module_name", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let value = 1

|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 0),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"A"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"value"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_excludes_current_module_name_in_submodule", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
module B {

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"A"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"B"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_nested_module_statement", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
module B {
  pr
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"provide"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"primitive"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"label":"return"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_nested_module_in_scope_provided_constructor", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
module B {
  provide enum Local { Apple }

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Apple"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_excludes_submodule_bindings", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
module B {
  let secret = 1
}

|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 0),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"secret"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"B"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_excludes_provided_submodule_bindings", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A

provide module B {
  provide let eeeee = 1
  provide let fffff = 2
  provide type Sept = Number
  provide record rec Node {
    key: Number,
  }
}

use B.{ eeeee }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 12, 0),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"fffff"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Node"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Sept"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"eeeee"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"B"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_includes_submodule_bindings_in_scope", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
module B {
  let secret = 1

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"secret"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_docblock_attribute", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
/**
 * Retrieves an element.
 * @pa
 */
let get = (index, array) => array[index]
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 5),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"@param"|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_optional_argument", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = (required, optional=1) => required + optional
f(1, )
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"optional"|})).toBe(true);
    expect.bool(string_contains(result, {|"newText":"optional="|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_optional_argument_imported", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
print(1, )
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 8),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"suffix"|})).toBe(true);
    expect.bool(string_contains(result, {|"newText":"suffix="|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_relative_path", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./prov
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"./provideAll.gr"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_stdlib_path", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "arr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"array"|})).toBe(true);
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_stdlib_path_empty", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from ""
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 6),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"array"|})).toBe(true);
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_stdlib_path_partial_string", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "arr"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"array"|})).toBe(true);
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_relative_path_partial_string", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./prov"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 12),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"./provideAll.gr"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_module_name", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" include Prov
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 35),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"ProvideAll"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":9|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_module_name_empty_prefix", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" include
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 30),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"ProvideAll"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":9|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_import_path", ({expect}) => {
    let code_uri = "file:///a.gr";
    let initial_code = {|module A
from "list" include List
let x = 1
|};
    let changed_code = {|module A
from "list" include List
use Li
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 6),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"List"|})).toBe(true);
    expect.bool(string_contains(result, {|"kind":9|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_toplevel_statement", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
pr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"provide"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"primitive"|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_from_snippet", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
fr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"from"|})).toBe(true);
    expect.bool(
      string_contains(result, {|"newText":"from \"$1\" include $2"|}),
    ).
      toBe(
      true,
    );
    expect.bool(string_contains(result, {|"insertTextFormat":2|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_block_statement", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = () => {
  re
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"return"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"record"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_expression_start_list_literal_filters_incorrect_types",
    ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "list" include List
let one = 1
let x: List<Number> = [one, ]
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 28),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"one"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"if"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Failure"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"let"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"true"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_expression_start_array_literal_filters_incorrect_types",
    ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let one = 1
let x: Array<Number> = [> one, ]
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 31),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"one"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"if"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Failure"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"let"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"true"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_header", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"record"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_header_text_edit", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 3),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(true);
    expect.bool(string_contains(result, {|"newText":"rec"|})).toBe(true);
    expect.bool(
      string_contains(
        result,
        {|"range":{"start":{"line":1,"character":3},"end":{"line":1,"character":3}}|},
      ),
    ).
      toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_provide_start", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide f
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"foreign"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"for"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"(%)"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_flow", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" inc
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 26),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"include"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_after_path", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 22),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"include"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_priority", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" i
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 24),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"include"|})).toBe(true);
    expect.bool(string_contains(result, {|"sortText":"00_include"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"sortText":"00_identify"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_if_else", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let x = if (true) 1 el
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 22),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"else"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_body_print", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => print("idk"),
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_body_print_partial", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => pr
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 8),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_guard_print_partial", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A, B }
let x = A
match (x) {
  A when pr => true,
  _ => false,
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_let_binding_name_not_modifier_prefix", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let z
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 5),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_local", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => {
    let r = "idk"
    print(r)
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 6, 10),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"r"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_print", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => {
    let r = "idk"
    print(r)
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 6, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_let", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => {
    let r = "idk"
    print(r)
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"let"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "string" include String
enum X { A }
let x = A
match (x) {
  A => {
    let z = String.concat("", "")
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 6, 19),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"concat"|})).toBe(true);
    expect.bool(string_contains(result, {|"(%)"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_expression_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let initial_code = {|module A
from "string" include String
let z = String.concat("", "")
|};
    let changed_code = {|module A
from "string" include String
let z = String.
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 15),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"concat"|})).toBe(true);
    expect.bool(string_contains(result, {|"(%)"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_string_literal_no_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "string" include String
let s = "String.|
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_string_literal_suppressed", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let s = "Failure()"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_pattern_no_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "string" include String
enum X { A }
let x = A
match (x) {
  String.|
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"concat"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_match_when", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = x => match (x) {
  Some(y) wh
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 12),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"when"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_match_when_after_pattern", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum Color { RGB(Number, Number, Number), Hex(String) }
let x = Hex("#555555")
match (x) {
  Hex(_) => print(x),
  Hex(y) wh
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"when"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_keyword_match_when_after_pattern_space", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum Color { RGB(Number, Number, Number), Hex(String) }
let x = Hex("#555555")
match (x) {
  Hex(_) => print(x),
  Hex(y)
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 8),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"when"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_match_pattern_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum Color { RGB(Number, Number, Number), Hex(String) }
let x = Hex("#555555")
match (x) {
  Hex(_) => print(x),
  Hex(y)
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 8),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"when"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_use_item_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{ code  }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"code"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_item_name_excludes_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{ code }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 13),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"code"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"as"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "list" include List |};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 25),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_foreign_wasm_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
foreign wasm getArg : Number  from "env"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 29),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_toplevel_excludes_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
pr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"provide"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"as"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_destructure_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let (a, b) = (1, 2)
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_bare_excludes_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let x = 1
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 6),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_lambda_param_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = ((a, b) ) => a
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_lambda_param_alias_no_arrow", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = ((a, b) )
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_lambda_bare_excludes_alias", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let g = (x ) => x
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"as"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_toplevel_match", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let x = 1
ma
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"match"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_pattern_no_types_or_modules", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum Colors { RGB(Number, Number, Number), Hex(String) }
let x = Hex("")
match (x) {
  _ => false,
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Colors"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Float64"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"x"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"A"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_pattern_new_match_arm", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum Color { RGB(Number, Number, Number), Hex(String) }
let x = Hex("#555555")
match (x) {
  Hex(s) => {
    let str = s
    print(str)
  },

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 7, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"IndexNonInteger"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"Color"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"x"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"A"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"str"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_pattern_constructor_args_suppressed", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum Colors { RGB(Number, Number, Number), Hex(String) }
let x = Hex("")
match (x) {
  Hex(
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 6),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Colors"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_record_field_mut", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
record R {
  m
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 3),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_enum_body_no_mut", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_enum_variant_type_reference", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,
  Banana(Number),
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_enum_variant_type_reference_incomplete", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,
  Banana(
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_enum_variant_type_reference_multiline_incomplete", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,
  Banana(

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"if"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Failure"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_explicit_type_annotation_with_prefix", ({expect}) => {
    let code_uri = "file:///a.gr";
    let initial_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x = Hex("")
|};
    let changed_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x: C
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 8),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"Color"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"x"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_explicit_type_annotation_prefers_types", ({expect}) => {
    let code_uri = "file:///a.gr";
    let initial_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x = Hex("")
|};
    let changed_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x:
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 7),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"Color"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"x"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"A"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"let"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"from"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_explicit_type_annotation_included_module_types", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let initial_code = {|module A
from "./aliases.gr" include Aliases
let x = Aliases.baz
|};
    let changed_code = {|module A
from "./aliases.gr" include Aliases
let x: Aliases.
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 15),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"Foo"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Bar"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Baz"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Qux"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"baz"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"qux"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_explicit_type_annotation_included_module_name", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let initial_code = {|module A
from "./aliases.gr" include Aliases
let x = Aliases.baz
|};
    let changed_code = {|module A
from "./aliases.gr" include Aliases
let x: Al
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 9),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"Aliases"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"baz"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_explicit_type_annotation_generic_args_reset_qualifier",
    ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let initial_code = {|module A
from "./aliases.gr" include Aliases
let x = Aliases.baz
|};
    let changed_code = {|module A
from "./aliases.gr" include Aliases
let x: Aliases.Bar<St
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 21),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Baz"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Qux"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_rec", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let apple = 1
let rec
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"apple"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"from"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_mut", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let apple = 1
let mut
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"apple"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"from"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_header_locals_only", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let apple = 1
let
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 3),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"apple"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_modifier_prefix", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let rangeStart = 1
let r
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 5),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"record"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"rangeStart"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_context_let_rhs", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let mut x =
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 12),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"break"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"continue"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"true"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_context_loop_control", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = () => {
  while (true) {

  }
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"break"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"continue"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_context_break_outside_loop", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = () => {

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"break"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"continue"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_provide_data_no_repeat_keywords", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide type rec
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 18),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"enum"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"abstract"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"type"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_items_plain", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{  }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"code"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"type Encoding"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"label":"module Ascii"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"label":"type"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_items_type_keyword", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{ type Encoding }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 19),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Encoding"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"type Encoding"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"code"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_items_module_keyword", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{ module Ascii }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 20),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Ascii"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"module Ascii"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"code"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_module_name_before_brackets", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{  }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 6),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Char"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"type Encoding"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_typed_let_binding_shows_matching_constructors", ({expect}) => {
    let code_uri = "file:///a.gr";
    let initial_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x = Hex("")
|};
    let changed_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x: Color =
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 15),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Some"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"Ok"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"None"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_typed_let_binding_with_prefix_shows_matching_constructors",
    ({expect}) => {
    let code_uri = "file:///a.gr";
    let initial_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x = Hex("")
|};
    let changed_code = {|module A
provide enum Color {
  RGB(Number, Number, Number),
  Hex(String),
}
let x: Color = R
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, initial_code);
    let change_request =
      lsp_notification(
        "textDocument/didChange",
        `Assoc([
          (
            "textDocument",
            `Assoc([("uri", `String(code_uri)), ("version", `Int(2))]),
          ),
          (
            "contentChanges",
            `List([`Assoc([("text", `String(changed_code))])]),
          ),
        ]),
      );
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 16),
      );
    let (result, code) =
      lsp(
        setup_request
        ++ lsp_request(change_request)
        ++ lsp_request(request)
        ++ teardown_request,
      );

    expect.bool(string_contains(result, {|"label":"RGB"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Hex"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"String"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_pipeline_caps_results", ({expect}) => {
    let code_uri = "file:///a.gr";
    let rec numbered_bindings = index =>
      if (index > 100) {
        "";
      } else {
        let number = string_of_int(index);
        "let value"
        ++ number
        ++ " = "
        ++ number
        ++ "\n"
        ++ numbered_bindings(index + 1);
      };
    let code = "module A\n" ++ numbered_bindings(0) ++ "\n";
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 102, 0),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"isIncomplete":true|})).toBe(
      true,
    );
    expect.int(string_occurrence_count(result, {|"label":|})).toBe(100);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_let_mut_no_type_suggests_modules_and_values", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module Test3

from "array" include Array
from "number" include Number

enum Example {
  E1,
  E2,
  E3,
}

let x: Number = 1

let mut delta = |};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 13, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Number"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"Array"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"x"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"E1"|})).toBe(true);
    expect.int(code).toBe(0);
  });
});
