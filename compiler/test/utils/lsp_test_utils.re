open Grain_tests.TestFramework;
open Grain_utils;

let lsp_position = (line, char) => {
  `Assoc([("line", `Int(line)), ("character", `Int(char))]);
};

let lsp_text_document_position = (uri, line, char) => {
  `Assoc([
    ("textDocument", `Assoc([("uri", `String(uri))])),
    ("position", lsp_position(line, char)),
  ]);
};

let make_test_utils_uri = filename => {
  let filename = Filepath.to_string(Fp.At.(test_libs_dir / filename));
  let uri = Uri.make(~scheme="file", ~host="", ~path=filename, ());
  Uri.to_string(uri);
};
