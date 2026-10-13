open Grain_utils;
open Grain_diagnostics;
let uri_to_filename = (uri: Uri.t): string => {
  Uri.path(uri) |> Str.replace_first(Str.regexp("^/\\(.\\):"), "\\1:");
};

let filename_to_uri = (filename: string): Uri.t => {
  Uri.make(~scheme="file", ~host="", ~path=filename, ());
};

let loc_to_range = (pos: Grain_parsing.Location.t): Protocol.range => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(pos.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(pos.loc_end);

  {
    range_start: {
      line: startline - 1,
      character: startchar,
    },
    range_end: {
      line: endline - 1,
      character: endchar,
    },
  };
};

let position_to_offset = (text: string, position: Protocol.position): int => {
  let len = String.length(text);
  let rec find_line = (offset, line) =>
    if (line <= 0 || offset >= len) {
      offset;
    } else {
      switch (String.index_from_opt(text, offset, '\n')) {
      | None => len
      | Some(nl) =>
        let next = nl + 1;
        find_line(next, line - 1);
      };
    };

  let line_start = find_line(0, position.line);
  if (line_start >= len) {
    len;
  } else {
    let rec line_end = i =>
      if (i >= len) {
        len;
      } else {
        // NOTE: LSP can technically have any of the following as line endings
        // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocuments
        switch (text.[i]) {
        | '\n' => i
        | '\r' when i + 1 < len && text.[i + 1] == '\n' => i
        | '\r' => i
        | _ => line_end(i + 1)
        };
      };
    let end_of_line = line_end(line_start);
    let char_offset = max(0, position.character);
    min(line_start + char_offset, end_of_line);
  };
};
