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

let rec find_position = (text, length, l, c, offset) =>
  if (l === 0 && c === 0 || offset >= length) {
    offset;
  } else if (l === 0) {
    // NOTE: LSP can technically have any of the following as line endings
    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocuments
    switch (text.[offset]) {
    | '\n' => offset + 1
    | '\r' when offset + 1 < length && text.[offset + 1] == '\n' => offset + 2
    | '\r' => offset + 1
    | _ => find_position(text, length, l, c - 1, offset + 1)
    };
  } else {
    // NOTE: LSP can technically have any of the following as line endings
    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocuments
    switch (text.[offset]) {
    | '\n' => find_position(text, length, l - 1, c, offset + 1)
    | '\r' when offset + 1 < length && text.[offset + 1] == '\n' =>
      find_position(text, length, l - 1, c, offset + 2)
    | '\r' => find_position(text, length, l - 1, c, offset + 1)
    | _ => find_position(text, length, l, c, offset + 1)
    };
  };

let position_to_offset = (text: string, position: Protocol.position): int => {
  find_position(
    text,
    String.length(text),
    position.line,
    position.character,
    0,
  );
};

let range_to_offsets = (text: string, range: Protocol.range): (int, int) => {
  let length = String.length(text);
  let start_line = range.range_start.line;
  let start_character = range.range_start.character;
  let end_line = range.range_end.line;
  let end_character = range.range_end.character;

  let start_offset =
    find_position(text, length, start_line, start_character, 0);
  // If the start and end are on the same line, we need to ensure that the end character is started from the line start offset.
  let (end_scan_line, end_scan_offset) =
    if (end_line === start_line) {
      (
        // This places us 1 line back at a random character (at max we need to go back by the number of characters in the line).
        1,
        start_offset - end_character,
      );
    } else {
      (end_line - start_line, 0);
    };
  let end_offset =
    find_position(
      text,
      String.length(text),
      end_scan_line,
      end_character,
      end_scan_offset,
    );
  (start_offset, end_offset);
};
