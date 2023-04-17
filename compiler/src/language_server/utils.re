open Grain_utils;
open Grain_diagnostics;
let uri_to_filename = (uri: Uri.t): string => {
  Uri.path(uri);
};

let filename_to_uri = (filename: string): Uri.t => {
  Uri.of_string(filename);
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
