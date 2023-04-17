open Grain_utils;
open Grain_diagnostics;

let uri_to_filename: Protocol.uri => string;
let filename_to_uri: string => Protocol.uri;

let loc_to_range: Grain_parsing.Location.t => Protocol.range;
