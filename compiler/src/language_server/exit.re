open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit
module RequestParams = {
  [@deriving yojson]
  type t = unit;
};
