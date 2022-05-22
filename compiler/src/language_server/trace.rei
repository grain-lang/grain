// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#setTrace
module RequestParams: {
  // Not abstract so it can pluck the value
  [@deriving yojson({strict: false})]
  type t = {value: Protocol.trace_value};
};

let set_level: string => unit;
let log: (~verbose: string=?, string) => unit;
