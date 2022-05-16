[@deriving yojson]
type msg_id;

type protocol_msg =
  | Message(msg_id, string, Yojson.Safe.t)
  | Error(string)
  | Notification(string, Yojson.Safe.t);

[@deriving yojson]
type position = {
  line: int,
  character: int,
};

[@deriving yojson]
type lens_t = {
  line: int,
  signature: string,
};

[@deriving yojson]
type range_t = {
  start_line: int,
  start_char: int,
  end_line: int,
  end_char: int,
};

[@deriving yojson]
type range = {
  start: position,
  [@key "end"]
  range_end: position,
};

[@deriving yojson]
type command_t = {
  title: string,
  // command: string,
};

[@deriving yojson]
type lsp_lens_t = {
  range,
  command: command_t,
};

[@deriving yojson]
type lens_response = {
  jsonrpc: string,
  id: msg_id,
  result: list(lsp_lens_t),
};

[@deriving yojson]
type markup_content = {
  kind: string,
  value: string,
};

[@deriving yojson]
type marked_string = {
  language: string,
  value: string,
};

[@deriving yojson]
type hover_result = {
  contents: markup_content,
  range,
};
[@deriving yojson]
type null_response = {
  jsonrpc: string,
  id: msg_id,
  result: option(string),
};

[@deriving yojson]
type hover_response = {
  jsonrpc: string,
  id: msg_id,
  result: hover_result,
};

[@deriving yojson]
type definition_result = {
  uri: string,
  range,
};

[@deriving yojson]
type definition_response = {
  jsonrpc: string,
  id: msg_id,
  result: definition_result,
};

let jsonrpc: string;

let read_message: in_channel => protocol_msg;

let send: (out_channel, string) => unit;

let send_null_message: (out_channel, msg_id) => unit;

let send_lenses: (~output: out_channel, ~id: msg_id, list(lens_t)) => unit;

let send_hover:
  (~output: out_channel, ~id: msg_id, ~range: range_t, string) => unit;
