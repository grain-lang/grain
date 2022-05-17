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
type range = {
  start: position,
  [@key "end"]
  range_end: position,
};

[@deriving yojson]
type marked_string = {
  language: string,
  value: string,
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
