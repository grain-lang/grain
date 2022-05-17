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

let jsonrpc: string;

let read_message: in_channel => protocol_msg;

let send: (out_channel, string) => unit;
