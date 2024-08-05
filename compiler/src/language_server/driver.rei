type status =
  | Reading
  | Break
  | Exit(int);

let process: (~toggle_type_hints: bool, Protocol.request_message) => status;
