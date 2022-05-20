type status =
  | Reading
  | Break
  | Exit(int);

let process: Protocol.request_message => status;
