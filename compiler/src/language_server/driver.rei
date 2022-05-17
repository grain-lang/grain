type status =
  | Reading
  | Break;

let process: Protocol.request_message => status;
