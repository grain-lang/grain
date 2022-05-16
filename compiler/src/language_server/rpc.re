let windows_mode = ref(false);

[@deriving yojson]
type msg_id = int;

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
  id: int,
  result: list(lsp_lens_t),
};

[@deriving yojson]
type marked_string = {
  language: string,
  value: string,
};
[@deriving yojson]
type null_response = {
  jsonrpc: string,
  id: int,
  result: option(string),
};

[@deriving yojson]
type definition_result = {
  uri: string,
  range,
};

[@deriving yojson]
type definition_response = {
  jsonrpc: string,
  id: int,
  result: definition_result,
};

let jsonrpc = "2.0";

let parse_message = raw => {
  let json = Yojson.Safe.from_string(raw);

  let action =
    Yojson.Safe.Util.member("method", json) |> Yojson.Safe.Util.to_string;

  let id_opt =
    Yojson.Safe.Util.member("id", json) |> Yojson.Safe.Util.to_int_option;

  switch (id_opt) {
  | None => Notification(action, json)
  | Some(id) => Message(id, action, json)
  };
};

let read_message = (input): protocol_msg => {
  Log.log("read_message");

  // TODO: Catch error?
  let clength = input_line(input);

  // This feeld a bit backwards, but on a Windows machine the \r will have been stripped out automatically
  // and so bizarrley if the Windows separator is there, then this is not a Windows machine!
  // if (!String.contains(clength, '\r')) {
  //   Log.log("Enabling Windows mode");
  //   windows_mode := true;
  // };

  let cl = "Content-Length: ";
  let cll = String.length(cl);
  if (String.sub(clength, 0, cll) == cl) {
    /* if on windows, dont need the extra -1 */

    let offset = windows_mode^ ? 0 : (-1); /* -1 for trailing \r */

    // Can't use from_channel as we need to read the number of characters specified only

    let num =
      String.sub(clength, cll, String.length(clength) - cll + offset);
    let num = (num |> int_of_string) + (windows_mode^ ? 1 : 2);
    let buffer = Buffer.create(num);
    Buffer.add_channel(buffer, input, num);
    let raw = Buffer.contents(buffer);

    parse_message(raw);
  } else {
    Error("Invalid header");
  };
};

let send = (output, content) => {
  let length = String.length(content);

  let sep = windows_mode^ ? "\n\n" : "\r\n\r\n";

  let len = string_of_int(length);

  let msg = "Content-Length: " ++ len ++ sep ++ content;

  Log.log(msg);

  output_string(output, msg);

  flush(output);
};

let send_null_message = (output, id) => {
  let empty_response: null_response = {jsonrpc: "2.0", id, result: None};
  let res = null_response_to_yojson(empty_response);
  let str_json = Yojson.Safe.to_string(res);
  send(output, str_json);
};

let send_lenses = (~output, ~id: int, lenses: list(lens_t)) => {
  let converted_lenses =
    List.map(
      (l: lens_t) => {
        let rstart: position = {line: l.line - 1, character: 1};
        let rend: position = {line: l.line - 1, character: 1};
        let range = {
          {start: rstart, range_end: rend};
        };

        //TODO: Why was this "command string"
        let command = {title: l.signature};
        let lsp_lens: lsp_lens_t = {range, command};
        lsp_lens;
      },
      lenses,
    );

  let response: lens_response = {jsonrpc, id, result: converted_lenses};
  let res = lens_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);
  send(output, str_json);
};
