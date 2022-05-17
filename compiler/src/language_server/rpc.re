open Grain_utils;

let windows_mode = ref(false);

[@deriving yojson]
type version = string;

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
type range = {
  start: position,
  [@key "end"]
  range_end: position,
};

let version: version = "2.0";

let read_message = (input): protocol_msg => {
  Logfile.log("read_message");

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

    let json = Yojson.Safe.from_string(raw);

    let action =
      Yojson.Safe.Util.member("method", json) |> Yojson.Safe.Util.to_string;

    let id_opt =
      Yojson.Safe.Util.member("id", json) |> Yojson.Safe.Util.to_int_option;

    switch (id_opt) {
    | None => Notification(action, json)
    | Some(id) => Message(id, action, json)
    };
  } else {
    Error("Invalid header");
  };
};

let send = (output, content) => {
  let length = String.length(content);

  let sep = windows_mode^ ? "\n\n" : "\r\n\r\n";

  let len = string_of_int(length);

  let msg = "Content-Length: " ++ len ++ sep ++ content;

  Logfile.log(msg);

  output_string(output, msg);

  flush(output);
};
