type trace =
  | Off
  | Messages
  | Verbose;

let trace_level = ref(Off);

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#setTrace
module RequestParams = {
  // Calling this "RequestParams" is lightly disingenuous because it is a notification sent *from* the client
  [@deriving yojson({strict: false})]
  type t = {value: Protocol.trace_value};
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#logTrace
module NotificationParams = {
  [@deriving yojson]
  type t = {
    message: string,
    [@default None]
    verbose: option(string),
  };
};

let set_level = (level: string) => {
  let level =
    switch (level) {
    | "verbose" => Verbose
    | "messages" => Messages
    | _ => Off
    };
  trace_level := level;
};

let log = (~verbose=?, message: string) =>
  switch (trace_level^) {
  | Off => ()
  | Messages =>
    Protocol.notification(
      ~method="$/logTrace",
      NotificationParams.to_yojson({message, verbose: None}),
    )
  | Verbose =>
    Protocol.notification(
      ~method="$/logTrace",
      NotificationParams.to_yojson({message, verbose}),
    )
  };
