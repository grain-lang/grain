type debug_level =
  | Off
  | Debug;

let debugging_level = ref(Off);

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#logTraceParams
module LogTraceParams = {
  [@deriving yojson]
  type t = {message: string};
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#logMessageParams

[@deriving (enum, yojson)]
type message_type =
  // Since these are using ppx_deriving enum, order matters
  | [@value 1] Error
  | Warning
  | Info
  | Log;

module LogMessageParams = {
  [@deriving yojson]
  type t = {
    [@key "type"]
    message_type,
    message: string,
  };
};

let set_debug_level = (level: debug_level) => {
  debugging_level := level;
};

let log_trace = (message: string) =>
  if (debugging_level^ == Debug) {
    Protocol.notification(
      ~method="$/logTrace",
      LogTraceParams.to_yojson({message: message}),
    );
  };

let log_message = (~message_type: message_type, message: string) =>
  if (debugging_level^ == Debug) {
    Protocol.notification(
      ~method="window/logMessage",
      LogMessageParams.to_yojson({message_type: Warning, message}),
    );
  };
