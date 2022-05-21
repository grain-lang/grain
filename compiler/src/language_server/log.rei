type debug_level =
  | Off
  | Debug;

type message_type =
  // Since these are using ppx_deriving enum, order matters
  | [@value 1] Error
  | Warning
  | Info
  | Log;

let set_debug_level: debug_level => unit;
let log_trace: string => unit;
let log_message: (~message_type: message_type, string) => unit;
