[@deriving (enum, yojson)]
type message_type =
  | [@value 1] Error
  | Warning
  | Info
  | Log
  | Debug;

[@deriving yojson]
type t = {
  [@key "type"]
  type_: int,
  message: string,
};

let log = (~message_type=Info, message) => {
  let type_ = message_type_to_enum(message_type);
  Protocol.notification(
    ~method="window/logMessage",
    to_yojson({
      message,
      type_,
    }),
  );
};
