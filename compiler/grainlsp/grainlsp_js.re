open Js_of_ocaml;

let process_message_internal = (msg: string, first: bool) => {
  Lspserver.process_js_message(msg, first);
};

let _ =
  Js.export(
    "lspServer",
    [%js
      {
        as _;
        pub log = msg => Lspserver.js_log(Js.to_string(msg));
        pub enabledebug = Lspserver.enable_logging();
        pub processmessage = (msg, first) => {
          let js_string = Js.to_string(msg);
          let js_first = Js.to_bool(first);
          process_message_internal(js_string, js_first) |> Js.bool;
        }
      }
    ],
  );
