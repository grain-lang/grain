let out = ref(None);

type log_level =
  | NoLog
  | DebugLog;

let log_dest = "lsp.log";

let set_level = (lvl: log_level) => {
  switch (out^) {
  | None => ()
  | Some(out) => close_out(out)
  };

  switch (lvl) {
  | NoLog =>
    output_string(stderr, "Logging disabled\n");
    flush(stderr);
  | DebugLog =>
    output_string(stderr, "Debug logging enabled to " ++ log_dest ++ "\n");
    flush(stderr);
    out := Some(open_out(log_dest));
  };
};

let close_log = () => {
  switch (out^) {
  | None => ()
  | Some(out) => close_out(out)
  };
};

let log = msg =>
  switch (out^) {
  | None => ()
  | Some(out) =>
    output_string(out, msg ++ "\n");
    flush(out);
  };
