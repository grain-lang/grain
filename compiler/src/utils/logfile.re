type log_status =
  | Log(out_channel)
  | NoLog;

let log = ref(NoLog);

let open_ = file => {
  switch (log^) {
  | NoLog => ()
  | Log(out) => close_out(out)
  };

  log := Log(open_out(file));
};

let close = () => {
  switch (log^) {
  | NoLog => ()
  | Log(out) => close_out(out)
  };
};

let log = msg =>
  switch (log^) {
  | NoLog => ()
  | Log(out) =>
    output_string(out, msg ++ "\n");
    flush(out);
  };
