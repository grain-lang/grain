let out = ref(None);

let initial_dest = Filename.concat(Filename.get_temp_dir_name(), "lsp.log");
out := Some(open_out(initial_dest));

let set_location = location => {
  switch (out^) {
  | None => ()
  | Some(out) => close_out(out)
  };
  output_string(stderr, "Setting log location: " ++ location ++ "\n");
  flush(stderr);
  out := Some(open_out(location));
};

let log = msg =>
  switch (out^) {
  | None => ()
  | Some(out) =>
    output_string(out, msg ++ "\n");
    flush(out);
  };
