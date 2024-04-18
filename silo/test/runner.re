open TestFramework;

let process_dir = Fp.absoluteCurrentPlatformExn(Unix.getcwd());

let open_process = (~dir=process_dir, args) => {
  Fs.mkDirPExn(dir);
  Unix.chdir(Fp.toString(dir));

  let (stdout, stdin, stderr) =
    Unix.open_process_args_full(args[0], args, Unix.environment());

  let current_time = Unix.time();

  let out_eof = ref(false);
  let err_eof = ref(false);

  let out_buf = Buffer.create(1024);
  let err_buf = Buffer.create(1024);

  // Windows buffers output, so read channels as the subprocess is running
  while ((! out_eof^ || ! err_eof^) && Unix.time() < current_time +. 15.) {
    try(Buffer.add_channel(out_buf, stdout, 1024)) {
    | End_of_file => out_eof := true
    };
    try(Buffer.add_channel(err_buf, stderr, 1024)) {
    | End_of_file => err_eof := true
    };
  };

  let timed_out = Unix.time() > current_time +. 15.;

  let status = Unix.close_process_full((stdout, stdin, stderr));
  let out = Buffer.contents(out_buf);
  let err = Buffer.contents(err_buf);

  let code =
    switch (status) {
    | Unix.WEXITED(code) => code
    | _ => failwith("process did not exit properly")
    };

  let out =
    if (timed_out) {
      "Timed out!\n" ++ out;
    } else {
      out;
    };

  (code, out, err);
};
