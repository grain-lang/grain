exception Timeout;

// https://stackoverflow.com/a/10789674
let minisleep = (sec: float) => ignore(Unix.select([], [], [], sec));

let waitpid_timeout = (timeout: float, ~wait=0.1, pid: int) => {
  let start_time = Unix.time();
  minisleep(0.1);
  let (retpid_raw, status_raw) = Unix.waitpid([Unix.WNOHANG], pid);
  let retpid = ref(retpid_raw);
  let status = ref(status_raw);
  while (retpid^ == 0) {
    if (Unix.time() -. start_time > timeout) {
      raise(Timeout);
    };
    minisleep(wait);
    let (retpid_raw, status_raw) = Unix.waitpid([Unix.WNOHANG], pid);
    retpid := retpid_raw;
    status := status_raw;
  };
  (retpid^, status^);
};

let mk_loc =
    (file, (start_line, start_col, start_bol), (end_line, end_col, end_bol)) => {
  Grain_parsing.Location.{
    loc_start: {
      pos_fname: file,
      pos_lnum: start_line,
      pos_bol: start_bol,
      pos_cnum: start_col,
    },
    loc_end: {
      pos_fname: file,
      pos_lnum: end_line,
      pos_bol: end_bol,
      pos_cnum: end_col,
    },
    loc_ghost: false,
  };
};
