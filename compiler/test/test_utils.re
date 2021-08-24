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
