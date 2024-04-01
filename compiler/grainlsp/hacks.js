//Provides: caml_ml_open_descriptor_in
//Requires: caml_sys_fds,caml_sys_open,caml_raise_sys_error, caml_ml_channels
//Requires: fs_node_supported, caml_string_of_jsstring
function caml_ml_open_descriptor_in(fd) {
  var file = caml_sys_fds[fd];
  if (file.flags.wronly) caml_raise_sys_error("fd " + fd + " is writeonly");
  var refill = null;
  if (fd == 0 && fs_node_supported()) {
    var fs = require("fs");
    var Buffer = require("buffer").Buffer;
    refill = function () {
      var buf = Buffer.alloc(256);
      // Ref https://gist.github.com/espadrine/172658142820a356e1e0
      var stdinFd;
      var needsClose = false;
      try {
        stdinFd = fs.openSync("/dev/stdin", "rs");
        needsClose = true;
      } catch (e) {
        // Opening /dev/stdin can fail (on Windows or in pkg)
        if (globalThis.process.platform === "win32") {
          // On Windows, we need to use the stdin fd
          stdinFd = process.stdin.fd;
        } else {
          // On Linux, we just want to use fd 0
          // due to this nodejs bug: https://github.com/nodejs/node/issues/42826
          stdinFd = 0;
        }
      }
      var bytesRead = fs.readSync(fd, buf);
      if (needsClose) {
        fs.closeSync(stdinFd);
      }
      // Ensures we don't have any trailing, empty data
      var str = buf.slice(0, bytesRead).toString("utf8");
      return caml_string_of_jsstring(str);
    };
  }
  var channel = {
    file: file,
    offset: file.flags.append ? file.length() : 0,
    fd: fd,
    opened: true,
    out: false,
    buffer_curr: 0,
    buffer_max: 0,
    buffer: new Uint8Array(65536),
    refill: refill,
  };
  caml_ml_channels[channel.fd] = channel;
  return channel.fd;
}

//Provides: js_print_stdout (const)
//Requires: caml_utf16_of_utf8
function js_print_stdout(s) {
  var s = caml_utf16_of_utf8(s);
  var g = joo_global_object;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    // Writing too much data at once causes node's stdout socket to choke
    // so we write it using 8-length chunks (which seem to flush quickly)
    var written = 0;
    var chunk_len = 8;
    while (written <= s.length) {
      var chunk = s.slice(written, written + chunk_len);
      // TODO: Do we need to handle the `drained` flag returned? Throw when not drained?
      process.stdout.write(chunk);
      written += chunk_len;
    }
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if (s.charCodeAt(s.length - 1) == 10) s = s.substr(0, s.length - 1);
    var v = g.console;
    v && v.log && v.log(s);
  }
}
