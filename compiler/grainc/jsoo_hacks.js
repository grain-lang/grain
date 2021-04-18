//Provides: caml_sys_open
//Requires: caml_raise_sys_error, caml_global_data
//Requires: caml_create_bytes,MlFakeFile,MlNodeFile
//Requires: js_print_stderr, js_print_stdout
//Requires: caml_std_output
//Requires: resolve_fs_device
//Requires: caml_jsbytes_of_string
function caml_sys_open_internal(idx, output, file, flags) {
  if (caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  flags = flags ? flags : {};
  var info = {};
  info.file = file;
  info.offset = flags.append ? file.length() : 0;
  info.flags = flags;
  info.output = output;
  caml_global_data.fds[idx] = info;
  if (!caml_global_data.fd_last_idx || idx > caml_global_data.fd_last_idx)
    caml_global_data.fd_last_idx = idx;
  return idx;
}
function caml_sys_open(name, flags, _perms) {
  var f = {};
  while (flags) {
    switch (flags[1]) {
      case 0: f.rdonly = 1; break;
      case 1: f.wronly = 1; break;
      case 2: f.append = 1; break;
      case 3: f.create = 1; break;
      case 4: f.truncate = 1; break;
      case 5: f.excl = 1; break;
      case 6: f.binary = 1; break;
      case 7: f.text = 1; break;
      case 8: f.nonblock = 1; break;
    }
    flags = flags[2];
  }
  if (f.rdonly && f.wronly)
    caml_raise_sys_error(caml_jsbytes_of_string(name) + " : flags Open_rdonly and Open_wronly are not compatible");
  if (f.text && f.binary)
    caml_raise_sys_error(caml_jsbytes_of_string(name) + " : flags Open_text and Open_binary are not compatible");
  var root = resolve_fs_device(name);
  var file = root.device.open(root.rest, f);
  var idx = caml_global_data.fd_last_idx ? caml_global_data.fd_last_idx : 0;
  return caml_sys_open_internal(idx + 1, caml_std_output, file, f);
}
caml_sys_open_internal(0, caml_std_output, new MlNodeFile(0)); //stdin
caml_sys_open_internal(1, js_print_stdout, new MlFakeFile(caml_create_bytes(0))); //stdout
caml_sys_open_internal(2, js_print_stderr, new MlFakeFile(caml_create_bytes(0))); //stderr

//Provides: caml_make_path
//Requires: caml_current_dir
//Requires: caml_jsstring_of_string
function caml_make_path(name) {
  name = caml_jsstring_of_string(name);
  // We needed to provide our own `caml_make_path` because it doesn't check
  // for Windows drives as root
  if (name.charCodeAt(0) != 47 && name.charCodeAt(1) != 58)
    name = caml_current_dir + name;
  var comp = name.split("/");
  var ncomp = []
  for (var i = 0; i < comp.length; i++) {
    switch (comp[i]) {
      case "..": if (ncomp.length > 1) ncomp.pop(); break;
      case ".": break;
      case "": if (ncomp.length == 0) ncomp.push(""); break;
      default: ncomp.push(comp[i]); break
    }
  }
  ncomp.orig = name;
  return ncomp;
}
