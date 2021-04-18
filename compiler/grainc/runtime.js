//Provides: internal_node_fs
var internal_node_fs = require('fs');

//Provides: internal_raise_unix_error
//Requires: caml_named_value, caml_string_of_jsbytes
function internal_raise_unix_error(tag, name, param) {
  throw [
    0,
    caml_named_value('Unix.Unix_error'),
    tag,
    caml_string_of_jsbytes(name || ""),
    caml_string_of_jsbytes(param || "")
  ];
}

//Provides: internal_unix_error_from_js
function internal_unix_error_from_js(err) {
  /* ===Unix.error===
   *
   * This array is in order of the variant in OCaml
   */
  var errors = [
    'E2BIG', 'EACCES', 'EAGAIN', 'EBADF', 'EBUSY', 'ECHILD', 'EDEADLK', 'EDOM',
    'EEXIST', 'EFAULT', 'EFBIG', 'EINTR', 'EINVAL', 'EIO', 'EISDIR', 'EMFILE',
    'EMLINK', 'ENAMETOOLONG', 'ENFILE', 'ENODEV', 'ENOENT', 'ENOEXEC', 'ENOLCK',
    'ENOMEM', 'ENOSPC', 'ENOSYS', 'ENOTDIR', 'ENOTEMPTY', 'ENOTTY', 'ENXIO',
    'EPERM', 'EPIPE', 'ERANGE', 'EROFS', 'ESPIPE', 'ESRCH', 'EXDEV', 'EWOULDBLOCK',
    'EINPROGRESS', 'EALREADY', 'ENOTSOCK', 'EDESTADDRREQ', 'EMSGSIZE',
    'EPROTOTYPE', 'ENOPROTOOPT', 'EPROTONOSUPPORT', 'ESOCKTNOSUPPORT',
    'EOPNOTSUPP', 'EPFNOSUPPORT', 'EAFNOSUPPORT', 'EADDRINUSE', 'EADDRNOTAVAIL',
    'ENETDOWN', 'ENETUNREACH', 'ENETRESET', 'ECONNABORTED', 'ECONNRESET', 'ENOBUFS',
    'EISCONN', 'ENOTCONN', 'ESHUTDOWN', 'ETOOMANYREFS', 'ETIMEDOUT', 'ECONNREFUSED',
    'EHOSTDOWN', 'EHOSTUNREACH', 'ELOOP', 'EOVERFLOW'
  ];
  var variantId = errors.indexOf(err.code);
  if (variantId < 0) {
    // TODO: Make sure this works `EUNKNOWNERR(int)`
    return BLOCK(0, err.errno);
  } else {
    return variantId;
  }
}

//Provides: internal_stats_to_file_kind
function internal_stats_to_file_kind(js_stats) {
  /* ===Unix.file_kind===
   * type file_kind =
   *     S_REG                       (** Regular file *)
   *   | S_DIR                       (** Directory *)
   *   | S_CHR                       (** Character device *)
   *   | S_BLK                       (** Block device *)
   *   | S_LNK                       (** Symbolic link *)
   *   | S_FIFO                      (** Named pipe *)
   *   | S_SOCK                      (** Socket *)
   */
  if (js_stats.isFile()) {
    return 0;
  }
  if (js_stats.isDirectory()) {
    return 1;
  }
  if (js_stats.isCharacterDevice()) {
    return 2;
  }
  if (js_stats.isBlockDevice()) {
    return 3;
  }
  if (js_stats.isSymbolicLink()) {
    return 4;
  }
  if (js_stats.isFIFO()) {
    return 5;
  }
  if (js_stats.isSocket()) {
    return 6;
  }
}

//Provides: caml_stats_from_js
//Requires: internal_stats_to_file_kind
function caml_stats_from_js(js_stats) {
  /* ===Unix.stats===
   * type stats =
   *  { st_dev : int;               (** Device number *)
   *    st_ino : int;               (** Inode number *)
   *    st_kind : file_kind;        (** Kind of the file *)
   *    st_perm : file_perm;        (** Access rights *)
   *    st_nlink : int;             (** Number of links *)
   *    st_uid : int;               (** User id of the owner *)
   *    st_gid : int;               (** Group ID of the file's group *)
   *    st_rdev : int;              (** Device ID (if special file) *)
   *    st_size : int;              (** Size in bytes *)
   *    st_atime : float;           (** Last access time *)
   *    st_mtime : float;           (** Last modification time *)
   *    st_ctime : float;           (** Last status change time *)
   *  }
   */
  return BLOCK(
    0,
    js_stats.dev,
    js_stats.ino,
    internal_stats_to_file_kind(js_stats),
    js_stats.mode,
    js_stats.nlink,
    js_stats.uid,
    js_stats.gid,
    js_stats.rdev,
    js_stats.size,
    js_stats.atimeMs,
    js_stats.mtimeMs,
    js_stats.ctimeMs
  );
}

//Provides: unix_stat
//Requires: internal_node_fs, caml_stats_from_js
//Requires: caml_jsbytes_of_string
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_stat(filename) {
  var js_filename = caml_jsbytes_of_string(filename);
  try {
    var js_stats = internal_node_fs.statSync(js_filename);
    return caml_stats_from_js(js_stats);
  } catch (err) {
    internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
  }
}

//Provides: unix_lstat
//Requires: internal_node_fs, caml_stats_from_js
//Requires: caml_jsbytes_of_string
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_lstat(filename) {
  var js_filename = caml_jsbytes_of_string(filename);
  try {
    var js_stats = internal_node_fs.lstatSync(js_filename);
    return caml_stats_from_js(js_stats);
  } catch (err) {
    internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
  }
}

//Provides: unix_mkdir
//Requires: internal_node_fs, caml_jsbytes_of_string
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_mkdir(dirname, perm) {
  var js_dirname = caml_jsbytes_of_string(dirname);
  if (!js_dirname.endsWith('/')) {
    js_dirname = js_dirname + '/';
  }
  try {
    internal_node_fs.mkdirSync(js_dirname, perm);
  } catch (err) {
    internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
  }
}

//Provides: unix_readlink
//Requires: internal_node_fs, caml_jsbytes_of_string, caml_string_of_jsbytes
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_readlink(filename) {
  var js_filename = caml_jsbytes_of_string(filename);
  try {
    var js_string = internal_node_fs.readlinkSync(js_filename, 'utf8');
    return caml_string_of_jsbytes(js_string);
  } catch (err) {
    internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
  }
}
