import { WASI } from "@wasmer/wasi";

let preopens = {};
JSON.parse(process.env.__WASI_DIR)?.forEach((preopen) => {
    let [guestDir, hostDir = guestDir] = preopen.split("=");
    preopens[guestDir] = hostDir;
});

var wasi = new WASI({
    args: process.argv,
    env: process.env,
    preopens: preopens ?? [],
});

export const args_get = wasi.wasiImport.args_get
export const args_sizes_get = wasi.wasiImport.args_sizes_get
export const environ_get = wasi.wasiImport.environ_get
export const environ_sizes_get = wasi.wasiImport.environ_sizes_get
export const clock_res_get = wasi.wasiImport.clock_res_get
export const clock_time_get = wasi.wasiImport.clock_time_get
export const fd_advise = wasi.wasiImport.fd_advise
export const fd_allocate = wasi.wasiImport.fd_allocate
export const fd_close = wasi.wasiImport.fd_close
export const fd_datasync = wasi.wasiImport.fd_datasync
export const fd_fdstat_get = wasi.wasiImport.fd_fdstat_get
export const fd_fdstat_set_flags = wasi.wasiImport.fd_fdstat_set_flags
export const fd_fdstat_set_rights = wasi.wasiImport.fd_fdstat_set_rights
export const fd_filestat_get = wasi.wasiImport.fd_filestat_get
export const fd_filestat_set_size = wasi.wasiImport.fd_filestat_set_size
export const fd_filestat_set_times = wasi.wasiImport.fd_filestat_set_times
export const fd_prestat_get = wasi.wasiImport.fd_prestat_get
export const fd_prestat_dir_name = wasi.wasiImport.fd_prestat_dir_name
export const fd_pwrite = wasi.wasiImport.fd_pwrite
export const fd_write = wasi.wasiImport.fd_write
export const fd_pread = wasi.wasiImport.fd_pread
export const fd_read = wasi.wasiImport.fd_read
export const fd_readdir = wasi.wasiImport.fd_readdir
export const fd_renumber = wasi.wasiImport.fd_renumber
export const fd_seek = wasi.wasiImport.fd_seek
export const fd_tell = wasi.wasiImport.fd_tell
export const fd_sync = wasi.wasiImport.fd_sync
export const path_create_directory = wasi.wasiImport.path_create_directory
export const path_filestat_get = wasi.wasiImport.path_filestat_get
export const path_filestat_set_times = wasi.wasiImport.path_filestat_set_times
export const path_link = wasi.wasiImport.path_link
export const path_open = wasi.wasiImport.path_open
export const path_readlink = wasi.wasiImport.path_readlink
export const path_remove_directory = wasi.wasiImport.path_remove_directory
export const path_rename = wasi.wasiImport.path_rename
export const path_symlink = wasi.wasiImport.path_symlink
export const path_unlink_file = wasi.wasiImport.path_unlink_file
export const poll_oneoff = wasi.wasiImport.poll_oneoff
export const proc_exit = wasi.wasiImport.proc_exit
export const proc_raise = wasi.wasiImport.proc_raise
export const random_get = wasi.wasiImport.random_get
export const sched_yield = wasi.wasiImport.sched_yield
export const sock_recv = wasi.wasiImport.sock_recv
export const sock_send = wasi.wasiImport.sock_send
export const sock_shutdown = wasi.wasiImport.sock_shutdown

export const _setMemory = wasi.setMemory.bind(wasi)
