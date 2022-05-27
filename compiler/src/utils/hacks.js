//Provides: os_type
var os_type = (globalThis.process &&
    globalThis.process.platform &&
    globalThis.process.platform == "win32") ? "Win32" : "Unix";

//Provides: unix_opendir
//Requires: caml_jsstring_of_string
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
function unix_opendir(path) {
    var fs = require('fs');
    var p = caml_jsstring_of_string(path);
    try {
        return fs.opendirSync(p);
    } catch (err) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args(err.code, err.syscall, err.path, err.errno));
    }
}

//Provides: unix_readdir
//Requires: caml_raise_end_of_file
//Requires: caml_string_of_jsstring
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
function unix_readdir(dir_handle) {
    var dir;
    try {
        dir = dir_handle.readSync();
    } catch (e) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "readdir", dir_handle.path));
    }
    if (dir === null) {
        caml_raise_end_of_file();
    } else {
        return caml_string_of_jsstring(dir.name);
    }
}

//Provides: unix_closedir
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
function unix_closedir(dir_handle) {
    try {
        dir_handle.closeSync();
    } catch (e) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "closedir", dir_handle.path));
    }
}

//Provides: win_findfirst
//Requires: caml_jsstring_of_string, caml_string_of_jsstring
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
//Requires: caml_raise_end_of_file
function win_findfirst(path) {
    var fs = require('fs');
    // The Windows code adds this glob to the path, so we need to remove it
    var p = caml_jsstring_of_string(path).replace("*.*", "");
    var dir_handle;
    try {
        dir_handle = fs.opendirSync(p);
    } catch (err) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args(err.code, err.syscall, err.path, err.errno));
    }
    var dir;
    try {
        dir = dir_handle.readSync();
    } catch (e) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "readdir", dir_handle.path));
    }
    if (dir === null) {
        caml_raise_end_of_file();
    } else {
        var first_entry = caml_string_of_jsstring(dir.name);
        // The Windows bindings type dir_handle as an `int` but it's not in JS
        return [0, first_entry, dir_handle];
    }
}

//Provides: win_findnext
//Requires: caml_raise_end_of_file
//Requires: caml_string_of_jsstring
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
function win_findnext(dir_handle) {
    var dir;
    try {
        dir = dir_handle.readSync();
    } catch (e) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "readdir", dir_handle.path));
    }
    if (dir === null) {
        caml_raise_end_of_file();
    } else {
        return caml_string_of_jsstring(dir.name);
    }
}

//Provides: win_findclose
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
function win_findclose(dir_handle) {
    try {
        dir_handle.closeSync();
    } catch (e) {
        var unix_error = caml_named_value('Unix.Unix_error');
        caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "closedir", dir_handle.path));
    }
}
