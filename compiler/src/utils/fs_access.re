/**
 Wrapper around file system accesses which provides
 various forms of caching.
 */

type eol =
  | CRLF
  | LF;

let determine_eol = line => {
  switch (line) {
  | Some(line) when String.length(line) > 0 =>
    // check what the last char was
    if (String.ends_with(~suffix="\r", line)) {
      CRLF;
    } else {
      LF;
    }
  | _ =>
    // must use OS default as this file has no newline we can use
    Sys.win32 ? CRLF : LF
  };
};

// TODO(#598): Should be safe for now, but we should harden this against
// relative paths by converting everything to absolute paths

let modified_cache = Hashtbl.create(16);

let last_modified = f => {
  switch (Hashtbl.find_opt(modified_cache, f)) {
  | Some(t) => t
  | None =>
    let t = Unix.(stat(f).st_mtime);
    Hashtbl.add(modified_cache, f, t);
    t;
  };
};

let exists_cache = Hashtbl.create(16);

let file_exists = f => {
  switch (Hashtbl.find_opt(exists_cache, f)) {
  | Some(e) => e
  | None =>
    let e = Sys.file_exists(f);
    Hashtbl.add(exists_cache, f, e);
    e;
  };
};

let cache_flushers: ref(list((string => unit, unit => unit))) = ref([]);

let register_cache_flusher = f => {
  cache_flushers := [f, ...cache_flushers^];
};

let flush_all_cached_data = () => {
  Hashtbl.clear(exists_cache);
  Hashtbl.clear(modified_cache);
  List.iter(((_, flusher)) => flusher(), cache_flushers^);
};

/** For writing out compiled output. Flushes caches as appropriate */
let open_file_for_writing = f => {
  let oc = open_out_bin(f);
  Hashtbl.remove(exists_cache, f);
  Hashtbl.add(exists_cache, f, true);
  Hashtbl.remove(modified_cache, f);
  List.iter(((flusher, _)) => flusher(f), cache_flushers^);
  oc;
};

/** Recursive readdir */
let rec readdir = dir => {
  Fs.readDirExn(dir)
  |> List.fold_left(
       (results, filepath) => {
         Sys.is_directory(Filepath.to_string(filepath))
           ? Array.append(results, readdir(filepath))
           : Array.append(results, [|filepath|])
       },
       [||],
     );
};

let ensure_parent_directory_exists = fname => {
  // TODO(#216): Cleanup once Fp.t is used everywhere
  let full_path = Filepath.String.derelativize(fname);
  // No longer swallowing the error because we can handle the CWD case
  // thus we should raise if something is actually wrong
  // TODO: Switch this to return the Result type
  Fs.mkDirPExn(Fp.dirName(full_path));
};
