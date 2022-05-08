// This file is converting strings into Fp.t and then back into Strings
// TODO: We should consider switching to type safe Fp.t where ever filepaths are used

// TODO: Would be cool to upstream this to @reason-native/fp
let remove_extension = baseName => {
  switch (String.rindex_opt(baseName, '.')) {
  | Some(index) => String_utils.slice(~last=index, baseName)
  | None => baseName
  };
};

let replace_extension = (baseName, newExt) => {
  Printf.sprintf("%s.%s", remove_extension(baseName), newExt);
};

let to_fp = fname => {
  Fp.testForPath(fname);
};

let get_cwd = () => Sys.getcwd();

let to_absolute = path => {
  switch (to_fp(path)) {
  | None => failwith("Invalid path")
  | Some(Fp.Absolute(absPath)) => absPath
  | Some(Fp.Relative(relpath)) =>
    let base = Fp.absoluteCurrentPlatformExn(get_cwd());
    Fp.join(base, relpath);
  };
};

let filename_to_module_name = fname => {
  let baseName =
    Option.bind(to_fp(fname), p =>
      switch (p) {
      | Absolute(path) => Fp.baseName(path)
      | Relative(path) => Fp.baseName(path)
      }
    );
  let name =
    switch (baseName) {
    | Some(baseName) => remove_extension(baseName)
    | None =>
      raise(
        Invalid_argument(
          Printf.sprintf("Invalid filepath (fname: '%s')", fname),
        ),
      )
    };
  String.capitalize_ascii(name);
};

let ensure_parent_directory_exists = fname => {
  // TODO: Use `derelativize` once Fp.t is used everywhere
  let full_path =
    switch (to_fp(fname)) {
    | Some(Absolute(path)) => path
    | Some(Relative(path)) =>
      let base = Fp.absoluteCurrentPlatformExn(get_cwd());
      Fp.join(base, path);
    | None =>
      raise(
        Invalid_argument(
          Printf.sprintf("Invalid filepath (fname: '%s')", fname),
        ),
      )
    };
  // No longer swallowing the error because we can handle the CWD case
  // thus we should raise if something is actually wrong
  // TODO: Switch this to return the Result type
  Fs.mkDirPExn(Fp.dirName(full_path));
};

/**
  Converts the given path to an absolute path. Relative paths will be
  treated as relative to [base], if given; otherwise, they will be
  assumed to be relative to the current working directory.
*/
let derelativize = (~base=?, fname) => {
  let full_path =
    switch (to_fp(fname)) {
    | Some(Absolute(path)) => path
    | Some(Relative(path)) =>
      let b =
        switch (base) {
        | None => Fp.absoluteCurrentPlatform(get_cwd())
        | Some(path) => Fp.absoluteCurrentPlatform(path)
        };
      switch (b) {
      | Some(base) => Fp.join(base, path)
      | None =>
        raise(
          Invalid_argument(
            Printf.sprintf(
              "Invalid filepath base (base: '%s')",
              Option.value(~default="unknown", base),
            ),
          ),
        )
      };
    | None =>
      raise(
        Invalid_argument(
          Printf.sprintf("Invalid filepath (fname: '%s')", fname),
        ),
      )
    };

  Fp.toString(full_path);
};

// Recursive readdir
let rec readdir = dir => {
  Fs.readDirExn(dir)
  |> List.fold_left(
       (results, filepath) => {
         Sys.is_directory(Fp.toString(filepath))
           ? Array.append(results, readdir(filepath))
           : Array.append(results, [|filepath|])
       },
       [||],
     );
};

let realpath = path => {
  switch (Fp.testForPath(path)) {
  | None => None
  | Some(Fp.Absolute(abspath)) => Some(Fp.toString(abspath))
  | Some(Fp.Relative(relpath)) =>
    let base = Fp.absoluteCurrentPlatformExn(get_cwd());
    let full_path = Fp.join(base, relpath);
    Some(Fp.toString(full_path));
  };
};

let realpath_quick = path => {
  switch (realpath(path)) {
  | None => path
  | Some(rp) => rp
  };
};

let smart_cat = (dir, file) => {
  switch (Fp.absoluteCurrentPlatform(dir)) {
  | None => Filename.concat(dir, file)
  | Some(abspath) =>
    switch (Fp.relative(file)) {
    | None => Filename.concat(Fp.toString(abspath), file)
    | Some(relpath) => Fp.toString(Fp.join(abspath, relpath))
    }
  };
};

let canonicalize_relpath = (base_path, unit_name) => {
  // PRECONDITION: is_relpath(unit_name) == true
  let abs_base_path =
    switch (realpath(base_path)) {
    | None =>
      failwith(
        Printf.sprintf(
          "Internal Grain error; please report! testForPath failed: %s",
          base_path,
        ),
      )
    | Some(abspath) => abspath
    };
  smart_cat(abs_base_path, unit_name);
};
