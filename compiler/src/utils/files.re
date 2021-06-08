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

// These utilities are needed until https://github.com/facebookexperimental/reason-native/pull/251
// is merged into the reason-native/fp library to support Windows-style separators
let normalize_separators = path => {
  // We hardcode Windows separator because JSOO always acts as unix
  let windows_sep = Str.regexp("\\");
  let normal_sep = "/";
  Str.global_replace(windows_sep, normal_sep, path);
};

let to_fp = fname => {
  Fp.testForPath(normalize_separators(fname));
};

let get_cwd = () => normalize_separators(Sys.getcwd());

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
  let fullPath =
    switch (to_fp(fname)) {
    | Some(Absolute(path)) => path
    | Some(Relative(path)) =>
      let base = Fp.absoluteExn(get_cwd());
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
  Fs.mkDirPExn(Fp.dirName(fullPath));
};

/**
  Converts the given path to an absolute path. Relative paths will be
  treated as relative to [base], if given; otherwise, they will be
  assumed to be relative to the current working directory.
*/
let derelativize = (~base=?, fname) => {
  let fullPath =
    switch (to_fp(fname)) {
    | Some(Absolute(path)) => path
    | Some(Relative(path)) =>
      let b =
        switch (base) {
        | None => Fp.absolute(get_cwd())
        | Some(path) => Fp.absolute(path)
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

  Fp.toString(fullPath);
};

// Recursive readdir
let rec readdir = (dir, excludes) => {
  Sys.readdir(dir)
  |> Array.fold_left(
       (results, filename) => {
         let filepath = Filename.concat(dir, filename);
         Sys.is_directory(filepath)
         && List.for_all(
              exclude => !String_utils.starts_with(filename, exclude),
              excludes,
            )
           ? Array.append(results, readdir(filepath, excludes))
           : Array.append(results, [|filepath|]);
       },
       [||],
     );
};
