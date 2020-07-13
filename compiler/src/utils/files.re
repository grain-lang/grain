// This file is converting strings into Fp.t and then back into Strings
// TODO: We should consider switching to type safe Fp.t where ever filepaths are used

// TODO: Would be cool to upstream this to @reason-native/fp
let remove_extension = baseName => {
  switch (String.rindex_opt(baseName, '.')) {
  | Some(index) => String_utils.slice(~last=index, baseName)
  | None => baseName
  };
};

let filename_to_module_name = fname => {
  let baseName =
    Option.bind(Fp.testForPath(fname), p =>
      switch (p) {
      | Absolute(path) => Fp.baseName(path)
      | Relative(path) => Fp.baseName(path)
      }
    );
  switch (baseName) {
  | Some(baseName) => remove_extension(baseName)
  | None =>
    raise(
      Invalid_argument(
        Printf.sprintf("Invalid filepath (fname: '%s')", fname),
      ),
    )
  };
};

let ensure_parent_directory_exists = fname => {
  // TODO: Use `derelativize` once Fp.t is used everywhere
  let fullPath =
    switch (Fp.testForPath(fname)) {
    | Some(Absolute(path)) => path
    | Some(Relative(path)) =>
      let cwd = Fp.absoluteExn(Sys.getcwd());
      Fp.join(cwd, path);
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
    switch (Fp.testForPath(fname)) {
    | Some(Absolute(path)) => path
    | Some(Relative(path)) =>
      let b =
        switch (base) {
        | None => Fp.absolute(Sys.getcwd())
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
