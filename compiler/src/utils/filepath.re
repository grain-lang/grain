let get_cwd = () => Fp.absoluteCurrentPlatformExn(Sys.getcwd());

let to_string = path => Fp.toString(path);

let from_string = fname => {
  Fp.testForPath(fname);
};

let dirname = path => Fp.dirName(path);

/**
  Converts the given path to an absolute path. Relative paths will be
  treated as relative to [base], if given; otherwise, they will be
  assumed to be relative to the current working directory.
*/
let derelativize = (~base=?, fname: Fp.firstClass) => {
  switch (fname) {
  | Absolute(path) => path
  | Relative(path) =>
    let b =
      switch (base) {
      | None => get_cwd()
      | Some(path) => path
      };
    Fp.join(b, path);
  };
};

module String = {
  // This module is converting strings into Fp.t and then back into Strings
  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used

  let derelativize = (~base=?, fname) => {
    let base =
      Option.bind(base, base =>
        switch (base) {
        | None => Some(get_cwd())
        | Some(path) => Fp.absoluteCurrentPlatform(path)
        }
      );

    switch (from_string(fname)) {
    | Some(fname) => derelativize(~base?, fname)
    | None =>
      raise(
        Invalid_argument(
          Printf.sprintf("Invalid filepath (fname: '%s')", fname),
        ),
      )
    };
  };

  // TODO: Upstream this to @reason-native/fp
  let remove_extension = baseName => {
    switch (String.rindex_opt(baseName, '.')) {
    | Some(index) => String_utils.slice(~last=index, baseName)
    | None => baseName
    };
  };

  // TODO: Upstream this to @reason-native/fp
  let replace_extension = (baseName, newExt) => {
    Printf.sprintf("%s.%s", remove_extension(baseName), newExt);
  };

  // TODO(#216): Turn this into a function that only operates on Fp
  let filename_to_module_name = fname => {
    let baseName =
      Option.bind(from_string(fname), p =>
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

  // TODO(#216): Turn this into a function that only operates on Fp
  let realpath = path => {
    switch (Fp.testForPath(path)) {
    | None => None
    | Some(Fp.Absolute(abspath)) => Some(to_string(abspath))
    | Some(Fp.Relative(relpath)) =>
      let base = get_cwd();
      let full_path = Fp.join(base, relpath);
      Some(to_string(full_path));
    };
  };

  // TODO(#216): Turn this into a function that only operates on Fp
  let realpath_quick = path => {
    switch (realpath(path)) {
    | None => path
    | Some(rp) => rp
    };
  };

  // TODO(#216): Turn this into a function that only operates on Fp
  let smart_cat = (dir, file) => {
    switch (Fp.absoluteCurrentPlatform(dir)) {
    | None => Filename.concat(dir, file)
    | Some(abspath) =>
      switch (Fp.relative(file)) {
      | None => Filename.concat(to_string(abspath), file)
      | Some(relpath) => to_string(Fp.join(abspath, relpath))
      }
    };
  };

  // TODO(#216): Turn this into a function that only operates on Fp
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

  // TODO(#216): Replace this with the `get_cwd` that operates on Fp
  let get_cwd = () => Sys.getcwd();
};
