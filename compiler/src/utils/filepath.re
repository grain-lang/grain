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

// All uses of `Filename` from OCaml should be constrained to this file because we need to do
// normalization on the filepaths those functions produce.
module String = {
  // This module is converting strings into Fp.t and then back into Strings
  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used

  let derelativize = (~base=?, fname) => {
    let base =
      switch (base) {
      | None => Some(get_cwd())
      | Some(path) => Fp.absoluteCurrentPlatform(path)
      };

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

  let normalize_separators = path =>
    if (Sys.unix) {
      path;
    } else {
      // If we aren't on a Unix-style system, convert `\\` separators to `/`
      // This is needed because using `Filename` from OCaml stdlib doesn't get along with Fp
      let windows_sep = Str.regexp("\\");
      let normal_sep = "/";
      Str.global_replace(windows_sep, normal_sep, path);
    };

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let concat = (p1, p2) => {
    normalize_separators(Filename.concat(p1, p2));
  };

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let dirname = path => {
    normalize_separators(Filename.dirname(path));
  };

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let basename = Filename.basename;

  // TODO: This is poorly named
  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let is_relpath = path =>
    Filename.is_relative(path) && !Filename.is_implicit(path);

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let is_relative = Filename.is_relative;

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let check_suffix = Filename.check_suffix;

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let chop_suffix = Filename.chop_suffix;

  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used
  let extension = Filename.extension;

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
    | None => concat(dir, file)
    | Some(abspath) =>
      switch (Fp.testForPath(file)) {
      | None => concat(dir, file)
      | Some(Absolute(path)) => to_string(path)
      | Some(Relative(relpath)) => to_string(Fp.join(abspath, relpath))
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
};

module Args = {
  module ExistingFile = {
    type t = Fp.t(Fp.absolute);

    type err =
      | InvalidPath(string)
      | NotExists(Fp.t(Fp.absolute))
      | NotFile(Fp.t(Fp.absolute));

    let query = fname => {
      switch (from_string(fname)) {
      | Some(path) =>
        let abs_path = derelativize(path);
        switch (Fs.query(abs_path)) {
        | Some(File(path, _stat)) => Ok(path)
        | Some(Dir(path, _))
        | Some(Other(path, _, _)) => Error(NotFile(path))
        | Some(Link(_, realpath, _)) =>
          Error(NotFile(derelativize(realpath)))
        | None => Error(NotExists(abs_path))
        };
      | None => Error(InvalidPath(fname))
      };
    };

    let prsr = fname => {
      switch (query(fname)) {
      | Ok(file) => `Ok(file)
      | Error(NotFile(path)) =>
        `Error(
          Format.sprintf("%s exists but is not a file", to_string(path)),
        )
      | Error(NotExists(path)) =>
        `Error(Format.sprintf("%s does not exist", to_string(path)))
      | Error(InvalidPath(fname)) =>
        `Error(Format.sprintf("Invalid path: %s", fname))
      };
    };

    let prntr = (formatter, value) => {
      Format.fprintf(formatter, "File: %s", to_string(value));
    };

    let cmdliner_converter = (prsr, prntr);
  };

  module ExistingFileOrDirectory = {
    type t =
      | File(Fp.t(Fp.absolute))
      | Directory(Fp.t(Fp.absolute));

    type err =
      | InvalidPath(string)
      | NotExists(Fp.t(Fp.absolute))
      | InvalidFileType(Fp.t(Fp.absolute));

    let query = fname => {
      switch (from_string(fname)) {
      | Some(path) =>
        let abs_path = derelativize(path);
        switch (Fs.query(abs_path)) {
        | Some(File(path, _stat)) => Ok(File(path))
        | Some(Dir(path, _)) => Ok(Directory(path))
        | Some(Other(path, _, _)) => Error(InvalidFileType(path))
        | Some(Link(_, realpath, _)) =>
          Error(InvalidFileType(derelativize(realpath)))
        | None => Error(NotExists(abs_path))
        };
      | None => Error(InvalidPath(fname))
      };
    };

    let prsr = fname => {
      switch (query(fname)) {
      | Ok(file) => `Ok(file)
      | Error(InvalidFileType(path)) =>
        `Error(
          Format.sprintf(
            "%s exists but is not a file or directory",
            to_string(path),
          ),
        )
      | Error(NotExists(path)) =>
        `Error(Format.sprintf("%s does not exist", to_string(path)))
      | Error(InvalidPath(fname)) =>
        `Error(Format.sprintf("Invalid path: %s", fname))
      };
    };

    let prntr = (formatter, value) => {
      switch (value) {
      | File(path) => Format.fprintf(formatter, "File: %s", to_string(path))
      | Directory(path) =>
        Format.fprintf(formatter, "Directory: %s", to_string(path))
      };
    };

    let cmdliner_converter = (prsr, prntr);
  };

  module MaybeExistingFile = {
    type t =
      | Exists(ExistingFile.t)
      | NotExists(ExistingFile.t);

    let prsr = fname => {
      switch (ExistingFile.query(fname)) {
      | Ok(path) => `Ok(Exists(path))
      | Error(NotExists(path)) => `Ok(NotExists(path))
      | Error(NotFile(path)) =>
        `Error(
          Format.sprintf("%s exists but is not a file", to_string(path)),
        )
      | Error(InvalidPath(fname)) =>
        `Error(Format.sprintf("Invalid path: %s", fname))
      };
    };

    let prntr = (formatter, value) => {
      switch (value) {
      | Exists(path) =>
        Format.fprintf(formatter, "File: %s", to_string(path))
      | NotExists(path) =>
        Format.fprintf(formatter, "Path: %s", to_string(path))
      };
    };

    let cmdliner_converter = (prsr, prntr);
  };

  module MaybeExistingFileOrDirectory = {
    type t =
      | Exists(ExistingFileOrDirectory.t)
      | NotExists(Fp.t(Fp.absolute));

    let prsr = fname => {
      switch (ExistingFileOrDirectory.query(fname)) {
      | Ok(path) => `Ok(Exists(path))
      | Error(NotExists(path)) => `Ok(NotExists(path))
      | Error(InvalidFileType(path)) =>
        `Error(
          Format.sprintf(
            "%s exists but is not a file or directory",
            to_string(path),
          ),
        )
      | Error(InvalidPath(fname)) =>
        `Error(Format.sprintf("Invalid path: %s", fname))
      };
    };

    let prntr = (formatter, value) => {
      switch (value) {
      | Exists(File(path)) =>
        Format.fprintf(formatter, "File: %s", to_string(path))
      | Exists(Directory(path)) =>
        Format.fprintf(formatter, "Directory: %s", to_string(path))
      | NotExists(path) =>
        Format.fprintf(formatter, "Path: %s", to_string(path))
      };
    };

    let cmdliner_converter = (prsr, prntr);
  };
};
