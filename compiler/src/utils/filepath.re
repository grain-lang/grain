type t = Fp.t(Fp.absolute);

let from_fp = fp => fp;
let to_fp = fp => fp;

let (==) = Fp.absoluteEq;

let root = Fp.root;

let cwd = () => Fp.absoluteCurrentPlatformExn(Sys.getcwd());

let to_string = path => Fp.toString(path);

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
      | None => cwd()
      | Some(path) => path
      };
    Fp.join(b, path);
  };
};

let from_string = (~base=?, fname) => {
  switch (Fp.testForPath(fname)) {
  | Some(fp) => Some(derelativize(~base?, fp))
  | None => None
  };
};

let from_absolute_string = Fp.absoluteCurrentPlatformExn;

let dirname = path => Fp.dirName(path);
let basename = path => {
  switch (Fp.baseName(path)) {
  | Some(baseName) =>
    switch (String.rindex_opt(baseName, '.')) {
    | Some(index) => (
        String_utils.slice(~last=index, baseName),
        String_utils.slice(~first=index, baseName),
      )
    | None => (baseName, "")
    }
  | None => ("", "")
  };
};

let append = (dir, fname) => Fp.append(dir, fname);

let is_absolute = fname =>
  switch (Fp.testForPath(fname)) {
  | Some(Absolute(_)) => true
  | _ => false
  };

let is_relative = fname =>
  switch (Fp.testForPath(fname)) {
  | Some(Relative(_)) => true
  | _ => false
  };

// A "module" filepath is a relative filepath that doesn't start with `./`
let is_module = fname => is_relative(fname) && Filename.is_implicit(fname); // This is the one usage of Filename we should allow

module String = {
  // This module is converting strings into Fp.t and then back into Strings
  // TODO(#216): We should consider switching to type safe Fp.t where ever filepaths are used

  // TODO(#216): Turn this into a function that only operates on Fp
  let filename_to_module_name = fname => {
    switch (from_string(fname)) {
    | Some(path) =>
      let (base, _ext) = basename(path);
      String.capitalize_ascii(base);
    | None =>
      raise(
        Invalid_argument(
          Printf.sprintf("Invalid filepath (fname: '%s')", fname),
        ),
      )
    };
  };
};

module At = Fp.At;

module Args = {
  module ExistingFile = {
    type arg = t;

    type err =
      | InvalidPath(string)
      | NotExists(t)
      | NotFile(t);

    let query = fname => {
      switch (from_string(fname)) {
      | Some(abs_path) =>
        switch (Fs.query(abs_path)) {
        | Some(File(path, _stat)) => Ok(path)
        | Some(Dir(path, _))
        | Some(Other(path, _, _)) => Error(NotFile(path))
        | Some(Link(_, realpath, _)) =>
          Error(NotFile(derelativize(realpath)))
        | None => Error(NotExists(abs_path))
        }
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

    let arg_cmdliner_converter = (prsr, prntr);
  };

  module MaybeExistingFile = {
    type arg =
      | Exists(t)
      | NotExists(t);

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

    let arg_cmdliner_converter = (prsr, prntr);
  };
};
