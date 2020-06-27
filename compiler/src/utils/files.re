let filename_to_module_name = fname => {
  open BatPathGen.OfString;
  let path = of_string(fname);
  try(name_core(path)) {
  | Invalid_argument(s) =>
    raise(Invalid_argument(Printf.sprintf("%s (fname: '%s')", s, fname)))
  };
};

let ensure_parent_directory_exists = fname => {
  open BatPathGen.OfString;
  let path = of_string(fname);
  try({
    let pdir = parent(path);
    try(ignore(Sys.is_directory(to_string(pdir)))) {
    | Sys_error(_) => Unix.mkdir(to_string(pdir), 0o755)
    };
  }) {
  | Invalid_argument(s) =>
    /* File appears to be in CWD. Eat the exception */
    ()
  };
};

/** Converts the given path to an absolute path. Relative paths will be
    treated as relative to [base], if given; otherwise, they will be
    assumed to be relative to the current working directory. */

let derelativize = (~base=?, fname) => {
  open BatPathGen.OfString;
  let path = of_string(fname);
  let path =
    if (is_absolute(path)) {
      path;
    } else {
      let base =
        of_string @@
        (
          switch (base) {
          | None => Sys.getcwd()
          | Some(path) => path
          }
        );

      Operators.(base /\/@ path);
    };

  to_string(normalize_in_tree(path));
};
