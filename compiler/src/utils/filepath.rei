type t;

let cwd: unit => t;

let from_fp: Fp.t(Fp.absolute) => t;
let to_fp: t => Fp.t(Fp.absolute);

let to_string: t => string;

let from_string: (~base: t=?, string) => option(t);
let from_absolute_string: string => t;

let dirname: t => t;
let basename: t => (string, string);
let append: (t, string) => t;

let is_absolute: string => bool;
let is_relative: string => bool;

module String: {let filename_to_module_name: string => string;};

module At: {
  let (/): (t, string) => t;
  let (/../): (t, string) => t;
  let (/../../): (t, string) => t;
  let (/../../../): (t, string) => t;
  let (/../../../../): (t, string) => t;
  let (/../../../../../): (t, string) => t;
  let (/../../../../../../): (t, string) => t;
};

module Args: {
  module ExistingFile: {
    type arg = t;

    let arg_cmdliner_converter: (
      string => [> | `Error(string) | `Ok(arg)],
      (Format.formatter, arg) => unit,
    );
  };

  module MaybeExistingFile: {
    type arg =
      | Exists(t)
      | NotExists(t);

    let arg_cmdliner_converter: (
      string => [> | `Error(string) | `Ok(arg)],
      (Format.formatter, arg) => unit,
    );
  };
};
