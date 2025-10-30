// A moderately bad version parsing library

type t = {
  major: int,
  minor: int,
  patch: int,
};

exception Invalid_version_string(string);

let parse = str => {
  let re = Str.regexp({|^v?\([0-9]+\)\.\([0-9]+\)\.\([0-9]+\)$|});
  if (Str.string_match(re, str, 0)) {
    let major = int_of_string(Str.matched_group(1, str));
    let minor = int_of_string(Str.matched_group(2, str));
    let patch = int_of_string(Str.matched_group(3, str));
    {
      major,
      minor,
      patch,
    };
  } else {
    raise(Invalid_version_string(str));
  };
};

// Adapted from https://github.com/dividat/ocaml-semver/blob/master/lib/semver.ml#L125-L141
let compare = (v1, v2) =>
  if (v1.major != v2.major) {
    compare(v1.major, v2.major);
  } else if (v1.minor != v2.minor) {
    compare(v1.minor, v2.minor);
  } else {
    compare(v1.patch, v2.patch);
  };

let equal = (v1, v2) => {
  compare(v1, v2) == 0;
};

let less_than = (v1, v2) => {
  compare(v1, v2) < 0;
};

let greater_than = (v1, v2) => {
  compare(v1, v2) > 0;
};

let () =
  Printexc.register_printer(exc =>
    switch (exc) {
    | Invalid_version_string(str) =>
      Some(Printf.sprintf("Invalid version string `%s`", str))
    | _ => None
    }
  );

module String = {
  let compare = (s1, s2) => {
    let v1 = parse(s1);
    let v2 = parse(s2);
    compare(v1, v2);
  };

  let equal = (s1, s2) => {
    compare(s1, s2) == 0;
  };

  let less_than = (s1, s2) => {
    compare(s1, s2) < 0;
  };

  let greater_than = (s1, s2) => {
    compare(s1, s2) > 0;
  };
};
