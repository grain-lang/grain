type error =
  | InvalidPath(string)
  | MustProvideName(string);
exception NewError(error);

let get_cwd = () => Fp.absoluteCurrentPlatformExn(Sys.getcwd());
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

let new_ = (~name=?, path) => {
  let abs_path =
    switch (Fp.testForPath(path)) {
    | Some(path) => derelativize(path)
    | None => raise(NewError(InvalidPath(path)))
    };
  let name =
    switch (name) {
    | Some(name) => name
    | None =>
      switch (Fp.baseName(abs_path)) {
      | Some(name) => name
      | None => raise(NewError(MustProvideName(path)))
      }
    };

  Fs.mkDirPExn(abs_path);
  Fs.mkDirPExn(Fp.join(abs_path, Fp.relativeExn("src")));

  let oc =
    open_out(Fp.toString(Fp.join(abs_path, Fp.relativeExn("silo.toml"))));
  Printf.fprintf(oc, "[package]\nname = \"%s\"\n", name);
  close_out(oc);

  let oc =
    open_out(
      Fp.toString(Fp.join(abs_path, Fp.relativeExn("src/main.gr"))),
    );
  Printf.fprintf(oc, "module Main\n\nprint(\"Hello, world!\")\n");
  close_out(oc);
};

let _ =
  Printexc.register_printer(exn => {
    switch (exn) {
    | NewError(error) =>
      switch (error) {
      | InvalidPath(path) => Some("Invalid path " ++ path)
      | MustProvideName(path) =>
        Some(
          Printf.sprintf(
            "The path at %s does not have a directory name; supply the --name flag to set the package name manually",
            path,
          ),
        )
      }
    | _ => None
    }
  });
