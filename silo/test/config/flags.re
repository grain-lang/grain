module C = Configurator.V1;

let () = {
  C.main(~name="grain_exe_flags", c => {
    let default = [];

    let flags =
      switch (C.ocaml_config_var(c, "system")) {
      | Some("macosx") =>
        // These flags preserve the original C++ error behavior. Ref https://github.com/ocaml/ocaml/issues/10423
        ["-cc", "clang++"]
      | Some("mingw64") =>
        // MinGW needs these flags passed directly to flexdll, to avoid needing MinGW locations in the path
        ["-ccopt", "-l:libpthread.a", "-ccopt", "-l:libstdc++.a"]
      | Some(_) => default
      | None => default
      };

    C.Flags.write_sexp("flags.sexp", flags);
  });
};
