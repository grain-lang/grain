open Printf;
open Lexing;
open Filename;
open Arg;

let version = () => {
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };
  print_endline(version);
  exit(0);
};

let run = () => {
  let name = ref(None);
  let name_opt =
    Cmd.opt(
      ~names=["--name"],
      ~doc="Set the package name, defaults to the directory name",
      ~usage="<path>",
      ~value=name,
      ~conv=name =>
      Some(name)
    );
  let path = ref("");
  let path_arg =
    Cmd.arg(
      ~name="path", ~doc="Path for the new project", ~value=path, ~conv=path =>
      path
    );
  let new_ =
    Cmd.make(
      ~name="new",
      ~doc="Create a new Grain project",
      ~usage="[options] <path>",
      ~options=[name_opt],
      ~args=[path_arg],
      Cmd.Thunk(() => New.new_(~name=?name^, path^)),
    );

  let profile = ref(Build.Dev);
  let release_opt =
    Cmd.flag(
      ~names=["--release"],
      ~doc="Compile using the release profile (production mode)",
      ~value=profile,
      ~conv=() =>
      Build.Release
    );
  let build =
    Cmd.make(
      ~name="build",
      ~doc="Compile the current project",
      ~usage="[options]",
      ~options=[release_opt],
      Cmd.Thunk(() => Build.build(profile^)),
    );

  let version =
    Cmd.flag(
      ~names=["-v", "--version"],
      ~doc="Print version and exit",
      ~value=ref(false),
      ~conv=version,
    );
  let silo =
    Cmd.make(
      ~name="silo",
      ~doc="Build and manage Grain projects 🌾",
      ~usage="[options] <command>",
      ~options=[version],
      ~commands=[new_, build],
      Cmd.Help,
    );

  Cmd.parse(silo);
};
