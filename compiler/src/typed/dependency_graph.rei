open Grain_utils;

module type Dependency_value = {
  type t;
  let get_dependencies: (t, Filepath.t => option(t)) => list(t);
  let get_filename: t => Filepath.t;
  let is_up_to_date: t => bool;
  let check_up_to_date: t => unit;
  // Guaranteed to only be called when dependencies are compiled.
  let compile_module: (~loc: Grain_parsing.Location.t=?, t) => unit;
  let compare: (t, t) => int;
  let hash: t => int;
  let equal: (t, t) => bool;
};

module Make:
  (DV: Dependency_value) =>
   {
    /**
   Loads the given module in the dependency graph, along
   with its dependencies.
   */
    let register: DV.t => unit;

    /**
   Returns the dependency graph node corresponding to the given filename.
   */
    let lookup_filename: Filepath.t => option(DV.t);

    /**
   Compiles the dependencies of the given filename.
   */
    let compile_dependencies:
      (~loc: Grain_parsing.Location.t=?, Filepath.t) => unit;

    /**
   Dumps the edges in this graph to stderr.
   */
    let dump: unit => unit;

    /**
   Clears the dependency graph. This should only be called in unit tests.
   */
    let clear: unit => unit;
  };
