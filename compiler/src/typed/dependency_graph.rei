module type Dependency_value = {
  type t;
  let get_dependencies: (t, string => option(t)) => list(t);
  let get_srcname: t => string;
  let get_filename: t => string;
  let is_up_to_date: t => bool;
  let check_up_to_date: t => unit;
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
    let lookup_filename: string => option(DV.t);

    /**
   Returns a topologically sorted list of all dependencies.
   */
    let get_dependencies: unit => list(string);

    /**
   Returns a topologically sorted list of out of date dependencies.
   */
    let get_out_of_date_dependencies: unit => list(string);

    /**
   Dumps the edges in this graph to stderr.
   */
    let dump: unit => unit;

    /**
   Clears the dependency graph. This should only be called in unit tests.
   */
    let clear: unit => unit;
  };
