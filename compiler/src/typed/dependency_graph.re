/**
 Data Structure for managing graph of module dependencies during compilation
 */
open Graph;

module type Dependency_value = {
  type t;
  let get_dependencies: (t, string => option(t)) => list(t);
  let get_filename: t => string;
  let is_up_to_date: t => bool;
  let check_up_to_date: t => unit;
  let compile_module: (~loc: Grain_parsing.Location.t=?, t) => unit;
  let compare: (t, t) => int;
  let hash: t => int;
  let equal: (t, t) => bool;
};

type dependency_state =
  | Needs_processing
  | In_progress
  | Processed;

module type G = {
  type t;
  module V: Sig.COMPARABLE;
  let iter_vertex: (V.t => unit, t) => unit;
  let iter_succ: (V.t => unit, t, V.t) => unit;
};

module Make = (DV: Dependency_value) => {
  module G =
    Imperative.Digraph.Concrete({
      type t = (DV.t, ref(dependency_state));
      let compare = ((a, _), (b, _)) => DV.compare(a, b);
      let equal = ((a, _), (b, _)) => DV.equal(a, b);
      let hash = ((a, _)) => DV.hash(a);
    });

  module G_topological =
    Topological.Make_stable({
      type t = G.t;
      module V = G.V;
      let in_degree = G.in_degree;
      let iter_succ = G.iter_succ;
      let iter_vertex = G.iter_vertex;
      let to_string = ((a, _): G.V.t) => DV.get_filename(a);

      let contains = (g, v) => G.mem_vertex(g, v);

      let all_vertices = g => G.fold_vertex((v, acc) => [v, ...acc], g, []);
    });

  let graph = G.create();
  let filename_to_nodes = Hashtbl.create(16);

  let add_dependency = dependency => {
    G.add_vertex(graph, (dependency, ref(Needs_processing)));
  };

  let add_edge = (depender, dependee) => {
    G.add_edge(graph, dependee, depender);
  };

  let edge_exists = (depender, dependee) => {
    G.mem_edge(graph, dependee, depender);
  };

  let lookup_filename = Hashtbl.find_opt(filename_to_nodes);

  let rec do_register = (~parent=?, dependency) => {
    let as_vertex = (dependency, ref(Needs_processing));
    if (!G.mem_vertex(graph, as_vertex)) {
      // We have this guard in order to avoid traversing into recursive dependencies unnecessarily.
      // Note that the equality check for verticies is such that we will skip this
      // even if the dependency exists in the graph in another state.
      add_dependency(dependency);
      // Connect *child* to *parent*. We do the edges in reverse, since topological sorting
      // would otherwise provide dependencies in reverse.
      switch (parent) {
      | Some(p) when !DV.equal(p, dependency) =>
        add_edge((p, ref(Needs_processing)), as_vertex)
      | Some(_)
      | None => ()
      };
      // Process recursive dependencies
      let deps = DV.get_dependencies(dependency, lookup_filename);
      List.iter(
        d => Hashtbl.add(filename_to_nodes, DV.get_filename(d), d),
        deps,
      );
      List.iter(do_register(~parent=dependency), deps);
    } else {
      switch (parent) {
      | Some(p)
          when
            !DV.equal(p, dependency)
            && !edge_exists((p, ref(Needs_processing)), as_vertex) =>
        add_edge((p, ref(Needs_processing)), as_vertex)
      | _ => ()
      };
    };
  };

  let register = dependency => {
    Hashtbl.add(filename_to_nodes, DV.get_filename(dependency), dependency);
    do_register(dependency);
  };

  let solve_next_out_of_date = (~stop=?, ()) => {
    let (stop_found, ret) =
      G_topological.fold(
        ((dep, state), acc) => {
          switch (acc) {
          | (true, _) => acc
          | (false, Some(_)) => acc
          | (false, None) =>
            let stop_found =
              switch (stop) {
              | Some(d) when DV.equal(d, dep) => true
              | _ => false
              };
            DV.check_up_to_date(dep);
            if (!DV.is_up_to_date(dep)) {
              (stop_found, Some(dep));
            } else {
              (stop_found, None);
            };
          }
        },
        graph,
        (false, None),
      );
    ret;
  };

  let compile_graph = () => {
    let to_compile = ref(solve_next_out_of_date());
    while (Option.is_some(to_compile^)) {
      DV.compile_module(Option.get(to_compile^));
      to_compile := solve_next_out_of_date();
    };
  };

  let get_dependencies = () => {
    List.rev(
      G_topological.fold(
        ((v1, _), acc) => [DV.get_filename(v1), ...acc],
        graph,
        [],
      ),
    );
  };

  let dump = () => {
    Printf.eprintf("-=-=-=- Dependency Graph -=-=-=-\n");
    G.iter_vertex(
      v => {
        let (v1, _) = v;
        Printf.eprintf(
          "%s (in degree: %d) (out degree: %d)\n",
          DV.get_filename(v1),
          G.in_degree(graph, v),
          G.out_degree(graph, v),
        );
      },
      graph,
    );
    Printf.eprintf("-=-=-=- Edges -=-=-=-\n");
    G.iter_edges(
      ((v1, _), (v2, _)) => {
        Printf.eprintf(
          "%s ---> %s\n",
          DV.get_filename(v1),
          DV.get_filename(v2),
        )
      },
      graph,
    );
    Printf.eprintf("-=-=-=- Topological Sort -=-=-=-\n");
    G_topological.iter(
      ((v1, _)) => Printf.eprintf("%s, ", DV.get_filename(v1)),
      graph,
    );
    Printf.eprintf("\n");
  };

  let clear = () => {
    Hashtbl.clear(filename_to_nodes);
    G.clear(graph);
  };
};
