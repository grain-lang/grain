open Grain_codegen;
open Compmod;
open Grain_typed;
open Grain_utils;
open Cmi_format;
open Binaryen;
open Graph;

module String = {
  type t = string;
  let compare = compare;
  let hash = Hashtbl.hash;
  let equal = (==);
  let default = "";
};

module G = Imperative.Digraph.Concrete(String);
module Topo = Topological.Make(G);

let dependency_graph = G.create(~size=10, ());
let modules: Hashtbl.t(string, Module.t) = Hashtbl.create(10);

let main_module = "main";

let grain_main = "_gmain";
let grain_start = "_start";

let grain_module_name = mod_name => {
  // Remove GRAIN$MODULE$
  Str.string_after(mod_name, 13);
};

let resolve = (~base_dir=?, mod_name) => {
  let mod_name = grain_module_name(mod_name);
  let fullpath =
    Module_resolution.locate_unit_object_file(~base_dir?, mod_name);
  let ic = open_in_bin(fullpath);
  let length = in_channel_length(ic);
  let module_bytes = Bytes.create(length);
  really_input(ic, module_bytes, 0, length);
  Module.read(module_bytes);
};

let is_grain_module = mod_name => {
  Str.string_match(Str.regexp_string("GRAIN$MODULE$"), mod_name, 0);
};

let new_base_dir = (cur_base_dir, imported_module) => {
  let mod_name = grain_module_name(imported_module);
  if (Module_resolution.is_relpath(mod_name)) {
    Filename.concat(cur_base_dir, Filename.dirname(mod_name));
  } else {
    cur_base_dir;
  };
};

let is_main_module = mod_name => mod_name == main_module;

let rec build_dependency_graph = (~save=true, ~base_dir, mod_name) => {
  let wasm_mod = Hashtbl.find(modules, mod_name);
  let num_globals = Global.get_num_globals(wasm_mod);
  for (i in 0 to num_globals - 1) {
    let global = Global.get_global_by_index(wasm_mod, i);
    let imported_module = Import.global_import_get_module(global);
    if (is_grain_module(imported_module)) {
      if (save) {
        G.add_edge(dependency_graph, mod_name, imported_module);
      };
      if (!Hashtbl.mem(modules, imported_module)) {
        Hashtbl.add(
          modules,
          imported_module,
          resolve(~base_dir, imported_module),
        );
        build_dependency_graph(
          ~base_dir=new_base_dir(base_dir, imported_module),
          imported_module,
        );
      };
    };
  };
  let num_funcs = Function.get_num_functions(wasm_mod);
  for (i in 0 to num_funcs - 1) {
    let func = Function.get_function_by_index(wasm_mod, i);
    let imported_module = Import.function_import_get_module(func);
    if (is_grain_module(imported_module)) {
      if (save) {
        G.add_edge(dependency_graph, mod_name, imported_module);
      };
      if (!Hashtbl.mem(modules, imported_module)) {
        Hashtbl.add(
          modules,
          imported_module,
          resolve(~base_dir, imported_module),
        );
        build_dependency_graph(
          new_base_dir(base_dir, imported_module),
          imported_module,
        );
      };
    };
  };
};

let gensym_counter = ref(0);
let gensym = name => {
  incr(gensym_counter);
  Printf.sprintf("%s.linked.%d", name, gensym_counter^);
};

let is_global_imported = global =>
  Import.global_import_get_base(global) != "";
let is_function_imported = func =>
  Import.function_import_get_base(func) != "";

// NOTE: A Not_found being raised in the below function likely means that there
//       is more than one place in the Binaryen program which has the same
//       subexpression (in an identity equality sense). The substitution
//       probably succeeded the first time the subexpression was found, but,
//       because this process is not idempotent, failed on the second time. The fix
//       for this problem is to produce two separate expression instances when
//       constructing the AST (either through constructing two separate instances or by
//       using Expression.copy())

let rec globalize_names = (local_names, expr) => {
  let kind = Expression.get_kind(expr);
  switch (kind) {
  | Invalid => failwith("Invalid expression")
  | Nop => ()
  | Block =>
    let internal_name = Expression.Block.get_name(expr);
    Option.iter(
      internal_name => {
        let new_name = gensym(internal_name);
        Hashtbl.add(local_names, internal_name, new_name);

        Expression.Block.set_name(expr, new_name);
      },
      internal_name,
    );

    let num_children = Expression.Block.get_num_children(expr);
    for (i in 0 to num_children - 1) {
      globalize_names(local_names, Expression.Block.get_child_at(expr, i));
    };
  | If =>
    globalize_names(local_names, Expression.If.get_condition(expr));
    globalize_names(local_names, Expression.If.get_if_true(expr));
    Option.iter(
      globalize_names(local_names),
      Expression.If.get_if_false(expr),
    );
  | Loop =>
    let internal_name = Expression.Loop.get_name(expr);
    let new_name = gensym(internal_name);
    Hashtbl.add(local_names, internal_name, new_name);

    Expression.Loop.set_name(expr, new_name);

    globalize_names(local_names, Expression.Loop.get_body(expr));
  | Break =>
    let internal_name = Expression.Break.get_name(expr);
    Expression.Break.set_name(
      expr,
      Hashtbl.find(local_names, internal_name),
    );

    Option.iter(
      globalize_names(local_names),
      Expression.Break.get_condition(expr),
    );
    Option.iter(
      globalize_names(local_names),
      Expression.Break.get_value(expr),
    );
  | Switch =>
    let num_names = Expression.Switch.get_num_names(expr);
    for (i in 0 to num_names - 1) {
      let internal_name = Expression.Switch.get_name_at(expr, i);
      Expression.Switch.set_name_at(
        expr,
        i,
        Hashtbl.find(local_names, internal_name),
      );
    };

    let internal_default_name = Expression.Switch.get_default_name(expr);
    Option.iter(
      name =>
        Expression.Switch.set_default_name(
          expr,
          Hashtbl.find(local_names, name),
        ),
      internal_default_name,
    );

    globalize_names(local_names, Expression.Switch.get_condition(expr));
    Option.iter(
      globalize_names(local_names),
      Expression.Switch.get_value(expr),
    );
  | Call =>
    let internal_name = Expression.Call.get_target(expr);
    Expression.Call.set_target(
      expr,
      Hashtbl.find(local_names, internal_name),
    );

    let num_operands = Expression.Call.get_num_operands(expr);
    for (i in 0 to num_operands - 1) {
      globalize_names(local_names, Expression.Call.get_operand_at(expr, i));
    };
  | CallIndirect =>
    globalize_names(local_names, Expression.Call_indirect.get_target(expr));

    Expression.Call_indirect.set_table(
      expr,
      Comp_utils.global_function_table,
    );

    let num_operands = Expression.Call_indirect.get_num_operands(expr);
    for (i in 0 to num_operands - 1) {
      globalize_names(
        local_names,
        Expression.Call_indirect.get_operand_at(expr, i),
      );
    };
  | LocalGet => ()
  | LocalSet =>
    globalize_names(local_names, Expression.Local_set.get_value(expr))
  | GlobalGet =>
    let internal_name = Expression.Global_get.get_name(expr);
    Expression.Global_get.set_name(
      expr,
      Hashtbl.find(local_names, internal_name),
    );
  | GlobalSet =>
    let internal_name = Expression.Global_set.get_name(expr);
    Expression.Global_set.set_name(
      expr,
      Hashtbl.find(local_names, internal_name),
    );
    globalize_names(local_names, Expression.Global_set.get_value(expr));
  | Load => globalize_names(local_names, Expression.Load.get_ptr(expr))
  | Store =>
    globalize_names(local_names, Expression.Store.get_ptr(expr));
    globalize_names(local_names, Expression.Store.get_value(expr));
  | MemoryCopy =>
    globalize_names(local_names, Expression.Memory_copy.get_dest(expr));
    globalize_names(local_names, Expression.Memory_copy.get_source(expr));
    globalize_names(local_names, Expression.Memory_copy.get_size(expr));
  | MemoryFill =>
    globalize_names(local_names, Expression.Memory_fill.get_dest(expr));
    globalize_names(local_names, Expression.Memory_fill.get_value(expr));
    globalize_names(local_names, Expression.Memory_fill.get_size(expr));
  | Const => ()
  | Unary => globalize_names(local_names, Expression.Unary.get_value(expr))
  | Binary =>
    globalize_names(local_names, Expression.Binary.get_left(expr));
    globalize_names(local_names, Expression.Binary.get_right(expr));
  | Select =>
    globalize_names(local_names, Expression.Select.get_if_true(expr));
    globalize_names(local_names, Expression.Select.get_if_false(expr));
    globalize_names(local_names, Expression.Select.get_condition(expr));
  | Drop => globalize_names(local_names, Expression.Drop.get_value(expr))
  | Return => globalize_names(local_names, Expression.Return.get_value(expr))
  | MemorySize => ()
  | MemoryGrow =>
    globalize_names(local_names, Expression.Memory_grow.get_delta(expr))
  | Unreachable => ()
  | TupleMake =>
    let num_operands = Expression.Tuple_make.get_num_operands(expr);
    for (i in 0 to num_operands - 1) {
      globalize_names(
        local_names,
        Expression.Tuple_make.get_operand_at(expr, i),
      );
    };
  | TupleExtract =>
    globalize_names(local_names, Expression.Tuple_extract.get_tuple(expr))
  | AtomicRMW
  | AtomicCmpxchg
  | AtomicWait
  | AtomicNotify
  | AtomicFence
  | SIMDExtract
  | SIMDReplace
  | SIMDShuffle
  | SIMDTernary
  | SIMDShift
  | SIMDLoad
  | SIMDLoadStoreLane
  | MemoryInit
  | DataDrop
  | Pop
  | RefNull
  | RefIs
  | RefFunc
  | RefEq
  | Try
  | Throw
  | Rethrow
  | I31New
  | I31Get
  | CallRef
  | RefTest
  | RefCast
  | BrOn
  | RttCanon
  | RttSub
  | StructNew
  | StructGet
  | StructSet
  | ArrayNew
  | ArrayGet
  | ArraySet
  | ArrayLen
  | RefAs => failwith("Linking NYI for wasm instruction")
  };
};

let table_offset = ref(0);
let module_id = ref(0);

type wasm_export =
  | ExportFound(string)
  | ExportForward(string, string);

let exported_names: Hashtbl.t(string, Hashtbl.t(string, wasm_export)) =
  Hashtbl.create(10);

let rec lookup_import = (module_, name) => {
  let exports = Hashtbl.find(exported_names, module_);
  switch (Hashtbl.find(exports, name)) {
  | ExportFound(name) => name
  | ExportForward(module_, name) => lookup_import(module_, name)
  };
};

let rec lookup_import_opt = (module_, name) => {
  let exports = Hashtbl.find(exported_names, module_);
  switch (Hashtbl.find_opt(exports, name)) {
  | Some(ExportFound(name)) => Some(name)
  | Some(ExportForward(module_, name)) =>
    Some(lookup_import(module_, name))
  | None => None
  };
};

let link_all = (linked_mod, dependencies, signature) => {
  table_offset := 0;

  List.iter(
    dep => {
      let wasm_mod = Hashtbl.find(modules, dep);
      // let local_exported_names = Comp_utils.get_exported_names(wasm_mod);x
      let num_exports = Export.get_num_exports(wasm_mod);
      let local_exported_names: Hashtbl.t(string, wasm_export) =
        Hashtbl.create(10);
      for (i in 0 to num_exports - 1) {
        let export = Export.get_export_by_index(wasm_mod, i);
        let export_kind = Export.export_get_kind(export);
        if (export_kind == Export.external_global) {
          let exported_name = Export.get_name(export);
          let global = Global.get_global(wasm_mod, Export.get_value(export));
          if (is_global_imported(global)) {
            let import_mod = Import.global_import_get_module(global);
            let import_base = Import.global_import_get_base(global);
            Hashtbl.add(
              local_exported_names,
              exported_name,
              ExportForward(import_mod, import_base),
            );
          } else {
            Hashtbl.add(
              local_exported_names,
              exported_name,
              ExportFound(gensym(exported_name)),
            );
          };
        } else if (export_kind == Export.external_function) {
          let exported_name = Export.get_name(export);
          let func =
            Function.get_function(wasm_mod, Export.get_value(export));
          if (is_function_imported(func)) {
            let import_mod = Import.function_import_get_module(func);
            let import_base = Import.function_import_get_base(func);
            Hashtbl.add(
              local_exported_names,
              exported_name,
              ExportForward(import_mod, import_base),
            );
          } else {
            Hashtbl.add(
              local_exported_names,
              exported_name,
              ExportFound(gensym(exported_name)),
            );
          };
        };
      };
      Hashtbl.add(exported_names, dep, local_exported_names);
    },
    dependencies,
  );

  let link_one = dep => {
    Printf.eprintf("linking one %s\n", dep);
    let local_names: Hashtbl.t(string, string) = Hashtbl.create(128);

    let wasm_mod = Hashtbl.find(modules, dep);

    let num_globals = Global.get_num_globals(wasm_mod);
    for (i in 0 to num_globals - 1) {
      let global = Global.get_global_by_index(wasm_mod, i);
      if (is_global_imported(global)) {
        let imported_module = Import.global_import_get_module(global);
        if (Hashtbl.mem(modules, imported_module) && imported_module != dep) {
          let imported_name = Import.global_import_get_base(global);
          let internal_name = Global.get_name(global);
          let new_name = lookup_import(imported_module, imported_name);
          Hashtbl.add(local_names, internal_name, new_name);
        } else {
          let imported_name = Import.global_import_get_base(global);
          let internal_name = Global.get_name(global);
          let new_name = gensym(internal_name);
          Hashtbl.add(local_names, internal_name, new_name);

          if (Comp_utils.is_grain_env(imported_module)) {
            let value =
              switch (imported_name) {
              | "relocBase" =>
                Expression.Const.make(
                  wasm_mod,
                  Literal.int32(Int32.of_int(table_offset^)),
                )
              | "moduleRuntimeId" =>
                incr(module_id);
                Expression.Const.make(
                  wasm_mod,
                  Literal.int32(Int32.of_int(module_id^)),
                );
              | value =>
                failwith(
                  Printf.sprintf("Unknown Grain runtime value `%s`", value),
                )
              };
            ignore @@
            Global.add_global(linked_mod, new_name, Type.int32, false, value);
          } else {
            let ty = Global.get_type(global);
            let mut = Global.is_mutable(global);
            Import.add_global_import(
              linked_mod,
              new_name,
              imported_module,
              imported_name,
              ty,
              mut,
            );
          };
        };
      } else {
        let internal_name = Global.get_name(global);
        let new_name =
          switch (lookup_import_opt(dep, internal_name)) {
          | Some(name) => name
          | None => gensym(internal_name)
          };
        Hashtbl.add(local_names, internal_name, new_name);

        let ty = Global.get_type(global);
        let mut = Global.is_mutable(global);
        let init = Global.get_init_expr(global);

        globalize_names(local_names, init);
        ignore @@ Global.add_global(linked_mod, new_name, ty, mut, init);
      };
    };

    let num_functions = Function.get_num_functions(wasm_mod);
    let funcs =
      List.init(num_functions, Function.get_function_by_index(wasm_mod));
    let (imported_funcs, funcs) =
      List.partition(is_function_imported, funcs);

    List.iter(
      func => {
        let imported_module = Import.function_import_get_module(func);
        if (Hashtbl.mem(modules, imported_module) && imported_module != dep) {
          let imported_name = Import.function_import_get_base(func);
          let internal_name = Function.get_name(func);
          let new_name = lookup_import(imported_module, imported_name);
          Hashtbl.add(local_names, internal_name, new_name);
        } else {
          let imported_name = Import.function_import_get_base(func);
          let internal_name = Function.get_name(func);
          let new_name =
            switch (lookup_import_opt(dep, internal_name)) {
            | Some(name) => name
            | None => gensym(internal_name)
            };
          Hashtbl.add(local_names, internal_name, new_name);

          let params = Function.get_params(func);
          let results = Function.get_results(func);
          Import.add_function_import(
            linked_mod,
            new_name,
            imported_module,
            imported_name,
            params,
            results,
          );
        };
      },
      imported_funcs,
    );

    // Register all function names first as functions may be recursive
    List.iter(
      func => {
        let internal_name = Function.get_name(func);
        let new_name = gensym(internal_name);
        Hashtbl.add(local_names, internal_name, new_name);
      },
      funcs,
    );

    List.iter(
      func => {
        let internal_name = Function.get_name(func);
        let new_name = Hashtbl.find(local_names, internal_name);

        let params = Function.get_params(func);
        let results = Function.get_results(func);
        let num_locals = Function.get_num_vars(func);
        let locals = Array.init(num_locals, i => Function.get_var(func, i));
        let body = Function.get_body(func);
        globalize_names(local_names, body);
        ignore @@
        Function.add_function(
          linked_mod,
          new_name,
          params,
          results,
          locals,
          body,
        );
      },
      funcs,
    );

    // let local_exported_names =
    //   Comp_utils.get_exported_names(~local_names, wasm_mod);
    // Hashtbl.add(exported_names, dep, local_exported_names);
    if (is_main_module(dep)) {
      let local_exported_names: Hashtbl.t(string, string) =
        Hashtbl.create(10);
      Hashtbl.iter(
        (name, _) => {
          Hashtbl.add(local_exported_names, name, lookup_import(dep, name))
        },
        Hashtbl.find(exported_names, dep),
      );
      Comp_utils.write_universal_exports(
        linked_mod,
        signature,
        local_exported_names,
      );
    };

    let num_element_segments = Table.get_num_element_segments(wasm_mod);
    for (i in 0 to num_element_segments - 1) {
      let segment = Table.get_element_segment_by_index(wasm_mod, i);
      let name = Element_segment.get_name(segment);
      let new_name = gensym(name);
      let size = Element_segment.get_length(segment);
      let elems =
        List.init(size, i =>
          Hashtbl.find(local_names, Element_segment.get_data(segment, i))
        );
      ignore @@
      Table.add_active_element_segment(
        linked_mod,
        Comp_utils.global_function_table,
        new_name,
        elems,
        Expression.Const.make(
          wasm_mod,
          Literal.int32(Int32.of_int(table_offset^)),
        ),
      );
      table_offset := table_offset^ + size;
    };
  };
  List.iter(
    dep =>
      if (Hashtbl.mem(modules, dep)) {
        link_one(dep);
      },
    dependencies,
  );
  ignore @@
  Table.add_table(
    linked_mod,
    Comp_utils.global_function_table,
    table_offset^,
    -1,
  );
  let (initial_memory, maximum_memory) =
    switch (Config.initial_memory_pages^, Config.maximum_memory_pages^) {
    | (initial_memory, Some(maximum_memory)) => (
        initial_memory,
        maximum_memory,
      )
    | (initial_memory, None) => (initial_memory, Memory.unlimited)
    };
  Memory.set_memory(
    linked_mod,
    initial_memory,
    maximum_memory,
    "memory",
    [],
    false,
  );

  let starts =
    List.filter_map(
      dep =>
        if (Hashtbl.mem(modules, dep)) {
          Some(
            Expression.Drop.make(
              linked_mod,
              Expression.Call.make(
                linked_mod,
                // Hashtbl.find(Hashtbl.find(exported_names, dep), grain_main),
                lookup_import(dep, grain_main),
                [],
                Type.int32,
              ),
            ),
          );
        } else {
          None;
        },
      dependencies,
    );

  let start_name = gensym(grain_start);
  ignore @@
  Function.add_function(
    linked_mod,
    start_name,
    Type.none,
    Type.none,
    [||],
    Expression.Block.make(linked_mod, gensym("start"), starts),
  );
  ignore @@ Export.add_function_export(linked_mod, start_name, grain_start);
};

let apply_wasi_polyfill = () => {
  let wasi_polyfill =
    "GRAIN$MODULE$./"
    ++ Filename.remove_extension(Option.get(Config.wasi_polyfill^));
  prerr_endline(wasi_polyfill);
  let wasi_module = "wasi_snapshot_preview1";
  let base_dir = Filename.dirname(Module_resolution.current_filename^());
  prerr_endline(base_dir);
  Hashtbl.add(modules, wasi_module, resolve(wasi_polyfill));
  prerr_endline("resolved the polyfill");
  build_dependency_graph(
    ~save=false,
    ~base_dir=new_base_dir(base_dir, wasi_polyfill),
    wasi_module,
  );
  Topo.fold((dep, _) => Some(dep), dependency_graph, None);
};

let link_modules = ({asm: wasm_mod, signature}) => {
  G.clear(dependency_graph);
  Hashtbl.clear(exported_names);
  Hashtbl.clear(modules);
  Hashtbl.add(modules, main_module, wasm_mod);
  let (wasi_polyfill, wasi_polyfill_insertion_point) =
    if (Option.is_some(Config.wasi_polyfill^)) {
      (true, apply_wasi_polyfill());
    } else {
      (false, None);
    };
  let dependencies =
    Topo.fold((dep, acc) => [dep, ...acc], dependency_graph, []);
  prerr_endline("POLYFILL_GRAPH");
  List.iter(prerr_endline, dependencies);
  build_dependency_graph(
    ~base_dir=Filename.dirname(Module_resolution.current_filename^()),
    main_module,
  );
  let dependencies =
    Topo.fold((dep, acc) => [dep, ...acc], dependency_graph, []);
  let dependencies =
    if (wasi_polyfill) {
      switch (wasi_polyfill_insertion_point) {
      | None => ["wasi_snapshot_preview1"]
      | Some(point) =>
        let rec insert_polyfill = deps => {
          switch (deps) {
          | [] =>
            failwith("Impossible: wasi polyfill insertion point not found")
          | [dep, ...deps] when dep == point => [
              dep,
              "wasi_snapshot_preview1",
              ...deps,
            ]
          | [dep, ...deps] => [dep, ...insert_polyfill(deps)]
          };
        };
        insert_polyfill(dependencies);
      };
    } else {
      dependencies;
    };
  prerr_endline("FULL_GRAPH");
  List.iter(prerr_endline, dependencies);
  prerr_endline("FULL_GRAPH!");
  G.iter_edges(
    (from_, to_) => Printf.eprintf("%s ---> %s\n", from_, to_),
    dependency_graph,
  );
  let linked_mod = Module.create();
  link_all(linked_mod, dependencies, signature);

  let features = Module.get_features(wasm_mod);
  let _ = Module.set_features(linked_mod, features);
  let _ = Settings.set_low_memory_unused(true);
  if (Module.validate(linked_mod) != 1) {
    failwith("Generated invalid linked module");
  };
  Module.optimize(linked_mod);
  linked_mod;
};
