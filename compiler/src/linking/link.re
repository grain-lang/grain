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

let grain_main = "_gmain";
let grain_start = "_start";

let grain_module_name = mod_name => {
  // Remove GRAIN$MODULE$
  Str.string_after(mod_name, 13);
};

let resolve = (~base_dir=?, mod_name) => {
  let mod_name = grain_module_name(mod_name);
  Module_resolution.locate_unit_object_file(~base_dir?, mod_name);
};

let load_module = fullpath => {
  let ic = open_in_bin(fullpath);
  let length = in_channel_length(ic);
  let module_bytes = Bytes.create(length);
  really_input(ic, module_bytes, 0, length);
  close_in(ic);
  Module.read(module_bytes);
};

let is_grain_module = mod_name => {
  Str.string_match(Str.regexp_string("GRAIN$MODULE$"), mod_name, 0);
};

let wasi_polyfill_module = () => {
  "GRAIN$MODULE$./"
  ++ Filename.remove_extension(Option.get(Config.wasi_polyfill^));
};

let is_wasi_module = mod_name => {
  mod_name == "wasi_snapshot_preview1";
};

let is_wasi_polyfill_module = mod_path =>
  mod_path == resolve(wasi_polyfill_module());

let new_base_dir = Filename.dirname;

let rec build_dependency_graph = (~base_dir, mod_path) => {
  let wasm_mod = Hashtbl.find(modules, mod_path);
  let num_globals = Global.get_num_globals(wasm_mod);
  for (i in 0 to num_globals - 1) {
    let global = Global.get_global_by_index(wasm_mod, i);
    let imported_module = Import.global_import_get_module(global);
    if (is_grain_module(imported_module)) {
      let resolved_import = resolve(~base_dir, imported_module);
      if (!Hashtbl.mem(modules, resolved_import)) {
        Hashtbl.add(modules, resolved_import, load_module(resolved_import));
        build_dependency_graph(
          ~base_dir=new_base_dir(resolved_import),
          resolved_import,
        );
      };
      G.add_edge(dependency_graph, mod_path, resolved_import);
    };
  };
  let num_funcs = Function.get_num_functions(wasm_mod);
  let has_wasi_polyfill = Option.is_some(Config.wasi_polyfill^);
  for (i in 0 to num_funcs - 1) {
    let func = Function.get_function_by_index(wasm_mod, i);
    let imported_module = Import.function_import_get_module(func);
    if (is_grain_module(imported_module)) {
      let resolved_import = resolve(~base_dir, imported_module);
      if (!Hashtbl.mem(modules, resolved_import)) {
        Hashtbl.add(modules, resolved_import, load_module(resolved_import));
        build_dependency_graph(
          new_base_dir(resolved_import),
          resolved_import,
        );
      };
      G.add_edge(dependency_graph, mod_path, resolved_import);
    } else if (has_wasi_polyfill
               && is_wasi_module(imported_module)
               && !is_wasi_polyfill_module(mod_path)) {
      // Perform any WASI polyfilling. Note that we skip this step if we are compiling the polyfill module itself.
      // If we are importing a foreign from WASI, then add a dependency to the polyfill instead.
      let imported_module = wasi_polyfill_module();
      let resolved_import = resolve(imported_module);
      if (!Hashtbl.mem(modules, resolved_import)) {
        Hashtbl.add(modules, resolved_import, load_module(resolved_import));
        build_dependency_graph(
          new_base_dir(resolved_import),
          resolved_import,
        );
      };
      G.add_edge(dependency_graph, mod_path, resolved_import);
    };
  };
};

let gensym_counter = ref(0);
let gensym = name => {
  incr(gensym_counter);
  Printf.sprintf("%s.linked.%d", name, gensym_counter^);
};

let exported_names: Hashtbl.t(string, Hashtbl.t(string, string)) =
  Hashtbl.create(10);

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

let link_all = (linked_mod, dependencies, signature) => {
  table_offset := 0;

  let link_one = dep => {
    let local_names: Hashtbl.t(string, string) = Hashtbl.create(128);

    let wasm_mod = Hashtbl.find(modules, dep);

    let num_globals = Global.get_num_globals(wasm_mod);
    for (i in 0 to num_globals - 1) {
      let global = Global.get_global_by_index(wasm_mod, i);
      if (is_global_imported(global)) {
        let imported_module = Import.global_import_get_module(global);
        if (is_grain_module(imported_module)) {
          let imported_name = Import.global_import_get_base(global);
          let internal_name = Global.get_name(global);
          let new_name =
            Hashtbl.find(
              Hashtbl.find(
                exported_names,
                resolve(~base_dir=new_base_dir(dep), imported_module),
              ),
              imported_name,
            );
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
        let new_name = gensym(internal_name);
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

    let has_wasi_polyfill = Option.is_some(Config.wasi_polyfill^);
    List.iter(
      func => {
        let imported_module = Import.function_import_get_module(func);
        if (is_grain_module(imported_module)) {
          let imported_name = Import.function_import_get_base(func);
          let internal_name = Function.get_name(func);
          let new_name =
            Hashtbl.find(
              Hashtbl.find(
                exported_names,
                resolve(~base_dir=new_base_dir(dep), imported_module),
              ),
              imported_name,
            );
          Hashtbl.add(local_names, internal_name, new_name);
        } else if (has_wasi_polyfill
                   && is_wasi_module(imported_module)
                   && !is_wasi_polyfill_module(dep)) {
          // Perform any WASI polyfilling. Note that we skip this step if we are compiling the polyfill module itself.
          // If we are importing a foreign from WASI, then we swap it out for the foreign from the polyfill.
          let imported_name = Import.function_import_get_base(func);
          let internal_name = Function.get_name(func);
          let wasi_polyfill = wasi_polyfill_module();
          let new_name =
            Hashtbl.find_opt(
              Hashtbl.find(exported_names, resolve(wasi_polyfill)),
              imported_name,
            );
          let new_name =
            switch (new_name) {
            | Some(new_name) => new_name
            | None =>
              failwith(
                Printf.sprintf(
                  "Unable to locate `%s` in your polyfill. Required by `%s`",
                  imported_name,
                  dep,
                ),
              )
            };
          Hashtbl.add(local_names, internal_name, new_name);
        } else {
          let imported_name = Import.function_import_get_base(func);
          let internal_name = Function.get_name(func);
          let new_name = gensym(internal_name);
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

    let local_exported_names =
      Comp_utils.get_exported_names(~local_names, wasm_mod);
    Hashtbl.add(exported_names, dep, local_exported_names);

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
  List.iter(link_one, dependencies);

  Comp_utils.write_universal_exports(
    linked_mod,
    signature,
    Hashtbl.find(exported_names, Module_resolution.current_filename^()),
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
    List.map(
      dep => {
        Expression.Drop.make(
          linked_mod,
          Expression.Call.make(
            linked_mod,
            Hashtbl.find(Hashtbl.find(exported_names, dep), grain_main),
            [],
            Type.int32,
          ),
        )
      },
      dependencies,
    );

  let start_name = gensym(grain_start);
  let start =
    Function.add_function(
      linked_mod,
      start_name,
      Type.none,
      Type.none,
      [||],
      Expression.Block.make(linked_mod, gensym("start"), starts),
    );

  if (Grain_utils.Config.use_start_section^) {
    Function.set_start(linked_mod, start);
  } else {
    ignore @@ Export.add_function_export(linked_mod, start_name, grain_start);
  };
};

let link_modules = ({asm: wasm_mod, signature}) => {
  G.clear(dependency_graph);
  Hashtbl.clear(modules);

  let main_module = Module_resolution.current_filename^();
  Hashtbl.add(modules, main_module, wasm_mod);

  G.add_vertex(dependency_graph, main_module);
  build_dependency_graph(
    ~base_dir=Filename.dirname(main_module),
    main_module,
  );
  let dependencies =
    Topo.fold((dep, acc) => [dep, ...acc], dependency_graph, []);
  let linked_mod = Module.create();
  link_all(linked_mod, dependencies, signature);

  let features = Module.get_features(wasm_mod);
  let _ = Module.set_features(linked_mod, features);
  // we set low_memory_unused := true iff the user has not specified a memory base.
  // This is because in many use cases in which this is specified (e.g. wasm4), users
  // will expect the static region of memory below the heap base to all be available.
  let _ =
    Settings.set_low_memory_unused(
      Option.is_none(Grain_utils.Config.memory_base^),
    );
  if (Module.validate(linked_mod) != 1) {
    failwith("Generated invalid linked module");
  };
  switch (Config.optimization_level^) {
  | Level_three => Module.optimize(linked_mod)
  | Level_zero
  | Level_one
  | Level_two => ()
  };
  linked_mod;
};
