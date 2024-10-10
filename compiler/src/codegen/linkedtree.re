open Grain_typed;
open Grain_utils;
open Mashtree;

type linked_program = {
  programs: list(mash_program),
  func_import_resolutions: Hashtbl.t(string, string),
  global_import_resolutions: Hashtbl.t(string, string),
  num_function_table_elements: int,
  signature: Cmi_format.cmi_infos,
};

let stack_size_zero = {
  stack_size_ptr: 0,
  stack_size_i32: 0,
  stack_size_i64: 0,
  stack_size_f32: 0,
  stack_size_f64: 0,
};
let max_stack_size = (s1, s2) => {
  {
    stack_size_ptr: max(s1.stack_size_ptr, s2.stack_size_ptr),
    stack_size_i32: max(s1.stack_size_i32, s2.stack_size_i32),
    stack_size_i64: max(s1.stack_size_i64, s2.stack_size_i64),
    stack_size_f32: max(s1.stack_size_f32, s2.stack_size_f32),
    stack_size_f64: max(s1.stack_size_f64, s2.stack_size_f64),
  };
};

let wasi_module = "wasi_snapshot_preview1";

let link = main_mashtree => {
  let main_module = Module_resolution.current_filename^();

  let new_base_dir = Filepath.String.dirname;

  let resolve = (~base_dir=?, mod_name) =>
    Module_resolution.locate_unit_object_file(~base_dir?, mod_name);

  let wasi_polyfill =
    Option.map(
      Module_resolution.get_object_name,
      Config.wasi_polyfill_path(),
    );

  let dependencies = Module_resolution.get_dependencies();
  let dependencies =
    switch (wasi_polyfill) {
    | Some(polyfill) => [polyfill, ...dependencies]
    | None => dependencies
    };

  let rec resolve_implicit_modules = (acc, modules) => {
    switch (modules) {
    | [mod_, ...rest] =>
      switch (resolve(Config.get_implicit_filepath(mod_))) {
      | file => resolve_implicit_modules([file, ...acc], rest)
      | exception _ => resolve_implicit_modules(acc, rest)
      }
    | [] => acc
    };
  };

  let implicit_modules =
    resolve_implicit_modules([], Config.all_implicit_opens);

  let func_import_resolutions = Hashtbl.create(2048);
  let global_import_resolutions = Hashtbl.create(2048);
  let func_export_resolutions = Hashtbl.create(2048);
  let global_export_resolutions = Hashtbl.create(2048);

  let num_function_table_elements = ref(0);

  let dep_id = ref(0);

  let process_mashtree = (~main, dep, tree) => {
    let table_offset_global = {
      id: tree.mash_code.global_function_table_offset,
      mutable_: false,
      allocation_type: Types.Unmanaged(WasmI32),
      initial_value:
        Some(
          MConstLiteral(
            MConstI32(Int32.of_int(num_function_table_elements^)),
          ),
        ),
    };

    let globals = [table_offset_global, ...tree.mash_code.globals];

    let imports =
      List.fold_left(
        (imports, import) => {
          let process_import = resolved_module => {
            let global =
              Hashtbl.find_opt(
                global_export_resolutions,
                (resolved_module, import.mimp_name),
              );
            let func =
              Hashtbl.find_opt(
                func_export_resolutions,
                (resolved_module, import.mimp_name),
              );
            let import_name =
              Printf.sprintf(
                "%s_%d",
                Ident.unique_name(import.mimp_id),
                dep_id^,
              );
            Option.iter(
              global =>
                Hashtbl.add(global_import_resolutions, import_name, global),
              global,
            );
            Option.iter(
              func => Hashtbl.add(func_import_resolutions, import_name, func),
              func,
            );
          };

          switch (import.mimp_kind) {
          | MImportWasm =>
            switch (wasi_polyfill) {
            | Some(polyfill)
                when import.mimp_mod == wasi_module && dep != polyfill =>
              process_import(polyfill);
              imports;
            | _ => [import, ...imports]
            }
          | MImportGrain =>
            let resolved_module =
              resolve(~base_dir=new_base_dir(dep), import.mimp_mod);
            process_import(resolved_module);
            imports;
          };
        },
        [],
        tree.mash_code.imports,
      );

    List.iter(
      export => {
        switch (export) {
        | WasmFunctionExport({ex_function_name, ex_function_internal_name}) =>
          let internal_name =
            Printf.sprintf("%s_%d", ex_function_internal_name, dep_id^);
          Hashtbl.add(
            func_export_resolutions,
            (dep, ex_function_name),
            internal_name,
          );

          if (List.mem(dep, implicit_modules)) {
            Hashtbl.add(
              func_import_resolutions,
              Ident.unique_name(Ident.create_persistent(ex_function_name)),
              internal_name,
            );
          };
        | WasmGlobalExport({ex_global_name, ex_global_internal_name}) =>
          let internal_name =
            Printf.sprintf("%s_%d", ex_global_internal_name, dep_id^);
          Hashtbl.add(
            global_export_resolutions,
            (dep, ex_global_name),
            internal_name,
          );

          if (List.mem(dep, implicit_modules)) {
            Hashtbl.add(
              global_import_resolutions,
              Ident.unique_name(Ident.create_persistent(ex_global_name)),
              internal_name,
            );
          };
        }
      },
      tree.mash_code.exports,
    );

    let exports =
      if (main) {
        tree.mash_code.exports;
      } else {
        [];
      };

    num_function_table_elements :=
      num_function_table_elements^
      + List.length(tree.mash_code.function_table_elements);

    incr(dep_id);

    {
      ...tree,
      mash_code: {
        ...tree.mash_code,
        globals,
        imports,
        exports,
      },
    };
  };

  let programs =
    List.rev_map(
      dep => process_mashtree(~main=false, dep, Emitmod.load_object(dep)),
      dependencies,
    );

  let main_program = process_mashtree(~main=true, main_module, main_mashtree);
  let programs = List.rev([main_program, ...programs]);
  let num_function_table_elements = num_function_table_elements^;
  let signature = main_mashtree.signature;

  {
    programs,
    func_import_resolutions,
    global_import_resolutions,
    num_function_table_elements,
    signature,
  };
};
