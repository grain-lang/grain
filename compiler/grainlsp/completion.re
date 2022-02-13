open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

// maps Grain types to LSP types
let rec get_kind = (desc: Types.type_desc) =>
  switch (desc) {
  | TTyVar(_) => 6
  | TTyArrow(_) => 3
  | TTyTuple(_) => 22
  | TTyRecord(_) => 22
  | TTyConstr(_) => 4
  | TTySubst(s) => get_kind(s.desc)
  | TTyLink(t) => get_kind(t.desc)
  | _ => 1
  };

let process_resolution =
    (
      log: string => unit,
      id,
      json,
      compiled_code: Stdlib__hashtbl.t(string, Typedtree.typed_program),
      cached_code: Stdlib__hashtbl.t(string, Typedtree.typed_program),
      documents,
    ) => {
  // right now we just resolve nothing new but respond to the client at least
  Rpc.send_completion(
    log,
    stdout,
    id,
    [],
  );
};

let get_module_exports =
    (log, mod_ident, compiled_code: Typedtree.typed_program) => {
  switch (Env.find_module(mod_ident, None, compiled_code.env)) {
  | lookup =>
    switch (lookup.md_filepath) {
    | None =>
      log("no module path found");
      [];
    | Some(p) =>
      let mtype: Grain_typed.Types.module_type = lookup.md_type;
      switch (mtype) {
      | TModSignature(sigs) =>
        let fnsigs =
          List.filter_map(
            (s: Types.signature_item) => {
              switch (s) {
              | TSigValue(ident, vd) =>
                let string_of_value_description = (~ident: Ident.t, vd) => {
                  Format.asprintf(
                    "%a",
                    Printtyp.value_description(ident),
                    vd,
                  );
                };
                let item: Rpc.completion_item = {
                  label: ident.name,
                  kind: 3,
                  detail: string_of_value_description(~ident, vd),
                };
                Some(item);

              | TSigType(ident, td, recstatus) =>
                //log("Completing A TSigType");
                // let string_of_type_declaration =
                //     (~ident: Ident.t, td) => {
                //   (
                //     ident.name,
                //     Format.asprintf(
                //       "%a",
                //       Printtyp.type_declaration(ident),
                //       td,
                //     ),
                //   );
                // };
                None
              | _ => None
              }
            },
            sigs,
          );
        fnsigs;
      | _ => []
      };
    }
  | exception _ =>
    log("Module not found");
    [];
  };
};

let process_completion =
    (
      log: string => unit,
      id,
      json,
      compiled_code: Stdlib__hashtbl.t(string, Typedtree.typed_program),
      cached_code: Stdlib__hashtbl.t(string, Typedtree.typed_program),
      documents,
    ) => {
  switch (Utils.getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    let completable =
      Utils.get_original_text(log, documents, uri, line, char);
    switch (completable) {
    | Nothing =>
      log("Nothing to complete");
      Rpc.send_completion(log, stdout, id, []);
    | Lident(text) =>
      if (!Hashtbl.mem(cached_code, uri)) {
        // no compiled code available
        Rpc.send_completion(
          log,
          stdout,
          id,
          [],
        );
      } else {
        let compiledCode = Hashtbl.find(cached_code, uri);

        let modules = Env.get_all_modules(compiledCode.env);
        let firstChar = text.[0];

        let completions =
          switch (firstChar) {
          | 'A' .. 'Z' =>
            if (String.contains(text, '.')) {
              // find module name
              let pos = String.rindex(text, '.');
              let modName = String.sub(text, 0, pos);
              let ident: Ident.t = {name: modName, stamp: 0, flags: 0};
              let mod_ident: Path.t = PIdent(ident);

              log("looking for module " ++ modName);
              // only look up completions for imported modules
              if (!List.exists((m: Ident.t) => m.name == modName, modules)) {
                [];
              } else {
                get_module_exports(log, mod_ident, compiledCode);
              };
            } else {
              let modules = Env.get_all_modules(compiledCode.env);
              let types = Env.get_all_type_names(compiledCode.env);

              let converted_modules =
                List.map(
                  (m: Ident.t) => {
                    let item: Rpc.completion_item = {
                      label: m.name,
                      kind: 9,
                      detail: "",
                    };
                    item;
                  },
                  modules,
                );

              let converted_types =
                List.map(
                  (t: Ident.t) => {
                    let item: Rpc.completion_item = {
                      label: t.name,
                      kind: 22,
                      detail: "",
                    };
                    item;
                  },
                  types,
                );

              converted_modules @ converted_types;
            }

          | _ =>
            let values: list((Ident.t, Types.value_description)) =
              Env.get_all_values(compiledCode.env);

            List.map(
              ((i: Ident.t, l: Types.value_description)) => {
                let item: Rpc.completion_item = {
                  label: i.name,
                  kind: get_kind(l.val_type.desc),
                  detail: Utils.lens_sig(l.val_type),
                };
                item;
              },
              values,
            );
          };

        Rpc.send_completion(log, stdout, id, completions);
      }
    };

  | _ => Rpc.send_completion(log, stdout, id, [])
  };
};
