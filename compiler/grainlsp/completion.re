open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

type completable =
  | Nothing
  | Lident(string);

let find_completable = (text, offset) => {
  let rec loop = i => {
    i < 0
      ? Lident(String.sub(text, i + 1, offset - (i + 1)))
      : (
        switch (text.[i]) {
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '.'
        | '_' => loop(i - 1)
        | _ =>
          i == offset - 1
            ? Nothing : Lident(String.sub(text, i + 1, offset - (i + 1)))
        }
      );
  };
  loop(offset - 1);
};

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

let get_original_text = (log, documents, uri, line, char) =>
  if (!Hashtbl.mem(documents, uri)) {
    log("Can't find source code for " ++ uri);
    Nothing;
  } else {
    let sourceCode = Hashtbl.find(documents, uri);
    // try and find the code we are completing in the original source

    let lines = String.split_on_char('\n', sourceCode);
    let line = List.nth(lines, line);
    let completable = find_completable(line, char);

    let _ =
      switch (completable) {
      | Nothing => log("nothing completable found")
      | Lident(ident) => log("Let's complete on " ++ ident)
      };
    completable;
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
    let completable = get_original_text(log, documents, uri, line, char);
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
                switch (Env.find_module(mod_ident, None, compiledCode.env)) {
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
                              let string_of_value_description =
                                  (~ident: Ident.t, vd) => {
                                Format.asprintf(
                                  "%a",
                                  Printtyp.value_description(ident),
                                  vd,
                                );
                              };
                              let item: Rpc.completion_item = {
                                label: ident.name,
                                kind: 3,
                                detail:
                                  string_of_value_description(~ident, vd),
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
            } else {
              let modules = Env.get_all_modules(compiledCode.env);

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
            }

          | _ =>
            let values: list((Ident.t, Types.value_description)) =
              Env.get_all_values(log, compiledCode.env);

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

  | _ => ()
  };
};
