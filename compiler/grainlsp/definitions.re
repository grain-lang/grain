open Grain_typed;
open Grain_diagnostics;

let goto_definition = (log, id, json, compiled_code, cached_code) => {
  switch (Utils.getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    let ln = line + 1;
    let compiled_code_opt = Hashtbl.find_opt(cached_code, uri);
    switch (compiled_code_opt) {
    | None => log("No compiled code available")
    | Some(compiled_code) =>
      let node = Utils.findBestMatch(compiled_code, ln, char);
      switch (node) {
      | Some(stmt) =>
        let node = Utils.getNodeFromStmt(log, stmt, ln, char);
        switch ((node: Utils.node_t)) {
        | Error(err) => log(err)
        | NotInRange => log("Not in range")
        | Expression(e) =>
          let desc = e.exp_desc;
          switch (desc) {
          | TExpIdent(path, _, _) =>
            switch (Env.find_value(path, compiled_code.env)) {
            | exception exn => log("No definition found")
            | lookup =>
              // Currently broken due to Subst.loc returning without location
              // so we don't advertise this.
              let loc = lookup.val_loc;
              let (filename, _, _, _) =
                Locations.get_raw_pos_info(loc.loc_start);
              let range = Utils.loc_to_range(loc);

              // if the file is the one we are looking up in, we get just the filename rather than the
              // full path, so we use the uri instead
              if (String.contains(filename, '/')
                  || String.contains(filename, '\\')) {
                Rpc.send_go_to_definition(log, stdout, id, filename, range);
              } else {
                Rpc.send_go_to_definition(log, stdout, id, uri, range);
              };
            }
          | _ => log("Not defined")
          };
        | Pattern(p) => log("Pattern definition")
        };

      | _ => log("No matching node found.")
      };
    };
  | _ => log("Error, missing paramaters from client")
  };
};
