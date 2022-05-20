open Grain_typed;

type export_kind =
  | Function
  | Record
  | Value;

type export = {
  name: string,
  kind: export_kind,
  signature: string,
};

let get_exports = (~path, compiled_code: Typedtree.typed_program) => {
  switch (Env.find_module(path, None, compiled_code.env)) {
  | lookup =>
    switch (lookup.md_filepath, lookup.md_type) {
    // Open question: Why does this need the filepath if it doesn't use it?
    | (Some(_), TModSignature(sigs)) =>
      let fnsigs =
        List.filter_map(
          (s: Types.signature_item) => {
            switch (s) {
            | TSigValue(ident, {val_repr: ReprFunction(_)} as vd) =>
              Some({
                name: ident.name,
                kind: Function,
                signature: Printtyp.string_of_value_description(~ident, vd),
              })
            | TSigValue(ident, {val_repr: ReprValue(_)} as vd) =>
              Some({
                name: ident.name,
                kind: Value,
                signature: Printtyp.string_of_value_description(~ident, vd),
              })
            | TSigType(ident, td, recstatus) =>
              Some({
                name: ident.name,
                kind: Record,
                signature: Printtyp.string_of_type_declaration(~ident, td),
              })
            | _ => None
            }
          },
          sigs,
        );
      fnsigs;
    | _ => []
    }
  | exception _ => []
  };
};
