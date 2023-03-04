open Grain_typed;

type export_kind =
  | Function
  | Value
  | Record
  | Enum
  | Abstract
  | Exception;

type export = {
  name: string,
  kind: export_kind,
  signature: string,
};

let rec get_provides = (md: Types.module_declaration) =>
  try(
    switch (md.md_type) {
    | TModSignature(sigs) =>
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
          | TSigType(ident, {type_kind: TDataRecord(_), _} as td, recstatus) =>
            Some({
              name: ident.name,
              kind: Record,
              signature: Printtyp.string_of_type_declaration(~ident, td),
            })
          | TSigType(ident, {type_kind: TDataVariant(_), _} as td, recstatus) =>
            Some({
              name: ident.name,
              kind: Enum,
              signature: Printtyp.string_of_type_declaration(~ident, td),
            })
          | TSigType(ident, {type_kind: TDataAbstract, _} as td, recstatus) =>
            Some({
              name: ident.name,
              kind: Abstract,
              signature: Printtyp.string_of_type_declaration(~ident, td),
            })
          | TSigType(ident, {type_kind: TDataOpen, _} as td, recstatus) =>
            Some({
              name: ident.name,
              kind: Exception, // Currently we only use TDataOpen for exceptions
              signature: Printtyp.string_of_type_declaration(~ident, td),
            })
          | _ => None
          }
        },
        sigs,
      )
    | _ => []
    }
  ) {
  | e => []
  };
