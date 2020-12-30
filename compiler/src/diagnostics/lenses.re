open Grain_typed;

// Left as a string not a variant as this is opaque to the
// work done here and gets parsed into a string immediately anyway

// t:
// S statement
// D data
// E expression
// P pattern

[@deriving yojson]
type lens_t = {
  sl: int, // start line
  sc: int, // start character
  sb: int, // start BOL
  el: int, // end line
  ec: int, // end character
  eb: int, // end BOL
  s: string, // type signature
  t: string // lens stype
};

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let make_value =
    (~location: Grain_parsing.Location.t, ~sigStr: string, ~valType: string) => {
  let (file, startline, startchar, sbol) =
    get_raw_pos_info(location.loc_start);
  let (_, endline, endchar, ebol) = get_raw_pos_info(location.loc_end);
  let lens: lens_t = {
    sl: startline,
    sc: startchar,
    sb: sbol,
    el: endline,
    ec: endchar,
    eb: ebol,
    s: sigStr,
    t: valType,
  };
  lens;
};

let hover_val =
    (
      ~location: Grain_parsing.Location.t,
      ~t: Types.type_expr,
      ~valType: string,
    ) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Printtyp.type_expr(ppf, t);
  Format.pp_print_flush(ppf, ());
  let sigStr = Buffer.contents(buf);
  make_value(~location, ~valType, ~sigStr);
};

let create_record_signature =
    (labels: list(Types.record_field), name: string) => {
  let recSig =
    List.fold_left(
      (acc, field: Types.record_field) => {
        let comma = acc == "" ? "" : ", ";
        let buf = Buffer.create(64);
        let ppf = Format.formatter_of_buffer(buf);
        Printtyp.type_expr(ppf, field.rf_type);
        Format.pp_print_flush(ppf, ());
        Printf.sprintf(
          "%s%s%s: %s",
          acc,
          comma,
          Ident.name(field.rf_name),
          Buffer.contents(buf),
        );
      },
      "",
      labels,
    );
  Printf.sprintf("record %s {%s }", name, recSig);
};

let create_enum_signature =
    (labels: list(Types.constructor_declaration), name: string) => {
  Printf.sprintf("enum %s", name);
};

let data_hover_val =
    (
      ~location: Grain_parsing.Location.t,
      ~t: Types.type_declaration,
      ~valType: string,
      ~name: Typedtree.loc(string),
    ) => {
  let sigStr =
    switch (t.type_kind) {
    | TDataVariant(cstrs) => create_enum_signature(cstrs, name.txt)
    | TDataRecord(labels) => create_record_signature(labels, name.txt)
    | TDataAbstract => ""
    | TDataOpen => ""
    };

  make_value(~location, ~valType, ~sigStr);
};

let print_tree = (stmts: list(Grain_typed.Typedtree.toplevel_stmt)) => {
  let lenses = ref([]);

  module Iterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;
      let enter_expression = (exp: Grain_typed__Typedtree.expression) => {
        let lens =
          hover_val(~t=exp.exp_type, ~location=exp.exp_loc, ~valType="E");
        lenses := List.append(lenses^, [lens]);
      };
      let enter_pattern = (pat: Grain_typed__Typedtree.pattern) => {
        let lens =
          hover_val(~t=pat.pat_type, ~location=pat.pat_loc, ~valType="P");
        lenses := List.append(lenses^, [lens]);
      };

      let enter_data_declaration =
          (d: Grain_typed__Typedtree.data_declaration) => {
        let lens =
          data_hover_val(
            ~t=d.data_type,
            ~location=d.data_loc,
            ~valType="D",
            ~name=d.data_name,
          );
        lenses := List.append(lenses^, [lens]);
      };
    });

  List.iter(
    (cur: Grain_typed__Typedtree.toplevel_stmt) => {
      let (file, startline, startchar, sbol) =
        get_raw_pos_info(cur.ttop_loc.loc_start);
      let (_, endline, endchar, ebol) =
        get_raw_pos_info(cur.ttop_loc.loc_end);
      let lens: lens_t = {
        sl: startline,
        sc: startchar,
        sb: sbol,
        el: endline,
        ec: endchar,
        eb: ebol,
        s: "",
        t: "S",
      };

      lenses := List.append(lenses^, [lens]);
      Iterator.iter_toplevel_stmt(cur);
    },
    stmts,
  );

  lenses;
};

let get_lenses_values = (program: Typedtree.typed_program) => {
  let lenses = print_tree(program.statements);
  lenses^;
};
