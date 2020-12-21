open Grain_typed;

[@deriving yojson]
type lens_t = {
  sl: int,
  sc: int,
  el: int,
  ec: int,
  s: string,
  t: string,
};

let make_value =
    (~location: Grain_parsing.Location.t, ~sigStr: string, ~valType: string) => {
  let (file, startline, startchar) =
    Location.get_pos_info(location.loc_start);
  let (_, endline, endchar) = Location.get_pos_info(location.loc_end);
  let lens: lens_t = {
    sl: startline,
    sc: startchar,
    el: endline,
    ec: endchar,
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

let data_hover_val =
    (
      ~location: Grain_parsing.Location.t,
      ~t: Types.type_declaration,
      ~valType: string,
    ) => {
  let sigStr = "data";
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
          data_hover_val(~t=d.data_type, ~location=d.data_loc, ~valType="D");
        lenses := List.append(lenses^, [lens]);
      };
    });

  List.iter(
    (cur: Grain_typed__Typedtree.toplevel_stmt) => {
      let (file, startline, startchar) =
        Location.get_pos_info(cur.ttop_loc.loc_start);
      let (_, endline, endchar) =
        Location.get_pos_info(cur.ttop_loc.loc_end);
      let lens: lens_t = {
        sl: startline,
        sc: startchar,
        el: endline,
        ec: endchar,
        s: "",
        t: "S",
      };

      lenses := List.append(lenses^, [lens]);
      switch (cur.ttop_desc) {
      | TTopException(_)
      | TTopForeign(_)
      | TTopImport(_)
      | TTopExport(_)
      | TTopExpr(_)
      | TTopLet(_) => Iterator.iter_toplevel_stmt(cur)
      | TTopData(_) => Iterator.iter_toplevel_stmt(cur)
      };
    },
    stmts,
  );

  lenses;
};

let get_lenses_values = (program: Typedtree.typed_program) => {
  let lenses = print_tree(program.statements);
  lenses^;
};
