open Grain_typed;

type lens_t = {
  line: int,
  signature: string,
};

let lens_to_yojson = (l: lens_t): Yojson.t =>
  `Assoc([("line", `Int(l.line)), ("signature", `String(l.signature))]);

let lens_list_to_yojson = (lenses: list(lens_t)): Yojson.t =>
  `List(List.map(l => lens_to_yojson(l), lenses));

let lenses_to_json_string = (l: list(lens_t)): string =>
  Yojson.to_string(lens_list_to_yojson(l));

let output_lenses = (program: Typedtree.typed_program): list(lens_t) => {
  let lenses =
    List.map(
      (statement: Typedtree.toplevel_stmt) => {
        let d = statement.ttop_desc;
        switch (d) {
        | TTopExpr(expr) =>
          let buf = Buffer.create(64);
          let ppf = Format.formatter_of_buffer(buf);
          let t: Types.type_expr = expr.exp_type;
          Printtyp.type_expr(ppf, t);
          Format.pp_print_flush(ppf, ());
          let signt = Buffer.contents(buf);
          let lens: lens_t = {
            line: statement.ttop_loc.loc_start.pos_lnum,
            signature: signt,
          };
          Some(lens);
        | TTopLet(_, _, _, valueBindings) =>
          let signatures =
            List.map(
              (vb: Typedtree.value_binding) => {
                let expr = vb.vb_expr;

                let t: Types.type_expr = expr.exp_type;
                let buf = Buffer.create(64);
                let ppf = Format.formatter_of_buffer(buf);
                Printtyp.type_expr(ppf, t);
                Format.pp_print_flush(ppf, ());
                Buffer.contents(buf);
              },
              valueBindings,
            );
          if (List.length(signatures) > 0) {
            let signature = List.hd(signatures);
            let lens: lens_t = {
              line: statement.ttop_loc.loc_start.pos_lnum,
              signature,
            };
            Some(lens);
          } else {
            None;
          };

        | _ => None
        };
      },
      program.statements,
    );

  List.fold_left(
    (acc, st: option(lens_t)) =>
      switch (st) {
      | None => acc
      | Some(s) => List.append(acc, [s])
      },
    [],
    lenses,
  );
};
