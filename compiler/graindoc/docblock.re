open Grain;
open Grain_typed;
open Grain_utils;
open Grain_diagnostics;

type t = {
  module_name: string,
  name: string,
  type_sig: string,
  description: string,
  attributes: list(Comments.Attribute.t),
};

let module_name_of_location = (loc: Grain_parsing.Location.t) => {
  Grain_utils.Files.filename_to_module_name(loc.loc_start.pos_fname);
};

let string_of_value_description = (~ident, vd) => {
  Format.asprintf("%a", Printtyp.value_description(ident), vd);
};

let string_of_type_declaration = (~ident, td) => {
  Format.asprintf("%a", Printtyp.type_declaration(ident), td);
};

let title_for_api = (~module_name, ident: Ident.t) => {
  Format.asprintf("%s.**%a**", module_name, Printtyp.ident, ident);
};

let types_for_function = (vd: Types.value_description) => {
  switch (Ctype.repr(vd.val_type).desc) {
  | TTyArrow(args, returns, _) => (Some(args), Some(returns))
  | _ => (None, None)
  };
};

let lookup_type_expr = (~idx, type_exprs) => {
  Option.bind(type_exprs, te => List.nth_opt(te, idx));
};

let string_of_type_expr = type_expr => {
  let to_string = type_expr => {
    Format.asprintf("%a", Printtyp.type_expr, type_expr);
  };
  Option.map(to_string, type_expr);
};

let for_value_description = (~ident: Ident.t, vd: Types.value_description) => {
  let module_name = module_name_of_location(vd.val_loc);
  let name = title_for_api(~module_name, ident);
  let type_sig = string_of_value_description(~ident, vd);
  let comment =
    Comments.Doc.ending_on_lnum(vd.val_loc.loc_start.pos_lnum - 1);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, attributes)) => (description, attributes)
    | None => ("", [])
    };

  let (args, returns) = types_for_function(vd);
  // This replaces the default `None` for `attr_type` on `Param` and `Returns` attributes
  let apply_types: (int, Comments.Attribute.t) => Comments.Attribute.t =
    (idx, attr) => {
      switch (attr) {
      | Param(attr) =>
        let attr_type = lookup_type_expr(~idx, args) |> string_of_type_expr;
        Param({...attr, attr_type});
      | Returns(attr) =>
        let attr_type = string_of_type_expr(returns);
        Returns({...attr, attr_type});
      | _ => attr
      };
    };
  let attributes = List.mapi(apply_types, attributes);

  {module_name, name, type_sig, description, attributes};
};

let for_type_declaration = (~ident: Ident.t, td: Types.type_declaration) => {
  let module_name = module_name_of_location(td.type_loc);
  let name = title_for_api(~module_name, ident);
  let type_sig = string_of_type_declaration(~ident, td);
  let comment =
    Comments.Doc.ending_on_lnum(td.type_loc.loc_start.pos_lnum - 1);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, _)) => (description, [])
    | None => ("", [])
    };

  {module_name, name, type_sig, description, attributes};
};

let for_signature_item = (~env: Env.t, sig_item: Types.signature_item) => {
  switch (sig_item) {
  | TSigValue(ident, vd) =>
    let vd = Env.find_value(vd.val_fullpath, env);
    let docblock = for_value_description(~ident, vd);
    Some(docblock);
  | TSigType(ident, td, _rec) =>
    let td = Env.find_type(td.type_path, env);
    let docblock = for_type_declaration(~ident, td);
    Some(docblock);
  | _ => None
  };
};

let signature_item_in_range =
    (~env: Env.t, sig_item: Types.signature_item, range: Grain_utils.Range.t) => {
  switch (sig_item) {
  | TSigValue(ident, vd) =>
    let vd = Env.find_value(vd.val_fullpath, env);
    Grain_utils.Range.inRange(vd.val_loc.loc_start.pos_lnum, range);
  | TSigType(ident, td, _rec) =>
    let td = Env.find_type(td.type_path, env);
    Grain_utils.Range.inRange(td.type_loc.loc_start.pos_lnum, range);
  | _ => false
  };
};

let to_markdown = docblock => {
  let buf = Buffer.create(0);
  Buffer.add_string(buf, Markdown.heading(~level=3, docblock.name));
  let deprecations =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_deprecated)
    |> List.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | Deprecated({attr_desc}) => attr_desc
         | _ =>
           failwith(
             "Unreachable: Non-`deprecated` attribute can't exist here.",
           )
         }
       });
  if (List.length(deprecations) > 0) {
    List.iter(
      msg =>
        Buffer.add_string(
          buf,
          Markdown.blockquote(Markdown.bold("Deprecated:") ++ " " ++ msg),
        ),
      deprecations,
    );
  };
  Buffer.add_string(buf, Markdown.code_block(docblock.type_sig));
  if (String.length(docblock.description) > 0) {
    Buffer.add_string(buf, Markdown.paragraph(docblock.description));
  };
  let params =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_param)
    |> List.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | Param({attr_name, attr_type, attr_desc}) => [
             Markdown.code(attr_name),
             Option.value(~default="", attr_type),
             attr_desc,
           ]
         | _ =>
           failwith("Unreachable: Non-`param` attribute can't exist here.")
         }
       });
  if (List.length(params) > 0) {
    Buffer.add_string(buf, Markdown.paragraph("Parameters:"));
    Buffer.add_string(
      buf,
      Markdown.table(~headers=["param", "type", "description"], params),
    );
  };
  let returns =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_returns)
    |> List.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | Returns({attr_type, attr_desc}) => [
             Option.value(~default="", attr_type),
             attr_desc,
           ]
         | _ =>
           failwith("Unreachable: Non-`returns` attribute can't exist here.")
         }
       });
  if (List.length(returns) > 0) {
    Buffer.add_string(buf, Markdown.paragraph("Returns:"));
    Buffer.add_string(
      buf,
      Markdown.table(~headers=["type", "description"], returns),
    );
  };
  let examples =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_example)
    |> List.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | Example({attr_desc}) => attr_desc
         | _ =>
           failwith("Unreachable: Non-`example` attribute can't exist here.")
         }
       });
  if (List.length(examples) > 0) {
    Buffer.add_string(buf, Markdown.paragraph("Examples:"));
    List.iter(
      example => Buffer.add_string(buf, Markdown.code_block(example)),
      examples,
    );
  };
  buf;
};
