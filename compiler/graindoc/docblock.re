open Grain;
open Grain_typed;
open Grain_utils;
open Grain_diagnostics;

type t = {
  module_name: string,
  name: string,
  type_sig: string,
  description: option(string),
  attributes: list(Comments.Attribute.t),
};

exception
  MissingFlag({
    flag: string,
    attr: string,
  });

let () =
  Printexc.register_printer(exn => {
    switch (exn) {
    | MissingFlag({flag, attr}) =>
      let msg =
        Printf.sprintf(
          "Must provide %s when generating docs with `%s` attribute.",
          flag,
          attr,
        );
      Some(msg);
    | _ => None
    }
  });

let enumerate_exports = stmts => {
  let id_tbl = ref(Ident.empty);

  let rec pattern_ids = ({pat_desc, pat_loc}: Typedtree.pattern) => {
    switch (pat_desc) {
    | TPatVar(id, _) => [(id, pat_loc)]
    | TPatAlias(subpat, id, _) => [(id, pat_loc), ...pattern_ids(subpat)]
    | TPatTuple(pats)
    | TPatArray(pats)
    | TPatConstruct(_, _, pats) => List.concat(List.map(pattern_ids, pats))
    | TPatRecord(elts, _) =>
      List.concat(List.map(((_, _, pat)) => pattern_ids(pat), elts))
    | _ => []
    };
  };

  module ExportIterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;

      let enter_toplevel_stmt =
          ({ttop_desc, ttop_attributes}: Typedtree.toplevel_stmt) => {
        switch (ttop_desc) {
        | TTopData(decls) =>
          List.iter(
            ({data_id, data_loc}: Typedtree.data_declaration) => {
              id_tbl := Ident.add(data_id, data_loc, id_tbl^)
            },
            decls,
          )
        | TTopExport(decls) =>
          List.iter(
            ({tex_id, tex_loc}: Typedtree.export_declaration) => {
              id_tbl := Ident.add(tex_id, tex_loc, id_tbl^)
            },
            decls,
          )
        | TTopForeign({tvd_id, tvd_loc}) =>
          id_tbl := Ident.add(tvd_id, tvd_loc, id_tbl^)
        | TTopLet(_, _, vbinds) =>
          List.iter(
            ({vb_pat}: Typedtree.value_binding) => {
              List.iter(
                ((id, loc)) => {id_tbl := Ident.add(id, loc, id_tbl^)},
                pattern_ids(vb_pat),
              )
            },
            vbinds,
          )
        | _ => ()
        };
      };
    });

  List.iter(ExportIterator.iter_toplevel_stmt, stmts);

  id_tbl^;
};

let location_for_ident = (~exports, ident) => {
  snd(Ident.find_name(Ident.name(ident), exports));
};

let module_name_of_location = (loc: Grain_parsing.Location.t) => {
  Grain_utils.Filepath.String.filename_to_module_name(
    loc.loc_start.pos_fname,
  );
};

let title_for_api = (~module_name, ident: Ident.t) => {
  Format.asprintf("%s.**%a**", module_name, Printtyp.ident, ident);
};

let output_for_since = (~current_version, attr_version) => {
  let current_version =
    switch (current_version) {
    | Some(version) => version
    | None => raise(MissingFlag({flag: "--current-version", attr: "@since"}))
    };
  let (<) = Version.String.less_than;
  if (current_version < attr_version) {
    Format.sprintf("Added in %s", Html.code("next"));
  } else {
    Format.sprintf("Added in %s", Html.code(attr_version));
  };
};

let output_for_history = (~current_version, attr_version, attr_desc) => {
  let current_version =
    switch (current_version) {
    | Some(version) => version
    | None =>
      raise(MissingFlag({flag: "--current-version", attr: "@history"}))
    };
  let (<) = Version.String.less_than;
  if (current_version < attr_version) {
    [Html.code("next"), attr_desc];
  } else {
    [Html.code(attr_version), attr_desc];
  };
};

let types_for_function = (~ident, vd: Types.value_description) => {
  switch (Ctype.repr(vd.val_type).desc) {
  | TTyArrow(args, returns, _) => (Some(args), Some(returns))
  | _ => (None, None)
  };
};

let lookup_type_expr = (~idx, type_exprs) => {
  Option.bind(type_exprs, te => List.nth_opt(te, idx));
};

let for_value_description =
    (
      ~comments,
      ~loc,
      ~module_name=module_name_of_location(loc),
      ~ident: Ident.t,
      vd: Types.value_description,
    ) => {
  let name = title_for_api(~module_name, ident);
  let type_sig = Printtyp.string_of_value_description(~ident, vd);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, attributes)) => (description, attributes)
    | None => (None, [])
    };

  let (args, returns) = types_for_function(~ident, vd);
  // This replaces the default `None` for `attr_type` on `Param` and `Returns` attributes
  let apply_types: (int, Comments.Attribute.t) => Comments.Attribute.t =
    (idx, attr) => {
      switch (attr) {
      | Param(attr) =>
        let attr_type =
          lookup_type_expr(~idx, args)
          |> Option.map(Printtyp.string_of_type_sch);
        Param({...attr, attr_type});
      | Returns(attr) =>
        let attr_type = Option.map(Printtyp.string_of_type_sch, returns);
        Returns({...attr, attr_type});
      | _ => attr
      };
    };
  let attributes = List.mapi(apply_types, attributes);

  {module_name, name, type_sig, description, attributes};
};

let for_type_declaration =
    (
      ~comments,
      ~loc,
      ~module_name=module_name_of_location(loc),
      ~ident: Ident.t,
      td: Types.type_declaration,
    ) => {
  let name = title_for_api(~module_name, ident);
  let type_sig = Printtyp.string_of_type_declaration(~ident, td);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, _)) => (description, [])
    | None => (None, [])
    };

  {module_name, name, type_sig, description, attributes};
};

let for_signature_item =
    (
      ~comments,
      ~exports: Ident.tbl(Grain_parsing.Location.t),
      ~module_name=?,
      sig_item: Types.signature_item,
    ) => {
  switch (sig_item) {
  | TSigValue(ident, vd) =>
    let loc = location_for_ident(~exports, ident);
    let docblock =
      for_value_description(~comments, ~module_name?, ~ident, ~loc, vd);
    Some(docblock);
  | TSigType(ident, td, _rec) =>
    let loc = location_for_ident(~exports, ident);
    let docblock =
      for_type_declaration(~comments, ~module_name?, ~ident, ~loc, td);
    Some(docblock);
  | _ => None
  };
};

let signature_item_in_range =
    (
      ~exports: Ident.tbl(Grain_parsing.Location.t),
      sig_item: Types.signature_item,
      range: Grain_utils.Range.t,
    ) => {
  switch (sig_item) {
  | TSigValue(ident, vd) =>
    let loc = location_for_ident(~exports, ident);
    Grain_utils.Range.inRange(loc.loc_start.pos_lnum, range);
  | TSigType(ident, td, _rec) =>
    let loc = location_for_ident(~exports, ident);
    Grain_utils.Range.inRange(loc.loc_start.pos_lnum, range);
  | _ => false
  };
};

// Used for joining multiple `@throws` annotations with the exact same type
module StringMap = Map.Make(String);

let to_markdown = (~current_version, docblock) => {
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
  // TODO(#787): Should we fail if more than one `@since` attribute?
  let since_attr =
    docblock.attributes
    |> List.find_opt(Comments.Attribute.is_since)
    |> Option.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | Since({attr_version}) =>
           output_for_since(~current_version, attr_version)
         | _ =>
           failwith("Unreachable: Non-`since` attribute can't exist here.")
         }
       });
  let history_attrs =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_history)
    |> List.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | History({attr_version, attr_desc}) =>
           output_for_history(~current_version, attr_version, attr_desc)
         | _ =>
           failwith("Unreachable: Non-`since` attribute can't exist here.")
         }
       });
  if (Option.is_some(since_attr) || List.length(history_attrs) > 0) {
    let summary = Option.value(~default="History", since_attr);
    let disabled = List.length(history_attrs) == 0 ? true : false;
    let details =
      if (List.length(history_attrs) == 0) {
        "No other changes yet.";
      } else {
        Html.table(~headers=["version", "changes"], history_attrs);
      };
    Buffer.add_string(buf, Html.details(~disabled, ~summary, details));
  };
  Buffer.add_string(buf, Markdown.code_block(docblock.type_sig));
  switch (docblock.description) {
  // Guard isn't be needed because we turn an empty string into None during extraction
  | Some(description) =>
    Buffer.add_string(buf, Markdown.paragraph(description))
  | None => ()
  };
  let params =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_param)
    |> List.map((attr: Comments.Attribute.t) => {
         switch (attr) {
         | Param({attr_name, attr_type, attr_desc}) => [
             Markdown.code(attr_name),
             Option.fold(~none="", ~some=Markdown.code, attr_type),
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
             Option.fold(~none="", ~some=Markdown.code, attr_type),
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
  let throws =
    docblock.attributes
    |> List.filter(Comments.Attribute.is_throws)
    |> List.fold_left(
         (map, attr: Comments.Attribute.t) => {
           switch (attr) {
           | Throws({attr_type: Some(attr_type), attr_desc}) =>
             StringMap.update(
               attr_type,
               descs => {
                 switch (descs) {
                 | None => Some([attr_desc])
                 | Some(descs) => Some([attr_desc, ...descs])
                 }
               },
               map,
             )
           | Throws({attr_type: None, attr_desc}) =>
             failwith(
               "Unreachable: `throws` attribute requires an exception type.",
             )
           | _ =>
             failwith("Unreachable: Non-`throws` attribute can't exist here.")
           }
         },
         StringMap.empty,
       )
    |> StringMap.bindings;
  if (List.length(throws) > 0) {
    Buffer.add_string(buf, Markdown.paragraph("Throws:"));
    List.iter(
      ((exception_type, exception_descriptions)) => {
        Buffer.add_string(
          buf,
          Markdown.paragraph(Markdown.code(exception_type)),
        );
        Buffer.add_string(
          buf,
          Markdown.bullet_list(List.rev(exception_descriptions)),
        );
      },
      throws,
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
