open Grain_parsing;
open Grain_typed;
open Grain_diagnostics;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let member_candidates =
    (~context, ~qualifier, ~include_export, ~completion_kind, env) =>
  try({
    let path =
      Env.lookup_module(~load=false, Identifier.parse(qualifier), None, env);
    let md = Env.find_module(path, None, env);
    Modules.get_provides(md)
    |> List.filter((provide: Modules.export) =>
         include_export(provide.kind)
       )
    |> List.map((provide: Modules.export) => {
         let name = provide.name;
         let is_operator =
           String.length(name) == 0 || !Syntax.Text.is_ident_char(name.[0]);
         make_candidate(
           ~source=Typedtree,
           ~label=is_operator ? Printf.sprintf("(%s)", name) : name,
           ~kind=completion_kind(provide.kind),
           ~detail=
             is_operator
               ? Printf.sprintf(
                   "%s.(%s) %s",
                   qualifier,
                   name,
                   provide.signature,
                 )
               : provide.signature,
           ~sort_group=
             if (is_operator) {Sort_group.Operator} else {Sort_group.Typed},
           ~filter_text=name,
           ~context,
           (),
         );
       });
  }) {
  | Not_found => []
  | exn =>
    Trace.log(
      "Completion member_access failed for qualifier `"
      ++ qualifier
      ++ "`: "
      ++ Printexc.to_string(exn),
    );
    [];
  };

let module_candidates = (~context, ~qualifier, env) =>
  member_candidates(
    ~context,
    ~qualifier,
    ~include_export=
      kind =>
        switch (kind) {
        | Modules.Enum => false
        | _ => true
        },
    ~completion_kind=
      kind =>
        switch (kind) {
        | Modules.Function => Function
        | Modules.Value => Value
        | Modules.Enum => Enum
        | Modules.Record => Struct
        | Modules.Abstract => TypeParameter
        | Modules.Exception => Constructor
        | Modules.Module => Module
        },
    env,
  );

let use_path_candidates = (~context, ~qualifier, env) =>
  member_candidates(
    ~context,
    ~qualifier,
    ~include_export=kind => kind == Modules.Module,
    ~completion_kind=_ => Module,
    env,
  );

let type_candidates = (~context, ~qualifier, env) =>
  member_candidates(
    ~context,
    ~qualifier,
    ~include_export=
      kind =>
        switch (kind) {
        | Modules.Record
        | Modules.Enum
        | Modules.Abstract
        | Modules.Module => true
        | Modules.Function
        | Modules.Value
        | Modules.Exception => false
        },
    ~completion_kind=
      kind =>
        switch (kind) {
        | Modules.Record => Class
        | Modules.Enum
        | Modules.Abstract => TypeParameter
        | Modules.Module => Module
        | Modules.Function => Function
        | Modules.Value => Value
        | Modules.Exception => Constructor
        },
    env,
  );

let use_completion_kind = (kind: Modules.export_kind) =>
  switch (kind) {
  // Functions treated as values to avoid appending `()`
  | Modules.Function => Value
  | Modules.Value => Value
  | Modules.Enum
  | Modules.Abstract => TypeParameter
  | Modules.Record => Struct
  | Modules.Exception => Constructor
  | Modules.Module => Module
  };

let use_shape_snippet_candidate = (~context) => {
  [
    make_candidate(
      ~source=TreeSitter,
      ~label="{ }",
      ~kind=Snippet,
      ~detail="use bindings",
      ~sort_group=Sort_group.Typed,
      ~filter_text="{",
      ~insert_text="{ $1 }",
      ~insert_text_format=SnippetFormat,
      ~context,
      (),
    ),
  ];
};

let use_item_candidates =
    (~context, ~qualifier, ~slot: Syntax.Types.use_item_slot, env) => {
  let include_export = (kind: Modules.export_kind) =>
    switch (slot) {
    | Syntax.Types.UsePlain => true
    | Syntax.Types.UseType =>
      switch (kind) {
      | Modules.Record
      | Modules.Enum
      | Modules.Abstract => true
      | _ => false
      }
    | Syntax.Types.UseModule => kind == Modules.Module
    | Syntax.Types.UseException => kind == Modules.Exception
    };
  let decorate = (kind: Modules.export_kind, name) =>
    switch (slot) {
    | Syntax.Types.UsePlain =>
      switch (kind) {
      | Modules.Record
      | Modules.Enum
      | Modules.Abstract => "type " ++ name
      | Modules.Module => "module " ++ name
      | Modules.Exception => "exception " ++ name
      | Modules.Function
      | Modules.Value => name
      }
    | Syntax.Types.UseType
    | Syntax.Types.UseModule
    | Syntax.Types.UseException => name
    };
  try({
    let path =
      Env.lookup_module(~load=false, Identifier.parse(qualifier), None, env);
    let md = Env.find_module(path, None, env);
    Modules.get_provides(md)
    |> List.filter((provide: Modules.export) =>
         include_export(provide.kind)
       )
    |> List.map((provide: Modules.export) => {
         let name = provide.name;
         let is_operator =
           String.length(name) == 0 || !Syntax.Text.is_ident_char(name.[0]);
         let display_name = is_operator ? Printf.sprintf("(%s)", name) : name;
         let insert = decorate(provide.kind, display_name);
         make_candidate(
           ~source=Typedtree,
           ~label=insert,
           ~kind=use_completion_kind(provide.kind),
           ~detail=provide.signature,
           ~sort_group=
             if (is_operator) {Sort_group.Operator} else {Sort_group.Typed},
           ~filter_text=display_name,
           ~insert_text=insert,
           ~context,
           (),
         );
       });
  }) {
  | Not_found => []
  | exn =>
    Trace.log(
      "Completion use_item_candidates failed for qualifier `"
      ++ qualifier
      ++ "`: "
      ++ Printexc.to_string(exn),
    );
    [];
  };
};
