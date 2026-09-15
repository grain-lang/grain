open Grain_parsing;
open Grain_typed;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;
open Syntax.Types;

type keyword_entry = {
  label: string,
  snippet: option(string),
  sort_label: option(string),
};

let entry_metadata = (slot: keyword_slot, label) =>
  switch (slot, label) {
  | (ToplevelStatement, "from") => {
      label,
      snippet: Some("from \"$1\" include $2"),
      sort_label: None,
    }
  | (ImportIncludeTail, "include") => {
      label,
      snippet: None,
      sort_label: Some("include"),
    }
  | (_, _) => {
      label,
      snippet: None,
      sort_label: None,
    }
  };

let keywords_for_slot = (slot: keyword_slot) =>
  Keywords.all
  |> List.filter(label => List.mem(slot, Keywords.slots_for(label)))
  |> List.map(label => entry_metadata(slot, label));

let expected_matches_path = (~env, expected_type, path) =>
  switch (Ctype.expand_head(env, expected_type).desc) {
  | TTyConstr(expected_path, [], _) => Path.same(expected_path, path)
  | _ => false
  };

let keyword_matches_expected_type = (~env, ~expected_type, label) =>
  switch (expected_type, env) {
  | (Some(expected_type), Some(env)) =>
    switch (label) {
    | "true"
    | "false" =>
      expected_matches_path(~env, expected_type, Builtin_types.path_bool)
    | "void" =>
      expected_matches_path(~env, expected_type, Builtin_types.path_void)
    | "let" => false
    | "assert"
    | "while"
    | "for" =>
      expected_matches_path(~env, expected_type, Builtin_types.path_void)
    | _ => true
    }
  | _ => true
  };

let keyword_candidates =
    (~context, ~env=?, ~expected_type=?, slot: keyword_slot) => {
  keywords_for_slot(slot)
  |> List.filter(entry =>
       keyword_matches_expected_type(~env, ~expected_type, entry.label)
     )
  |> List.map(entry => {
       let insert_text_format =
         switch (entry.snippet) {
         | Some(_) => Some(SnippetFormat)
         | None => None
         };
       make_candidate(
         ~source=KeywordSource,
         ~label=entry.label,
         ~kind=Keyword,
         ~sort_group=Sort_group.Keyword,
         ~context,
         ~insert_text=?entry.snippet,
         ~insert_text_format?,
         ~sort_label=?entry.sort_label,
         (),
       );
     });
};
