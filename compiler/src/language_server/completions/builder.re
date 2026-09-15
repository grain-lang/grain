open Completion_types;
open Completion_types.ResponseResult;

let make_candidate =
    (
      ~source,
      ~label,
      ~kind,
      ~detail=?,
      ~sort_group: Sort_group.t,
      ~context: Syntax.Types.t,
      ~relevance_boost=0,
      ~filter_text=?,
      ~insert_text=?,
      ~insert_text_format=?,
      ~sort_label=?,
      ~tags=?,
      (),
    ) => {
  let insert = Option.value(~default=label, insert_text);
  let relevance =
    Relevance.make(
      ~source,
      ~sort_group,
      ~label,
      ~filter_text?,
      ~prefix=context.prefix,
      ~boost=relevance_boost,
      (),
    );
  let text_edit = {
    let edit: Protocol.text_edit = {
      range: context.replace_range,
      new_text: insert,
    };
    Some(edit);
  };
  let sort_text =
    switch (sort_label) {
    | Some(sort_label) => "00_" ++ sort_label
    | None => Relevance.to_sort_text(~relevance, ~label)
    };
  let command =
    switch (kind) {
    | Snippet =>
      Some(
        {
          title: "",
          command: "editor.action.triggerSuggest",
        }: Protocol.command,
      )
    | _ => None
    };
  let item = {
    label,
    kind,
    detail,
    sort_text,
    filter_text,
    insert_text: None,
    insert_text_format,
    text_edit,
    command,
    tags,
  };
  {
    item,
    source,
    sort_group,
    relevance,
  };
};
