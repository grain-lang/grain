open Grain_diagnostics;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let docblock_attribute_candidates = (~context) =>
  Graindoc_attributes.all
  |> List.map(label =>
       make_candidate(
         ~source=DocblockAttributeSource,
         ~label,
         ~kind=Snippet,
         ~detail=label,
         ~sort_group=Sort_group.Typed,
         ~context,
         (),
       )
     );
