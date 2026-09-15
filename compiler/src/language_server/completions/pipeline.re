open Completion_types;

let candidate_cap = 100;

let more_authoritative = (candidate, incumbent) =>
  if (source_rank(candidate.source) < source_rank(incumbent.source)) {
    candidate;
  } else if (source_rank(candidate.source) == source_rank(incumbent.source)
             && Relevance.compare(candidate.relevance, incumbent.relevance)
             < 0) {
    candidate;
  } else {
    incumbent;
  };

let dedupe_candidates = candidates => {
  let best = Hashtbl.create(256);
  let order = ref([]);
  List.iter(
    candidate => {
      // FIXME: Due to obtaining candidates from both tree-sitter and typed env, the same label may have 2 different kinds (tree-sitter treats functions as variables currently)
      let key = candidate.item.label;
      switch (Hashtbl.find_opt(best, key)) {
      | None =>
        Hashtbl.add(best, key, candidate);
        order := [key, ...order^];
      | Some(incumbent) =>
        Hashtbl.replace(best, key, more_authoritative(candidate, incumbent))
      };
    },
    candidates,
  );
  List.rev_map(key => Hashtbl.find(best, key), order^);
};

let compare_candidate = (left, right) =>
  switch (Relevance.compare(left.relevance, right.relevance)) {
  | 0 =>
    switch (
      Int.compare(
        Sort_group.rank(left.sort_group),
        Sort_group.rank(right.sort_group),
      )
    ) {
    | 0 => String.compare(left.item.label, right.item.label)
    | cmp => cmp
    }
  | cmp => cmp
  };

let filter_prefix = (~prefix, candidates) =>
  List.filter(
    candidate => {
      let text =
        switch (candidate.item.filter_text) {
        | Some(filter_text) => filter_text
        | None => candidate.item.label
        };
      prefix == "" || String.starts_with(~prefix, text);
    },
    candidates,
  );

let take_candidates = (count, candidates) => {
  let rec loop = (remaining, acc, candidates) =>
    if (remaining <= 0) {
      List.rev(acc);
    } else {
      switch (candidates) {
      | [] => List.rev(acc)
      | [candidate, ...rest] =>
        loop(remaining - 1, [candidate, ...acc], rest)
      };
    };
  loop(count, [], candidates);
};

let finalize_candidates = (~prefix, candidates) => {
  let candidates =
    candidates
    |> dedupe_candidates
    |> filter_prefix(~prefix)
    |> List.sort(compare_candidate);
  let is_incomplete = List.length(candidates) > candidate_cap;
  let items =
    candidates
    |> take_candidates(candidate_cap)
    |> List.map(candidate => candidate.item);
  (is_incomplete, items);
};
