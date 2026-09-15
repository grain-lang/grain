open Types;

let byte_offset = (source, pos: position) => {
  let rec line_offset = (idx, lines, acc) =>
    switch (lines) {
    | [] => acc
    | [line, ...rest] =>
      if (idx == pos.line) {
        acc
        + Edit.clamp(
            ~min_value=0,
            ~max_value=String.length(line),
            pos.character,
          );
      } else {
        line_offset(idx + 1, rest, acc + String.length(line) + 1);
      }
    };
  line_offset(0, String.split_on_char('\n', source), 0);
};

let replace_range =
    (pos: position, start_character, end_character): Protocol.range => {
  range_start: {
    line: pos.line,
    character: start_character,
  },
  range_end: {
    line: pos.line,
    character: end_character,
  },
};

let line_at = (source, pos: position) => {
  switch (String.split_on_char('\n', source)) {
  | [] => ""
  | lines =>
    let rec at = (idx, lines) =>
      switch (lines) {
      | [] => ""
      | [line, ...rest] => idx == pos.line ? line : at(idx + 1, rest)
      };
    at(0, lines);
  };
};

let is_ident_char = c =>
  c >= '0'
  && c <= '9'
  || c >= 'A'
  && c <= 'Z'
  || c >= 'a'
  && c <= 'z'
  || c == '_';

let qualifier_before_dot_in_text = (text, cursor_offset) =>
  if (!String.contains(text, '.')) {
    None;
  } else if (cursor_offset < 0 || cursor_offset > String.length(text)) {
    None;
  } else {
    let before_cursor = String.sub(text, 0, cursor_offset);
    switch (String.rindex_opt(before_cursor, '.')) {
    | None => None
    | Some(dot_index) =>
      let qualifier = String.sub(text, 0, dot_index);
      qualifier == "" ? None : Some(qualifier);
    };
  };

let qualifier_from_node =
    (tree: parse_tree, node: Tree.node, cursor_byte: int) =>
  qualifier_before_dot_in_text(
    Tree.node_text(tree, node),
    cursor_byte - Tree.node_start_byte(node),
  );

let qualifier_from_line = (~require_module_name=true, source, pos: position) => {
  let line = line_at(source, pos);
  let character =
    Edit.clamp(~min_value=0, ~max_value=String.length(line), pos.character);
  let before_cursor = String.sub(line, 0, character);
  let rec find_segment_start = idx =>
    if (idx <= 0) {
      0;
    } else if (is_ident_char(before_cursor.[idx - 1])) {
      find_segment_start(idx - 1);
    } else {
      idx;
    };
  let segment_start = find_segment_start(character);
  if (segment_start <= 0 || before_cursor.[segment_start - 1] != '.') {
    None;
  } else {
    let dot_index = segment_start - 1;
    let rec find_start = idx =>
      if (idx <= 0) {
        0;
      } else if (is_ident_char(before_cursor.[idx - 1])
                 || before_cursor.[idx - 1] == '.') {
        find_start(idx - 1);
      } else {
        idx;
      };
    let start = find_start(dot_index);
    let qualifier = String.sub(before_cursor, start, dot_index - start);
    if (String.length(qualifier) == 0) {
      None;
    } else if (require_module_name
               && !(qualifier.[0] >= 'A' && qualifier.[0] <= 'Z')) {
      None;
    } else {
      Some(qualifier);
    };
  };
};

let prefix_from_cursor = (source, pos: position) => {
  let line = line_at(source, pos);
  let character =
    Edit.clamp(~min_value=0, ~max_value=String.length(line), pos.character);
  let rec find_start = idx =>
    if (idx <= 0) {
      0;
    } else if (is_ident_char(line.[idx - 1])) {
      find_start(idx - 1);
    } else {
      idx;
    };
  let start = find_start(character);
  (String.sub(line, start, character - start), start, character);
};

let prefix_from_doc_attribute = (source, pos: position) => {
  let line = line_at(source, pos);
  let character =
    Edit.clamp(~min_value=0, ~max_value=String.length(line), pos.character);
  let before_cursor = String.sub(line, 0, character);
  switch (String.rindex_opt(before_cursor, '@')) {
  | None => ("", character, character)
  | Some(at_index) =>
    let rec valid_suffix = idx =>
      if (idx >= character) {
        true;
      } else if (is_ident_char(line.[idx])) {
        valid_suffix(idx + 1);
      } else {
        false;
      };
    if (valid_suffix(at_index + 1)) {
      (
        String.sub(line, at_index, character - at_index),
        at_index,
        character,
      );
    } else {
      ("", character, character);
    };
  };
};
