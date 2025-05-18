let heading = (~level=1, str) => {
  let hashes = String.make(level, '#');
  Format.sprintf("%s %s\n\n", hashes, str);
};

let paragraph = str => {
  Format.sprintf("%s\n\n", str);
};

let code_block = (~syntax="grain", code) => {
  Format.sprintf("```%s\n%s\n```\n\n", syntax, code);
};

let code = code => {
  Format.sprintf("`%s`", code);
};

let compute_column_widths = (headers, rows) => {
  // Minimum markdown column width is 3
  let column_widths =
    Array.of_list(
      List.map(header => max(String.length(header), 3), headers),
    );
  List.iter(
    row => {
      List.iteri(
        (idx, cell) => {
          column_widths[idx] = max(column_widths[idx], String.length(cell))
        },
        row,
      )
    },
    rows,
  );
  column_widths;
};

let table = (~headers: list(string), rows) => {
  let header_len = List.length(headers);
  let all_match = List.for_all(row => List.length(row) == header_len, rows);
  if (all_match) {
    let rows =
      List.map(
        row =>
          List.map(
            cell => Str.global_replace(Str.regexp("\n"), "<br/>", cell),
            row,
          ),
        rows,
      );
    let column_widths = compute_column_widths(headers, rows);
    let buffer = Buffer.create(512);

    let write_row = (~pad_char=' ', row) => {
      List.iteri(
        (idx, cell) => {
          let width = column_widths[idx];
          Buffer.add_string(buffer, "| ");
          Buffer.add_string(buffer, cell);
          Buffer.add_string(
            buffer,
            String.make(width - String.length(cell), pad_char),
          );
          Buffer.add_string(buffer, " ");
        },
        row,
      );
      Buffer.add_string(buffer, "|\n");
    };

    write_row(headers);
    write_row(~pad_char='-', List.map(_ => "---", headers));
    List.iter(write_row, rows);
    Buffer.add_string(buffer, "\n");

    Buffer.contents(buffer);
  } else {
    failwith("Your rows didn't all have the correct length");
  };
};

let frontmatter = rows => {
  let to_key_value = ((key, value)) => {
    Format.sprintf("%s: %s", key, value);
  };
  rows
  |> List.map(to_key_value)
  |> String.concat("\n")
  |> Format.sprintf("---\n%s\n---\n\n");
};

let bold = str => {
  let escaped_str =
    Str.global_substitute(
      Str.regexp({|\(^\*+\)\|\(\*\*+\)\|\(\*+$\)|}),
      str => {
        let matched = Str.matched_string(str);
        Str.global_replace(Str.regexp({|\*|}), {|\*|}, matched);
      },
      str,
    );
  Format.sprintf("**%s**", escaped_str);
};

let blockquote = str => {
  Format.sprintf("> %s\n\n", str);
};

let bullet_list = items => {
  let bullets =
    List.map(item => Format.sprintf("* %s", item), items)
    |> String.concat("\n");
  Format.sprintf("%s\n\n", bullets);
};
