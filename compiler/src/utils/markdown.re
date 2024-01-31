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

let table = (~headers: list(string), rows) => {
  let header_len = List.length(headers);
  let all_match = List.for_all(row => List.length(row) == header_len, rows);
  if (all_match) {
    let header = String.concat("|", headers);
    let rows =
      List.map(
        row =>
          List.map(
            cell => Str.global_replace(Str.regexp("\n"), "<br/>", cell),
            row,
          ),
        rows,
      );
    let separator =
      String.concat(
        "|",
        List.init(
          header_len,
          idx => {
            let len = List.nth(headers, idx) |> String.length;
            String.init(len, _ => '-');
          },
        ),
      );
    let body =
      List.map(row => Format.sprintf("|%s|", String.concat("|", row)), rows)
      |> String.concat("\n");
    Format.sprintf("|%s|\n|%s|\n%s\n\n", header, separator, body);
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
