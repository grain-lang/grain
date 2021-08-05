let details = (~disabled=false, ~summary, str) => {
  Format.sprintf(
    "%s\n%s%s</summary>\n%s\n</details>\n\n",
    // The `disabled` html attribute doesn't do anything to <details> but we add it for easier styling
    disabled ? {|<details disabled>|} : {|<details>|},
    // We also add tabIndex="-1" if it is disabled so the element can't be tabbed to
    // Let us know if that is bad and we can remove
    disabled ? {|<summary tabindex="-1">|} : {|<summary>|},
    summary,
    str,
  );
};

let code = code => {
  Format.sprintf("<code>%s</code>", code);
};

let table = (~headers: list(string), rows) => {
  let header_len = List.length(headers);
  let all_match = List.for_all(row => List.length(row) == header_len, rows);
  if (all_match) {
    let header =
      headers
      |> List.map(str => Format.sprintf("<th>%s</th>", str))
      |> String.concat("")
      |> Format.sprintf("<tr>%s</tr>");

    let body =
      rows
      |> List.map(row => {
           row
           |> List.map(str => Format.sprintf("<td>%s</td>", str))
           |> String.concat("")
           |> Format.sprintf("<tr>%s</tr>")
         })
      |> String.concat("\n");
    Format.sprintf(
      "<table>\n<thead>\n%s\n</thead>\n<tbody>\n%s\n</tbody>\n</table>",
      header,
      body,
    );
  } else {
    failwith("Your rows didn't all have the correct length");
  };
};
