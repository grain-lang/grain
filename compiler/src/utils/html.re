let details = (~is_open=false, ~summary, str) => {
  Format.sprintf(
    "%s\n<summary>%s</summary>\n%s\n</details>\n\n",
    is_open ? "<details open>" : "<details>",
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
    Format.sprintf("<table>\n%s\n%s\n</table>", header, body);
  } else {
    failwith("Your rows didn't all have the correct length");
  };
};
