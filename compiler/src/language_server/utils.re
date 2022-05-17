type source_location = {
  uri: string,
  line: int,
  char: int,
};

// TODO: Either RPC or message and then pass into each `process` function
let get_text_document_uri_and_position = json => {
  let params = Yojson.Safe.Util.member("params", json);
  let textDocument = Yojson.Safe.Util.member("textDocument", params);
  let position = Yojson.Safe.Util.member("position", params);

  let uri =
    Yojson.Safe.Util.member("uri", textDocument)
    |> Yojson.Safe.Util.to_string_option;

  let line =
    Yojson.Safe.Util.member("line", position)
    |> Yojson.Safe.Util.to_int_option;

  let char =
    Yojson.Safe.Util.member("character", position)
    |> Yojson.Safe.Util.to_int_option;

  switch (uri, line, char) {
  | (Some(uri), Some(line), Some(char)) => Some({uri, line, char})
  | _ => None
  };
};

let convert_uri_to_filename = (uri: string) => {
  let filetype = "file://";
  let typelen = String.length(filetype);
  switch (String.sub(uri, 0, typelen)) {
  | exception exn => uri
  | start =>
    String.lowercase_ascii(start) == filetype
      ? String.sub(uri, typelen, String.length(uri) - typelen) : uri
  };
};
