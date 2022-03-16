let explode = string => {
  let get_char = String.get(string);
  List.init(String.length(string), get_char);
};

let starts_with = (~offset=0, string, prefix) => {
  let prefixLength = String.length(prefix);
  let stringLength = String.length(string) - offset;
  if (stringLength < prefixLength) {
    false;
  } else {
    String.sub(string, offset, prefixLength) == prefix;
  };
};

let deasterisk_each_line = str => {
  Str.global_replace(Str.regexp("^[ \t]*\\*"), "", str);
};

/**
Slices a string given optional zero-based [~first] and [~last] indexes. The character
at the [~last] index will not be included in the result.

If either index is a negative number, it will be treated as a reverse index from
the end of the string. e.g. [slice(~first=1, ~last=-1, "abc") === "b"].
*/
let slice = (~first=?, ~last=?, string) => {
  let stringLength = String.length(string);

  let wrapNegativeIndex = idx => idx >= 0 ? idx : stringLength + idx;

  let first = Option.fold(~none=0, ~some=wrapNegativeIndex, first);
  let last = Option.fold(~none=stringLength, ~some=wrapNegativeIndex, last);
  // Ensure we aren't working with a `last` value that is too big
  let last = last > stringLength ? stringLength : last;

  let newLength = last - first;
  if (newLength < 0) {
    "";
  } else if (newLength > stringLength) {
    string;
  } else {
    String.sub(string, first, newLength);
  };
};

type trim =
  | KeepIndent
  | FullTrim;

let get_common_indentation = lines => {
  let min_whitespace_length =
    List.fold_left(
      (min_whitespace_length, line) => {
        let non_empty_line =
          Str.string_match(Str.regexp("^\\([ \t]*\\)[^ \t]"), line, 0);
        if (non_empty_line) {
          let whitespace = Str.matched_group(1, line);
          let whitespace_length = String.length(whitespace);
          switch (min_whitespace_length) {
          | None => Some(whitespace_length)
          | Some(min_whitespace_length) =>
            Some(min(min_whitespace_length, whitespace_length))
          };
        } else {
          min_whitespace_length;
        };
      },
      None,
      lines,
    );

  Option.value(~default=0, min_whitespace_length);
};

let trim_each_line = (~style=FullTrim, str) => {
  let lines = str |> Str.split(Str.regexp("\\(\r\n\\|\n\\)"));

  let min_whitespace_length =
    switch (style) {
    | KeepIndent => get_common_indentation(lines)
    | FullTrim => 0
    };

  let trim_style = line => {
    switch (style) {
    | KeepIndent =>
      let line = slice(~first=min_whitespace_length, line);
      Str.global_replace(Str.regexp("[ \t]+$"), "", line);
    | FullTrim => String.trim(line)
    };
  };

  lines |> List.map(trim_style) |> String.concat("\n");
};

/** TODO(#436): Re-enable these when we can include in the Windows build
let%expect_test "empty string" = {
  print_endline(slice(""));
  %expect
  {||};
};

let%expect_test "no first/last" = {
  print_endline(slice("abc"));
  %expect
  {| abc |};
};

let%expect_test "last: positive index within string" = {
  print_endline(slice(~last=1, "abc"));
  %expect
  {| a |};
};

let%expect_test "last: string length" = {
  let str = "abc";
  print_endline(slice(~last=String.length(str), str));
  %expect
  {| abc |};
};

let%expect_test "last: greater than length" = {
  print_endline(slice(~last=5, "abc"));
  %expect
  {| abc |};
};

let%expect_test "last: negative index (wraps)" = {
  print_endline(slice(~last=-1, "abc"));
  %expect
  {| ab |};
};

let%expect_test "first: positive index within string" = {
  print_endline(slice(~first=1, "abc"));
  %expect
  {| bc |};
};

let%expect_test "first: string length" = {
  let str = "abc";
  print_endline(slice(~first=String.length(str), str));
  %expect
  {||};
};

let%expect_test "first: greater than length" = {
  print_endline(slice(~first=4, "abc"));
  %expect
  {||};
};

let%expect_test "first: negative index (wraps)" = {
  print_endline(slice(~first=-1, "abc"));
  %expect
  {| c |};
};

let%expect_test "first + last: positive indexes within string" = {
  print_endline(slice(~first=1, ~last=2, "abc"));
  %expect
  {| b |};
};

let%expect_test "first + last: overlapping indexes" = {
  print_endline(slice(~first=1, ~last=1, "abc"));
  %expect
  {||};
};

let%expect_test "first + last: wrapping first with appropriate last" = {
  let str = "abc";
  print_endline(slice(~first=-1, ~last=String.length(str), str));
  %expect
  {| c |};
};

let%expect_test "first + last: incorrect wrapping" = {
  print_endline(slice(~first=-1, ~last=1, "abc"));
  %expect
  {||};
};

let%expect_test "first + last: wrapping in last" = {
  print_endline(slice(~first=1, ~last=-1, "abc"));
  %expect
  {| b |};
};

let%expect_test "first + last: greater than length" = {
  print_endline(slice(~first=1, ~last=5, "abc"));
  %expect
  {| bc |};
};

let%expect_test "first + last: both negative" = {
  print_endline(slice(~first=-2, ~last=-1, "abc"));
  %expect
  {| b |};
};
*/;
