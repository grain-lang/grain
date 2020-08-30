let explode = string => {
  let get_char = String.get(string);
  List.init(String.length(string), get_char);
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
