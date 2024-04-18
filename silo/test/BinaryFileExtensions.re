open Rely.MatcherTypes;

let print_bytes = ((), bytes) => {
  let buf = Buffer.create(Bytes.length(bytes) * 3);
  Bytes.iter(c => Printf.bprintf(buf, "%02x ", Char.code(c)), bytes);
  Buffer.contents(buf);
};

type binaryFileExtensions = {
  toExist: unit => unit,
  toStartWith: bytes => unit,
};

let binaryFileExtensions = (actual, {createMatcher}) => {
  let pass = (() => "", true);
  let createExistsMatcher =
    createMatcher(
      ({formatReceived, formatExpected}, actualThunk, expectedThunk) => {
      let actual = actualThunk();
      let actualExists = Sys.file_exists(Fp.toString(actual));

      if (!actualExists) {
        let failureMessage =
          String.concat(
            "",
            ["Expected ", formatExpected(Fp.toString(actual)), " to exist"],
          );
        (() => failureMessage, false);
      } else {
        pass;
      };
    });
  let createStartsWithMatcher =
    createMatcher(
      ({formatReceived, formatExpected}, actualThunk, expectedThunk) => {
      let actual = actualThunk();
      let expected = expectedThunk();

      let ic = open_in_bin(Fp.toString(actual));
      let expectedLen = Bytes.length(expected);
      let buf = Bytes.make(expectedLen, '\000');
      really_input(ic, buf, 0, expectedLen);
      close_in(ic);

      let actualStartsWith = buf == expected;

      if (!actualStartsWith) {
        let failureMessage =
          String.concat(
            "",
            [
              "Expected ",
              Fp.toString(actual),
              " to start with bytes ",
              formatExpected(Printf.sprintf("%a", print_bytes, expected)),
              "\nReceived ",
              formatReceived(Printf.sprintf("%a", print_bytes, buf)),
            ],
          );
        (() => failureMessage, false);
      } else {
        pass;
      };
    });

  {
    toExist: () => createExistsMatcher(() => actual, () => ()),
    toStartWith: expected =>
      createStartsWithMatcher(() => actual, () => expected),
  };
};
