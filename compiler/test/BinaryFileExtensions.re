open TestFramework;
open Rely.MatcherTypes;

type binaryFileExtensions = {toBinaryMatch: string => unit};
let passMessageThunk = () => "";

let binaryFileExtensions = (actual: string, {createMatcher}) => {
  let createBinaryFilesMatcher =
    createMatcher(
      (
        {matcherHint, formatReceived, formatExpected, indent, prepareDiff},
        actualThunk,
        expectedThunk,
      ) => {
      let filePath = expectedThunk();
      let expected_output = open_in_bin(filePath);
      let n = in_channel_length(expected_output);
      let expected_buffer = Buffer.create(n);
      let _ = Buffer.add_channel(expected_buffer, expected_output, n);
      let _ = close_in(expected_output);

      let expected_string =
        Bytes.to_string(Buffer.to_bytes(expected_buffer));

      let pass = expected_string == actual;

      if (!pass) {
        let message =
          String.concat(
            "",
            [
              "Byte level comparison",
              "\n\n",
              "Expected actual to equal the file: ",
              formatExpected(filePath),
              "\n",
              "Contents:\n",
              indent(formatExpected(expected_string)),
              "\n",
              "Received:\n",
              indent(formatReceived(actual)),
              "\n\nDifference:\n",
              indent(prepareDiff(actual, expected_string)),
            ],
          );
        (() => message, false);
      } else {
        (passMessageThunk, true);
      };
    });
  {
    toBinaryMatch: expected =>
      createBinaryFilesMatcher(() => "", () => expected),
  };
};
