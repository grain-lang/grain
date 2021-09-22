open TestFramework;
open Rely.MatcherTypes;
open Grain_utils;

type warningExtensions = {
  toHaveTriggered: Warnings.t => unit,
  toHaveTriggeredNoWarnings: unit => unit,
};

let warningExtensions = ({createMatcher}) => {
  let pass = (() => "", true);
  let createTriggeredMatcher =
    createMatcher(
      ({formatReceived, formatExpected}, actualThunk, expectedThunk) => {
      let expected = expectedThunk();
      let generatedWarnings = Warnings.get_warnings();
      let warningTriggered =
        List.exists(((_, warn)) => warn == expected, generatedWarnings);

      if (!warningTriggered) {
        let receivedWarnings =
          List.length(generatedWarnings) == 0
            ? ["No warnings."]
            : List.map(
                ((_, warn)) => Warnings.message(warn),
                generatedWarnings,
              );
        let failureMessage =
          String.concat(
            "\n",
            [
              "Expected warning:",
              Warnings.message(expected),
              "\nReceived:",
              ...receivedWarnings,
            ],
          );
        (() => failureMessage, false);
      } else {
        pass;
      };
    });
  let createNotTriggeredMatcher =
    createMatcher(
      ({formatReceived, formatExpected}, actualThunk, expectedThunk) => {
      let generatedWarnings = Warnings.get_warnings();
      let warningTriggered = List.length(generatedWarnings) > 0;

      if (warningTriggered) {
        let receivedWarnings =
          List.map(
            ((_, warn)) => Warnings.message(warn),
            generatedWarnings,
          );
        let failureMessage =
          String.concat(
            "\n",
            ["Expected no warnings, received:", ...receivedWarnings],
          );
        (() => failureMessage, false);
      } else {
        pass;
      };
    });
  {
    toHaveTriggered: warn => createTriggeredMatcher(() => (), () => warn),
    toHaveTriggeredNoWarnings: () =>
      createNotTriggeredMatcher(() => (), () => ()),
  };
};
