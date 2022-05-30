open Grain_utils;

open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("utils/string_utils", ({describe}) => {
  describe("slice", ({test}) => {
    test("empty string", ({expect}) => {
      expect.string(String_utils.slice("")).toEqual("")
    });
    test("no first/last", ({expect}) => {
      expect.string(String_utils.slice("abc")).toEqual("abc")
    });

    test("last: positive index within string", ({expect}) => {
      expect.string(String_utils.slice(~last=1, "abc")).toEqual("a")
    });

    test("last: string length", ({expect}) => {
      let str = "abc";
      expect.string(String_utils.slice(~last=String.length(str), str)).
        toEqual(
        "abc",
      );
    });

    test("last: greater than length", ({expect}) => {
      expect.string(String_utils.slice(~last=5, "abc")).toEqual("abc")
    });

    test("last: negative index (wraps)", ({expect}) => {
      expect.string(String_utils.slice(~last=-1, "abc")).toEqual("ab")
    });

    test("first: positive index within string", ({expect}) => {
      expect.string(String_utils.slice(~first=1, "abc")).toEqual("bc")
    });

    test("first: string length", ({expect}) => {
      let str = "abc";
      expect.string(String_utils.slice(~first=String.length(str), str)).
        toEqual(
        "",
      );
    });

    test("first: greater than length", ({expect}) => {
      expect.string(String_utils.slice(~first=4, "abc")).toEqual("")
    });

    test("first: negative index (wraps)", ({expect}) => {
      expect.string(String_utils.slice(~first=-1, "abc")).toEqual("c")
    });

    test("first + last: positive indexes within string", ({expect}) => {
      expect.string(String_utils.slice(~first=1, ~last=2, "abc")).toEqual(
        "b",
      )
    });

    test("first + last: overlapping indexes", ({expect}) => {
      expect.string(String_utils.slice(~first=1, ~last=1, "abc")).toEqual(
        "",
      )
    });

    test("first + last: wrapping first with appropriate last", ({expect}) => {
      let str = "abc";
      expect.string(
        String_utils.slice(~first=-1, ~last=String.length(str), str),
      ).
        toEqual(
        "c",
      );
    });

    test("first + last: incorrect wrapping", ({expect}) => {
      expect.string(String_utils.slice(~first=-1, ~last=1, "abc")).toEqual(
        "",
      )
    });

    test("first + last: wrapping in last", ({expect}) => {
      expect.string(String_utils.slice(~first=1, ~last=-1, "abc")).toEqual(
        "b",
      )
    });

    test("first + last: greater than length", ({expect}) => {
      expect.string(String_utils.slice(~first=1, ~last=5, "abc")).toEqual(
        "bc",
      )
    });

    test("first + last: both negative", ({expect}) => {
      expect.string(String_utils.slice(~first=-2, ~last=-1, "abc")).toEqual(
        "b",
      )
    });
  })
});
