open Grain_utils;

open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("utils/markdown", ({test}) => {
  test("bold_escape_leading_star", ({expect}) => {
    expect.string(Markdown.bold("*")).toEqual({|**\***|})
  });

  test("bold_escape_many_leading_stars", ({expect}) => {
    expect.string(Markdown.bold("*****")).toEqual({|**\*\*\*\*\***|})
  });

  // This is how an operator is passed
  test("bold_no_escape_one_nonleading_star", ({expect}) => {
    expect.string(Markdown.bold("(*)")).toEqual({|**(*)**|})
  });

  // This is the pow operator
  test("bold_escape_two_nonleading_star", ({expect}) => {
    expect.string(Markdown.bold("(**)")).toEqual({|**(\*\*)**|})
  });

  test("bold_escape_many_nonleading_star", ({expect}) => {
    expect.string(Markdown.bold("(******)")).toEqual({|**(\*\*\*\*\*\*)**|})
  });

  test("bold_escape_trailing_star", ({expect}) => {
    expect.string(Markdown.bold("foo*")).toEqual({|**foo\***|})
  });

  test("bold_escape_many_trailing_star", ({expect}) => {
    expect.string(Markdown.bold("foo*****")).toEqual({|**foo\*\*\*\*\***|})
  });

  test("bold_escape_many_leading_and_trailing_star", ({expect}) => {
    expect.string(Markdown.bold("*****foo*****")).toEqual(
      {|**\*\*\*\*\*foo\*\*\*\*\***|},
    )
  });
});
