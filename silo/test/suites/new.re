open Silo_tests.TestFramework;
open Silo_tests.BinaryFileExtensions;
open Silo_tests.Runner;

type custom_matchers = {
  binaryFile: Fp.t(Fp.absolute) => binaryFileExtensions,
};

let customMatchers = createMatcher => {
  binaryFile: file => binaryFileExtensions(file, createMatcher),
};

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("new", ({test}) => {
  test("hello", ({expect}) => {
    let dir = test_output_dir;
    let hello_dir = Fp.At.(test_output_dir / "hello");

    let cmd = [|"silo", "new", "hello"|];

    let (code, out, err) = open_process(~dir, cmd);

    expect.int(code).toBe(0);
    expect.string(err).toBeEmpty();
    expect.file(Fp.toString(Fp.At.(hello_dir / "silo.toml"))).toEqual(
      "[package]\nname = \"hello\"",
    );
    expect.file(Fp.toString(Fp.At.(hello_dir / "src" / "main.gr"))).toEqual(
      "module Main\n\nprint(\"Hello, world!\")",
    );
  });
  test("hello_nested", ({expect}) => {
    let dir = test_output_dir;
    let hello_dir = Fp.At.(test_output_dir / "foo" / "bar" / "hello");

    let cmd = [|"silo", "new", "foo/bar/hello"|];

    let (code, out, err) = open_process(~dir, cmd);

    expect.int(code).toBe(0);
    expect.string(err).toBeEmpty();
    expect.file(Fp.toString(Fp.At.(hello_dir / "silo.toml"))).toEqual(
      "[package]\nname = \"hello\"",
    );
    expect.file(Fp.toString(Fp.At.(hello_dir / "src" / "main.gr"))).toEqual(
      "module Main\n\nprint(\"Hello, world!\")",
    );
  });
  test("name", ({expect}) => {
    let dir = test_output_dir;
    let hello_dir = Fp.At.(test_output_dir / "name");

    let cmd = [|"silo", "new", "--name", "foo", "name"|];

    let (code, out, err) = open_process(~dir, cmd);

    expect.int(code).toBe(0);
    expect.string(err).toBeEmpty();
    expect.file(Fp.toString(Fp.At.(hello_dir / "silo.toml"))).toEqual(
      "[package]\nname = \"foo\"",
    );
    expect.file(Fp.toString(Fp.At.(hello_dir / "src" / "main.gr"))).toEqual(
      "module Main\n\nprint(\"Hello, world!\")",
    );
  });
});
