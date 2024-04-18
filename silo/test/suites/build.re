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

describe("build", ({test}) => {
  test("minimal", ({expect}) => {
    let dir = Fp.At.(test_input_dir / "minimal");
    let output = Fp.At.(dir / "minimal.wasm");

    let cmd = [|"silo", "build"|];

    let (code, out, err) = open_process(~dir, cmd);

    expect.int(code).toBe(0);
    expect.string(err).toBeEmpty();
    expect.string(out).toMatch("Finished");
    expect.ext.binaryFile(output).toExist();
    expect.ext.binaryFile(output).toStartWith(Bytes.of_string("\x00asm"));
  });
  test("path", ({expect}) => {
    let dir = Fp.At.(test_input_dir / "path");
    let output = Fp.At.(dir / "path.wasm");

    let cmd = [|"silo", "build"|];

    let (code, out, err) = open_process(~dir, cmd);

    expect.int(code).toBe(0);
    expect.string(err).toBeEmpty();
    expect.string(out).toMatch("Finished");
    expect.ext.binaryFile(output).toExist();
    expect.ext.binaryFile(output).toStartWith(Bytes.of_string("\x00asm"));
  });
});
