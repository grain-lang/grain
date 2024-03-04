open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("wasi args and env", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRun = makeRunner(test_or_skip);

  let print_wasi_info = {|
  from "wasi/process" include Process
  from "array" include Array
  from "string" include String

  match (Process.argv()) {
    Ok(args) => print(Array.slice(1, args)),
    _ => print("Error reading args")
  }
  match (Process.env()) {
    Ok(env) => print(env),
    _ => print("Error reading env")
  }
  |};

  assertRun(
    ~extra_args=[|"--", "a", "b"|],
    "print_args1",
    print_wasi_info,
    "[> \"a\", \"b\"]\n[> ]\n",
  );
  assertRun(
    ~extra_args=[|"a", "b"|],
    "print_args2",
    print_wasi_info,
    "[> ]\n[> ]\n",
  );
  assertRun(
    ~extra_args=[|"--env=FOO=bar", "a", "b"|],
    "print_args3",
    print_wasi_info,
    "[> ]\n[> \"FOO=bar\"]\n",
  );
  assertRun(
    ~extra_args=[|"--env", "FOO=bar", "BAR=baz", "BAZ"|],
    "print_args4",
    print_wasi_info,
    "[> ]\n[> \"FOO=bar\", \"BAR=baz\", \"BAZ=\"]\n",
  );
  assertRun(
    ~extra_args=[|"--env", "FOO=bar", "--", "a", "b"|],
    "print_args5",
    print_wasi_info,
    "[> \"a\", \"b\"]\n[> \"FOO=bar\"]\n",
  );
  assertRun(
    ~extra_args=[|"--", "a", "b", "--env", "FOO=bar"|],
    "print_args6",
    print_wasi_info,
    "[> \"a\", \"b\", \"--env\", \"FOO=bar\"]\n[> ]\n",
  );
});
