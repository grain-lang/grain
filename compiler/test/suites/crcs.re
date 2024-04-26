open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_utils;

describe("cyclic redundancy checks", ({test}) => {
  let assertMatchingCRCs = (~config_fn=?, name, program_str1, program_str2) => {
    test(
      name,
      ({expect}) => {
        let prog1 =
          compile(
            ~config_fn?,
            ~hook=Grain.Compile.stop_after_typed,
            name,
            program_str1,
          );
        let prog2 =
          compile(
            ~config_fn?,
            ~hook=Grain.Compile.stop_after_typed,
            name,
            program_str2,
          );

        let crc1 =
          switch (prog1.cstate_desc) {
          | TypeChecked(typed) => Digest.to_hex(typed.signature.cmi_crc)
          | _ => failwith("impossible")
          };
        let crc2 =
          switch (prog2.cstate_desc) {
          | TypeChecked(typed) => Digest.to_hex(typed.signature.cmi_crc)
          | _ => failwith("impossible")
          };
        expect.string(crc1).toEqual(crc2);
      },
    );
  };

  assertMatchingCRCs("test_empty_modules", "module Main", "module Main");
  assertMatchingCRCs(
    "test_same_module",
    {|
      module Main

      provide let foo = 5
      provide let bar = "string"

      provide enum Foo {
        Bar(String),
        Qux{num: Number}
      }

      provide module Bar {
        provide let baz = Bar("bar")
      }
    |},
    {|
      module Main

      provide let foo = 5
      provide let bar = "string"

      provide enum Foo {
        Bar(String),
        Qux{num: Number}
      }

      provide module Bar {
        provide let baz = Bar("bar")
      }
    |},
  );
  assertMatchingCRCs(
    "test_different_module_same_interface",
    {|
      module Main

      provide let foo = 5
      provide let bar = "string"

      provide enum Foo {
        Bar(String),
        Qux{num: Number}
      }

      provide module Bar {
        provide let baz = Bar("bar")
      }
    |},
    {|
      module Main

      provide let foo = 17
      provide let bar = "different string"

      provide enum Foo {
        Bar(String),
        Qux{num: Number}
      }

      provide module Bar {
        provide let baz = Qux{num: 6}
      }
    |},
  );
});
