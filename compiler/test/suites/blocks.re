open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;
open Grain_utils.Warnings;

describe("blocks", ({test}) => {
  let assertParse = makeParseRunner(test);
  Grain_parsing.(
    Ast_helper.(
      assertParse(
        "block_parse_lone_no_args_enum",
        "module Test; { Foo }",
        {
          attributes: [],
          module_name: Location.mknoloc("Test"),
          statements: [
            Toplevel.expr(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Expression.block(
                ~loc=Location.dummy_loc,
                ~core_loc=Location.dummy_loc,
                [
                  Expression.singleton_construct(
                    ~loc=Location.dummy_loc,
                    ~core_loc=Location.dummy_loc,
                    Location.mknoloc(
                      Identifier.IdentName(Location.mknoloc("Foo")),
                    ),
                  ),
                ],
              ),
            ),
          ],
          comments: [],
          prog_loc: Location.dummy_loc,
          prog_core_loc: Location.dummy_loc,
        },
      )
    )
  );
});
