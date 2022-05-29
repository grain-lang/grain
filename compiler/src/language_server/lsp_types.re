open Grain_typed;
open Sourcetree;

type code = {
  program: Typedtree.typed_program,
  sourcetree: Sourcetree.sourcetree,
  dirty: bool,
};
