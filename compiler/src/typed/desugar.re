module DesugarMapArgument = {
  include TypedtreeMap.DefaultMapArgument;
};

module DesugarMapper = TypedtreeMap.MakeMap(DesugarMapArgument);

let desugar = DesugarMapper.map_typed_program;
