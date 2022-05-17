open Binaryen;

let optimize: (~optimize_level: int=?, ~shrink_level: int=?, Module.t) => unit;
