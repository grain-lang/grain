type t = {mutable limbs: array(int64)};

let zero: unit => t;
let of_int: int => t;
let unsigned_add_i64: (t, int64) => t;
let unsigned_mul_i32: (t, int32) => t;
