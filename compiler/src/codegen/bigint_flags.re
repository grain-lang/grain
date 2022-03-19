open Sexplib.Conv;

[@deriving sexp]
type t =
  | BigIntNegative;

let to_int32 = n =>
  switch (n) {
  | BigIntNegative => 1l
  };

let all_to_int32 =
  List.fold_left((acc, cur) => Int32.logor(acc, to_int32(cur)), 0l);
