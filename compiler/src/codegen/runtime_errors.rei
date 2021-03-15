/** Runtime Error definitions */;

[@deriving sexp]
type grain_error =
  | IndexOutOfBounds
  | MatchFailure
  | AssertionError;
