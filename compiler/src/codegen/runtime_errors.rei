/** Runtime Error definitions */;

[@deriving sexp]
type grain_error =
  | IndexOutOfBounds
  | IndexNonInteger
  | MatchFailure;
