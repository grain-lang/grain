/** Runtime Error definitions */
open Sexplib.Conv;

[@deriving sexp]
type grain_error =
  | IndexOutOfBounds
  | MatchFailure
  | AssertionError;
