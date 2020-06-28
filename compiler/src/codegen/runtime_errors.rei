/** Runtime Error definitions */;

[@deriving sexp]
type grain_error =
  | ComparisonError
  | ArithmeticError
  | DivisionByZeroError
  | ModuloByZeroError
  | LogicError
  | IfError
  | OverflowError
  | ArityMismatch
  | CalledNonFunction
  | GetItemNotTuple
  | GetItemIndexNotNumber
  | GetItemIndexTooSmall
  | GetItemIndexTooLarge
  | ArrayIndexOutOfBounds
  | SetItemNotTuple
  | SetItemIndexNotNumber
  | SetItemIndexTooSmall
  | SetItemIndexTooLarge
  | SwitchError
  | InvalidArgument
  | AssertionError
  | Failure
  | SystemError
  | GenericNumberError;

let all_grain_errors: list(grain_error);

let err_COMP_NOT_NUM: int;
let err_ARITH_NOT_NUM: int;
let err_LOGIC_NOT_BOOL: int;
let err_IF_NOT_BOOL: int;
let err_OVERFLOW: int;
let err_GET_NOT_TUP: int;
let err_GET_ITEM_INDEX_NOT_NUMBER: int;
let err_GET_ITEM_INDEX_TOO_SMALL: int;
let err_GET_ITEM_INDEX_TOO_LARGE: int;
let err_CALLED_NON_FUNCTION: int;
let err_ARITY_MISMATCH: int;
let err_SET_NOT_TUP: int;
let err_SET_ITEM_INDEX_NOT_NUMBER: int;
let err_SET_ITEM_INDEX_TOO_SMALL: int;
let err_SET_ITEM_INDEX_TOO_LARGE: int;
let err_SWITCH: int;
let err_GENERIC_NUM: int;

let code_of_error: grain_error => int;

let label_of_error: grain_error => string;

let arity_of_error: grain_error => int;

let validate_args: (grain_error, list('a)) => unit;

/** Pads the given argument list to the maximum error arity
    using the given item. */

let pad_args: ('a, list('a)) => list('a);

let error_of_code: int => grain_error;

let max_arity: int;
