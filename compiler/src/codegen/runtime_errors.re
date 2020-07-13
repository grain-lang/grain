/** Runtime Error definitions */
open Sexplib.Conv;

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

let all_grain_errors = [
  ComparisonError,
  ArithmeticError,
  DivisionByZeroError,
  ModuloByZeroError,
  LogicError,
  IfError,
  OverflowError,
  ArityMismatch,
  CalledNonFunction,
  GetItemNotTuple,
  GetItemIndexNotNumber,
  GetItemIndexTooSmall,
  GetItemIndexTooLarge,
  ArrayIndexOutOfBounds,
  SetItemNotTuple,
  SetItemIndexNotNumber,
  SetItemIndexTooSmall,
  SetItemIndexTooLarge,
  SwitchError,
  InvalidArgument,
  AssertionError,
  Failure,
  SystemError,
  GenericNumberError,
];

let err_COMP_NOT_NUM = 0;
let err_ARITH_NOT_NUM = 1;
let err_LOGIC_NOT_BOOL = 2;
let err_IF_NOT_BOOL = 3;
let err_OVERFLOW = 4;
let err_GET_NOT_TUP = 5;
let err_GET_ITEM_INDEX_NOT_NUMBER = 6;
let err_GET_ITEM_INDEX_TOO_SMALL = 7;
let err_GET_ITEM_INDEX_TOO_LARGE = 8;
let err_CALLED_NON_FUNCTION = 9;
let err_ARITY_MISMATCH = 10;
let err_SET_NOT_TUP = 12;
let err_SET_ITEM_INDEX_NOT_NUMBER = 13;
let err_SET_ITEM_INDEX_TOO_SMALL = 14;
let err_SET_ITEM_INDEX_TOO_LARGE = 15;
let err_SWITCH = 16;
let err_DIVISION_BY_ZERO = 17;
let err_MODULO_BY_ZERO = 18;
let err_ARRAY_INDEX_OUT_OF_BOUNDS = 19;
let err_INVALID_ARGUMENT = 20;
let err_ASSERTION_ERROR = 21;
let err_FAILURE = 22;
let err_SYSTEM_ERROR = 23;
let err_GENERIC_NUM = 99;

let code_of_error =
  fun
  | ArithmeticError => err_ARITH_NOT_NUM
  | DivisionByZeroError => err_DIVISION_BY_ZERO
  | ModuloByZeroError => err_MODULO_BY_ZERO
  | ComparisonError => err_COMP_NOT_NUM
  | IfError => err_IF_NOT_BOOL
  | LogicError => err_LOGIC_NOT_BOOL
  | ArityMismatch => err_ARITY_MISMATCH
  | CalledNonFunction => err_CALLED_NON_FUNCTION
  | GetItemNotTuple => err_GET_NOT_TUP
  | GetItemIndexNotNumber => err_GET_ITEM_INDEX_NOT_NUMBER
  | GetItemIndexTooSmall => err_GET_ITEM_INDEX_TOO_SMALL
  | GetItemIndexTooLarge => err_GET_ITEM_INDEX_TOO_LARGE
  | ArrayIndexOutOfBounds => err_ARRAY_INDEX_OUT_OF_BOUNDS
  | SetItemNotTuple => err_SET_NOT_TUP
  | SetItemIndexNotNumber => err_SET_ITEM_INDEX_NOT_NUMBER
  | SetItemIndexTooLarge => err_SET_ITEM_INDEX_TOO_LARGE
  | SetItemIndexTooSmall => err_SET_ITEM_INDEX_TOO_SMALL
  | SwitchError => err_SWITCH
  | GenericNumberError => err_GENERIC_NUM
  | OverflowError => err_OVERFLOW
  | InvalidArgument => err_INVALID_ARGUMENT
  | AssertionError => err_ASSERTION_ERROR
  | Failure => err_FAILURE
  | SystemError => err_SYSTEM_ERROR;

let arity_of_error =
  fun
  | ArithmeticError => 1
  | DivisionByZeroError => 0
  | ModuloByZeroError => 0
  | ComparisonError => 1
  | IfError => 1
  | LogicError => 1
  | ArityMismatch => 2
  | CalledNonFunction => 1
  | GetItemNotTuple => 1
  | GetItemIndexNotNumber => 1
  | GetItemIndexTooSmall => 2
  | GetItemIndexTooLarge => 2
  | ArrayIndexOutOfBounds => 0
  | SetItemNotTuple => 1
  | SetItemIndexNotNumber => 1
  | SetItemIndexTooLarge => 2
  | SetItemIndexTooSmall => 2
  | SwitchError => 1
  | GenericNumberError => 1
  | OverflowError => 1
  | InvalidArgument => 1
  | AssertionError => 0
  | Failure => 1
  | SystemError => 1;

let label_of_error =
  fun
  | ArithmeticError => "error_not_number_arith"
  | DivisionByZeroError => "error_division_by_zero"
  | ModuloByZeroError => "error_modulo_by_zero"
  | ComparisonError => "error_not_number_comp"
  | IfError => "error_not_boolean_if"
  | LogicError => "error_not_boolean_logic"
  | ArityMismatch => "error_arity_mismatch"
  | CalledNonFunction => "error_called_non_function"
  | GenericNumberError => "error_not_number_generic"
  | GetItemNotTuple => "error_not_tuple_get_item"
  | GetItemIndexNotNumber => "error_not_number_get_item_idx"
  | GetItemIndexTooSmall => "error_too_small_get_item_idx"
  | GetItemIndexTooLarge => "error_too_large_get_item_idx"
  | ArrayIndexOutOfBounds => "error_array_index_out_of_bounds"
  | SetItemNotTuple => "error_not_tuple_set_item"
  | SetItemIndexNotNumber => "error_not_number_set_item_idx"
  | SetItemIndexTooSmall => "error_too_small_set_item_idx"
  | SetItemIndexTooLarge => "error_too_large_set_item_idx"
  | OverflowError => "error_overflow"
  | SwitchError => "error_switch"
  | InvalidArgument => "error_invalid_argument"
  | AssertionError => "error_assertion"
  | Failure => "error_failure"
  | SystemError => "error_system";

let error_of_code = c =>
  switch (c) {
  | x when x == err_ARITH_NOT_NUM => ArithmeticError
  | x when x == err_DIVISION_BY_ZERO => DivisionByZeroError
  | x when x == err_MODULO_BY_ZERO => ModuloByZeroError
  | x when x == err_COMP_NOT_NUM => ComparisonError
  | x when x == err_IF_NOT_BOOL => IfError
  | x when x == err_LOGIC_NOT_BOOL => LogicError
  | x when x == err_ARITY_MISMATCH => ArityMismatch
  | x when x == err_CALLED_NON_FUNCTION => CalledNonFunction
  | x when x == err_GET_NOT_TUP => GetItemNotTuple
  | x when x == err_GET_ITEM_INDEX_NOT_NUMBER => GetItemIndexNotNumber
  | x when x == err_GET_ITEM_INDEX_TOO_SMALL => GetItemIndexTooSmall
  | x when x == err_GET_ITEM_INDEX_TOO_LARGE => GetItemIndexTooLarge
  | x when x == err_ARRAY_INDEX_OUT_OF_BOUNDS => ArrayIndexOutOfBounds
  | x when x == err_SET_NOT_TUP => SetItemNotTuple
  | x when x == err_SET_ITEM_INDEX_NOT_NUMBER => SetItemIndexNotNumber
  | x when x == err_SET_ITEM_INDEX_TOO_LARGE => SetItemIndexTooLarge
  | x when x == err_SET_ITEM_INDEX_TOO_SMALL => SetItemIndexTooSmall
  | x when x == err_SWITCH => SwitchError
  | x when x == err_GENERIC_NUM => GenericNumberError
  | x when x == err_OVERFLOW => OverflowError
  | x when x == err_INVALID_ARGUMENT => InvalidArgument
  | x when x == err_ASSERTION_ERROR => AssertionError
  | x when x == err_FAILURE => Failure
  | x when x == err_SYSTEM_ERROR => SystemError
  | c => failwith(Printf.sprintf("Unknown error code: %d", c))
  };

let max_arity =
  List.fold_left((x, y) => max(x, arity_of_error(y)), 0, all_grain_errors);

let pad_args: 'a. ('a, list('a)) => list('a) =
  (pad_elt, args) => {
    let pad_amount = max_arity - List.length(args);
    if (pad_amount == 0) {
      args;
    } else {
      args @ List.init(pad_amount, _ => pad_elt);
    };
  };

let validate_args: 'a. (grain_error, list('a)) => unit =
  (error, args) => {
    let arity = arity_of_error(error);
    let num_args = List.length(args);
    if (num_args != arity) {
      failwith(
        Printf.sprintf(
          "Internal error (runtime_errors): Error %s expects %d arguments; generated code calls with %d.",
          Sexplib.Sexp.to_string_hum(sexp_of_grain_error(error)),
          arity,
          num_args,
        ),
      );
    };
  };
