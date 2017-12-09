(** Runtime Error definitions *)

type snek_error =
  | ComparisonError
  | ArithmeticError
  | LogicError
  | IfError
  | OverflowError
  | ArityMismatch
  | CalledNonFunction
  | GetItemNotTuple
  | GetItemIndexNotNumber
  | GetItemIndexTooSmall
  | GetItemIndexTooLarge
  | SetItemNotTuple
  | SetItemIndexNotNumber
  | SetItemIndexTooSmall
  | SetItemIndexTooLarge
  | GenericNumberError

val all_snek_errors : snek_error list

val err_COMP_NOT_NUM              : int
val err_ARITH_NOT_NUM             : int
val err_LOGIC_NOT_BOOL            : int
val err_IF_NOT_BOOL               : int
val err_OVERFLOW                  : int
val err_GET_NOT_TUP               : int
val err_GET_ITEM_INDEX_NOT_NUMBER : int
val err_GET_ITEM_INDEX_TOO_SMALL  : int
val err_GET_ITEM_INDEX_TOO_LARGE  : int
val err_CALLED_NON_FUNCTION       : int
val err_ARITY_MISMATCH            : int
val err_SET_NOT_TUP               : int
val err_SET_ITEM_INDEX_NOT_NUMBER : int
val err_SET_ITEM_INDEX_TOO_SMALL  : int
val err_SET_ITEM_INDEX_TOO_LARGE  : int
val err_GENERIC_NUM               : int

val code_of_error : snek_error -> int

val label_of_error : snek_error -> string

val error_of_code : int -> snek_error
