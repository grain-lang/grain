(** Runtime Error definitions *)

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
  | SetItemNotTuple
  | SetItemIndexNotNumber
  | SetItemIndexTooSmall
  | SetItemIndexTooLarge
  | SwitchError
  | GenericNumberError
[@@deriving sexp]

val all_grain_errors : grain_error list

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
val err_SWITCH                    : int
val err_GENERIC_NUM               : int

val code_of_error : grain_error -> int

val label_of_error : grain_error -> string

val arity_of_error : grain_error -> int

val validate_args : grain_error -> 'a list -> unit

(** Pads the given argument list to the maximum error arity
    using the given item. *)
val pad_args : 'a -> 'a list -> 'a list

val error_of_code : int -> grain_error

val max_arity : int
