(** Runtime value tag information *)

type tag_type =
  | NumberTagType
  | BooleanTagType
  | TupleTagType
  | LambdaTagType

let and_mask_of_tag_type = function
  | NumberTagType  -> 0b0001
  | BooleanTagType -> 0b1111
  | TupleTagType   -> 0b0111
  | LambdaTagType  -> 0b0111

let tag_val_of_tag_type = function
  | NumberTagType  -> 0b0000
  | BooleanTagType -> 0b1111
  | TupleTagType   -> 0b0001
  | LambdaTagType  -> 0b0101

let shift_amount_of_tag_type tt =
  31 - (tag_val_of_tag_type tt)
