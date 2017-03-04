(** Runtime value tag information *)

type tag_type =
  | NumberTagType
  | BooleanTagType
  | TupleTagType
  | LambdaTagType
  | GenericHeapType

let and_mask_of_tag_type = function
  | NumberTagType   -> 0b0001
  | BooleanTagType  -> 0b1111
  | TupleTagType    -> 0b0111
  | LambdaTagType   -> 0b0111
  | GenericHeapType -> 0b0111

let tag_val_of_tag_type = function
  | NumberTagType   -> 0b0000
  | BooleanTagType  -> 0b1111
  | TupleTagType    -> 0b0001
  | LambdaTagType   -> 0b0101
  | GenericHeapType -> 0b0011


type heap_tag_type =
  | StringType

let tag_val_of_heap_tag_type = function
  | StringType -> 1

let heap_tag_type_of_tag_val = function
  | x when x = 1 -> StringType
  | x -> failwith (Printf.sprintf "Unknown tag type: %d" x)

let shift_amount_of_tag_type tt =
  31 - (tag_val_of_tag_type tt)
