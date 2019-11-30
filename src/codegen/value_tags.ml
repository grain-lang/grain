(** Runtime value tag information *)
open Sexplib.Conv

type heap_tag_type =
  | StringType
  | DOMType
  | ADTType
  | RecordType
[@@deriving sexp]

let tag_val_of_heap_tag_type = function
  | StringType -> 1
  | DOMType -> 2
  | ADTType -> 3
  | RecordType -> 4

let heap_tag_type_of_tag_val = function
  | x when x = 1 -> StringType
  | x when x = 2 -> DOMType
  | x when x = 3 -> ADTType
  | x when x = 4 -> RecordType
  | x -> failwith (Printf.sprintf "Unknown tag type: %d" x)


type tag_type =
  | NumberTagType
  | ConstTagType
  | TupleTagType
  | LambdaTagType
  | GenericHeapType of heap_tag_type option
[@@deriving sexp]

let and_mask_of_tag_type = function
  | NumberTagType     -> 0b0001
  | ConstTagType      -> 0b1111
  | TupleTagType      -> 0b0111
  | LambdaTagType     -> 0b0111
  | GenericHeapType _ -> 0b0111

let tag_val_of_tag_type = function
  | NumberTagType     -> 0b0000
  | ConstTagType      -> 0b1111
  | TupleTagType      -> 0b0001
  | LambdaTagType     -> 0b0101
  | GenericHeapType _ -> 0b0011

let shift_amount_of_tag_type tt =
  31 - (tag_val_of_tag_type tt)
