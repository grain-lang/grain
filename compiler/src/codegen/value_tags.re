/** Runtime value tag information */
open Sexplib.Conv;

[@deriving sexp]
type heap_tag_type =
  | StringType
  | ADTType
  | RecordType
  | ArrayType
  | BoxedNumberType;

let tag_val_of_heap_tag_type =
  fun
  | StringType => 1
  | ADTType => 3
  | RecordType => 4
  | ArrayType => 5
  | BoxedNumberType => 6;

let heap_tag_type_of_tag_val =
  fun
  | x when x == 1 => StringType
  | x when x == 3 => ADTType
  | x when x == 4 => RecordType
  | x when x == 5 => ArrayType
  | x when x == 6 => BoxedNumberType
  | x => failwith(Printf.sprintf("Unknown tag type: %d", x));

[@deriving sexp]
type boxed_number_tag_type =
  | BoxedInt32
  | BoxedInt64
  | BoxedRational
  | BoxedFloat32
  | BoxedFloat64;

let tag_val_of_boxed_number_tag_type =
  fun
  | BoxedFloat32 => 1
  | BoxedFloat64 => 2
  | BoxedInt32 => 3
  | BoxedInt64 => 4
  | BoxedRational => 5;

let boxed_number_tag_type_of_tag_val =
  fun
  | x when x == 1 => BoxedFloat32
  | x when x == 2 => BoxedFloat64
  | x when x == 3 => BoxedInt32
  | x when x == 4 => BoxedInt64
  | x when x == 5 => BoxedRational
  | x => failwith(Printf.sprintf("Unknown boxed num tag type: %d", x));

[@deriving sexp]
type tag_type =
  | NumberTagType
  | ConstTagType
  | TupleTagType
  | LambdaTagType
  | GenericHeapType(option(heap_tag_type));

let and_mask_of_tag_type =
  fun
  | NumberTagType => 0b0001
  | ConstTagType => 0b1111
  | TupleTagType => 0b0111
  | LambdaTagType => 0b0111
  | GenericHeapType(_) => 0b0111;

let tag_val_of_tag_type =
  fun
  | NumberTagType => 0b0000
  | ConstTagType => 0b1111
  | TupleTagType => 0b0001
  | LambdaTagType => 0b0101
  | GenericHeapType(_) => 0b0011;

let shift_amount_of_tag_type = tt => 31 - tag_val_of_tag_type(tt);
