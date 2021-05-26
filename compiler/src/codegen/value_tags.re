/** Runtime value tag information */
open Sexplib.Conv;

[@deriving sexp]
type heap_tag_type =
  | StringType
  | CharType
  | ADTType
  | RecordType
  | ArrayType
  | BoxedNumberType
  | LambdaType
  | TupleType
  | BytesType;

let tag_val_of_heap_tag_type =
  fun
  | StringType => 1
  | CharType => 2
  | ADTType => 3
  | RecordType => 4
  | ArrayType => 5
  | BoxedNumberType => 6
  | LambdaType => 7
  | TupleType => 8
  | BytesType => 9;

let heap_tag_type_of_tag_val =
  fun
  | 1 => StringType
  | 2 => CharType
  | 3 => ADTType
  | 4 => RecordType
  | 5 => ArrayType
  | 6 => BoxedNumberType
  | 7 => LambdaType
  | 8 => TupleType
  | 9 => BytesType
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
  | 1 => BoxedFloat32
  | 2 => BoxedFloat64
  | 3 => BoxedInt32
  | 4 => BoxedInt64
  | 5 => BoxedRational
  | x => failwith(Printf.sprintf("Unknown boxed num tag type: %d", x));

[@deriving sexp]
type tag_type =
  | NumberTagType
  | ConstTagType
  | GenericHeapType(option(heap_tag_type));

let and_mask_of_tag_type =
  fun
  | NumberTagType => 0b0001
  | ConstTagType => 0b0111
  | GenericHeapType(_) => 0b0111;

let tag_val_of_tag_type =
  fun
  | NumberTagType => 0b0001
  | ConstTagType => 0b0110
  | GenericHeapType(_) => 0b0000;

let shift_amount_of_tag_type = tt => 31 - tag_val_of_tag_type(tt);
