/** Runtime value tag information */
open Sexplib.Conv;

[@deriving sexp]
type heap_tag_type =
  | StringType
  | ADTType
  | RecordType
  | ArrayType
  | BoxedNumberType
  | LambdaType
  | TupleType
  | BytesType
  | Int32Type
  | Float32Type
  | Uint32Type
  | Uint64Type;

let tag_val_of_heap_tag_type =
  fun
  | StringType => 1
  | ADTType => 2
  | RecordType => 3
  | ArrayType => 4
  | BoxedNumberType => 5
  | LambdaType => 6
  | TupleType => 7
  | BytesType => 8
  | Int32Type => 9
  | Float32Type => 10
  | Uint32Type => 11
  | Uint64Type => 12;

let heap_tag_type_of_tag_val =
  fun
  | 1 => StringType
  | 2 => ADTType
  | 3 => RecordType
  | 4 => ArrayType
  | 5 => BoxedNumberType
  | 6 => LambdaType
  | 7 => TupleType
  | 8 => BytesType
  | 9 => Int32Type
  | 10 => Float32Type
  | 11 => Uint32Type
  | 12 => Uint64Type
  | x => failwith(Printf.sprintf("Unknown tag type: %d", x));

[@deriving sexp]
type boxed_number_tag_type =
  | BoxedInt64
  | BoxedRational
  | BoxedFloat64
  | BoxedBigInt;

let tag_val_of_boxed_number_tag_type =
  fun
  | BoxedFloat64 => 1
  | BoxedInt64 => 2
  | BoxedRational => 3
  | BoxedBigInt => 4;

let boxed_number_tag_type_of_tag_val =
  fun
  | 1 => BoxedFloat64
  | 2 => BoxedInt64
  | 3 => BoxedRational
  | 4 => BoxedBigInt
  | x => failwith(Printf.sprintf("Unknown boxed num tag type: %d", x));

[@deriving sexp]
type tag_type =
  | NumberTagType
  | ConstTagType
  | ShortValTagType
  | GenericHeapType(option(heap_tag_type));
