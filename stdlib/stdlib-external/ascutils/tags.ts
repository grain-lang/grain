export const GRAIN_NUMBER_TAG_TYPE: i32       = 0b0000;
export const GRAIN_CONST_TAG_TYPE: i32        = 0b1111;
export const GRAIN_TUPLE_TAG_TYPE: i32        = 0b0001;
export const GRAIN_LAMBDA_TAG_TYPE: i32       = 0b0101;
export const GRAIN_GENERIC_HEAP_TAG_TYPE: i32 = 0b0011;

export const GRAIN_NUMBER_TAG_MASK: i32 = 0b0001;
export const GRAIN_GENERIC_TAG_MASK: i32 = 0b0111;

export const GRAIN_STRING_HEAP_TAG: i32 = 1;
export const GRAIN_DOM_ELEM_TAG: i32 = 2;
export const GRAIN_ADT_HEAP_TAG: i32 = 3;
export const GRAIN_RECORD_HEAP_TAG: i32 = 4;
export const GRAIN_ARRAY_HEAP_TAG: i32 = 5;
export const GRAIN_INT32_HEAP_TAG: i32 = 6;
export const GRAIN_INT64_HEAP_TAG: i32 = 7;

export const GRAIN_FILE_DESCRIPTOR_TYPE_ID: i32 = 9;