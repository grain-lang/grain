export const GRAIN_NUMBER_TAG_TYPE: i32       = 0b0001;
export const GRAIN_CONST_TAG_TYPE: i32        = 0b0110;
export const GRAIN_GENERIC_HEAP_TAG_TYPE: i32 = 0b0000;

export const GRAIN_NUMBER_TAG_MASK: i32 = 0b0001;
export const GRAIN_GENERIC_TAG_MASK: i32 = 0b0111;

export const GRAIN_STRING_HEAP_TAG: i32 = 1;
export const GRAIN_CHAR_HEAP_TAG: i32 = 2;
export const GRAIN_ADT_HEAP_TAG: i32 = 3;
export const GRAIN_RECORD_HEAP_TAG: i32 = 4;
export const GRAIN_ARRAY_HEAP_TAG: i32 = 5;
export const GRAIN_BOXED_NUM_HEAP_TAG: i32 = 6;
export const GRAIN_LAMBDA_HEAP_TAG: i32 = 7;
export const GRAIN_TUPLE_HEAP_TAG: i32 = 8;

export const GRAIN_FILE_DESCRIPTOR_TYPE_ID: i32 = 9;

// Boxed number types
export const GRAIN_FLOAT32_BOXED_NUM_TAG = 1;
export const GRAIN_FLOAT64_BOXED_NUM_TAG = 2;
export const GRAIN_INT32_BOXED_NUM_TAG = 3;
export const GRAIN_INT64_BOXED_NUM_TAG = 4;
export const GRAIN_RATIONAL_BOXED_NUM_TAG = 5;
