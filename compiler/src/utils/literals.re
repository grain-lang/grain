// Simple numbers span the 31-bit range as we reserve one tag bit
let simple_number_max = 1073741824L;
let simple_number_min = (-1073741824L);

let conv_number_int = s => {
  Int64.of_string_opt(s);
};

let conv_number_float = s => {
  Float.of_string_opt(s);
};

let conv_int32 = s => {
  Int32.of_string_opt(s);
};

let conv_int64 = s => {
  Int64.of_string_opt(s);
};

let conv_float32 = s => {
  Float.of_string_opt(s);
};

let conv_float64 = s => {
  Float.of_string_opt(s);
};
