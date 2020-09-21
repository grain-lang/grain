// Simple numbers span the 31-bit range as we reserve one tag bit
let simple_number_max = 1073741824L;
let simple_number_min = (-1073741824L);

let conv_number_int = s => {
  Int64.of_string_opt(s);
};

let conv_number_float = s => {
  Float.of_string_opt(s);
};

let conv_number_rational = (n, d) => {
  open Int32;
  // https://en.wikipedia.org/wiki/Binary_GCD_algorithm
  let rec gcd_help = (x, y) => {
    switch (x, y) {
    | (x, y) when x == y => y
    | (x, y) when x == 0l => y
    | (x, y) when y == 0l => x
    | (x, y) when logand(lognot(x), 1l) != 0l =>
      // x is even
      if (logand(y, 1l) != 0l) {
        // y is odd
        gcd_help(shift_right(x, 1), y);
      } else {
        shift_left(gcd_help(shift_right(x, 1), shift_right(y, 1)), 1);
      }
    | (x, y) when logand(lognot(y), 1l) != 0l =>
      // y is even and x is odd
      gcd_help(x, shift_right(y, 1))
    | (x, y) when x > y => gcd_help(sub(x, y), y)
    | _ => gcd_help(sub(y, x), x)
    };
  };
  // Algorithm above breaks on negatives, so
  // we make sure that they are positive at the beginning
  let gcd = (x, y) => gcd_help(abs(x), abs(y));

  let numerator = Int32.of_string_opt(n);
  let denominator = Int32.of_string_opt(d);
  switch (numerator, denominator) {
  | (Some(n), Some(d)) =>
    // Division by zero handled by well-formedness
    let factor = gcd(n, d);
    if (d < 0l) {
      // Never do negative/negative or negative denominator
      Some((
        div(neg(n), factor),
        div(neg(d), factor),
      ));
    } else {
      Some((div(n, factor), div(d, factor)));
    };
  | _ => None
  };
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
