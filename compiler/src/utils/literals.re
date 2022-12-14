// Simple numbers span the 31-bit range as we reserve one tag bit
let simple_number_max = 1073741823L;
let simple_number_min = (-1073741824L);

let conv_number_int = s => {
  switch (Int64.of_string_opt(s)) {
  | Some(n) =>
    // We may accidentally parse the number using its two's
    // compliment representation when it should be a bigint
    // We just return None in this case
    if (n < 0L && s.[0] != '-' || n > 0L && s.[0] == '-') {
      None;
    } else {
      Some(n);
    }
  | None => None
  };
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

let conv_wasmi32 = s => {
  Int32.of_string_opt(s);
};

let conv_wasmi64 = s => {
  Int64.of_string_opt(s);
};

let conv_wasmf32 = s => {
  Float.of_string_opt(s);
};

let conv_wasmf64 = s => {
  Float.of_string_opt(s);
};

let digit_value = c => {
  switch (c) {
  | '0' .. '9' => Char.code(c) - Char.code('0')
  | 'a' .. 'f' => 10 + (Char.code(c) - Char.code('a'))
  | 'A' .. 'F' => 10 + (Char.code(c) - Char.code('A'))
  | _ => failwith("Impossible: Bad char: " ++ String.make(1, c))
  };
};

let conv_bigint = s =>
  if (String.length(s) == 0) {
    None;
  } else {
    let neg = s.[0] == '-';
    let first = if (neg) {1} else {0};
    let (first, base) =
      // This function supports the 0u prefix for parity with Int64.of_string
      // Note that Grain doesn't support the 0u prefix (as of writing), so this should
      // never receive a string which starts with 0u, but we still include the support
      // so as to keep the semantics of all of our string->int conversion functions
      // synced up.
      if (String_utils.starts_with(~offset=first, s, "0u")
          || String_utils.starts_with(~offset=first, s, "0U")) {
        (first + 2, 10);
      } else if (String_utils.starts_with(~offset=first, s, "0b")
                 || String_utils.starts_with(~offset=first, s, "0B")) {
        (first + 2, 2);
      } else if (String_utils.starts_with(~offset=first, s, "0o")
                 || String_utils.starts_with(~offset=first, s, "0O")) {
        (first + 2, 8);
      } else if (String_utils.starts_with(~offset=first, s, "0x")
                 || String_utils.starts_with(~offset=first, s, "0X")) {
        (first + 2, 16);
      } else {
        (first, 10);
      };
    if (base == 2 || base == 8 || base == 16) {
      // easier case for bases which are powers of two
      let bits =
        if (base == 2) {
          1;
        } else if (base == 8) {
          3;
        } else {
          4;
        };
      let acc = ref(Int64.zero);
      let accBits = ref(0);
      let results = ref([]);
      for (i in String.length(s) - 1 downto first) {
        switch (s.[i]) {
        | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as digit =>
          let digit = Int64.of_int(digit_value(digit));
          acc := Int64.logor(acc^, Int64.shift_left(digit, accBits^));
          accBits := accBits^ + bits;
          if (accBits^ >= 64) {
            results := [acc^, ...results^];
            accBits := accBits^ - 64;
            acc := Int64.shift_right_logical(digit, bits - accBits^);
          };
        | '_' => ()
        | _ => failwith("Impossible: Bad char")
        };
      };
      if (Int64.unsigned_compare(acc^, Int64.zero) > 0) {
        results := [acc^, ...results^];
      };
      Some((neg, Array.of_list(List.rev(results^))));
    } else {
      // Base 10. Need to use the mini-bigint implementation
      // to properly convert to a power-of-two base.
      // [NOTE] We save some operations by reading 9 places at a time
      //        (signed i32 max is 10 digits in base 10)
      let places_at_a_time = 9;
      let rec compute_chunk_locations = (start, string_length, acc) => {
        let remaining_length = string_length - start;
        if (remaining_length <= places_at_a_time) {
          [(start, string_length), ...acc];
        } else {
          compute_chunk_locations(
            start,
            string_length - places_at_a_time,
            [(string_length - places_at_a_time, string_length), ...acc],
          );
        };
      };
      let get_chunk = ((start_idx, end_idx)) => {
        let result = ref(Int64.zero);
        let valid_digits = ref(0);
        for (i in start_idx to end_idx - 1) {
          let digit = s.[i];
          if (digit != '_') {
            incr(valid_digits);
            result :=
              Int64.add(
                Int64.mul(result^, Int64.of_int(base)),
                Int64.of_int(digit_value(s.[i])),
              );
          };
        };
        (result^, valid_digits^);
      };
      // factor == 10l ** (places_at_a_time)
      let get_factor = num_digits =>
        List.fold_left(
          Int32.mul,
          Int32.one,
          List.init(num_digits, n => Int32.of_int(base)),
        );
      let chunks =
        List.map(
          get_chunk,
          compute_chunk_locations(first, String.length(s), []),
        );
      let result =
        List.fold_left(
          (acc, (chunk, num_digits)) => {
            let ret =
              Mini_bigint.unsigned_add_i64(
                Mini_bigint.unsigned_mul_i32(acc, get_factor(num_digits)),
                chunk,
              );
            ret;
          },
          Mini_bigint.zero(),
          chunks,
        );
      Some((neg, result.limbs));
    };
  };
