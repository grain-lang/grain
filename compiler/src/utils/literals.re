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

let int8_max = 127l;
let int8_min = (-128l);

let conv_int8 = s => {
  let non_decimal =
    String.length(s) > 2
    && List.mem(String.sub(s, 0, 2), ["0x", "0b", "0o"]);
  switch (Int32.of_string_opt(s)) {
  | None => None
  | Some(n) =>
    let (&) = Int32.logand;
    let (>>) = Int32.shift_right;
    let (<<) = Int32.shift_left;
    if (n > int8_max) {
      // Trick to allow setting the sign bit in representations like 0xFF
      if (non_decimal && (n & 0xFFFFFF80l) >> 7 == 1l) {
        Some(n << 24 >> 24);
      } else {
        None;
      };
    } else if (n < int8_min) {
      None;
    } else if (non_decimal && n < 0l) {
      None; // Reject something like 0xFFFFFFFFs
    } else {
      Some(n);
    };
  };
};

let int16_max = 32767l;
let int16_min = (-32768l);

let conv_int16 = s => {
  let non_decimal =
    String.length(s) > 2
    && List.mem(String.sub(s, 0, 2), ["0x", "0b", "0o"]);
  switch (Int32.of_string_opt(s)) {
  | None => None
  | Some(n) =>
    let (&) = Int32.logand;
    let (>>) = Int32.shift_right;
    let (<<) = Int32.shift_left;
    if (n > int16_max) {
      // Trick to allow setting the sign bit in representations like 0xFFFF
      if (non_decimal && (n & 0xFFFF8000l) >> 15 == 1l) {
        Some(n << 16 >> 16);
      } else {
        None;
      };
    } else if (n < int16_min) {
      None;
    } else if (non_decimal && n < 0l) {
      None; // Reject something like 0xFFFFFFFFS
    } else {
      Some(n);
    };
  };
};

let conv_int32 = s => {
  Int32.of_string_opt(s);
};

let conv_int64 = s => {
  Int64.of_string_opt(s);
};

let uint_format_str = s =>
  if (String.length(s) > 2) {
    let prefix = String.sub(s, 0, 2);
    if (prefix == "0b" || prefix == "0x" || prefix == "0o") {
      s;
    } else {
      "0u" ++ s;
    };
  } else {
    "0u" ++ s;
  };

let uint8_max = 255l;

let conv_uint8 = s => {
  switch (Int32.of_string_opt(s)) {
  | None => None
  | Some(n) =>
    if (n < 0l || n > uint8_max) {
      None;
    } else {
      Some(n);
    }
  };
};

let uint16_max = 65535l;

let conv_uint16 = s => {
  switch (Int32.of_string_opt(s)) {
  | None => None
  | Some(n) =>
    if (n < 0l || n > uint16_max) {
      None;
    } else {
      Some(n);
    }
  };
};

let conv_uint32 = s => {
  Int32.of_string_opt(uint_format_str(s));
};

let conv_uint64 = s => {
  Int64.of_string_opt(uint_format_str(s));
};

let get_neg_uint8_hex = n => {
  Printf.sprintf("%lx", Int32.logand(Int32.neg(n), 0xFFl));
};

let get_neg_uint16_hex = n => {
  Printf.sprintf("%lx", Int32.logand(Int32.neg(n), 0xFFFFl));
};

let get_neg_uint32_hex = n => {
  Printf.sprintf("%lx", Int32.neg(n));
};

let get_neg_uint64_hex = n => {
  Printf.sprintf("%Lx", Int64.neg(n));
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

exception IllegalUnicodeCodePoint(string);
exception IllegalByteStringUnicodeChar(string);
exception IllegalByteStringUnicodeEscape(string);

let newline_char = [%sedlex.regexp?
  0x0A | 0x0C | 0x0D | 0x85 | 0x2028 | 0x2029
];
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'];
let oct_digit = [%sedlex.regexp? '0' .. '7'];
let unicode_esc = [%sedlex.regexp? ("\\u{", Rep(hex_digit, 1 .. 6), "}")];
let unicode4_esc = [%sedlex.regexp? ("\\u", Rep(hex_digit, 4))];
let hex_esc = [%sedlex.regexp? ("\\x", Rep(hex_digit, 1 .. 2))];
let oct_esc = [%sedlex.regexp? ("\\", Rep(oct_digit, 1 .. 3))];
let num_esc = [%sedlex.regexp? unicode_esc | unicode4_esc | hex_esc | oct_esc];

let add_code_point = (buf, str, unicode) => {
  let (esc, numstr) = (
    String.sub(str, 1, 1),
    String.sub(str, 2, String.length(str) - 2),
  );
  let code_point =
    switch (esc) {
    | "u" when !unicode => raise(IllegalByteStringUnicodeEscape(str))
    | "u" when numstr.[0] == '{' =>
      Scanf.sscanf(String.sub(numstr, 1, String.length(numstr) - 1), "%x", x =>
        x
      )
    | "u"
    | "x" => Scanf.sscanf(numstr, "%x", x => x)
    | _ => Scanf.sscanf(esc ++ numstr, "%o", x => x)
    };
  if (unicode && Uchar.is_valid(code_point)) {
    Buffer.add_utf_8_uchar(buf, Uchar.of_int(code_point));
  } else if (!unicode) {
    Buffer.add_uint8(buf, code_point);
  } else {
    raise(IllegalUnicodeCodePoint(str));
  };
};

let rec read_str_inner = (buf, unicode, lexbuf) => {
  switch%sedlex (lexbuf) {
  | ('\\', newline_char) => read_str_inner(buf, unicode, lexbuf)
  | "\\b" =>
    Buffer.add_char(buf, '\b');
    read_str_inner(buf, unicode, lexbuf);
  | "\\f" =>
    Buffer.add_char(buf, '\012');
    read_str_inner(buf, unicode, lexbuf);
  | "\\n" =>
    Buffer.add_char(buf, '\n');
    read_str_inner(buf, unicode, lexbuf);
  | "\\r" =>
    Buffer.add_char(buf, '\r');
    read_str_inner(buf, unicode, lexbuf);
  | "\\t" =>
    Buffer.add_char(buf, '\t');
    read_str_inner(buf, unicode, lexbuf);
  | "\\v" =>
    Buffer.add_char(buf, '\011');
    read_str_inner(buf, unicode, lexbuf);
  | "\\\"" =>
    Buffer.add_char(buf, '"');
    read_str_inner(buf, unicode, lexbuf);
  | "\\\\" =>
    Buffer.add_char(buf, '\\');
    read_str_inner(buf, unicode, lexbuf);
  | num_esc =>
    add_code_point(buf, Sedlexing.Utf8.lexeme(lexbuf), unicode);
    read_str_inner(buf, unicode, lexbuf);
  | '"' =>
    if (unicode) {
      Buffer.contents(buf);
    } else {
      Buffer.contents(buf);
    }
  | 0 .. 127 =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_str_inner(buf, unicode, lexbuf);
  | any =>
    if (unicode) {
      Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
      read_str_inner(buf, unicode, lexbuf);
    } else {
      raise(IllegalByteStringUnicodeChar(Sedlexing.Utf8.lexeme(lexbuf)));
    }
  | _ => failwith("Impossible: Unclosed string in literal")
  };
};

let read_str = s => {
  let lexbuf = Sedlexing.Utf8.from_string(s);
  switch%sedlex (lexbuf) {
  | '"' => read_str_inner(Buffer.create(16), true, lexbuf)
  | _ => failwith("Impossible: Invalid start to string literal")
  };
};

let read_bytes = s => {
  let lexbuf = Sedlexing.Utf8.from_string(s);
  switch%sedlex (lexbuf) {
  | "b\"" => read_str_inner(Buffer.create(16), false, lexbuf)
  | _ => failwith("Impossible: Invalid start to bytes literal")
  };
};

let conv_bytes = s => {
  switch (read_bytes(s)) {
  | exception (IllegalUnicodeCodePoint(str)) =>
    Error(Format.sprintf("Illegal unicode code point: %S", str))
  | exception (IllegalByteStringUnicodeChar(str)) =>
    Error(
      Format.sprintf(
        "Byte literals may not contain non-ascii unicode characters: %S",
        str,
      ),
    )
  | exception (IllegalByteStringUnicodeEscape(str)) =>
    Error(
      Format.sprintf(
        "Byte literals may not contain unicode escapes: %S",
        str,
      ),
    )
  | str => Ok(Bytes.of_string(str))
  };
};

let conv_string = s => {
  switch (read_str(s)) {
  | exception (IllegalUnicodeCodePoint(str)) =>
    Error(Format.sprintf("Illegal unicode code point: %S", str))
  | exception (IllegalByteStringUnicodeChar(str)) =>
    Error(
      Format.sprintf(
        "String literals may not contain non-ascii unicode characters: %S",
        str,
      ),
    )
  | exception (IllegalByteStringUnicodeEscape(str)) =>
    Error(
      Format.sprintf(
        "String literals may not contain unicode escapes: %S",
        str,
      ),
    )
  | str => Ok(str)
  };
};

let rec read_char_inner = (buf, lexbuf) => {
  switch%sedlex (lexbuf) {
  | "\\b" =>
    Buffer.add_char(buf, '\b');
    read_char_inner(buf, lexbuf);
  | "\\f" =>
    Buffer.add_char(buf, '\012');
    read_char_inner(buf, lexbuf);
  | "\\n" =>
    Buffer.add_char(buf, '\n');
    read_char_inner(buf, lexbuf);
  | "\\r" =>
    Buffer.add_char(buf, '\r');
    read_char_inner(buf, lexbuf);
  | "\\t" =>
    Buffer.add_char(buf, '\t');
    read_char_inner(buf, lexbuf);
  | "\\v" =>
    Buffer.add_char(buf, '\011');
    read_char_inner(buf, lexbuf);
  | "\\'" =>
    Buffer.add_char(buf, '\'');
    read_char_inner(buf, lexbuf);
  | "\\\\" =>
    Buffer.add_char(buf, '\\');
    read_char_inner(buf, lexbuf);
  | num_esc =>
    add_code_point(buf, Sedlexing.Utf8.lexeme(lexbuf), true);
    read_char_inner(buf, lexbuf);
  | "'" => Buffer.contents(buf)
  | any =>
    Buffer.add_string(buf, Sedlexing.Utf8.lexeme(lexbuf));
    read_char_inner(buf, lexbuf);
  | _ => failwith("Impossible: Unclosed char in literal")
  };
};

let read_char = s => {
  let lexbuf = Sedlexing.Utf8.from_string(s);
  switch%sedlex (lexbuf) {
  | "'" => read_char_inner(Buffer.create(4), lexbuf)
  | _ => failwith("Impossible: Invalid start to char literal")
  };
};

let conv_char = s => {
  switch (read_char(s)) {
  | exception (IllegalUnicodeCodePoint(str)) =>
    Error(Format.sprintf("Illegal unicode code point: %S", str))
  | exception (IllegalByteStringUnicodeChar(str)) =>
    Error(
      Format.sprintf(
        "Character literals may not contain non-ascii unicode characters: %S",
        str,
      ),
    )
  | exception (IllegalByteStringUnicodeEscape(str)) =>
    Error(
      Format.sprintf(
        "Character literals may not contain unicode escapes: %S",
        str,
      ),
    )
  | c =>
    switch (String.length(c)) {
    | 0 =>
      Error(
        "Character literals must contain a character. Did you mean to create an empty string \"\" instead?",
      )
    | len when String_utils.Utf8.utf_length_at_offset(c, 0) == len => Ok(c)
    | _ =>
      Error(
        Format.sprintf(
          "Character literals cannot contain multiple characters: '%s'\nDid you mean to create the string \"%s\" instead?",
          c,
          Str.global_replace(Str.regexp({|"|}), {|\"|}, c),
        ),
      )
    }
  };
};
