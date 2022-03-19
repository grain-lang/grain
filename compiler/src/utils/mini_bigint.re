// Miniature bigint implementation
// (just contains bare essentials for parsing)

// unsigned only
type t = {mutable limbs: array(int64)};

let zero = () => {
  {limbs: [||]};
};

let of_int = n => {
  {limbs: [|Int64.of_int(n)|]};
};

// Like Int64.of_int32, but guarantees leading 32 bits are zero (since Int64.of_int32 is a signed conversion)
let int64_of_uint32 = (n: int32) =>
  Int64.logand(Int64.of_int32(n), 0xffffffffL);

let smart_add = (a, b) => {
  let res = Int64.add(a, b);
  let carry =
    if (Int64.unsigned_compare(res, a) < 0) {
      1L;
    } else {
      0L;
    };
  (res, carry);
};

let unsigned_add_i64 = (num1: t, num2: int64) => {
  let num1_limbs = Array.length(num1.limbs);
  let ret = {limbs: Array.make(num1_limbs, Int64.zero)};
  let tmp = ref(num2);
  for (i in 0 to num1_limbs - 1) {
    let (limb_val, new_tmp) = smart_add(tmp^, num1.limbs[i]);
    ret.limbs[i] = limb_val;
    tmp := new_tmp;
  };
  if (!Int64.equal(tmp^, Int64.zero)) {
    ret.limbs = Array.append(ret.limbs, [|tmp^|]);
  };
  ret;
};

let lower_half = (i64: int64) => {
  Int64.to_int32(i64);
};

let upper_half = (i64: int64) => {
  Int64.to_int32(Int64.shift_right_logical(i64, 32));
};

let concat_halves = (lower: int32, upper: int32) => {
  Int64.logor(
    Int64.shift_left(int64_of_uint32(upper), 32),
    int64_of_uint32(lower),
  );
};

let merge_half_limbs = (half_limbs: array(int32)) => {
  Array.init((Array.length(half_limbs) + 1) / 2, n =>
    if (n * 2 >= Array.length(half_limbs) - 1) {
      int64_of_uint32(half_limbs[n * 2]);
    } else {
      concat_halves(half_limbs[n * 2], half_limbs[n * 2 + 1]);
    }
  );
};

let trim_trailing_zeros = (limbs: array(int64)) => {
  let needed = ref(Array.length(limbs));
  let nonzero_found = ref(false);
  for (i in Array.length(limbs) - 1 downto 0) {
    if (! nonzero_found^ && Int64.equal(limbs[i], Int64.zero)) {
      needed := needed^ - 1;
    } else {
      nonzero_found := true;
    };
  };
  if (needed^ != Array.length(limbs)) {
    Array.sub(limbs, 0, needed^);
  } else {
    limbs;
  };
};

let unsigned_mul_i32 = (num1: t, num2: int32) => {
  let num1_half_limbs =
    Array.init(2 * Array.length(num1.limbs), n =>
      if (n mod 2 == 0) {
        lower_half(num1.limbs[n / 2]);
      } else {
        upper_half(num1.limbs[n / 2]);
      }
    );
  let ret_half_limbs = Array.make(Array.length(num1_half_limbs), Int32.zero);
  let tmp = ref(0L);
  for (i in 0 to Array.length(num1_half_limbs) - 1) {
    tmp :=
      Int64.add(
        tmp^,
        Int64.mul(
          int64_of_uint32(num1_half_limbs[i]),
          Int64.of_int32(num2),
        ),
      );
    ret_half_limbs[i] = Int64.to_int32(tmp^);
    tmp := Int64.shift_right_logical(tmp^, 32);
  };
  let ret_limbs =
    if (Int64.equal(tmp^, Int64.zero)) {
      merge_half_limbs(ret_half_limbs);
    } else {
      merge_half_limbs(
        Array.append(ret_half_limbs, [|Int64.to_int32(tmp^)|]),
      );
    };
  {limbs: trim_trailing_zeros(ret_limbs)};
};
