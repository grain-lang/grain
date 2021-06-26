type t =
  | Inclusive(int, int)
  | Exclusive(int, int);

let inRange = (value, range) => {
  switch (range) {
  | Inclusive(lower, upper) when value >= lower && value <= upper => true
  | Inclusive(upper, lower) when value >= lower && value <= upper => true
  | Exclusive(lower, upper) when value >= lower && value < upper => true
  | Exclusive(upper, lower) when value >= lower && value < upper => true
  | _ => false
  };
};

let forEach = (fn, range) => {
  switch (range) {
  | Inclusive(lower, upper) when lower <= upper =>
    let idx = ref(lower);
    while (idx^ <= upper) {
      fn(idx^);
      incr(idx);
    };
  | Inclusive(upper, lower) =>
    let idx = ref(upper);
    while (idx^ >= lower) {
      fn(idx^);
      decr(idx);
    };
  | Exclusive(lower, upper) when lower <= upper =>
    let idx = ref(lower);
    while (idx^ < upper) {
      fn(idx^);
      incr(idx);
    };
  | Exclusive(upper, lower) =>
    let idx = ref(upper);
    while (idx^ > lower) {
      fn(idx^);
      decr(idx);
    };
  };
};

let map = (fn, range) => {
  let result = ref([]);
  switch (range) {
  | Inclusive(lower, upper) when lower <= upper =>
    let idx = ref(upper);
    while (idx^ >= lower) {
      result := [fn(idx^), ...result^];
      decr(idx);
    };
  | Inclusive(upper, lower) =>
    let idx = ref(lower);
    while (idx^ <= upper) {
      result := [fn(idx^), ...result^];
      incr(idx);
    };
  | Exclusive(lower, upper) when lower <= upper =>
    let idx = ref(upper - 1);
    while (idx^ >= lower) {
      result := [fn(idx^), ...result^];
      decr(idx);
    };
  | Exclusive(upper, lower) =>
    let idx = ref(lower + 1);
    while (idx^ <= upper) {
      result := [fn(idx^), ...result^];
      incr(idx);
    };
  };
  result;
};
