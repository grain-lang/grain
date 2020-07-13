// Based on BatList

let fold_lefti = (fn, init, lis) => {
  let rec iter = (idx, result, lis) => {
    switch (lis) {
    | [] => result
    | [item, ...rest] => iter(idx + 1, fn(result, idx, item), rest)
    };
  };
  iter(0, init, lis);
};
