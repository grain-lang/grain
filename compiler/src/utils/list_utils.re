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

let filter_mapi = (f, list) => {
  let rec filter_mapi = (count, f) =>
    fun
    | [hd, ...tl] => {
        switch (f(hd, count)) {
        | Some(result) => [result, ...filter_mapi(count + 1, f, tl)]
        | None => filter_mapi(count + 1, f, tl)
        };
      }
    | [] => [];

  filter_mapi(0, f, list);
};
