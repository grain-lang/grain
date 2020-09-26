/** Catenable lists. Adapted from Pyret. */
open Sexplib.Conv;

[@deriving sexp]
type t('a) =
  | Empty
  | Singleton('a)
  | Append(t('a), t('a))
  | Cons('a, t('a))
  | Snoc(t('a), 'a)
  | Wrapped(list('a)); /* <- for faster conversions (will lazily be converted to other forms) */

let list_of_t = cl => {
  let rec to_list_acc = (cl, acc) =>
    switch (cl) {
    | Empty => acc
    | Singleton(e) => [e, ...acc]
    | Append(l1, l2) => to_list_acc(l1, to_list_acc(l2, acc))
    | Cons(e, l) => [e, ...to_list_acc(l, acc)]
    | Snoc(l, e) => to_list_acc(l, [e, ...acc])
    | Wrapped(l) => l
    };
  to_list_acc(cl, []);
};

let mapped_list_of_t = (f, cl) => {
  let rec map_onto = (lst, acc) =>
    switch (lst) {
    | [] => acc
    | [hd, ...tl] =>
      let hd = f(hd);
      let tl = map_onto(tl, acc);
      [hd, ...tl];
    };
  let rec to_list_acc = (cl, acc) =>
    switch (cl) {
    | Empty => acc
    | Singleton(e) => [f(e), ...acc]
    | Append(l1, l2) => to_list_acc(l1, to_list_acc(l2, acc))
    | Cons(e, l) =>
      let hd = f(e);
      let tl = to_list_acc(l, acc);
      [hd, ...tl];
    | Snoc(l, e) => to_list_acc(l, [f(e), ...acc])
    | Wrapped(l) => map_onto(l, acc)
    };
  to_list_acc(cl, []);
};

let left_mapped_list_of_t = (f, cl) => {
  let rec revmap_to_list_acc = (f, acc, cl) =>
    switch (cl) {
    | Empty => acc
    | Singleton(e) => [f(e), ...acc]
    | Append(left, right) =>
      revmap_to_list_acc(f, revmap_to_list_acc(f, acc, left), right)
    | Cons(e, l) => revmap_to_list_acc(f, [f(e), ...acc], l)
    | Snoc(l, e) =>
      let newhead = revmap_to_list_acc(f, acc, l);
      [f(e), ...newhead];
    | Wrapped(l) => List.rev_map(f, l)
    };
  List.rev(revmap_to_list_acc(f, [], cl));
};

let rec map = (f, cl) =>
  switch (cl) {
  | Empty => Empty
  | Singleton(e) => Singleton(f(e))
  | Append(l1, l2) => Append(map(f, l1), map(f, l2))
  | Cons(e, l) => Cons(f(e), map(f, l))
  | Snoc(l, e) => Snoc(map(f, l), f(e))
  | Wrapped([]) => Empty
  | Wrapped([hd, ...tl]) => Cons(f(hd), map(f, Wrapped(tl)))
  };

let rec iter: 'a. ('a => unit, t('a)) => unit =
  (f, cl) =>
    switch (cl) {
    | Empty => ()
    | Singleton(e) => f(e)
    | Append(l1, l2) =>
      iter(f, l1);
      iter(f, l2);
    | Cons(e, l) =>
      f(e);
      iter(f, l);
    | Snoc(l, e) =>
      iter(f, l);
      f(e);
    | Wrapped(l) => List.iter(f, l)
    };

let rec fold_left = (f, base, cl) =>
  switch (cl) {
  | Empty => base
  | Singleton(e) => f(base, e)
  | Append(l1, l2) => fold_left(f, fold_left(f, base, l1), l2)
  | Cons(e, l) => fold_left(f, f(base, e), l)
  | Snoc(l, e) => f(fold_left(f, base, l), e)
  | Wrapped(l) => List.fold_left(f, base, l)
  };

let rec fold_right = (f, cl, base) =>
  switch (cl) {
  | Empty => base
  | Singleton(e) => f(e, base)
  | Append(l1, l2) => fold_right(f, l1, fold_right(f, l2, base))
  | Cons(e, l) => f(e, fold_right(f, l, base))
  | Snoc(l, e) => fold_right(f, l, f(e, base))
  | Wrapped(l) => List.fold_right(f, l, base)
  };

let length = cl => fold_left((acc, _) => acc + 1, 0, cl);

let rec is_empty = cl =>
  switch (cl) {
  | Empty
  | Wrapped([]) => true
  | Append(l1, l2) => is_empty(l1) && is_empty(l2)
  | _ => false
  };

let rec rev = cl =>
  switch (cl) {
  | Empty
  | Singleton(_)
  | Wrapped([]) => cl
  | Append(l1, l2) => Append(rev(l2), rev(l1))
  | Cons(e, l) => Snoc(rev(l), e)
  | Snoc(l, e) => Cons(e, rev(l))
  | Wrapped([hd, ...tl]) => Snoc(rev(Wrapped(tl)), hd)
  };

let rec hd = cl =>
  switch (cl) {
  | Singleton(e)
  | Cons(e, _) => e
  | Snoc(l, e) when is_empty(l) => e
  | Snoc(l, _) => hd(l)
  | Append(l1, _) when !is_empty(l1) => hd(l1)
  | Append(_, l2) => hd(l2)
  | Wrapped([hd, ..._]) => hd
  | Wrapped([])
  | Empty => raise(Not_found)
  };

let rec tl = cl =>
  switch (cl) {
  | Singleton(_)
  | Empty
  | Wrapped([]) => raise(Failure("tl"))
  | Cons(_, rest) => rest
  | Wrapped([_, ...rest]) => Wrapped(rest)
  | Append(l1, rest) when is_empty(l1) => rest
  | Append(l1, l2) => Append(tl(l1), l2)
  | Snoc(l, e) when is_empty(l) => Singleton(e)
  | Snoc(l, e) => Snoc(tl(l), e)
  };

/** Returns the last element of the given list. */

let rec last = cl =>
  switch (cl) {
  | Singleton(e)
  | Snoc(_, e) => e
  | Cons(e, l) when is_empty(l) => e
  | Cons(e, l) => last(l)
  | Append(_, l2) when !is_empty(l2) => last(l2)
  | Append(l1, _) => last(l1)
  | Wrapped([hd]) => hd
  | Wrapped([hd, ...rest]) => last(Wrapped(rest))
  | Wrapped([])
  | Empty => raise(Failure("last"))
  };

let rec mapped_t_of_list: 'a 'b. ('a => 'b, list('a)) => t('b) =
  (f, lst) =>
    switch (lst) {
    | [] => Empty
    | [x] => Singleton(f(x))
    | [hd, ...tl] => Cons(f(hd), mapped_t_of_list(f, tl))
    };

let t_of_list: 'a. list('a) => t('a) = lst => Wrapped(lst);

/** The empty concatlist. */
/** Constructs a one-item concatlist. */

let empty: 'a. t('a) = Empty;

/** Constructs a one-item concatlist. */
/** Appends the two given concatlists. */

let singleton: 'a. 'a => t('a) = x => Singleton(x);

/** Appends the two given concatlists. */
/** Adds the given item to the front of the given concatlist. */

let append: 'a. (t('a), t('a)) => t('a) = (a, b) => Append(a, b);

/** Adds the given item to the front of the given concatlist. */
/** Adds the given item to the end of the given concatlist. */

let cons: 'a. ('a, t('a)) => t('a) = (a, b) => Cons(a, b);

/** Adds the given item to the end of the given concatlist. */
/** Wraps the given list into a concatlist (a synonym for [t_of_list])*/

let snoc: 'a. (t('a), 'a) => t('a) = (a, b) => Snoc(a, b);

/** Wraps the given list into a concatlist (a synonym for [t_of_list])*/

let wrapped: 'a. list('a) => t('a) = t_of_list;

let (@) = append;
let (@+) = (l1, l2) => Append(l1, t_of_list(l2));
let (+@) = (@+);

let flatten: 'a. list(t('a)) => t('a) =
  concatlists => List.fold_right((@), concatlists, Empty);
