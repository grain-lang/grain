/** Catenable lists. Adapted from Pyret. */;

[@deriving sexp]
type t('a);

/** Flattens the given concatlist into a list. */

let list_of_t: t('a) => list('a);

/** Maps the given function over the given concatlist,
    collecting the result as a list. */

let mapped_list_of_t: ('a => 'b, t('a)) => list('b);

/** Like [mapped_list_of_t], but guarantees that the function will
    be called with the items in order. */

let left_mapped_list_of_t: ('a => 'b, t('a)) => list('b);

/** Like [List.map], but over concatlists. */

let map: ('a => 'b, t('a)) => t('b);

/** Like [List.iter], but over concatlists. */

let iter: ('a => unit, t('a)) => unit;

/** Like [List.fold_left], but over concatlists. */

let fold_left: (('b, 'a) => 'b, 'b, t('a)) => 'b;

/** Like [List.fold_right], but over concatlists. */

let fold_right: (('a, 'b) => 'b, t('a), 'b) => 'b;

/** Returns the number of elements in the given concatlist */

let length: t('a) => int;

/** Returns true if the given concatlist contains no elements. */

let is_empty: t('a) => bool;

/** Reverses the given concatlist. */

let rev: t('a) => t('a);

/** Returns the first element of the given concatlist. If the list has
    no elements, [Failure "hd"] is raised. */
/** Returns all but the first element of the given concatlist. If the list has no tail,
    [Failure "tl"] is raised. */

let hd: t('a) => 'a;

/** Returns all but the first element of the given concatlist. If the list has no tail,
    [Failure "tl"] is raised. */
/** Returns the last element of the given concatlist. If the list has no tail,
    [Failure "last"] is raised. */

let tl: t('a) => t('a);

/** Returns the last element of the given concatlist. If the list has no tail,
    [Failure "last"] is raised. */

let last: t('a) => 'a;

/** Maps the given function over all items in the given list and
    collects the result in a concatlist. */

let mapped_t_of_list: ('a => 'b, list('a)) => t('b);

/** Wraps the given list as a concatlist. */

let t_of_list: list('a) => t('a);

/** Flattens the given list of concatlists into a single concatlist. */

let flatten: list(t('a)) => t('a);

/** The empty concatlist. */
/** Constructs a one-item concatlist. */

let empty: t('a);

/** Constructs a one-item concatlist. */
/** Appends the two given concatlists. */

let singleton: 'a => t('a);

/** Appends the two given concatlists. */
/** Adds the given item to the front of the given concatlist. */

let append: (t('a), t('a)) => t('a);

/** Adds the given item to the front of the given concatlist. */
/** Adds the given item to the end of the given concatlist. */

let cons: ('a, t('a)) => t('a);

/** Adds the given item to the end of the given concatlist. */
/** Wraps the given list into a concatlist (a synonym for [t_of_list])*/

let snoc: (t('a), 'a) => t('a);

/** Wraps the given list into a concatlist (a synonym for [t_of_list])*/

let wrapped: list('a) => t('a);

/** Infix operator for [append] */
/** Like [@], but wraps the right-hand side */

let (@): (t('a), t('a)) => t('a);

/** Like [@], but wraps the right-hand side */
/** Alias for [@+] (for left-associativity) */

let (@+): (t('a), list('a)) => t('a);

/** Alias for [@+] (for left-associativity) */

let (+@): (t('a), list('a)) => t('a);
