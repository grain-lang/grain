(** Catenable lists. Adapted from Pyret. *)
open Sexplib.Conv

type 'a t =
  | Empty
  | Singleton of 'a
  | Append of 'a t * 'a t
  | Cons of 'a * 'a t
  | Snoc of 'a t * 'a
  | Wrapped of 'a list (* <- for faster conversions (will lazily be converted to other forms) *)
[@@deriving sexp]

let list_of_t cl =
  let rec to_list_acc cl acc =
    match cl with
    | Empty -> acc
    | Singleton(e) -> e::acc
    | Append(l1, l2) -> to_list_acc l1 (to_list_acc l2 acc)
    | Cons(e, l) -> e::(to_list_acc l acc)
    | Snoc(l, e) -> to_list_acc l (e::acc)
    | Wrapped(l) -> l in
  to_list_acc cl []

let mapped_list_of_t f cl =
  let rec map_onto lst acc =
    match lst with
    | [] -> acc
    | hd::tl ->
      let hd = f hd in
      let tl = map_onto tl acc in
      hd::tl in
  let rec to_list_acc cl acc =
    match cl with
    | Empty -> acc
    | Singleton(e) -> (f e)::acc
    | Append(l1, l2) -> to_list_acc l1 (to_list_acc l2 acc)
    | Cons(e, l) ->
      let hd = f e in
      let tl = to_list_acc l acc in
      hd::tl
    | Snoc(l, e) -> to_list_acc l ((f e)::acc)
    | Wrapped(l) -> (map_onto l acc) in
  to_list_acc cl []

let left_mapped_list_of_t f cl =
  let rec revmap_to_list_acc f acc cl =
    match cl with
    | Empty -> acc
    | Singleton(e) -> (f e)::acc
    | Append(left, right) -> revmap_to_list_acc f (revmap_to_list_acc f acc left) right
    | Cons(e, l) -> revmap_to_list_acc f ((f e)::acc) l
    | Snoc(l, e) ->
      let newhead = revmap_to_list_acc f acc l in
      (f e)::newhead
    | Wrapped(l) -> List.rev_map f l in
  List.rev (revmap_to_list_acc f [] cl)

let rec map f cl =
  match cl with
  | Empty -> Empty
  | Singleton(e) -> Singleton(f e)
  | Append(l1, l2) -> Append(map f l1, map f l2)
  | Cons(e, l) -> Cons(f e, map f l)
  | Snoc(l, e) -> Snoc(map f l, f e)
  | Wrapped([]) -> Empty
  | Wrapped(hd::tl) -> Cons(f hd, map f (Wrapped tl))

let rec iter : 'a. ('a -> unit) -> 'a t -> unit = fun f cl ->
  match cl with
  | Empty -> ()
  | Singleton(e) -> f e
  | Append(l1, l2) -> iter f l1; iter f l2
  | Cons(e, l) -> f e; iter f l
  | Snoc(l, e) -> iter f l; f e
  | Wrapped(l) -> List.iter f l

let rec fold_left f base cl =
  match cl with
  | Empty -> base
  | Singleton e -> f base e
  | Append(l1, l2) -> fold_left f (fold_left f base l1) l2
  | Cons(e, l) -> fold_left f (f base e) l
  | Snoc(l, e) -> f (fold_left f base l) e
  | Wrapped(l) -> List.fold_left f base l

let rec fold_right f cl base =
  match cl with
  | Empty -> base
  | Singleton e -> f e base
  | Append(l1, l2) -> fold_right f l1 (fold_right f l2 base)
  | Cons(e, l) -> f e (fold_right f l base)
  | Snoc(l, e) -> fold_right f l (f e base)
  | Wrapped(l) -> List.fold_right f l base

let length cl = fold_left (fun acc _ -> acc + 1) 0 cl

let rec is_empty cl =
  match cl with
  | Empty
  | Wrapped([]) -> true
  | Append(l1, l2) -> (is_empty l1) && (is_empty l2)
  | _ -> false

let rec rev cl =
  match cl with
  | Empty
  | Singleton _
  | Wrapped([]) -> cl
  | Append(l1, l2) -> Append(rev l2, rev l1)
  | Cons(e, l) -> Snoc(rev l, e)
  | Snoc(l, e) -> Cons(e, rev l)
  | Wrapped(hd::tl) -> Snoc(rev (Wrapped tl), hd)

let rec hd cl =
  match cl with
  | Singleton(e)
  | Cons(e, _) -> e
  | Snoc(l, e) when is_empty l -> e
  | Snoc(l, _) -> hd l
  | Append(l1, _) when not(is_empty l1) -> hd l1
  | Append(_, l2) -> hd l2
  | Wrapped(hd::_) -> hd
  | Wrapped([])
  | Empty -> raise Not_found


let rec tl cl =
  match cl with
  | Singleton(_)
  | Empty
  | Wrapped([]) -> raise (Failure "tl")
  | Cons(_, rest) -> rest
  | Wrapped(_::rest) -> Wrapped(rest)
  | Append(l1, rest) when (is_empty l1) -> rest
  | Append(l1, l2) -> Append(tl l1, l2)
  | Snoc(l, e) when is_empty l -> Singleton(e)
  | Snoc(l, e) -> Snoc(tl l, e)

(** Returns the last element of the given list. *)
let rec last cl =
  match cl with
  | Singleton(e)
  | Snoc(_, e) -> e
  | Cons(e, l) when is_empty l -> e
  | Cons(e, l) -> last l
  | Append(_, l2) when not(is_empty l2) -> last l2
  | Append(l1, _) -> last l1
  | Wrapped(hd::[]) -> hd
  | Wrapped(hd::rest) -> last (Wrapped(rest))
  | Wrapped([])
  | Empty -> raise (Failure "last")

let rec mapped_t_of_list : 'a 'b. ('a -> 'b) -> 'a list -> 'b t = fun f lst ->
  match lst with
  | [] -> Empty
  | [x] -> Singleton(f x)
  | hd::tl -> Cons(f hd, mapped_t_of_list f tl)

let t_of_list : 'a. 'a list -> 'a t = fun lst -> Wrapped lst

(** The empty concatlist. *)
let empty : 'a. 'a t = Empty
(** Constructs a one-item concatlist. *)
let singleton : 'a. 'a -> 'a t = fun x -> Singleton(x)
(** Appends the two given concatlists. *)
let append : 'a. 'a t -> 'a t -> 'a t = fun a b -> Append(a, b)
(** Adds the given item to the front of the given concatlist. *)
let cons : 'a. 'a -> 'a t -> 'a t = fun a b -> Cons(a, b)
(** Adds the given item to the end of the given concatlist. *)
let snoc : 'a. 'a t -> 'a -> 'a t = fun a b -> Snoc(a, b)
(** Wraps the given list into a concatlist (a synonym for [t_of_list])*)
let wrapped : 'a. 'a list -> 'a t = t_of_list

let (@) = append
let (@+) l1 l2 = Append(l1, t_of_list l2)
let (+@) = (@+)

let flatten : 'a. 'a t list -> 'a t = fun concatlists -> List.fold_right (@) concatlists Empty

