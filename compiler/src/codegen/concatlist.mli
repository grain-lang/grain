(** Catenable lists. Adapted from Pyret. *)

type 'a t [@@deriving sexp]

(** Flattens the given concatlist into a list. *)
val list_of_t : 'a t -> 'a list

(** Maps the given function over the given concatlist,
    collecting the result as a list. *)
val mapped_list_of_t : ('a -> 'b) -> 'a t -> 'b list

(** Like [mapped_list_of_t], but guarantees that the function will
    be called with the items in order. *)
val left_mapped_list_of_t : ('a -> 'b) -> 'a t -> 'b list

(** Like [List.map], but over concatlists. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Like [List.iter], but over concatlists. *)
val iter : ('a -> unit) -> 'a t -> unit

(** Like [List.fold_left], but over concatlists. *)
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** Like [List.fold_right], but over concatlists. *)
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(** Returns the number of elements in the given concatlist *)
val length : 'a t -> int

(** Returns true if the given concatlist contains no elements. *)
val is_empty : 'a t -> bool

(** Reverses the given concatlist. *)
val rev : 'a t -> 'a t

(** Returns the first element of the given concatlist. If the list has
    no elements, [Failure "hd"] is raised. *)
val hd : 'a t -> 'a
(** Returns all but the first element of the given concatlist. If the list has no tail,
    [Failure "tl"] is raised. *)
val tl : 'a t -> 'a t
(** Returns the last element of the given concatlist. If the list has no tail,
    [Failure "last"] is raised. *)
val last : 'a t -> 'a

(** Maps the given function over all items in the given list and
    collects the result in a concatlist. *)
val mapped_t_of_list : ('a -> 'b) -> 'a list -> 'b t

(** Wraps the given list as a concatlist. *)
val t_of_list : 'a list -> 'a t

(** Flattens the given list of concatlists into a single concatlist. *)
val flatten : 'a t list -> 'a t

(** The empty concatlist. *)
val empty : 'a t
(** Constructs a one-item concatlist. *)
val singleton : 'a -> 'a t
(** Appends the two given concatlists. *)
val append : 'a t -> 'a t -> 'a t
(** Adds the given item to the front of the given concatlist. *)
val cons : 'a -> 'a t -> 'a t
(** Adds the given item to the end of the given concatlist. *)
val snoc : 'a t -> 'a -> 'a t
(** Wraps the given list into a concatlist (a synonym for [t_of_list])*)
val wrapped : 'a list -> 'a t

(** Infix operator for [append] *)
val (@) : 'a t -> 'a t -> 'a t
(** Like [@], but wraps the right-hand side *)
val (@+) : 'a t -> 'a list -> 'a t
(** Alias for [@+] (for left-associativity) *)
val (+@) : 'a t -> 'a list -> 'a t

