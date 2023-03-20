type 'a t = HLeaf | Hole of 'a t * 'a t | HNode of 'a * 'a t * 'a t
[@@deriving iter, map, fold]

val create : 'a option -> 'a t -> 'a t -> 'a t
val empty : 'a t
val root : 'a t -> 'a option
val left : 'a t -> 'a t
val right : 'a t -> 'a t
val size : 'a t -> int
val is_empty_node : 'a t -> bool
val is_leaf : 'a t -> bool
