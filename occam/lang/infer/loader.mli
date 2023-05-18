exception RecursiveImports of string list

type t

val make : string list -> t
val dequeue : t -> string option
val loaded : string -> Schema.t -> t -> t
val result : t -> Schema.t list
