exception DepNotFound of string
exception RecursiveImport of string

type ctx
type t
type ns = Path of string list | Parent of ns
type tree = SimpleNode of string | Node of string * tree list

val make : ctx -> t

val create_ctx :
  on_missing:(string -> unit) ->
  on_recursive_import:(string -> unit) ->
  is_valid_target:(string -> bool) ->
  ns_to_target:(ns -> string -> string) ->
  ctx

val create_fs_ctx : string list -> string -> ctx
val add : t -> string -> ns list -> t
val find : t -> string -> ns list
val tree : t -> string -> tree
