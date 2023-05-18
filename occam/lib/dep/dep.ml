exception DepNotFound of string
exception RecursiveImport of string

type ns = Path of string list | Parent of ns

type ctx = {
  on_missing : string -> unit;
  on_recursive_import : string -> unit;
  is_valid_target : string -> bool;
  ns_to_target : ns -> string -> string;
}

type t = {
  ctx : ctx;
  mutable dependencies : (string, ns list) Hashtbl.t;
  mutable queue : string Queue.t;
  mutable visited : (string, unit) Hashtbl.t;
}

type tree = SimpleNode of string | Node of string * tree list

let make ctx =
  {
    ctx;
    dependencies = Hashtbl.create 10;
    queue = Queue.create ();
    visited = Hashtbl.create 10;
  }

let create_ctx ~on_missing ~on_recursive_import ~is_valid_target ~ns_to_target =
  { on_missing; on_recursive_import; is_valid_target; ns_to_target }

let rec fs_ns_to_target search_dirs current_dir suffix = function
  | Path path -> String.concat Filename.dir_sep path ^ suffix
  | Parent ns ->
      let parent_dir = Filename.dirname current_dir in
      if parent_dir = current_dir then
        raise (DepNotFound (fs_ns_to_target search_dirs current_dir suffix ns))
      else fs_ns_to_target search_dirs parent_dir suffix ns

let create_fs_ctx search_dirs suffix =
  create_ctx
    ~on_missing:(fun target -> raise (DepNotFound target))
    ~on_recursive_import:(fun target -> raise (RecursiveImport target))
    ~is_valid_target:Sys.file_exists
    ~ns_to_target:(fun ns current ->
      fs_ns_to_target search_dirs (Filename.dirname current) suffix ns)

let add dep identity namespaces =
  Hashtbl.add dep.dependencies identity namespaces;
  dep

let find dep identity = Hashtbl.find dep.dependencies identity

let tree dep identity =
  Node
    ( identity,
      List.map
        (fun ns -> SimpleNode (dep.ctx.ns_to_target ns identity))
        (find dep identity) )
