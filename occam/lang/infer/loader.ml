exception RecursiveImports of string list
exception FileNotFound of string

type t = {
  mutable dependencies : (string, Schema.path list) Hashtbl.t;
  mutable queue : string Queue.t;
  mutable loaded : (string, Schema.t) Hashtbl.t;
}

let make _ =
  {
    dependencies = Hashtbl.create 0;
    queue = Queue.create ();
    loaded = Hashtbl.create 0;
  }

let dequeue { queue; _ } = Queue.take_opt queue

let rec resolve_direct_dependencies schema =
  match schema with
  | [] -> []
  | Schema.ImportDeclaration (path, _) :: rest ->
      let direct_dependencies = resolve_direct_dependencies rest in
      path :: direct_dependencies
  | _ :: rest -> resolve_direct_dependencies rest

let rec paths_to_files ?(predicate = Sys.file_exists) ?(extension = ".inf")
    search_dirs current_dir paths =
  match paths with
  | [] -> []
  | path :: rest ->
      let file =
        path_to_file search_dirs current_dir path ~predicate ~extension
      in
      file :: paths_to_files search_dirs current_dir rest ~predicate ~extension

and path_to_file search_dirs current_dir path ~predicate ~extension =
  match path with
  | Schema.ParentAccess sibling ->
      let parent_dir = Filename.dirname current_dir in
      path_to_file [] parent_dir sibling ~predicate ~extension
  | Schema.Path segments ->
      let path_as_file =
        String.concat Filename.dir_sep (List.map Schema.unwrap segments)
        ^ extension
      in
      find_matched (current_dir :: search_dirs) path_as_file ~predicate

and find_matched search_dirs path ~predicate =
  match search_dirs with
  | [] -> raise (FileNotFound path)
  | search_dir :: rest ->
      let file = Filename.concat search_dir path in
      if predicate file then file else find_matched rest path ~predicate

let loaded dependant schema loader =
  let { loaded; queue; dependencies } = loader in
  let direct_dependencies = resolve_direct_dependencies schema in
  Hashtbl.add dependencies dependant direct_dependencies;
  Queue.add dependant queue;
  Hashtbl.add loaded dependant schema;
  loader

let result _ = []

let%test _ =
  let search_dirs = [] in
  let current_dir = "/test" in
  let paths =
    [
      Schema.ParentAccess
        (Schema.Path [ Schema.Identifier "a"; Schema.Identifier "b" ]);
      Schema.Path [ Schema.Identifier "a"; Schema.Identifier "b" ];
      Schema.Path [ Schema.Identifier "b" ];
      Schema.Path [ Schema.Identifier "c" ];
    ]
  in
  let files =
    paths_to_files search_dirs current_dir paths ~predicate:(fun _ -> true)
  in
  files = [ "/a/b.inf"; "/test/a/b.inf"; "/test/b.inf"; "/test/c.inf" ]
