type 'a t = HLeaf | Hole of 'a t * 'a t | HNode of 'a * 'a t * 'a t
[@@deriving iter, map, fold]

let create v l r = match v with None -> Hole (l, r) | Some v -> HNode (v, l, r)
let is_empty_node = function HLeaf -> true | Hole (_, _) -> true | _ -> false
let is_leaf = function HLeaf -> true | _ -> false
let size t = fold (fun x _ -> x + 1) 0 t
let empty = HLeaf

let%test _ = empty = empty

let root = function
  | HLeaf -> None
  | Hole (_, _) -> None
  | HNode (v, _, _) -> Some v

let left = function HLeaf -> HLeaf | Hole (l, _) -> l | HNode (_, l, _) -> l
let right = function HLeaf -> HLeaf | Hole (_, r) -> r | HNode (_, _, r) -> r
