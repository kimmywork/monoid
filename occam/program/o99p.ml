let rec last l = match l with [] -> None | [ a ] -> Some a | _ :: t -> last t

let%test _ = last [ 1; 2; 3 ] = Some 3
let%test _ = last [] = None

let rec last_two l =
  match l with [] -> None | [ a; b ] -> Some (a, b) | _ :: t -> last_two t

let%test _ = last_two [ 1; 2; 3 ] = Some (2, 3)
let%test _ = last_two [ 1 ] = None
let%test _ = last_two [] = None

let rec at n l =
  match l with [] -> None | h :: t -> if n = 1 then Some h else at (n - 1) t

let%test _ = at 3 [ 1; 2; 3 ] = Some 3
let%test _ = at 3 [ 1 ] = None

let length l =
  let rec trlen l n = match l with [] -> n | _ :: t -> trlen t (n + 1) in
  trlen l 0

let%test _ = length [ 1; 2; 3 ] = 3
let%test _ = length [] = 0

let rec rev l = match l with [] -> [] | h :: t -> rev t @ [ h ]

let%test _ = rev [ 1; 2; 3 ] = [ 3; 2; 1 ]
let%test _ = rev [] = []

let is_palindrom l = l = rev l

let%test _ = is_palindrom [ 1; 2; 3 ] = false
let%test _ = is_palindrom [ 1; 2; 1 ]

type 'a node = One of 'a | Many of 'a node list

let rec flatten l =
  match l with
  | [] -> []
  | h :: t -> (
      match h with One a -> a :: flatten t | Many l -> flatten l @ flatten t)

let%test _ =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]

let compress l =
  let rec filter_same l e =
    match l with
    | [] -> [ e ]
    | h :: t -> if h = e then filter_same t e else e :: filter_same t h
  in
  match l with [] -> [] | h :: t -> filter_same t h

let%test _ =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]

let%test _ = compress [ "a"; "a"; "a" ] = [ "a" ]

let pack l =
  let rec pack_with l e s =
    match l with
    | [] -> [ s ]
    | h :: t ->
        if h = e then pack_with t e (s @ [ e ]) else s :: pack_with t h [ h ]
  in
  match l with [] -> [] | h :: t -> pack_with t h [ h ]

let%test _ = pack [] = []
let%test _ = pack [ 1; 2; 3 ] = [ [ 1 ]; [ 2 ]; [ 3 ] ]
let%test _ = pack [ 1; 2; 2; 3; 3; 3 ] = [ [ 1 ]; [ 2; 2 ]; [ 3; 3; 3 ] ]

let encode l =
  let rec encode_with l e n =
    match l with
    | [] -> [ (n, e) ]
    | h :: t ->
        if h = e then encode_with t e (n + 1) else (n, e) :: encode_with t h 1
  in
  match l with [] -> [] | h :: t -> encode_with t h 1

let%test _ = encode [ 1; 2; 3 ] = [ (1, 1); (1, 2); (1, 3) ]
let%test _ = encode [ 1; 2; 2; 3; 3; 3 ] = [ (1, 1); (2, 2); (3, 3) ]
let%test _ = encode [] = []

type 'a rle = One of 'a | Many of int * 'a

let encode l =
  let rle_of n e = match n with 1 -> One e | _ -> Many (n, e) in
  let rec encode_with l e n =
    match l with
    | [] -> [ rle_of n e ]
    | h :: t ->
        if h = e then encode_with t e (n + 1)
        else rle_of n e :: encode_with t h 1
  in
  match l with [] -> [] | h :: t -> encode_with t h 1

let%test _ = encode [ 1; 2; 3 ] = [ One 1; One 2; One 3 ]

let%test _ =
  encode [ "a"; "b"; "b"; "c"; "c"; "c" ]
  = [ One "a"; Many (2, "b"); Many (3, "c") ]

let%test _ = encode [] = []

let rec decode rle =
  let rec repeat x n = if x = 0 then [] else n :: repeat (x - 1) n in
  match rle with
  | [] -> []
  | One a :: t -> a :: decode t
  | Many (n, e) :: t -> repeat n e @ decode t

let%test _ = decode [] = []

let%test _ =
  decode [ One "a"; Many (2, "b"); Many (3, "c") ]
  = [ "a"; "b"; "b"; "c"; "c"; "c" ]

let%test _ = decode [ One 1; One 2; One 3 ] = [ 1; 2; 3 ]

let encode l =
  let rec replace l =
    match l with
    | [] -> []
    | h :: t -> (
        match h with
        | [ a ] -> One a :: replace t
        | ha :: _ -> Many (length h, ha) :: replace t
        | [] -> replace t)
  in
  replace (pack l)

let%test _ = encode [ 1; 2; 3 ] = [ One 1; One 2; One 3 ]

let%test _ =
  encode [ "a"; "b"; "b"; "c"; "c"; "c" ]
  = [ One "a"; Many (2, "b"); Many (3, "c") ]

let%test _ = encode [] = []

let rec duplicate l = match l with [] -> [] | h :: t -> h :: h :: duplicate t

let%test _ = duplicate [] = []
let%test _ = duplicate [ 1; 2; 3 ] = [ 1; 1; 2; 2; 3; 3 ]
let%test _ = duplicate [ 1; 2; 2; 3 ] = [ 1; 1; 2; 2; 2; 2; 3; 3 ]

let rec replicate l n =
  let rec repeat e n = if n = 0 then [] else e :: repeat e (n - 1) in
  match l with [] -> [] | h :: t -> repeat h n @ replicate t n

let%test _ = replicate [ 1 ] 3 = [ 1; 1; 1 ]
let%test _ = replicate [] 3 = []
let%test _ = replicate [ 1; 2; 2; 3 ] 3 = [ 1; 1; 1; 2; 2; 2; 2; 2; 2; 3; 3; 3 ]

let rec drop l n =
  let rec take l n a =
    if n = 0 then (a, l)
    else match l with [] -> (a, l) | h :: t -> take t (n - 1) (a @ [ h ])
  in
  let rec drop_last l =
    match l with [] | [ _ ] -> [] | h :: t -> h :: drop_last t
  in
  if length l < n then l
  else
    let h, t = take l n [] in
    drop_last h @ drop t n

let%test _ =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]

let split l n =
  let rec split_at l n a =
    if n = 0 then (a, l)
    else match l with [] -> (a, l) | h :: t -> split_at t (n - 1) (a @ [ h ])
  in
  split_at l n []

let%test _ =
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])

let%test _ = split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], [])

let slice l start_from end_at =
  let _, rhs = split l start_from in
  let lhs, _ = split rhs (end_at - start_from + 1) in
  lhs

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
  = [ "c"; "d"; "e"; "f"; "g" ]

let rotate l p =
  let h, t = if p > 0 then split l p else split l (length l + p) in
  t @ h

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2)
  = [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ]

let rec range l r =
  if l = r then [ l ]
  else if l > r then l :: range (l - 1) r
  else l :: range (l + 1) r

let%test _ = range 1 3 = [ 1; 2; 3 ]
let%test _ = range 3 1 = [ 3; 2; 1 ]

let rec remove_at x l =
  match l with
  | [] -> []
  | h :: t -> if x == 0 then t else h :: remove_at (x - 1) t

let%test _ = remove_at 0 [ 1; 2; 3 ] = [ 2; 3 ]
let%test _ = remove_at 3 [ 1; 2; 3; 4 ] = [ 1; 2; 3 ]

let rec insert_at a x l =
  match l with
  | [] -> [ a ]
  | h :: t -> if x == 0 then a :: l else h :: insert_at a (x - 1) t

let%test _ = insert_at 1 0 [] = [ 1 ]
let%test _ = insert_at 1 1 [ 1; 2; 3 ] = [ 1; 1; 2; 3 ]
let%test _ = insert_at 1 3 [ 1; 2; 3 ] = [ 1; 2; 3; 1 ]

let rand_select list n =
  let rec extract acc n l =
    match l with
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len = extract [] (Random.int len) list in
  let rec aux n acc list len =
    if n = 0 then acc
    else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
  in
  let len = List.length list in
  aux (min n len) [] list len

let lotto_select n m = rand_select (range 1 m) n

let%test _ = List.length (lotto_select 5 10) = 5

let permutation l = rand_select l (List.length l)

let%test _ =
  List.length (permutation [ 1; 2; 3; 4; 5 ]) = List.length [ 1; 2; 3; 4; 5 ]

let rec extract k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
        let without_h = extract k tl in
        with_h @ without_h

let%test _ =
  extract 2 [ 1; 2; 3; 4 ]
  = [ [ 1; 2 ]; [ 1; 3 ]; [ 1; 4 ]; [ 2; 3 ]; [ 2; 4 ]; [ 3; 4 ] ]

let group list sizes =
  let initial = List.map (fun size -> (size, [])) sizes in
  let prepend p list =
    let emit l acc = l :: acc in
    let rec aux emit acc = function
      | [] -> emit [] acc
      | ((n, l) as h) :: t ->
          let acc = if n > 0 then emit ((n - 1, p :: l) :: t) acc else acc in
          aux (fun l acc -> emit (h :: l) acc) acc t
    in
    aux emit [] list
  in
  let rec aux = function
    | [] -> [ initial ]
    | h :: t -> List.concat (List.map (prepend h) (aux t))
  in
  let all = aux list in
  let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
  List.map (List.map snd) complete

let%test _ =
  group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]
  = [
      [ [ "a"; "b" ]; [ "c" ] ];
      [ [ "a"; "c" ]; [ "b" ] ];
      [ [ "b"; "c" ]; [ "a" ] ];
      [ [ "a"; "b" ]; [ "d" ] ];
      [ [ "a"; "c" ]; [ "d" ] ];
      [ [ "b"; "c" ]; [ "d" ] ];
      [ [ "a"; "d" ]; [ "b" ] ];
      [ [ "b"; "d" ]; [ "a" ] ];
      [ [ "a"; "d" ]; [ "c" ] ];
      [ [ "b"; "d" ]; [ "c" ] ];
      [ [ "c"; "d" ]; [ "a" ] ];
      [ [ "c"; "d" ]; [ "b" ] ];
    ]

let rec insert cmp e = function
  | [] -> [ e ]
  | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t

let rec sort cmp = function [] -> [] | h :: t -> insert cmp h (sort cmp t)

let length_sort lists =
  sort (fun a b -> compare (List.length a) (List.length b)) lists

let%test _ = length_sort [ [ 1; 2 ]; [ 1 ]; [] ] = [ []; [ 1 ]; [ 1; 2 ] ]

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

let%test _ = is_prime 1 = false
let%test _ = is_prime 2
let%test _ = is_prime 7
let%test _ = is_prime 12 = false

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let coprime a b = gcd a b = 1

let phi n =
  let rec count_coprime acc d =
    if d < n then count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  if n = 1 then 1 else count_coprime 0 1

let%test _ = phi 10 = 4
let%test _ = phi 13 = 12

let factors n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then d :: aux d (n / d)
    else aux (d + 1) n
  in
  aux 2 n

let%test _ = factors 315 = [ 3; 3; 5; 7 ]

let factors_2 n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then
      match aux d (n / d) with
      | (h, n) :: t when h = d -> (h, n + 1) :: t
      | l -> (d, 1) :: l
    else aux (d + 1) n
  in
  aux 2 n

let rec pow n p = if p < 1 then 1 else n * pow n (p - 1)

let phi_improved n =
  let rec aux acc = function
    | [] -> acc
    | (p, m) :: t -> aux ((p - 1) * pow p (m - 1) * acc) t
  in
  aux 1 (factors_2 n)

let%test _ = phi_improved 10 = 4
let%test _ = phi_improved 13 = 12
