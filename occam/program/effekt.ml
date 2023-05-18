open Effect
open Effect.Deep

type 'a t += Recur : 'a -> 'b t

let recur a = perform (Recur a)

let rec loop : 'a -> ('a -> 'b) -> 'b =
 fun init fn ->
  try_with fn init
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Recur value ->
              Some (fun (_ : (a, _) continuation) -> loop (Obj.magic value) fn)
          | _ -> None);
    }

let fact i =
  loop (i, 1) (fun (iter, acc) ->
      if iter > 0 then recur (iter - 1, iter * acc, 1) else acc)

let () = print_int (fact 10)
