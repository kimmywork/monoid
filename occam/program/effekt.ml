open Effect
open Effect.Deep

type _ Effect.t += Xchg : int -> int t

type 'a status =
  | Complete of 'a
  | Suspended of { msg : int; cont : (int, 'a status) continuation }

let step (f : unit -> 'a) () : 'a status =
  match_with f ()
    {
      retc = (fun v -> Complete v);
      exnc = raise;
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Xchg msg ->
              Some (fun (cont : (a, _) continuation) -> Suspended { msg; cont })
          | _ -> None);
    }

let rec run_both a b =
  match (a (), b ()) with
  | Complete va, Complete vb -> (va, vb)
  | Suspended { msg = m1; cont = k1 }, Suspended { msg = m2; cont = k2 } ->
      run_both (fun () -> continue k1 m2) (fun () -> continue k2 m1)
  | _ -> failwith "Improper sync"

type _ Effect.t += Fork : (unit -> unit) -> unit t | Yield : unit t

let fork f = perform (Fork f)
let yield () = perform Yield
let xchg v = perform (Xchg v)

let run (main : unit -> unit) : unit =
  let exchanger = ref None in
  let run_q = Queue.create () in
  let enqueue k v =
    let task () = continue k v in
    Queue.push task run_q
  in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else
      let task = Queue.pop run_q in
      task ()
  in
  let rec spawn (f : unit -> unit) : unit =
    match_with f ()
      {
        retc = dequeue;
        exnc =
          (fun e ->
            print_endline (Printexc.to_string e);
            dequeue ());
        effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Yield ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k ();
                    dequeue ())
            | Fork f ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k ();
                    spawn f)
            | Xchg n ->
                Some
                  (fun (k : (int, unit) continuation) ->
                    match !exchanger with
                    | Some (n', k') ->
                        exchanger := None;
                        enqueue k' n;
                        continue k n'
                    | None ->
                        exchanger := Some (n, k);
                        dequeue ())
            | _ -> None);
      }
  in
  spawn main
