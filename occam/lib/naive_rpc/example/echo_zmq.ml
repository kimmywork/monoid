open Echo_service
open Naive_rpc
open Naive_zmq

exception Unexpected_input

let () =
  let t = read_line () in
  match t with
  | "server" ->
      let module ZmqListener = ZmqServerListener () in
      let () = ZmqListener.bind "tcp://*:5555" in
      ZmqListener.serve
        [ (module MakeServer (Echo_service_of (Echo_service_impl)) : SERVER) ]
  | "client" ->
      let module ZmqConnector = ZmqClientConnector () in
      let () = ZmqConnector.connect "tcp://localhost:5555" in
      let module Echo = Echo_client (ZmqConnector) in
      let open Echo_message in
      Echo.echo { message = "Hello" }
      |> Option.map (fun { message } -> message)
      |> Option.map print_endline |> Option.value ~default:();
      ZmqConnector.close ()
  | _ -> raise Unexpected_input
