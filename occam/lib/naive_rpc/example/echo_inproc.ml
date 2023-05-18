open Echo_service
open Naive_rpc
open Naive_inproc

let () =
  let module InProcListener = InProcServerListener () in
  let () = InProcListener.bind "5555" in
  let () =
    InProcListener.serve
      [ (module MakeServer (Echo_service_of (Echo_service_impl)) : SERVER) ]
  in
  let module InProcConnector = InProcClientConnector () in
  let () = InProcConnector.connect "5555" in
  let module Echo = Echo_client (InProcConnector) in
  Echo.second Second_request.{ message = "Hello"; age = 42 }
  |> Option.map (fun Echo_message.{ message } -> message)
  |> Option.map print_endline |> Option.value ~default:();
  InProcConnector.close ()
