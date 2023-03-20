open Echo_service
open Naive_rpc
open Naive_http

exception Unexpected_input

let () =
  let t = read_line () in
  match t with
  | "server" ->
      let module HttpListener = HttpServerListener () in
      let () = HttpListener.bind "5555" in
      HttpListener.serve
        [ (module MakeServer (Echo_service_of (Echo_service_impl)) : SERVER) ]
  | "client" ->
      let module HttpConnector = HttpClientConnector () in
      let () = HttpConnector.connect "http://localhost:5555" in
      let module Echo = Echo_client (HttpConnector) in
      let open Echo_message in
      Echo.echo { message = "Hello" }
      |> Option.map (fun { message } -> message)
      |> Option.map print_endline |> Option.value ~default:();
      HttpConnector.close ()
  | _ -> raise Unexpected_input
