open Naive_rpc
open Lwt
open Cohttp_lwt_unix

module HttpServerListener () : SERVER_LISTENER = struct
  let port = ref 8080
  let bind port_str = port := int_of_string port_str

  let serve svcs =
    let callback _conn _ body =
      let lwt_body = body |> Cohttp_lwt.Body.to_string in
      lwt_body
      >|= (fun string_body ->
            let json_body =
              string_body |> Yojson.Safe.from_string |> extract_json
            in
            match json_body with
            | `String svc, `String mtd, payload ->
                to_result (dispatch_service svc mtd svcs payload) svc mtd
            | _, _, _ -> to_error "Cannot identitfy service/method")
      >|= Yojson.Safe.to_string
      >>= fun body -> Server.respond_string ~status:`OK ~body ()
    in
    let server =
      Server.create ~mode:(`TCP (`Port !port)) (Server.make ~callback ())
    in
    ignore (Lwt_main.run server)

  let shutdown () = ()
end

module HttpClientConnector () : CLIENT_CONNECTOR = struct
  let server_uri = ref "http://localhost:5555"
  let connect srv = server_uri := srv

  let send svc mtd body =
    let invoke () =
      let body =
        Cohttp_lwt.Body.of_string
          (Yojson.Safe.to_string
             (`Assoc
               [
                 ("service", `String svc);
                 ("method", `String mtd);
                 ("payload", body);
               ]))
      in
      Client.post ~body (Uri.of_string !server_uri) >>= fun (_, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun string_body ->
      let json_body = string_body |> Yojson.Safe.from_string |> extract_json in
      match json_body with _, _, resp -> resp
    in
    Lwt_main.run (invoke ())

  let close () = ()
end
