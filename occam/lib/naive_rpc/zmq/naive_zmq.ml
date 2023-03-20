open Naive_rpc

exception Unexpected_json

let extract_response (json : Yojson.Safe.t) =
  let rec get_field assoc_list key =
    match assoc_list with
    | (k, v) :: _ when key = k -> v
    | (_, _) :: rest -> get_field rest key
    | [] -> raise Unexpected_json
  in
  match json with
  | `Assoc assoc_list ->
      ( get_field assoc_list "service",
        get_field assoc_list "method",
        get_field assoc_list "result" )
  | _ -> raise Unexpected_json

module ZmqClientConnector () : CLIENT_CONNECTOR = struct
  let context = Zmq.Context.create ()
  let responder = Zmq.Socket.create context Zmq.Socket.req
  let connect target = Zmq.Socket.connect responder target

  let send svc mtd obj =
    let () =
      Zmq.Socket.send responder
        (Yojson.Safe.to_string
           (`Assoc
             [
               ("service", `String svc);
               ("method", `String mtd);
               ("payload", obj);
             ]))
    in
    let response = Zmq.Socket.recv responder in
    let extracted = extract_response (Yojson.Safe.from_string response) in
    match extracted with _, _, resp -> resp

  let close () =
    Zmq.Socket.close responder;
    Zmq.Context.terminate context
end

let extract_payload (json : Yojson.Safe.t) =
  let rec get_field assoc_list key =
    match assoc_list with
    | (k, v) :: _ when key = k -> v
    | (_, _) :: rest -> get_field rest key
    | [] -> raise Unexpected_json
  in
  match json with
  | `Assoc assoc_list ->
      ( get_field assoc_list "service",
        get_field assoc_list "method",
        get_field assoc_list "payload" )
  | _ -> raise Unexpected_json

let rec dispatch_service svc mtd svcs payload =
  match svcs with
  | s :: rest ->
      let module Srv = (val s : SERVER) in
      if Srv.Svc.name = svc then Srv.invoke mtd payload
      else dispatch_service svc mtd rest payload
  | [] -> None

let to_error message : Yojson.Safe.t =
  `Assoc [ ("status", `String "error"); ("message", `String message) ]

let to_result (result : Yojson.Safe.t option) svc mtd : Yojson.Safe.t =
  match result with
  | Some obj ->
      `Assoc
        [
          ("service", `String svc);
          ("method", `String mtd);
          ("result", obj);
          ("status", `String "success");
        ]
  | None -> to_error "Unexpected result."

let respond_error ?(message = "error") responder =
  Zmq.Socket.send responder (Yojson.Safe.to_string (to_error message))

module ZmqServerListener () : SERVER_LISTENER = struct
  let context = Zmq.Context.create ()
  let responder = Zmq.Socket.create context Zmq.Socket.rep
  let bind target = Zmq.Socket.bind responder target

  let serve svcs =
    while true do
      let request = Zmq.Socket.recv responder in
      Printf.printf "Received request: [%s]\n%!" request;
      let request_object = Yojson.Safe.from_string request in
      let payload = extract_payload request_object in
      match payload with
      | `String svc, `String mtd, payload ->
          let result = dispatch_service svc mtd svcs payload in
          Zmq.Socket.send responder
            (Yojson.Safe.to_string (to_result result svc mtd))
      | _, _, _ ->
          respond_error responder ~message:"Cannot identitfy service/method"
    done

  let shutdown () =
    Zmq.Socket.close responder;
    Zmq.Context.terminate context
end
