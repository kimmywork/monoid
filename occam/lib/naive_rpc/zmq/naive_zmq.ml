open Naive_rpc

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
    let extracted = extract_json (Yojson.Safe.from_string response) in
    match extracted with _, _, resp -> resp

  let close () =
    Zmq.Socket.close responder;
    Zmq.Context.terminate context
end

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
      let payload = extract_json request_object in
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
