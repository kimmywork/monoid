open Naive_rpc

module Connection = struct
  type t = (string, (module SERVER) list) Hashtbl.t

  let conns : t = Hashtbl.create 43

  let new_conn conn svcs =
    let connection = Hashtbl.find_opt conns conn in
    match connection with
    | Some _ -> Hashtbl.replace conns conn svcs
    | None -> Hashtbl.add conns conn svcs

  let get_conn conn = Hashtbl.find conns conn
end

module InProcServerListener () : SERVER_LISTENER = struct
  let connection = ref "default"
  let bind conn = connection := conn
  let serve svcs = Connection.new_conn !connection svcs
  let shutdown () = ()
end

module InProcClientConnector () : CLIENT_CONNECTOR = struct
  let svcs = ref []
  let connect conn = svcs := Connection.get_conn conn

  let send svc mtd body =
    let result = dispatch_service svc mtd !svcs body in
    match result with
    | Some obj -> obj
    | None -> `Assoc [ ("status", `String "error") ]

  let close () = ()
end
