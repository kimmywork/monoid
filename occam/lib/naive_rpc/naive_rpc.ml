exception Not_implemented
exception Unexpected_json

let extract_json (json : Yojson.Safe.t) =
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

let to_error message : Yojson.Safe.t =
  `Assoc [ ("status", `String "error"); ("message", `String message) ]

let to_result (result : Yojson.Safe.t option) svc mtd : Yojson.Safe.t =
  match result with
  | Some obj ->
      `Assoc
        [
          ("service", `String svc);
          ("method", `String mtd);
          ("payload", obj);
          ("status", `String "success");
        ]
  | None -> to_error "Unexpected result."

module type MESSAGE = sig
  type t

  val from_json : Yojson.Safe.t -> t option
  val to_json : t -> Yojson.Safe.t
end

module type METHOD_SIGNATURE = sig
  module Req : MESSAGE
  module Res : MESSAGE

  val name : string
end

module type METHOD = sig
  module Sig : METHOD_SIGNATURE

  val invoke : Sig.Req.t -> Sig.Res.t
end

module type SERVICE_DESCRIPTOR = sig
  val name : string
  val methods : (module METHOD) list
end

module type SERVER = sig
  module Svc : SERVICE_DESCRIPTOR

  val invoke : string -> Yojson.Safe.t -> Yojson.Safe.t option
end

module type SERVER_LISTENER = sig
  val bind : string -> unit
  val serve : (module SERVER) list -> unit
  val shutdown : unit -> unit
end

module type CLIENT_CONNECTOR = sig
  val connect : string -> unit
  val send : string -> string -> Yojson.Safe.t -> Yojson.Safe.t
  val close : unit -> unit
end

module type CLIENT = sig
  module Svc : SERVICE_DESCRIPTOR
  module Clt : CLIENT_CONNECTOR

  val invoke : string -> Yojson.Safe.t -> Yojson.Safe.t
end

module MakeServer (Svc : SERVICE_DESCRIPTOR) : SERVER = struct
  module Svc = Svc

  let invoke mtd payload =
    let rec invoke_method (methods : (module METHOD) list) =
      match methods with
      | m :: rest ->
          let module M = (val m : METHOD) in
          if M.Sig.name = mtd then
            let req = M.Sig.Req.from_json payload in
            req |> Option.map M.invoke |> Option.map M.Sig.Res.to_json
          else invoke_method rest
      | [] -> None
    in
    invoke_method Svc.methods
end

module MakeClient (Svc : SERVICE_DESCRIPTOR) (Clt : CLIENT_CONNECTOR) : CLIENT =
struct
  module Svc = Svc
  module Clt = Clt

  let invoke = Clt.send Svc.name
end

let rec dispatch_service svc mtd svcs payload =
  match svcs with
  | s :: rest ->
      let module Srv = (val s : SERVER) in
      if Srv.Svc.name = svc then Srv.invoke mtd payload
      else dispatch_service svc mtd rest payload
  | [] -> None
