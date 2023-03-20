exception Not_implemented

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
