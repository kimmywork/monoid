exception Not_implemented
exception Unexpected_json

val extract_json :
  Yojson.Safe.t -> Yojson.Safe.t * Yojson.Safe.t * Yojson.Safe.t

val to_error : string -> Yojson.Safe.t
val to_result : Yojson.Safe.t option -> string -> string -> Yojson.Safe.t

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

module MakeServer (Svc : SERVICE_DESCRIPTOR) : SERVER
module MakeClient (Svc : SERVICE_DESCRIPTOR) (Clt : CLIENT_CONNECTOR) : CLIENT

val dispatch_service :
  string ->
  string ->
  (module SERVER) list ->
  Yojson.Safe.t ->
  Yojson.Safe.t option
