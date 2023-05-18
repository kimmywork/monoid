open Schema
module G = Generator

module Snippet = struct
  let capitalize = String.capitalize_ascii
  let message_type = capitalize
  let method_signature mtd = Printf.sprintf "%s_signature" (capitalize mtd)
  let method_stub mtd = Printf.sprintf "%s_stub" (capitalize mtd)
  let method_impl mtd = Printf.sprintf "%s_impl" (capitalize mtd)
  let method_functor mtd = Printf.sprintf "%s_method" (capitalize mtd)
  let svc_intf svc = Printf.sprintf "%s_interface" (capitalize svc)
  let svc_name svc = Printf.sprintf "%s_service_of" (capitalize svc)
  let svc_dummy svc = Printf.sprintf "%s_dummy_impl" (capitalize svc)
  let svc_client svc = Printf.sprintf "%s_client" (capitalize svc)
  let open_naive_rpc = {|open Naive_rpc|}
  let deriving_yojson = {|[@@deriving yojson]|}
  let fn_from_json = {|let from_json json = of_yojson json |> Result.to_option|}
  let fn_to_json = {|let to_json = to_yojson|}
  let sig_stub_invoke = {|val invoke: Req.t -> Res.t|}
  let fn_method_invoke = {|let invoke = Impl.invoke|}
  let fn_dummy_invoke = {|let invoke _ = raise Not_implemented|}
  let method_impl_impl = {|module Impl = Impl|}
  let struct_of name = Printf.sprintf "module %s = struct" name
  let signature_of name = Printf.sprintf "module type %s = sig" name
  let let_name_eq name = Printf.sprintf "let name = \"%s\"" name
  let module_req req = Printf.sprintf "module Req = %s" (message_type req)
  let module_res res = Printf.sprintf "module Res = %s" (message_type res)

  let make_client svc =
    Printf.sprintf "module Client = MakeClient(%s(%s))(Connector)"
      (svc_name svc) (svc_dummy svc)

  let open_signature name = Printf.sprintf "open %s" (method_signature name)

  let method_functor_name name =
    Printf.sprintf "%s(Impl: %s): METHOD" (name |> method_functor)
      (name |> method_stub)

  let method_impl_of_stub name =
    Printf.sprintf "module %s: %s" (method_impl name) (method_stub name)

  let module_sig_eq_signature name =
    Printf.sprintf "module Sig = %s" (name |> method_signature)

  let method_initiation name =
    let method_functor_name = name |> method_functor in
    let method_impl_name = name |> method_impl in
    Printf.sprintf "(module %s(Impl.%s):METHOD)" method_functor_name
      method_impl_name

  let service_functor_name name =
    Printf.sprintf "%s(Impl: %s)" (name |> svc_name) (name |> svc_intf)

  let svc_dummy_impl_intf name =
    Printf.sprintf "%s: %s" (svc_dummy name) (svc_intf name)

  let service_client_functor_name name =
    Printf.sprintf "%s (Connector: CLIENT_CONNECTOR)" (svc_client name)

  let method_client_invocation_header name = Printf.sprintf "let %s req =" name

  let method_client_invocation name =
    let sign = method_signature name in
    Printf.sprintf
      "%s.Res.from_json (Client.invoke %s.name (%s.Req.to_json req))" sign sign
      sign

  let field_name_of_type name typ = Printf.sprintf "%s: %s;" name typ
end

open Occam_ext.Buf.Text
open Occam_ext.Fn

let method_name = function MethodDeclaration (Identifier id, _, _) -> id

let within_module_type ?(level = 0) typ name fn =
  around ~level (name |> Snippet.capitalize |> typ) "end" fn

let within_struct ?(level = 0) = within_module_type ~level Snippet.struct_of

let within_signature ?(level = 0) =
  within_module_type ~level Snippet.signature_of

let type_repr = function
  | DirectType d -> d |> unwrap (* TODO: reference of defined types *)
  | StringType -> "string"
  | t ->
      G.unsupported (Printf.sprintf "unsupported type of %s" (show_type_body t))

let gen_fields fields =
  let gen_field identifier type_body =
    indent 2
    *> append
         (Snippet.field_name_of_type (unwrap identifier) (type_repr type_body))
    *> endline
  in
  around ~level:1 "type t = {"
    (Printf.sprintf "} %s" Snippet.deriving_yojson)
    (bind_into (List.map (uncurry gen_field) fields))

let gen_record name records =
  within_struct name
    (bind_into
       [
         gen_fields records;
         endline;
         indent 1 *> append Snippet.fn_from_json *> endline;
         endline;
         indent 1 *> append Snippet.fn_to_json *> endline;
       ])

let gen_type name _ body =
  match body with
  | RecordType records -> gen_record name records
  | _ -> G.unsupported "Only accept record type"

let gen_method_signature name req res =
  within_struct
    (name |> Snippet.method_signature)
    (bind_into
       [
         indent 1 *> append (Snippet.module_req req) *> endline;
         indent 1 *> append (Snippet.module_res res) *> endline;
         indent 1 *> append (Snippet.let_name_eq name) *> endline;
       ])

let gen_method_stub name =
  within_signature
    (name |> Snippet.method_stub)
    (bind_into
       [
         indent 1 *> append (Snippet.open_signature name) *> endline;
         indent 1 *> append Snippet.sig_stub_invoke *> endline;
       ])

let gen_method_functor name =
  within_struct
    (Snippet.method_functor_name name)
    (bind_into
       [
         indent 1 *> append Snippet.method_impl_impl *> endline;
         indent 1 *> append (Snippet.module_sig_eq_signature name) *> endline;
         indent 1 *> append Snippet.fn_method_invoke *> endline;
       ])

let gen_method (mtd : method_decl) =
  let (MethodDeclaration (name, req, res)) = mtd in
  let name, req, res = (name |> unwrap, req |> unwrap, res |> unwrap) in
  bind_into
    [
      gen_method_signature name req res;
      gen_method_stub name;
      gen_method_functor name;
    ]

let gen_service_interface name methods =
  within_signature (name |> Snippet.svc_intf)
    (bind_into
       (methods
       |> List.map (fun m ->
              indent 1
              *> append (Snippet.method_impl_of_stub (m |> method_name))
              *> endline)))

let gen_service_dummy_impl name methods =
  within_struct
    (Snippet.svc_dummy_impl_intf name)
    (bind_into
       (methods
       |> List.map (fun m ->
              within_struct ~level:1
                (Snippet.method_impl (m |> method_name))
                (indent 2 *> append Snippet.fn_dummy_invoke *> endline))))

let gen_service_functor name methods =
  within_struct
    (name |> Snippet.service_functor_name)
    (bind_into
       [
         indent 1 *> append (name |> Snippet.let_name_eq) *> endline;
         around ~level:1 "let methods = [" "]"
           (bind_into
              (methods
              |> List.map (fun m ->
                     indent 2
                     *> append (Snippet.method_initiation (m |> method_name))
                     *> append ";" *> endline)));
       ])

let gen_service_client name methods =
  within_struct
    (Snippet.service_client_functor_name name)
    (bind_into
       [
         indent 1 *> append (Snippet.make_client name) *> endline;
         bind_into
           (methods
           |> List.map (fun m ->
                  bind_into
                    [
                      indent 1
                      *> append
                           (Snippet.method_client_invocation_header
                              (m |> method_name))
                      *> endline;
                      indent 2
                      *> append
                           (Snippet.method_client_invocation (m |> method_name))
                      *> endline;
                    ]));
       ])

let gen_service name methods =
  bind_into
    [
      bind_into (List.map gen_method methods);
      gen_service_interface name methods;
      gen_service_functor name methods;
      gen_service_dummy_impl name methods;
      gen_service_client name methods;
    ]

let gen_decl decl buffer =
  match decl with
  | Schema.TypeDeclaration (id, mods, body) ->
      gen_type (id |> unwrap) mods body buffer
  | Schema.ServiceDeclaration (id, mtds) ->
      gen_service (id |> unwrap) mtds buffer
  | _ -> G.unsupported "Unsupported entity"

let gen decls =
  let buffer = Buffer.create 256 in
  buffer
  |> bind_into
       [
         append Snippet.open_naive_rpc *> endline;
         bind_into (List.map gen_decl decls);
       ];
  buffer |> Buffer.to_bytes |> Bytes.to_string

let%test _ =
  let gened =
    gen
      (G.parse
         {|
type echo_request = {
  message: string
}

type echo_response = {
  message: string
}

service echo {
  echo(echo_request): echo_response
}
|})
  in
  gened
  = {|open Naive_rpc
module Echo_request = struct
  type t = {
    message: string;
  } [@@deriving yojson]

  let from_json json = of_yojson json |> Result.to_option

  let to_json = to_yojson
end
module Echo_response = struct
  type t = {
    message: string;
  } [@@deriving yojson]

  let from_json json = of_yojson json |> Result.to_option

  let to_json = to_yojson
end
module Echo_signature = struct
  module Req = Echo_request
  module Res = Echo_response
  let name = "echo"
end
module type Echo_stub = sig
  open Echo_signature
  val invoke: Req.t -> Res.t
end
module Echo_method(Impl: Echo_stub): METHOD = struct
  module Impl = Impl
  module Sig = Echo_signature
  let invoke = Impl.invoke
end
module type Echo_interface = sig
  module Echo_impl: Echo_stub
end
module Echo_service_of(Impl: Echo_interface) = struct
  let name = "echo"
  let methods = [
    (module Echo_method(Impl.Echo_impl):METHOD);
  ]
end
module Echo_dummy_impl: Echo_interface = struct
  module Echo_impl = struct
    let invoke _ = raise Not_implemented
  end
end
module Echo_client (Connector: CLIENT_CONNECTOR) = struct
  module Client = MakeClient(Echo_service_of(Echo_dummy_impl))(Connector)
  let echo req =
    Echo_signature.Res.from_json (Client.invoke Echo_signature.name (Echo_signature.Req.to_json req))
end
|}
