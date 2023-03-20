(** Capabilities for Generators to describe themselves *)
module Capability = struct
  type t =
    | SupportsTuple
    | SupportsUnionType
    | SupportsInlineRecords
    | SupportMutability
    | CaseInsensitive
    | SupportsDynamicType
    | SupportsServices
    | SupportsUserDefinedDirectives
    | SupportsMappingType
end

module type Gen = sig
  val gen : Schema.t -> string
  (** Main generation method *)
end

(** Translate text to {!type:Schema.schema} *)
let parse text = Parser.schema Lexer.token (Lexing.from_string text)

exception Not_implemented of string

let unsupported msg = raise (Not_implemented msg)
