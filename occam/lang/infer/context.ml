type module_content =
  | Import of string * string list
  | ExportedType of string * Schema.type_body
  | Service of string * service_content list

and service_content = Method of string * string * string

let of_schema _ = []
