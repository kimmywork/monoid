type t = schema
and schema = declaration list

and declaration =
  | ImportDeclaration of path * id list
  | TypeDeclaration of id * modifier list * type_body
  | ServiceDeclaration of id * method_decl list
  | OptionDeclaration of path * string

and id = Identifier of string
and path = ParentAccess of path | Path of id list
and modifier = Mutable | Const | Optional | Deprecated | Alias of id

and type_body =
  | DirectType of id
  | AnyType
  | NullType
  | UnionType of type_body list
  | ArrayType of type_body
  | RecordType of (id * type_body) list
  | EnumType of id list
  | TupleType of type_body list
  | StringType
  | NumberType
  | AbstractType
  | ObjectType
  | WithModifier of type_body * modifier list
  | MappingType of type_body * type_body

and method_decl = MethodDeclaration of id * id * id

val as_string : schema -> string
val unwrap : id -> string
val show_type_body : type_body -> string
