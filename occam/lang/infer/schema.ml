type t = schema
and schema = declaration list [@@deriving show]

and declaration =
  | ImportDeclaration of path * id list
  | TypeDeclaration of id * modifier list * type_body
  | ServiceDeclaration of id * method_decl list
  | OptionDeclaration of path * string
[@@deriving show]

and id = Identifier of string [@@deriving show]
and path = ParentAccess of path | Path of id list [@@deriving show]

and modifier = Mutable | Const | Optional | Deprecated | Alias of id
[@@deriving show]

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
[@@deriving show]

and method_decl = MethodDeclaration of id * id * id [@@deriving show]

let as_string = show
let unwrap = function Identifier id -> id
