exception AstError

type identity = string [@@deriving show]

and expr =
  | IntegerValue of int
  | StringValue of string
  | BooleanValue of bool
  | UnitLiteral (*  `()` *)
  | WildcardLiteral (* `_` *)
  | SpreadLiteral (* `...` *)
  | ArrayLiteral of expr list
  | TupleLiteral of expr list
  | IdentityExpr of identity
  | Assignment of string * expr
  | BinaryOperation of string * expr * expr
  | AccessorExpr of expr * identity
  | ApplicationExpr of expr * expr list
  | IfElseExpr of expr * expr * expr
  | LoopCollectExpr of expr * expr
  | LetBindingExpr of (binding_pattern * expr) list * expr
  | FunctionLiteral of binding_pattern list * fundef_body
[@@deriving show]

and argument = ApplyingArgument of expr | PartialApplyArgument of expr
[@@deriving show]

and stmt = SimpleStatement of expr | ReturnStatement of expr [@@deriving show]

and fundef_body = FunStmtsBody of stmt list | FunExprBody of expr
[@@deriving show]

and binding_pattern =
  | DirectBinding of identity
  | DestructTupleBinding of identity option * binding_pattern list
  | ListBinding of binding_pattern list
  | ObjectBinding of (identity * binding_pattern) list
  | SpreadBinding of identity option
  | WildcardBinding
  | UnitBinding
  | AnnotatedBinding of identity * type_annotation
[@@deriving show]

and type_paramenter =
  | SimpleTypeParameter of identity
  | ParametricTypeParameter of identity * type_paramenter list
  | PlaceHolderParameter
[@@deriving show]

and func_signature = type_paramenter list * type_annotation list

and type_annotation =
  | SimpleTypeAnn of identity
  | ParamerticTypeAnn of identity * type_annotation list
  | FuncSignature of func_signature
  | ListTypeAnn of type_annotation
  | TupleTypeAnn of type_annotation list
  | PlaceHolderAnn
  | UnitTypeAnn
[@@deriving show]

and type_definition_body =
  | TypeDeclaration
  | TypeAlias of type_annotation
  | DataTypeDefinition of type_annotation list
  | RecordDataTypeDefinition of (identity * type_annotation) list
  | SumTypeDefinition of (identity * type_annotation option) list
[@@deriving show]

and concept_body_element =
  | ConceptTypeDeclaration of type_definition
  | ConceptFunDeclaration of signature_definition
  | ConceptValDeclaration of value_definition
  | ConceptDeclaration
[@@deriving show]

and impl_body_element =
  | ImplFunDef of function_definition
  | ImplTypeDef of type_definition
  | ImplValBinding of value_definition
  | ImplDeclaration
[@@deriving show]

and signature_definition = SignatureDefinition of identity * type_annotation

and type_definition =
  | TypeDefinition of identity * type_paramenter list * type_definition_body

and function_definition =
  | FunctionDefinition of identity * binding_pattern list * fundef_body

and value_definition =
  | ValueBinding of (binding_pattern * expr) list
  | ValueDeclaration of identity * type_annotation

and definition =
  | TypeDef of type_definition
  | ValueDef of value_definition
  | ConceptDef of identity * type_paramenter list * concept_body_element list
  | ImplDef of identity * type_annotation * impl_body_element list
  | SignatureDef of signature_definition
  | FunDef of function_definition
[@@deriving show]

and program_element = Definition of definition | Statement of stmt
[@@deriving show]

and program = Program of program_element list [@@deriving show]

let as_string = show_program
let expr_as_string = show_expr

module Pred = struct
  let is_definition = function Definition _ -> true | _ -> false
end

module Name = struct
  let mangle_sequence = ref 0

  let mangling_name () =
    let () = mangle_sequence := !mangle_sequence + 1 in
    Printf.sprintf "_V%02x" !mangle_sequence

  let mangling_index_at index =
    let () = mangle_sequence := !mangle_sequence + 1 in
    Printf.sprintf "_%iI%02x" index !mangle_sequence
end
