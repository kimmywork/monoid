%{
open Schema
%}

%token EOF


%token FROM
%token IMPORT
%token TYPE
%token MUTABLE
%token CONST
%token OPTIONAL
%token DEPRECATED
%token ALIAS
%token ABSTRACT
%token SERVICE
%token OPTION
%token <string> IDENTIFIER
%token EQ
%token T_STRING
%token T_NUMBER
%token T_OBJECT
%token T_ANY
%token T_ARRAY
%token T_UNION
%token T_ENUM
%token T_NULL
%token T_MAP
%token EXAMINATION_MARK
%token AT_SIGN
%token DOT
%token <string> STRING

%token L_PAREN
%token R_PAREN
%token L_CURLY
%token R_CURLY
%token L_SQUARE
%token R_SQUARE
%token LESS
%token MORE

%token COLUMN
%token COMMA
%token QUESTION_MARK
%token COMMENT

%start schema
%type <schema> schema

%%

schema:
    | decls=nonempty_list(declaration) EOF { decls }
    ;

declaration:
    | import_decl { $1 }
    | type_decl { $1 }
    | service_decl { $1 }
    | option_decl { $1 }
    ;

import_decl:
    | FROM path IMPORT separated_nonempty_list(COMMA, id)
    {
        ImportDeclaration ($2, $4)
    }
    ;

service_decl:
    | SERVICE id L_CURLY nonempty_list(method_decl) R_CURLY { ServiceDeclaration ($2, $4) }
    ;

path:
    | separated_nonempty_list(DOT, id) { Path $1 }
    | non_parental_path { $1 }
    | parental_path { $1 }
    ;


non_parental_path:
    | DOT separated_nonempty_list(DOT, id) { Path $2 }
    ;

parental_path:
    | DOT non_parental_path {  ParentAccess $2 }
    | DOT parental_path { ParentAccess $2 }

type_decl:
    | TYPE id EQ list(modifier) type_body
    {
        TypeDeclaration($2, $4, $5)
    }
    ;

modifier:
    | MUTABLE { Mutable }
    | CONST { Const }
    | OPTIONAL { Optional }
    | DEPRECATED { Deprecated }
    | ALIAS L_PAREN id R_PAREN { Alias $3 }
    ;

type_body:
    | id { DirectType $1 }
    | T_ANY { AnyType }
    | T_NULL { NullType }
    | T_NUMBER { NumberType }
    | T_STRING { StringType }
    | T_OBJECT { ObjectType }
    | ABSTRACT { AbstractType }
    | T_ARRAY LESS type_body MORE { ArrayType $3 }
    | T_UNION LESS separated_nonempty_list(COMMA, type_body) MORE
    {
        UnionType $3
    }
    | L_SQUARE type_body R_SQUARE { ArrayType $2 }
    | L_CURLY separated_list(COMMA, record_field) R_CURLY
    {
        RecordType $2
    }
    | type_body QUESTION_MARK { UnionType [$1; NullType]}
    | type_body nonempty_list(preceded(AT_SIGN, modifier))
    {
        WithModifier ($1, $2)
    }
    | T_ENUM L_CURLY separated_list(COMMA, id) R_CURLY
    {
        EnumType $3
    }
    | L_PAREN separated_list(COMMA, type_body) R_PAREN
    {
        TupleType $2
    }
    | T_MAP L_SQUARE type_body R_SQUARE type_body
    {
        MappingType ($3, $5)
    }
    | T_MAP LESS type_body COMMA type_body MORE
    {
        MappingType ($3, $5)
    }
    ;

record_field:
    | id COLUMN type_body { ($1, $3) }
    | keyword_as_id COLUMN type_body { ($1, $3) }
    | nonempty_list(preceded(AT_SIGN, modifier)) record_field { (fst ($2), WithModifier(snd($2), $1)) }
    ;

method_decl:
    | id L_PAREN id R_PAREN COLUMN id { MethodDeclaration ($1, $3, $6) }
    ;

option_decl:
    | OPTION path EQ text { OptionDeclaration ($2, $4) }
    ;

text:
    | STRING { $1 }

id:
    | IDENTIFIER { Identifier $1 }
    ;

keyword_as_id:
(* TODO: add more context keywords options *)
    | T_NUMBER { Identifier "number" }
    ;
