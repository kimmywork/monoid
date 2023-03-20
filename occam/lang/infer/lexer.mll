{
    open Parser

   module Slex = String_lexer
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t']+

let newline = '\r' | '\n' | "\r\n"

rule token = parse
    | whitespace { token lexbuf }
    | "from" { FROM }
    | "import" { IMPORT }
    | "type" { TYPE }
    | "mutable" { MUTABLE }
    | "const" { CONST }
    | "optional" { OPTIONAL }
    | "deprecated" { DEPRECATED }
    | "alias" { ALIAS }
    | "abstract" { ABSTRACT }
    | "=" { EQ }
    | "string" { T_STRING }
    | "number" { T_NUMBER }
    | "object" { T_OBJECT }
    | "any" { T_ANY }
    | "array" { T_ARRAY }
    | "union" { T_UNION }
    | "enum" { T_ENUM }
    | "null" { T_NULL }
    | "map" { T_MAP }
    | "service" { SERVICE }
    | "option" { OPTION }
    | "?" { QUESTION_MARK }
    | "," { COMMA }
    | ":" { COLUMN }
    | '('  { L_PAREN }
    | ')'  { R_PAREN }
    | '['  { L_SQUARE }
    | ']'  { R_SQUARE }
    | '{'  { L_CURLY }
    | '}'  { R_CURLY }
    | '<'  { LESS }
    | '>'  { MORE }
    | '@' { AT_SIGN }
    | '.' { DOT }
    | '"' { STRING (Slex.wrap_without_quote Slex.string lexbuf) }
    | id as lxm { IDENTIFIER(lxm) }
    | _ { token lexbuf }
    | eof  { EOF }

{

let expect_lex text tok = token (Lexing.from_string text) = tok

let%test _ = expect_lex "from" (FROM)
    && expect_lex "import" (IMPORT)
    && expect_lex "type" (TYPE)
    && expect_lex "mutable" (MUTABLE)
    && expect_lex "const" (CONST)
    && expect_lex "deprecated" (DEPRECATED)
    && expect_lex "optional" (OPTIONAL)
    && expect_lex "alias" (ALIAS)
    && expect_lex "abstract" (ABSTRACT)
    && expect_lex "string" (T_STRING)
    && expect_lex "number" (T_NUMBER)
    && expect_lex "object" (T_OBJECT)
    && expect_lex "any" (T_ANY)
    && expect_lex "array" (T_ARRAY)
    && expect_lex "union" (T_UNION)
    && expect_lex "enum" (T_ENUM)
    && expect_lex "null" (T_NULL)
    && expect_lex "service" (SERVICE)
    && expect_lex "option" (OPTION)
    && expect_lex "map" (T_MAP)
    && expect_lex "\"abc\\n\"" (STRING ("abc\n"))

open Schema


let enable_trace = false

let try_parse text =
    let _ = Parsing.set_trace true in
    let _ = schema token (Lexing.from_string text)
    in true

let trace_parse text =
    let _ = Parsing.set_trace true in
    let res = schema token (Lexing.from_string text) in
    if enable_trace then
        print_string (as_string res);
    true

let expect_parse text ast =
    let _ = Parsing.set_trace true in
    let res = schema token (Lexing.from_string text) in
    if enable_trace then print_string (as_string res);
    res = ast

let%test _=trace_parse "from a import b, c, d"

let%test _=trace_parse "type a = mutable b"

let%test _=trace_parse {|
type a = {
    a: number,
    b: string,
    c: union<int, null>,
    d: [number]?
}
|}

let%test _=trace_parse {|
from deps import Dep

type a = number

type b = string

type Phone = {
    area_code: string,
    number: string,
    extension: string
}

type Name = {
    @alias(title)
    prefix: string?,
    @alias(give_name)
    first_name: string,
    middle_name: string?,
    @alias(sur_name)
    @alias(family_name)
    last_name: string,
    suffix: string?
}

type Concat = mutable {
    emails: [{ tag: string, email: string }],
    phones: [{ tag: string, phone: union<string, Phone> }]
}

type Party = {
    nickname: string,
    names: {
        legal: Name,
        prefered: Name,
        foreign: [{ tag: string, name: Name }]
    },

    contact: Contact,

    tags: [string] @mutable,
    extensions: any
        @deprecated
        @alias(attrs)
}

|}


let%test _=trace_parse {|
type a = enum {
    FIRST,
    SECOND,
    THIRD
}

type b = {
    @deprecated
    field_type: enum { AGE, NAME } @const,
    value: string
}
|}


let%test _=trace_parse {|

type unit = ()

type structured = {
    fields: [(string, number)?]
}

|}

let%test _=trace_parse {|

from a import b

from a.b import c

from a.b.c import d, e, f

from .a import b, c

from ..a.b import c, d

from .....a import b, c, d

|}


let%test _= trace_parse {|
from a import b

service A {
    hello (HellRequest): HelloResponse
    echo (EhoRequest): EchoResponse
}
|}


let%test _= trace_parse {|
option python.package = "play.occam.rpc"

from a import b

option ocaml.module = "Occam_rpc"
|}

let%test _= trace_parse {|

type int_string_map = map[int]string

type string_int_map = map<string, int>

|}

}
