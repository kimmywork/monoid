{

type error =
    | Unterminated_string

exception Error of error

let error e = raise (Error(e))


let string_buffer = Buffer.create 256

let reset_string_buffer () = Buffer.reset string_buffer
let get_buffered_string () = Buffer.contents string_buffer

let store_char ch = Buffer.add_char string_buffer ch
let store_utf8_char ch = Buffer.add_utf_8_uchar string_buffer ch
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

let is_in_string = ref false

let in_string () = !is_in_string
let start_string () = is_in_string := true
let end_string () = is_in_string := false

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_hexdecimal_code lexbuf n =
    Char.chr (num_value lexbuf ~base:16 ~first:n ~last:(n+1) )

let uchar_for_uchar_escape lexbuf =
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let _ = last - first + 1 in
  let cp = num_value lexbuf ~base:16 ~first ~last in
    if Uchar.is_valid cp then Uchar.unsafe_of_int cp else assert false


let wrap_string_lexer f lexbuf =
    reset_string_buffer();
    let _ = f lexbuf in
    get_buffered_string ()

let wrap_without_quote f lexbuf =
    reset_string_buffer();
    is_in_string := true;
    let _ = f lexbuf in
    get_buffered_string ()


type char_t = [`Ch of char |`Utf8Ch of Uchar.t]

}


let newline = '\r' | '\n' | "\r\n"

let whitespace = [' ' '\t']

let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']


rule string = parse
  | '"' { if in_string()
            then begin is_in_string := false; lexbuf.lex_start_p end
            else begin is_in_string := true; string lexbuf end
        }
  | '\\' newline (whitespace *) { store_lexeme lexbuf; string lexbuf }
  | '\\' ([ '\\' '\'' '"' 'n' 't' 'b' 'r' ' ' ] as c)
      { store_char (char_for_backslash c); string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_char (char_for_hexdecimal_code lexbuf 2); string lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
        { store_utf8_char (uchar_for_uchar_escape lexbuf);
          string lexbuf }
  | newline
        {  store_lexeme lexbuf; string lexbuf }
  | eof { is_in_string := false; error Unterminated_string }
  | (_ as c)
        { store_char c; string lexbuf }

and character = parse
  | '\'' { character_without_quote lexbuf }

and character_without_quote = parse
   '\\' ([ '\\' '\'' '"' 'n' 't' 'b' 'r' ' ' ] as c) '\''
    { `Ch (char_for_backslash c) }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] '\''
    { `Ch (char_for_hexdecimal_code lexbuf 2) }
  | (_ as c) '\'' { (`Ch c) }
  | '\\' 'u' '{' hex_digit+ '}' '\''
     { (`Utf8Ch (uchar_for_uchar_escape lexbuf)) }

{

let trace_lexing_string text =
    let result = wrap_string_lexer string (Lexing.from_string text) in
    let _ = print_string result in
    true

let assert_lex_string_eq text str =
    let result = wrap_string_lexer string (Lexing.from_string text) in
    let _ = print_string result in
    result = str


let%test _ = trace_lexing_string "\"abc\""

let%test _ = assert_lex_string_eq "\"\\n\"" "\n"
let%test _ = assert_lex_string_eq "\"abc\"" "abc"

let assert_lex_char_eq text ch =
    let result = character (Lexing.from_string text) in
    result = ch

let%test  _= assert_lex_char_eq "'a'" (`Ch 'a')
let%test  _= assert_lex_char_eq "'\\x41'" (`Ch '\x41')
let%test  _= assert_lex_char_eq "'\\n'" (`Ch '\n')

let parse_file_by_line filename =
    let file = In_channel.open_text filename in
    let rec parse_lines ch = match In_channel.input_line ch with
    | Some text -> (trace_lexing_string text) && (parse_lines ch)
    | None -> true
    in
    parse_lines file

let%test _ = parse_file_by_line "example/simple.str"
}
