open Infer
open Occam_ext.Fn

let usage_msg = "infer_gen <file> [-l <language: ocaml, ...>] [-o <output>]"
let input_file = ref ""
let output_file = ref ""
let language = ref "ocaml"

let spec_list =
  [
    ("-o", Arg.Set_string output_file, "Set output file name");
    ("-l", Arg.Set_string language, "Set language type, default ocaml");
  ]

let generators = [ ("ocaml", (module Generator_ocaml : Generator.Gen)) ]

let find_generator language =
  match List.find_opt (fst *> String.equal language) generators with
  | Some (_, gen) -> gen
  | None -> raise (Invalid_argument ("Unsupported language: " ^ language))

let anon_fun filename = input_file := filename

let () =
  try
    Arg.parse spec_list anon_fun usage_msg;
    if not (Sys.file_exists !input_file) then
      raise (Invalid_argument ("file [" ^ !input_file ^ "] cannot be found!"));
    In_channel.with_open_text !input_file (fun input ->
        let module G = (val find_generator !language : Generator.Gen) in
        let generated = G.gen (Generator.parse (In_channel.input_all input)) in
        let callback ch = Out_channel.output_string ch generated in
        if !output_file = "" then callback Out_channel.stdout
        else Out_channel.with_open_text !output_file callback)
  with Invalid_argument msg ->
    print_endline ("Error: " ^ msg);
    exit 3
