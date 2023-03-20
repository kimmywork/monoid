open Ng_lib.Stupid

let () = print_string (show_context pp_obj (eval (read_line ())))
