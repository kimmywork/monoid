module Text = struct
  let append text buf =
    Buffer.add_string buf text;
    buf

  let endline buf = Buffer.add_char buf '\n'

  let rec indent level buf =
    match level with
    | 0 -> buf
    | x when x > 0 -> indent (level - 1) (buf |> append "  ")
    | _ -> raise (Invalid_argument "indent level must greater or eq 0")

  let around ?(level = 0) text_begin text_end fn buf =
    buf |> indent level |> append text_begin |> endline;
    fn buf;
    buf |> indent level |> append text_end |> endline
end
