let print_arm l =
  let s = (Bparser.toplevel Blexer.token l) in
  print_string (Barmspillgenerator.toplevel_to_arm s); print_newline ()

let file f = 
    let inchan = open_in f in
    try
        print_arm (Lexing.from_channel inchan);
    close_in inchan
    with e -> (close_in inchan; raise e)

let () = 
    let files = ref [] in
    Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
