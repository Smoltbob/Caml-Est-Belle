let print_arm l =
  let s = (Parser.toplevel Lexer.token l) in
  Barmgenerator.toplevel_to_arm s

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
