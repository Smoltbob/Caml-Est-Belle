let print_ast l =
  print_string "Parsed asml AST :\n";
  let s = (Parser.toplevel Lexer.token l) in
  print_string (Syntax.to_string_top s); print_newline ();
  print_string "Generated ARM :\n";
  print_string (Syntax.to_arm_top s); print_newline ()

let file f = 
    let inchan = open_in f in
    try
        print_ast (Lexing.from_channel inchan);
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
