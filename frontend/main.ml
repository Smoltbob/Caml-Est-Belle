let print_ast l =
  let s = (Parser.exp Lexer.token l) in
  (*print_string (Syntax.to_string s); print_newline ()*)
  (*print_string (Syntax.to_string (Knormal.syn_of_knorm  (Knormal.knormal s))); print_newline ()*)
  print_string (Knormal.k_to_string (Knormal.knormal s)); print_newline ()

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
