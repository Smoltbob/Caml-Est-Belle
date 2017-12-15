let print_ast l =
  let s = (Parser.exp Lexer.token l) in
  (* print_string (Knormal.k_to_string (Knormal.knormal s)); print_newline () *)
  (* print_string (Knormal.k_to_string (Reduction.reduc (Knormal.knormal s))); print_newline () *)
  (* print_string (Asmlgen.closure_to_asmlstring_main (Closure.clos (Reduction.reduc (Knormal.knormal s)))); print_newline () *)
  Asmlgen.asml_head (Closure.clos (Reduction.reduc (Knormal.knormal s)))

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
