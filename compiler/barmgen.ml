(** This module is used to test the backend only. It takes an .asml file as input, parses it and returns ARM code
 *)

(** Calls toplevel_to_arm on the ast of an asml program to generate ARM code.
 @param l The output of the lexer for the program
 *)
let print_arm l =
  let s = (Bparser.toplevel Blexer.token l) in
  print_string (Barmspillgenerator.toplevel_to_arm s); print_newline ()

(** Opens the *.asml file and parses it in order to generate ARM 
    @param f The *.asml file *)
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
