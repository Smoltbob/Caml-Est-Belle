let print_asml l =
    let s = (Fparser.exp Flexer.token l) in
    (* print_string (Fknormal.k_to_string (Fknormal.knormal s)); print_newline () *)
    (* print_string (Fknormal.k_to_string (Freduction.reduc (Fknormal.knormal s))); print_newline () *)
    (* print_string (Fasmlgen.closure_to_asmlstring_main (Fclosure.clos (Freduction.reduc (Fknormal.knormal s)))); print_newline () *)
    Bsyntax.to_arm_top (Fasmlgen.asml_head (Fclosure.clos (Freduction.reduc (Fknormal.knormal s))))

let file f =
    let inchan = open_in f in
    try
        print_asml (Lexing.from_channel inchan);
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
