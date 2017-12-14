print_string "let _ ="

let rec gen c = match c with
    | LetRec() -> print_string "label:" "parameters:" "code:"
    | Let()
