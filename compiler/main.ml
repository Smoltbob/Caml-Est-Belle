open Ftype;;

let display_version = ref false
let type_check_only = ref false
let parse_only = ref false
let asml_only = ref false
let linear_scanning = ref false
let version = ref "Version: Fancy Camembert"
let output_file = ref "a.out"
let optimize = ref false

let catchfailwith funct x = try (funct x) with
    | Failure err -> (Printf.fprintf stdout "%s" err; exit 1)
    | _ -> funct x
(*
let writeInFile  file s=
    let input = ref "" in
    let oc = open_out  file in
        let s1= Fknormal.knormal s in
            input:= !input ^"\n**********knormal***********\n"^
            Fknormal.k_to_string s1;

        let s2 = Falphaconversion.alpha s1 in
            input:= !input ^"\n***********alpha**********\n"^
            Fknormal.k_to_string s2;

         let s3 = Fbeta.f s2 in
            input:= !input ^"\n************Fbeta.f*********\n"^
            Fknormal.k_to_string s3;

        let s4 =  Freduction.reduc s3 in
            input:= !input ^"\n***********Freduction.reduc**********\n"^
            Fknormal.k_to_string s4;

        let s5 = Finline.f s4 in
            input:= !input ^"\n************Finline.f*********\n"^
            Fknormal.k_to_string s5;

        let s6 =Fconstfold.f s5 in
            input:= !input ^"\n************Fconstfold.f*********\n"^
            Fknormal.k_to_string s6;

        let s7 = Felim.f s6 in
            input:= !input ^"\n************Felim.f*********\n"^
            Fknormal.k_to_string s7;

         let s8 =  Fclosure.clos_out s7 in
             input:= !input ^"\n***********Fclosure.clos_out**********\n"^
             Fclosure.clos_to_string s8;

        Printf.fprintf oc "%s" !input;
    close_out oc *)

let print_asml l =
    let s = Fparser.exp Flexer.token l in
    let c =
    if !optimize
    then
        Fclosure.clos_out
        (
        Freduction.reduc
        (
        Falphaconversion.alpha
        (
        Fknormal.knormal s
        )))
    else
        Fclosure.clos_out
        (
        Felim.f
        (
        Fconstfold.f
        (
            
        Finline.f
        (
        Freduction.reduc
        (
        Fbeta.f
        (
        Falphaconversion.alpha
        (
        Fknormal.knormal s
        )))))))
    in
    let prog = Fasmlgen.asml_head c in
    if !asml_only then
        Fasmlgen.toplevel_to_string prog
    else if !linear_scanning then
		(Bliveinterval.calcu_live_interval prog;
		let live_interval_s_ht = Bliveinterval.to_hashtbl !Bliveinterval.live_interval_s in
		let live_interval_e_ht = Bliveinterval.to_hashtbl !Bliveinterval.live_interval_e in
		Barmlineargenerator.toplevel_to_arm (Blinearscan.registeralloc prog live_interval_s_ht live_interval_e_ht))
    else
        Barmspillgenerator.toplevel_to_arm prog


let file fin fout =
    let inchan = open_in fin in
    let outchan = open_out fout in
    try
       if !type_check_only then
       begin
         let s = (Fparser.exp Flexer.token (Lexing.from_channel inchan)) in
         let env =  [("print_int" ,Fun( [Int] , Unit ));("print_float" ,Fun( [Float] , Unit ))] in
          let eq= Typechecking.genEquations env s Unit in
           Typechecking.unification eq

       end
     else
         begin
            Printf.fprintf outchan "%s" (print_asml (Lexing.from_channel inchan));
        end;
    close_in inchan;
    close_out outchan
    with e -> (close_in inchan; close_out outchan; raise e)

let () =
    let input_file = ref "" in
    let optionlist = [
        ("-o", Arg.Set_string output_file, "Specify output file (default a.out)");
        ("-v", Arg.Set display_version, "Display the version");
        ("-t", Arg.Set type_check_only, "Type check only");
        ("-p", Arg.Set parse_only, "Parse only");
        ("-asml", Arg.Set asml_only, "Output ASML only");
        ("-linear", Arg.Set linear_scanning, "Use linear scanning");
        ("-opt", Arg.Set optimize, "Use optimisation");
        ("-h", Arg.Unit (fun _ -> ()), "-o: output file\n-h: display help\n-v: display version\n-t: type check only\n-p: parse only\n-asml: output ASML")
    ] in

    Arg.parse
        optionlist
        (fun s -> input_file := s)
        "Mincaml compiler for ARMv7 architecture";


    if !display_version then
        print_endline !version;

    if !input_file = "" then
        failwith (Printf.sprintf "usage: %s filename" Sys.argv.(0))
    else
        file !input_file !output_file
