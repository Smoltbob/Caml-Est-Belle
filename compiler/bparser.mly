%{
open Bsyntax
let addtyp x = (x, Btype.gentyp ())
%}

%token <int> INT
%token <float> FLOAT
%token <Bid.t> IDENT
%token <Bid.t> LABEL
%token LPAREN
%token RPAREN
%token PLUS
%token EQUAL
%token FEQUAL
%token LE
%token FLE
%token GE
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token DOT
%token NEG
%token FNEG
%token MEM
%token FMUL
%token FDIV
%token FSUB
%token FADD
%token ASSIGN
%token ADD
%token SUB
%token CALL
%token NEW
%token NOP
%token APPLCLO
%token UNDERSC
%token EOF

%type <Bsyntax.toplevel> toplevel
%start toplevel
%%

toplevel:
| fundefs
    { Fundef([$1]) }

fundefs:
|   LET UNDERSC EQUAL asmt
    { Body($4) }

asmt:
|   LPAREN asmt RPAREN
    { $2 }
|   LET IDENT EQUAL exp IN asmt
    { Let($2, $4, $6) }
|   exp
    { Expression($1) }

exp:
| LPAREN exp RPAREN
    { $2 }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var($1) }
| LABEL
    { Var($1) } /* Make a special label function ? */
| NEG IDENT
    { Neg($2) }
| FNEG IDENT
    { Fneg($2) }
| FADD IDENT IDENT
    { Fadd($2, $3) }
| FSUB IDENT IDENT
    { Fsub($2, $3) }
| FMUL IDENT IDENT
    { Fmul($2, $3) }
| FDIV IDENT IDENT
    { Fdiv($2, $3) }
| ADD IDENT ident_or_imm /* addition */
    { Add($2, $3) }
| SUB IDENT ident_or_imm
    { Sub($2, $3) }
| EQUAL IDENT ident_or_imm
    { Eq($2, $3) }
| CALL LABEL formal_args
    { Call($2, $3) }
| NOP
    { Nop }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

formal_args:
| IDENT
    { Arg($1) }
/*| IDENT formal_args
    { Args($1, $2) }*/
/* Implement NIL | IDENT formal_args */


ident_or_imm:
| INT
    { Int($1) }
| IDENT
    { Var($1) }
