%{
open Bsyntax
let addtyp x = (x, Btype.gentyp ())
%}

%token <int> INT
%token <float> FLOAT
%token <Id.t> IDENT
%token <Id.t> LABEL
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
%token NIL

%type <Bsyntax.toplevel> toplevel
%start toplevel
%%

toplevel:
| fundefs
    { Fundefs([$1]) }

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
| IDENT
    { Var($1) }
| LABEL
    { Var($1) } /* Make a special label function ? */
| ADD IDENT IDENT /* addition */
    { Add($2, $3) }
| SUB IDENT IDENT
    { Sub($2, $3) }
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
| NIL
    { [] }
| IDENT
    { [$1] }
| IDENT formal_args
    { $1 :: $2 }

ident_or_imm:
| INT
    { Int($1) }
| IDENT
    { Var($1) }