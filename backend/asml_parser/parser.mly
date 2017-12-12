%{
open Syntax
let addtyp x = (x, Type.gentyp ())
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

%type <Syntax.t> toplevel
%start toplevel
%%

toplevel:
| fundefs
    { $1 }

fundefs:
|   LET UNDERSC EQUAL asmt 
    { $4 }

asmt:
|   LPAREN asmt RPAREN
    { $2 }
|   LET IDENT EQUAL exp IN asmt
    { Let($2, $4, $6) }
|   exp
    { $1 }   

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
    { Var($1) }
| ADD IDENT ident_or_imm /* addition */
    { Add($2, $3) }
| SUB IDENT ident_or_imm
    { Sub($2, $3) }
| EQUAL IDENT ident_or_imm
    { Eq($2, $3) }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

ident_or_imm:
| INT
    { Int($1) }
| IDENT
    { Var($1) }
