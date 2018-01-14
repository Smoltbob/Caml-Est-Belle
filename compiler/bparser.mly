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
%token LAND
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
    { {name = ""; args = []; body = $4} }

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
| NEW ident_or_imm
    { New($2) }
| LABEL
    { Var($1) } /* Make a special label function ? */
| ADD IDENT ident_or_imm /* addition */ /* should be ADD IDENT ident_or_imm */
    { Add($2, $3) } /* should be ADD IDENT ident_or_imm */
| SUB IDENT ident_or_imm
    { Sub($2, $3) }
| LAND IDENT ident_or_imm
    { Land($2, $3) }
/* MEM LPAREN IDENT PLUS ident_or_imm RPAREN */
| MEM LPAREN IDENT PLUS ident_or_imm RPAREN DOT
    { MemAcc($3, $5) }
/* Should be MEM LPAREN IDENT PLUS ident_or_imm RPAREN ASSIGN IDENT */
| MEM LPAREN IDENT PLUS ident_or_imm RPAREN ASSIGN IDENT
    { MemAff($3, $5, $8) }
| IF IDENT EQUAL IDENT THEN asmt ELSE asmt /* should be ADD IDENT equal ident_or_imm THEN asmt ELSE asmt */
    { If($2, $4, $6, $8, "beq") }
| IF IDENT LE IDENT THEN asmt ELSE asmt /* should be ADD IDENT equal ident_or_imm THEN asmt ELSE asmt */
    { If($2, $4, $6, $8, "ble") }
| IF IDENT GE IDENT THEN asmt ELSE asmt /* should be ADD IDENT equal ident_or_imm THEN asmt ELSE asmt */
    { If($2, $4, $6, $8, "bge") }
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
