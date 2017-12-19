{
open Bparser
open Btype
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let underscore = ['_']

rule token = parse
| space+
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| digit+ 
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| "neg"
    { NEG }
| "fneg"
    { FNEG }
| '='
    { EQUAL }
| "let"
    { LET }
| "in"
    { IN }
| "fmul"
    { FMUL }
| "fdiv"
    { FDIV }
| "fadd"
    { FADD }
| "fsub"
    { FSUB }
| "add"
    { ADD }
| "sub" 
    { SUB }
| "call"
    { CALL }
| "nop" 
    { NOP }
| "()" | ""
    { NIL }
| '_'
    { UNDERSC }
| underscore (digit|lower|upper|underscore)*
    { LABEL(Lexing.lexeme lexbuf) }
| lower (digit|lower|upper|underscore)*
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }
