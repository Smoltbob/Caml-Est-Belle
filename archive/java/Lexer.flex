import java_cup.runtime.*;
      
%%
   
/* -----------------Options and Declarations Section----------------- */
   
%class Lexer

/*
  The current line number can be accessed with the variable yyline
  and the current column number with the variable yycolumn.
*/
%line
%column
    
/* 
   Will switch to a CUP compatibility mode to interface with a CUP
   generated parser.
*/
%cup
   
/*
  Declarations
   
  Code between %{ and %}, both of which must be at the beginning of a
  line, will be copied letter to letter into the lexer class source.
  Here you declare member variables and functions that are used inside
  scanner actions.  
*/
%{   
    /* To create a new java_cup.runtime.Symbol with information about
       the current token, the token will have no value in this
       case. */
    private Symbol symbol(int type) {
        return new Symbol(type, yyline, yycolumn);
    }
    
    /* Also creates a new java_cup.runtime.Symbol with information
       about the current token, but this object has a value. */
    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline, yycolumn, value);
    }
%}
   

/*
  Macro Declarations
  
  These declarations are regular expressions that will be used latter
  in the Lexical Rules Section.  
*/
   
space = [ \t\n\r]
digit = [0-9]
lower = [a-z]
upper = [A-Z]
comment =  "(*" [^*] ~"*)" 
%%
/* ------------------------Lexical Rules Section---------------------- */
   
/*
   This section contains regular expressions and actions, i.e. Java
   code, that will be executed when the scanner matches the associated
   regular expression. */
   
   /* YYINITIAL is the state at which the lexer begins scanning.  So
   these regular expressions will only be matched if the scanner is in
   the start state YYINITIAL. */
   
<YYINITIAL> {

{space}+  { }
{comment}   {  }
"("     { return symbol(sym.LPAREN); }
")"     { return symbol(sym.RPAREN); }
"true"  { return symbol(sym.BOOL, true); }
"false" { return symbol(sym.BOOL, false); }
"not"   { return symbol(sym.NOT); }

{digit}+  { return symbol(sym.INT, new Integer(yytext())); }
{digit}+ ("." {digit}*)? (["e" "E"] ["+" "-"]? digit+)?  
        { return symbol(sym.FLOAT, new java.lang.Float(yytext())); } 

"-"     { return symbol(sym.MINUS); }
"+"     { return symbol(sym.PLUS); }
"-."    { return symbol(sym.MINUS_DOT); }
"+."    { return symbol(sym.PLUS_DOT); }
"*."    { return symbol(sym.AST_DOT); }
"/."    { return symbol(sym.SLASH_DOT); }
"="     { return symbol(sym.EQUAL); }
"<>"    { return symbol(sym.LESS_GREATER); }
"<="    { return symbol(sym.LESS_EQUAL); }
">="    { return symbol(sym.GREATER_EQUAL); }
"<"     { return symbol(sym.LESS); }
">"     { return symbol(sym.GREATER); }
"if"    { return symbol(sym.IF); }
"then"  { return symbol(sym.THEN); }
"else"  { return symbol(sym.ELSE); }
"let"   { return symbol(sym.LET); }
"in"    { return symbol(sym.IN); }
"rec"   { return symbol(sym.REC); }
","     { return symbol(sym.COMMA); }
"_"     { return symbol(sym.IDENT, Id.gen()); }
"Array.create" { return symbol(sym.ARRAY_CREATE); }
"."     { return symbol(sym.DOT); }
"<-"    { return symbol(sym.LESS_MINUS); }
";"     { return symbol(sym.SEMICOLON); }
eof     { return symbol(sym.EOF); }

{lower} ({digit}|{lower}|{upper}|"_")*   { return symbol(sym.IDENT, new Id(yytext())); }
}
[^]                    { throw new Error("Illegal character <"+yytext()+">"); }



