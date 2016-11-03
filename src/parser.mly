%{
open Number;;
open Expressions;;
%}

%token <int> INT
%token <float> FLOAT
%token <int*int> RATIO
%token <float*float> COMPLEX
%token <string> SYMBOL
%token <string> STRING
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token QUOTE
%token QMARK
%token IF
%token LET
%token LAMBDA
%token DEFINE
%token NOT
%token AND
%token OR
%token ADD
%token MUL
%token SUB
%token DIV
%token EQ
%token CONS
%token CAR
%token CDR
%token PAIRP
%token LISTP
%token ATOMP
%token NULLP
%token DOT

%token EOF

%start <Expressions.sexp option> prog
%%
                             
prog:
  | EOF       { None }
  | s = sexp  { Some s }
  ;

sexp:
  | LPAREN; sl = list_sexp; RPAREN
          { List sl }
  | LPAREN; sl = list_sexp; DOT; s = sexp; RPAREN
          { Dot (sl, s) }
  | QUOTE
          { Symbol Quote }
  | QMARK; s = sexp
          { List [Symbol Quote; s] }
  | IF
          { Symbol If }
  | LET
          { Symbol Let }
  | LAMBDA
          { Symbol Lambda }
  | DEFINE
          { Symbol Define }
  | NOT
          { Symbol Not }
  | AND
          { Symbol And }
  | OR
          { Symbol Or }
  | ADD
          { Symbol Add }
  | MUL
          { Symbol Mul }
  | SUB
          { Symbol Sub }
  | DIV
          { Symbol Div }
  | EQ
          { Symbol Eq }
  | CONS
          { Symbol Cons }
  | CAR
          { Symbol Car }
  | CDR
          { Symbol Cdr }
  | PAIRP
          { Symbol Pairp }
  | ATOMP
          { Symbol Atomp }
  | LISTP
          { Symbol Listp }
  | NULLP
          { Symbol Nullp }
  | s = SYMBOL
          { Symbol (Symb s) }
  | s = STRING
          { String s }	  
  | i = INT
    { Number (Number.Integer i) }
  | x = FLOAT
    { Number (Number.Float x) }
  | r = RATIO
    { let (d,n) = r in Number (Number.Rational (d,n)) }
  | c = COMPLEX
    { let (r,i) = c in Number (Number.Complex (r,i)) }
  | TRUE
    { Boolean true }
  | FALSE
    { Boolean false }
  ;

list_sexp:
  | (* empty *) { [] }
  | sl = rev_vals { List.rev sl }
  ;

rev_vals:
  | s = sexp { [s] }
  | sl = rev_vals; s = sexp
    { s :: sl }
  ;
