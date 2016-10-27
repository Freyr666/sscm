%{
open Number;;
open Expressions;;
%}

%token <int> INT
%token <float> FLOAT
%token <string> SYMBOL
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LPAREN
%token RPAREN
%token QUOTE
%token IF
%token LET
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
  | IF
          { Symbol If }
  | LET
          { Symbol Let }
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
  | s = SYMBOL
          { Symbol (Symb s) }
  | s = STRING
          { String s }
  | i = INT
    { Number (Number.Integer i) }
  | x = FLOAT
    { Number (Number.Float x) }
  | TRUE
    { Boolean true }
  | FALSE
    { Boolean false }
  | NULL
    { List [] }
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
