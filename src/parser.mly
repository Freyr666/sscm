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
  | ADD
          { Symbol Add }
  | MUL
          { Symbol Mul }
  | SUB
          { Symbol Sub }
  | DIV
          { Symbol Expressions.Div }
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
          { Expressions.Symbol (Expressions.Symb s) }
  | s = STRING
          { Expressions.String s }
  | i = INT
    { Expressions.Number (Number.Integer i) }
  | x = FLOAT
    { Expressions.Number (Number.Float x) }
  | TRUE
    { Expressions.Boolean true }
  | FALSE
    { Expressions.Boolean false }
  | NULL
    { Expressions.List [] }
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
