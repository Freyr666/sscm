open Core.Std
open Number
open Expressions
open Eval
open Print
open Lexer
open Lexing

let () =
  let exp =
      (List [Symbol If;
             List [Symbol Eq;
                   Number (Number.Float 0.5);
                   List [Symbol Car;
                         List [Symbol Cons;
                               Number (Number.Float 0.5);
                               Number (Number.Integer 1)]]];
             Number (Number.Complex (1.1, 2.3)); Number (Number.Integer 0)])
  in (print_exp exp;
      print_exp (eval exp))
;;
