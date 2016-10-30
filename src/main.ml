open Core.Std
open Number
open Expressions
open Eval
open Print
open Lexer
open Lexing
open Env

   
let () =
  let lexbuf = Lexing.from_string "(let ((f (lambda (x) (+ x 1) (- x 2)))) (f (/ 1 3)))" in
  match Parser.prog Lexer.read lexbuf with
  | Some sexp -> (print_exp sexp;
                  print_exp (eval sexp Env_empty))
  | None -> ()
(*   let test = {args = ["x"]; *)
(*               env  = Env_empty; *)
(*               body = List [Symbol Div; *)
(*                            Number (Number.Rational 1); *)
(*                            Number (Number.Integer 1)]} in *)
(*   let envi = new_env Env_empty in *)
(*   let _    = set_var envi "fun" (Proc test) in *)
(*   let texp = List [Symbol (Symb "fun"); Number (Number.Float 0.5)] in *)
(* print_exp (eval texp envi) "(if (eq? 2 (cdr (4 . 2))) (/ 1 2) 5)"*)
  
;;
