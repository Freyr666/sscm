open Core.Std
open Number
open Expressions
open Eval
open Print
open Lexer
open Lexing
open Env

   
let () =
  let lexbuf = Lexing.from_string "(let ((f    (lambda (x) (+ x 1)))
                                         (lst  (quote (1 2 3))))
                                      (define (map fun acc l)
                                         (if (eq? l (quote ()))
                                           acc
                                           (map fun 
                                                (cons (fun (car l)) acc)
                                                (cdr l)))) 
                                      (map f (quote ()) lst))" in
  match Parser.prog Lexer.read lexbuf with
  | Some sexp -> (print_exp sexp;
                  print_exp (eval sexp Env_empty))
  | None -> ()
  
;;
