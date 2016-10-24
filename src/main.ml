open Core.Std
open Number
open Expressions
open Eval
open Print
(**open Lexer
open Lexing*)
open Env
open Closure

let () =
  let test = {args = ["x"];
              env  = Env_empty;
              body = List [Symbol Add;
                           Symbol (Symb "x");
                           Number (Number.Float 0.5);
                           Number (Number.Complex (1.1, 2.3));
                           Number (Number.Integer 0)]} in
  let envi = new_env Env_empty in
  let _    = set_var envi "fun" (Proc test) in
  let texp = List [Symbol (Symb "fun"); Number (Number.Float 0.5)] in
  print_exp (eval texp envi)
  
;;
