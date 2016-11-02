open Core.Std
open Expressions
open Eval
open Read
open Print
open Env

   
let () =

  let input = In_channel.stdin in
  let env   = new_env Env_empty in
  
  let rec promt str in_chan env =
    let s  = str ^ (input_line in_chan) in
    let e  = Read.read s in
    match e with
    | Some sexp -> let _   = (print_string "=> ";
                              print_exp (eval sexp env)) in
                   promt "" in_chan env
    | None      -> promt s  in_chan env
  in

  promt "" input env 
  
;;
