open Core.Std
open Number
open Object
open Expressions
   
let () =
  print_int (sum (Cons (Number (Number.Float 1.1), Number (Number.Float 2.1))))
;;
