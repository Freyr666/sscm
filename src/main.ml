open Core.Std
open Number
open Object
open Expressions
   
let () =
  let s = sub (Cons (Number (Number.Float 1.1),
                     (Cons (Number (Number.Rational (2,3)),
                            Null))))
  in match s with
     | Number n -> print_string (Number.show n)
     | _        -> print_string "Oups"
;;
