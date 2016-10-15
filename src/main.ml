open Core.Std
open Number
open Expressions
open Eval
   
let () =
  let s = eval
            (List [Symbol "+";
                   List [Symbol "-"; Number (Number.Float 1.1); Number (Number.Rational (1,2))];
                   Number (Number.Complex (1.1, 2.3))])
  in match s with
     | Number n -> print_string (Number.show n)
     | _        -> print_string "Oups"
;;
