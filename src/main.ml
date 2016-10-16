open Core.Std
open Number
open Expressions
open Eval
   
let () =
  let s = eval
            (List [Symbol If;
                   List [Symbol Eq; Number (Number.Float 0.5); Number (Number.Float 0.5)];
                   Number (Number.Complex (1.1, 2.3)); Number (Number.Integer 0)])
  in match s with
     | Number n -> print_string (Number.show n)
     | _        -> print_string "Oups"
;;
