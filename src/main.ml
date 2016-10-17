open Core.Std
open Number
open Expressions
open Eval
   
let () =
  let s =
    eval
      (List [Symbol If;
             List [Symbol Eq;
                   Number (Number.Float 0.5);
                   List [Symbol Car;
                         List [Symbol Cons;
                               Number (Number.Float 0.5);
                               Number (Number.Integer 1)]]];
             Number (Number.Complex (1.1, 2.3)); Number (Number.Integer 0)])
  in match s with
     | Number  n -> print_string (Number.show n)
     | Boolean b -> if b then print_string "Ok" else print_string "Bad"
     | List _    -> print_string "List"
     | _         -> print_string "Oups"
;;
