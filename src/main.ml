open Core.Std
open Number
   
let () =
  let num1 = Number.Integer 1 in
  let num2 = Number.Complex (1., 2.) in
  let res  = Number.sub num1 num2 in
  match res with
  | Number.Complex (r, i) -> printf "Result: %f + %fi\n" r i
  | _ -> printf "Error\n"
;;
