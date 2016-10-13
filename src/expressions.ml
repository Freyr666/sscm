open Number
open Object
   
let sum lst =
  match lst with
  | Cons (_,_) -> 1
  | _ -> raise Wrong_exp_type
