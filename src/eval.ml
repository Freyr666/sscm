open Number
open Expressions

let apply fn args =
  match fn with
  | "+" -> Expressions.sum args
  | "-" -> Expressions.sub args
  | "*" -> Expressions.mul args
  | "/" -> Expressions.div args
  | _ -> raise Wrong_exp_type
   
let rec eval value =
  match value with
  | Nil
  | Number  _
  | Boolean _
  | Symbol  _
  | String  _
  | Cons    _ -> value
  | List (Symbol "quote" :: tl)
    -> List tl
  | List (Symbol "if" :: p :: exp1 :: exp2 :: [])
    -> exp1
  | List (Symbol fn :: args) -> apply fn (List (List.map eval args))
  | _ -> raise Wrong_exp_type
