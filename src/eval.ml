open Number
open Expressions

let apply fn args =
  match fn with
  | Add   -> Expressions.sum   args
  | Sub   -> Expressions.sub   args
  | Mul   -> Expressions.mul   args
  | Div   -> Expressions.div   args
  | Eq    -> Expressions.equal args
  | Cons  -> Expressions.cons  args
  | Car   -> Expressions.car   args
  | Cdr   -> Expressions.cdr   args
  | Pairp
  | Listp -> Expressions.pairp args
  | Atomp -> Expressions.atomp args
  | _ -> raise Wrong_exp_type
   
let rec eval value =
  match value with
  | Number  _
  | Boolean _
  | Symbol  _
  | String  _
  | Dot     _ -> value
  | List (Symbol Quote :: tl :: [])
    -> tl
  | List (Symbol If :: p :: exp1 :: exp2 :: [])
    -> (match (eval p) with
       | Boolean true  -> eval exp1
       | Boolean false -> eval exp2
       | _ -> raise Wrong_exp_type)
  | List (Symbol fn :: args) -> apply fn (List.map eval args)
  | _ -> raise Wrong_exp_type
