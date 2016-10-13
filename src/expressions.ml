open Number
open Object

let sum args =
  let rec rsum acc lst =
    match lst with
    | Cons (Number n, Null) -> Number (Number.sum n acc)
    | Cons (Number n, tl)   -> rsum (Number.sum n acc) tl
    | _ -> raise Wrong_exp_type
  in
  rsum (Number.Integer 0) args

let mul args =
  let rec rmul acc lst =
    match lst with
    | Cons (Number n, Null) -> Number (Number.mul n acc)
    | Cons (Number n, tl)   -> rmul (Number.mul n acc) tl
    | _ -> raise Wrong_exp_type
  in
  rmul (Number.Integer 1) args

let sub args =
  match args with
  | Cons (Number x, Number y)
  | Cons (Number x,
          Cons (Number y, Null)) -> Number (Number.sub x y)
  | _ -> raise Wrong_exp_type

let div args =
  match args with
  | Cons (Number x, Number y)
  | Cons (Number x,
          Cons (Number y, Null)) -> Number (Number.div x y)
  | _ -> raise Wrong_exp_type

       
;;
