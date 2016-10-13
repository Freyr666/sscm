exception Wrong_exp_type

open Number

type sobject =
  Null
| Number  of Number.t
| Boolean of bool
| Symbol  of string
| String  of string
| Cons    of sobject * sobject
          
let car cns =
  match cns with
  | Cons (hd, tl) -> hd
  | _             -> raise Wrong_exp_type
                      
let cdr cns =
  match cns with
  | Cons (hd, tl) -> tl
  | _             -> raise Wrong_exp_type

let atomp a =
  match a with
  | Cons (_,_) -> false
  | _          -> true

let rec listp lst =
  match lst with
  | Cons (_, Null) -> true
  | Cons (_, tl)   -> listp tl
  | _              -> false
                    
let pairp pr =
  match pr with
  | Cons (_,_) -> true
  | _          -> false
;;
