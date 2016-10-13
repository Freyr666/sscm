exception Wrong_exp_type

open Number

type sobject =
| Number  of Number.t
| Boolean of bool
| Symbol  of string
| String  of string
| List    of sobject list
          
let car args =
  match args with
  | [List (hd::_)]  -> hd
  | _               -> raise Wrong_exp_type
                      
let cdr args =
  match args with
  | [List (hd::tl)] -> List tl
  | _               -> raise Wrong_exp_type

let cons args =
  match args with
  | [hd; List []] -> List [hd]
  | [hd; List ls] -> List (hd::ls)
  | [hd; tl]      -> List (hd::[tl])
  | _             -> raise Wrong_exp_type

let list args = List args

let atomp a =
  match a with
  | List _  -> Boolean false
  | _       -> Boolean true
                    
let pairp pr =
  match pr with
  | List _     -> Boolean true
  | _          -> Boolean false

let rec eq args =
  match args with
  | [Number a; Number b]   -> Boolean (a = b)
  | [Boolean a; Boolean b] -> Boolean (a = b)
  | [String a; String b]   -> Boolean (a = b)
  | [Symbol a; Symbol b]   -> Boolean (a = b)
  | [List []; List []]     -> Boolean true
  | [List (ha::ta); List (hb::tb)] ->
     if (ha <> hb)
     then Boolean false
     else eq [List ta; List tb]
  | _ -> raise Wrong_exp_type
;;
