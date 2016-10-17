exception Wrong_exp_type

open Core.Std
open Number

type symb =
  Symb of string      
| If
| Quote
| Define
| Add
| Mul
| Sub
| Div
| Eq
| Cons
| Car
| Cdr
| Pairp
| Listp
| Atomp
   
type sexp =
  Number  of Number.t
| Boolean of bool
| Symbol  of symb
| String  of string
| Cons    of sexp * sexp
| List    of sexp list

let unpack_number = function
  | Number n -> n
  | _ -> raise Wrong_exp_type

let rec eq arg1 arg2 =
  match (arg1, arg2) with
  | (Number a, Number b)   -> Boolean (a = b)
  | (Boolean a, Boolean b) -> Boolean (a = b)
  | (String a, String b)   -> Boolean (a = b)
  | (Symbol a, Symbol b)   -> Boolean (a = b)
  | (List [], List [])     -> Boolean true
  | (List (ha::ta), List (hb::tb)) ->
     if (ha <> hb)
     then Boolean false
     else eq (List ta) (List tb)
  | _ -> Boolean false
       
let car = function
  | [Cons (hd, tl)] -> hd 
  | [List (hd::_)]  -> hd
  | _               -> raise Wrong_exp_type
                      
let cdr = function
  | [Cons (hd, tl)] -> tl
  | [List (hd::tl)] -> List tl
  | _               -> raise Wrong_exp_type

let cons = function
  | [hd; List ls] -> List (hd::ls)
  | [hd; Cons (hc, tc)] -> List (hd::hc::[tc])
  | [hd; tl]      -> Cons (hd, tl)
  | _             -> raise Wrong_exp_type

let atomp = function
  | [List _]    -> Boolean false
  | [Cons(_,_)] -> Boolean false
  | _           -> Boolean true
                    
let pairp = function
  | [List _]     -> Boolean true
  | [Cons(_,_)]  -> Boolean true
  | _            -> Boolean false

let equal = function
    exps -> let res = List.reduce ~f:(fun x y -> eq x y)
                                  exps
            in (match res with
                | Some x -> x
                | None -> Boolean true)
       
let sum = function
    exps -> let res = List.fold ~f:(fun x y -> Number.sum x y)
                                ~init:(Number.Integer 0)
                                (List.map exps (fun x -> unpack_number x))
            in Number res

let mul = function
    exps -> let res = List.fold ~f:(fun x y -> Number.mul x y)
                                ~init:(Number.Integer 1)
                                (List.map exps (fun x -> unpack_number x))
            in Number res

let sub = function
    exps -> let res = List.fold ~f:(fun x y -> Number.sub x y)
                                ~init:(Number.Integer 0)
                                (List.map exps (fun x -> unpack_number x))
            in Number res

let div = function
    exps -> let res = List.fold ~f:(fun x y -> Number.div x y)
                                ~init:(Number.Integer 1)
                                (List.map exps (fun x -> unpack_number x))
            in Number res
       
;;
