exception Wrong_exp_type

open Core.Std
open Number
   
type sexp =
  Nil
| Number  of Number.t
| Boolean of bool
| Symbol  of string
| String  of string
| Cons    of sexp * sexp
| List    of sexp list

let unpackNumber = function
  | Number n -> n
  | _ -> raise Wrong_exp_type
                            
let car arg =
  match arg with
  | Cons (hd, tl) -> hd 
  | List (hd::_)  -> hd
  | _             -> raise Wrong_exp_type
                      
let cdr arg =
  match arg with
  | Cons (hd, tl) -> tl
  | List (hd::tl) -> List tl
  | _             -> raise Wrong_exp_type

let cons arg1 arg2 =
  match (arg1, arg2) with
  | (hd, List []) -> List [hd]
  | (hd, List ls) -> List (hd::ls)
  | (hd, Cons (hc, tc)) -> List (hd::hc::[tc])
  | (hd, tl)      -> Cons (hd, tl)

let atomp a =
  match a with
  | List _    -> Boolean false
  | Cons(_,_) -> Boolean false
  | _         -> Boolean true
                    
let pairp pr =
  match pr with
  | List _     -> Boolean true
  | Cons(_,_)  -> Boolean true
  | _          -> Boolean false

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
  | _ -> raise Wrong_exp_type

let sum args =
  match args with
  | List exps -> let res = List.fold ~f:(fun x y -> Number.sum x y)
                                     ~init:(Number.Integer 0)
                                     (List.map exps (fun x -> unpackNumber x))
                 in Number res
  | _ -> raise Wrong_exp_type

let mul args =
  match args with
  | List exps -> let res = List.fold ~f:(fun x y -> Number.mul x y)
                                      ~init:(Number.Integer 1)
                                      (List.map exps (fun x -> unpackNumber x))
                 in Number res
  | _ -> raise Wrong_exp_type

let sub args =
  match args with
  | List exps -> let res = List.fold ~f:(fun x y -> Number.sub x y)
                                     ~init:(Number.Integer 0)
                                     (List.map exps (fun x -> unpackNumber x))
                 in Number res
  | _ -> raise Wrong_exp_type

let div args =
  match args with
  | List exps -> let res = List.fold ~f:(fun x y -> Number.div x y)
                                     ~init:(Number.Integer 1)
                                     (List.map exps (fun x -> unpackNumber x))
                 in Number res
  | _ -> raise Wrong_exp_type

;;
