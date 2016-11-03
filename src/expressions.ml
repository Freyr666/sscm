exception Wrong_exp_type

open Core.Std
open Number
open Env

type symb =
  Symb of string
| Tail_call
| If
| Quote
| Define
| Lambda
| Let
| Read
| Eval
| Load
| Not
| And
| Or
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
| Nullp

type sexp =
  Number  of Number.t
| Boolean of bool
| Symbol  of symb
| String  of string
| Dot     of sexp list * sexp
| List    of sexp list
| Closure of closure
and closure = {args : string list;
               env  : sexp env_type;
               body : sexp list;}
             
let make_closure vars body env =
  let vars          = List.map vars
                               ~f:(fun x ->
                                 match x with
                                 | Symbol Symb s -> s
                                 | _             -> raise Wrong_exp_type) in
  {args = vars;
   env  = env;
   body = body;} 
           
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
  | [Dot ([], hd)]  -> hd
  | [Dot (hd::_,_)] -> hd 
  | [List (hd::_)]  -> hd
  | _               -> raise Wrong_exp_type
                      
let cdr = function
  | [Dot ([],tl)]
  | [Dot ([_],tl)]   -> tl
  | [Dot (hd::tl,x)] -> Dot (tl,x)
  | [List (hd::tl)]  -> List tl
  | _                -> raise Wrong_exp_type

let cons = function
  | [hd; List ls] -> List (hd::ls)
  | [hd; Dot (hc, tc)] -> Dot (hd::hc,tc)
  | [hd; tl]      -> Dot ([hd], tl)
  | _             -> raise Wrong_exp_type

let atomp = function
  | [List _]    -> Boolean false
  | [Dot(_,_)]  -> Boolean false
  | _           -> Boolean true
                    
let pairp = function
  | [List _]     -> Boolean true
  | [Dot(_,_)]   -> Boolean true
  | _            -> Boolean false

let nullp = function
  | [List []]    -> Boolean true
  | _            -> Boolean false

let l_not = function
  | [List []]       -> Boolean true
  | [Boolean false] -> Boolean true
  | _               -> Boolean false
                 
let l_or  = function
    exps -> let res = List.find ~f:(fun e ->
                                  match e with
                                  | List []       -> false
                                  | Boolean false -> false
                                  | _             -> true)
                                exps
            in (match res with
                | Some _ -> Boolean true
                | None   -> Boolean false)

let l_and = function
    exps -> let res = List.find ~f:(fun e ->
                                  match e with
                                  | List []       -> true
                                  | Boolean false -> true
                                  | _             -> false)
                                exps
            in (match res with
                | Some _ -> Boolean false
                | None   -> Boolean true)
                  
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
    exps -> let res = List.reduce ~f:(fun x y -> Number.sub x y)
                                  (List.map exps (fun x -> unpack_number x))
            in match res with
               | Some n -> Number n
               | None   -> raise Wrong_exp_type

let div = function
    exps -> let res = List.reduce ~f:(fun x y -> Number.div x y)
                                  (List.map exps (fun x -> unpack_number x))
            in match res with
               | Some n -> Number n
               | None   -> raise Wrong_exp_type
       
;;
