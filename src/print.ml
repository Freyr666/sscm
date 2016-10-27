open Expressions
open Number
open Core.Std

let string_of_symbol = function
  | Symb s    -> s
  | If        -> "if"
  | Quote     -> "quote"
  | Add       -> "+"
  | Mul       -> "*"
  | Sub       -> "-"
  | Div       -> "/"
  | Eq        -> "equal?"
  | Cons      -> "cons"
  | Car       -> "car"
  | Cdr       -> "cdr"
  | Pairp     -> "pair?"
  | Atomp     -> "atom?"
  | Listp     -> "list?"
  | Define    -> "define"
  | Let       -> "let"
  | Tail_call -> "TAIL CALL:OPTIMISED OUT"
   
let rec string_of_exp = function
  | Boolean true   -> "#t"
  | Boolean false  -> "#f"
  | Number n       -> Number.string_of_number n
  | String s       -> "\"" ^ s ^ "\""
  | Symbol s       -> string_of_symbol s
  | List vals      -> let cont =
                        List.reduce ~f:(fun acc x -> acc ^ " " ^ x)
                                    (List.map vals string_of_exp)
                      in (match cont with
                          | Some s -> "(" ^ s ^ ")"
                          | None   -> "()")
  | Dot (lst, tl)  -> let cont =
                        List.reduce ~f:(fun acc x -> acc ^ " " ^ x)
                                    (List.map lst string_of_exp)
                      and last = string_of_exp tl
                      in (match cont with
                          | Some s -> "(" ^ s ^ " . " ^ last ^ ")"
                          | None   -> last)

let print_exp = Fn.compose print_endline string_of_exp
