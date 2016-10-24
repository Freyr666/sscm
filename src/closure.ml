open Env
open Expressions
open Core.Std

type variable =
  | Var  of sexp
  | Proc of closure
and  closure = {args : string list;
                env  : variable env_type;
                body : sexp;}
             
let make_closure args env =
  match args with
  | [(List vars); body] ->
      let vars      = List.map vars
                               ~f:(fun x ->
                                 match x with
                                 | Symbol Symb s -> s
                                 | _             -> raise Wrong_exp_type) in
      {args = vars;
       env  = env;
       body = body;} 
  | _                   -> raise Wrong_exp_type

;;
