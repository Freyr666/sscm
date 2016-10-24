open Number
open Expressions
open Env
open Closure
open Print

open Core.Std
   
let rec eval value env =
  match value with
  | Number  _
  | Boolean _
  | String  _
  | Dot     _ -> value
  | Symbol (Symb s) -> let var = lookup env s in
                       (match var with
                        | Some (Var v) -> v
                        | _            -> raise Wrong_exp_type)
  | Symbol _ -> value
  | List (Symbol Quote :: tl :: [])
    -> tl
  | List (Symbol If :: p :: exp1 :: exp2 :: [])
    -> (match (eval p env) with
       | Boolean true  -> eval exp1 env
       | Boolean false -> eval exp2 env
       | _ -> raise Wrong_exp_type)
  | List (Symbol fn :: args) -> apply fn
                                      (List.map args ~f:(fun arg -> eval arg env))
                                      env
  | _ -> raise Wrong_exp_type

and apply fn args env =
  match fn with
  | Add    -> Expressions.sum   args
  | Sub    -> Expressions.sub   args
  | Mul    -> Expressions.mul   args
  | Div    -> Expressions.div   args
  | Eq     -> Expressions.equal args
  | Cons   -> Expressions.cons  args
  | Car    -> Expressions.car   args
  | Cdr    -> Expressions.cdr   args
  | Pairp
  | Listp  -> Expressions.pairp args
  | Atomp  -> Expressions.atomp args
  | Symb s -> let proc = lookup env s in
              (match proc with
               | Some (Var _)    -> raise Wrong_exp_type
               | Some (Proc cls) -> call cls args env
               | None   -> raise Wrong_exp_type)
  | _ -> raise Wrong_exp_type

and call cls args env =
  let arg_env = new_env cls.env in
  let _       = List.iter2_exn cls.args
                               args
                               ~f:(fun sb vl ->
                                 set_var arg_env sb (Var vl)) in
  eval cls.body arg_env

;;
