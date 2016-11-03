open Number
open Expressions
open Env
open Print
open Read

open Core.Std
   
let rec eval value env =
  match value with
  | Number  _
  | Boolean _
  | String  _
  | Dot     _ -> value
  | List (List h :: tl) ->
     let new_lst = List ( (eval (List h) env) ::
                          (List.map tl
                                    ~f:(fun arg -> eval arg env))) in
     eval new_lst env
  | Symbol (Symb s) -> let var = lookup env s in
                       (match var with
                        | Some v -> v
                        | _      -> raise Wrong_exp_type)
  | Symbol _ -> value
  | List (Symbol Quote :: tl :: [])
    -> tl
  | List (Symbol If :: p :: exp1 :: exp2 :: [])
    -> (match (eval p env) with
       | Boolean true  -> eval exp1 env
       | Boolean false -> eval exp2 env
       | _ -> raise Wrong_exp_type)  
  | List (Symbol Tail_call :: args)
    -> List (Symbol Tail_call :: (List.map args
                                           ~f:(fun arg -> eval arg env)))
  | List (Symbol Let :: List binds :: body)
    -> bind_eval binds body env
  | List (Symbol Lambda :: List args :: body)
    -> Closure (make_closure args body env)
  | List (Symbol Define :: def :: exps)
    -> define def exps env
  | List (Closure cls :: args)
    -> call cls args env
  | List (Symbol Read :: String s :: [])
    -> let sexp = Read.read s
       in (match sexp with
           | Some e -> List (Symbol Quote :: e :: [])
           | None   -> raise Wrong_exp_type)
  | List (Symbol Eval :: exp :: [])
    -> (match (eval exp env) with
        | List [Symbol Quote; e] -> eval e env
        | _                      -> raise Wrong_exp_type)
  | List (Symbol Load :: String f :: [])
    -> List.fold ~init:(List[])
                 ~f:(fun acc e -> eval e env)
                 (load_file f)
  | List (Symbol fn :: args)
    -> apply fn
             (List.map args ~f:(fun arg -> eval arg env))
             env
  | _ -> raise Wrong_exp_type

and apply fn args env =
  match fn with
  | Not       -> Expressions.l_not args
  | And       -> Expressions.l_and args
  | Or        -> Expressions.l_or  args
  | Add       -> Expressions.sum   args
  | Sub       -> Expressions.sub   args
  | Mul       -> Expressions.mul   args
  | Div       -> Expressions.div   args
  | Eq        -> Expressions.equal args
  | Cons      -> Expressions.cons  args
  | Car       -> Expressions.car   args
  | Cdr       -> Expressions.cdr   args
  | Pairp
  | Listp     -> Expressions.pairp args
  | Atomp     -> Expressions.atomp args
  | Nullp     -> Expressions.nullp args
  | Symb s    -> let proc = lookup env s in
                 (match proc with
                  | Some (Closure cls) -> call cls args env
                  | Some _ 
                  | None  -> raise Wrong_exp_type)
  | _ -> raise Wrong_exp_type

and call cls args env =
  let arg_env = new_env cls.env in
  let _       = List.iter2_exn cls.args
                               args
                               ~f:(fun sb vl ->
                                 set_var arg_env sb vl) in
  List.fold
    ~init:(List [])
    ~f:(fun acc e -> eval e arg_env)
    cls.body

and bind_eval bindings body env =
  if List.is_empty body
  then raise Wrong_exp_type
  else 
    let nenv = new_env env in
    let _    = List.iter
                 ~f:(fun bnd ->
                   match bnd with
                   | List [Symbol (Symb s); v]
                     -> let e = eval v nenv in
                        (match e with
                         | Closure c -> set_var nenv s (Closure (optimise_cls s c))
                         | _         -> set_var nenv s e)
                   | _
                     -> raise Wrong_exp_type)
                 bindings in
    List.fold
      ~init:(List [])
      ~f:(fun acc b -> eval b nenv)
      body

and define def exps env =
  match (def,exps) with
  | (Symbol Symb s, [exp])
    -> let e = eval exp env in
       let _ = (match e with
                | Closure c -> set_var env s (Closure (optimise_cls s c))
                | _         -> set_var env s e) in
       List []
  | (List (Symbol Symb f :: args), body)
    -> let _ = set_var env f (Closure
                                (optimise_cls f
                                              (make_closure args body env))) in
       List []
  | _ -> raise Wrong_exp_type

and optimise_cls name cls =
  let rec opt_body = function
    | List (Symbol Symb n :: args) as exp -> if n = name
                                             then List (Symbol Tail_call :: args)
                                             else exp
    | List (Symbol If :: exp1 :: exp2 :: [])
      -> List (Symbol If :: (opt_body exp1) :: (opt_body exp2) :: [])
    | exp -> exp
  in { cls
     with body = List.map ~f:opt_body
                          cls.body }

;;
