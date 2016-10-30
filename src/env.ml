exception Wrong_exp_type

open Core.Std

module Scheme_env = Hashtbl.Make(String)
                  
type ('var) env_type =
  Env_empty
| Env of 'var Scheme_env.t * 'var env_type

let new_env old =
  Env ((Scheme_env.create ()), old)
  
let rec lookup env var =
  match env with
  | Env_empty         -> None
  | Env (table, next) ->
     let res = Scheme_env.find table var
     in (match res with
         | Some _ -> res
         | None   -> lookup next var)

let set_var env var value =
  match env with
  | Env_empty         -> raise Wrong_exp_type
  | Env (table, next) -> Scheme_env.set table
                                        ~key:var
                                        ~data:value 
         
;;

