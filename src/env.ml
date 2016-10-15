open Expressions
open Core.Std

module Scheme_env = Hashtbl.Make(String)

type env =
  Env_empty
| Env of sexp Scheme_env.t  * env
