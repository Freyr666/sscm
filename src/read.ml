open Core.Std
open Lexer
open Lexing
open Expressions

let read str =
  let lexbuf = Lexing.from_string str in
  try
    Parser.prog Lexer.read lexbuf
  with _ -> None
          
let read_channel ch =
  let lexbuf = Lexing.from_channel ch in
  Parser.prog Lexer.read lexbuf

let load_file file =
  let rec read_local acc buf =
    let exp = Parser.prog Lexer.read buf
    in match exp with
       | Some e -> read_local (List.append acc [e]) buf
       | None   -> acc
  in 
  let inx    = In_channel.create file  in
  let lexbuf = Lexing.from_channel inx in
  let _      = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  let res    = read_local [] lexbuf in
  let _      = In_channel.close inx in
  res
