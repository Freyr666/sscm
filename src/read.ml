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
