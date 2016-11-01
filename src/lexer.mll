{
open Core.Std
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let ratio_of_string str =
  match (String.split ~on:'/' str) with
  | [a;  b] -> ((int_of_string a), (int_of_string b))
  | _ -> raise (SyntaxError ("not a ratio: " ^ str))

let complex_of_string str =
  let stri = String.drop_suffix str 1 in
  match (String.split ~on:'+' stri) with
  | [""; _]
  | [_] ->
     (match (String.split ~on:'-' stri) with
      | [""; a; b] -> ((-. (float_of_string a)),
                       (-. (float_of_string b)))
      | [a; b]     -> ((float_of_string a),
                       (-. (float_of_string b)))
      | _ -> raise (SyntaxError ("not a complex: " ^ str)))
  | [a; b] -> ((float_of_string a),
               (float_of_string b))
  | _ -> raise (SyntaxError ("not a complex: " ^ str))
       
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*             
let float = digit* frac?
let ratio = digit+ '/' digit+
let aod = '+' | '-'
let im  = aod float 'i'  
let complex = '-'? float? im                                     

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?']*
             
let eq = "eq?" | "equal?"
             
rule read =
  parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }
  | int       { INT   (int_of_string (Lexing.lexeme lexbuf)) }
  | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | ratio     { RATIO (ratio_of_string (Lexing.lexeme lexbuf)) }
  | complex   { COMPLEX (complex_of_string (Lexing.lexeme lexbuf)) }
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "null"    { NULL }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | " . "     { DOT }
  | "quote"   { QUOTE }
  | "if"      { IF }
  | "let"     { LET }
  | "lambda"  { LAMBDA }
  | "define"  { DEFINE }
  | "+"       { ADD }
  | "*"       { MUL }
  | "-"       { SUB }
  | "/"       { DIV }
  | eq        { EQ }
  | "cons"    { CONS }
  | "car"     { CAR }
  | "cdr"     { CDR }
  | "pair?"   { PAIRP }
  | "list?"   { LISTP }
  | "atom?"   { ATOMP }
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | text as s { SYMBOL s }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
