{
    open Lexing
    open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let int = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = 
    parse
    | white {read lexbuf}
    | newline {new_line lexbuf; read lexbuf}
    | int {NUMBER (int_of_string (Lexing.lexeme lexbuf))}
    | "." {DOT}
    | "|" {BAR}
    | "," {COMMA}
    | ":" {COL}
    | ";" {SEMICOL}
    | "in" {IN}
    | "return" {RETURN}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "match" {MATCH}
    | "with" {WITH}
    | "switch" {SWITCH}
    | "skip" {SKIP}
    | "while" {WHILE}
    | "do" {DO}
    | "*" {STAR}
    | "+" {SUM}
    | "-" {NEG}
    | ":=" {ASSIGN}
    | "=" {EQUALS}
    | "unit" {UNIT}
    | "void" {VOID}
    | "int" {INT}
    | "(" {LPAR}
    | ")" {RPAR}
    | "<" {LTPAR}
    | ">" {RTPAR}
    | "outl" {OUTL}
    | "outr" {OUTR}
    | "inl" {INL}
    | "inr" {INR}
    | "abort" {ABORT}
    | "ptr" {PTR}
    | "vars" {VARS}
    | "var" {VAR}
    | "type" {TYPE}
    | "new" {NEW}
    | "debug_print" {DEBUG_PRINT}
    | id {ID (Lexing.lexeme lexbuf)}
    | _ {failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf)}
    | eof {EOF}





