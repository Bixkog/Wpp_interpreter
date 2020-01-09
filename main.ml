open Core
open Lexer
open Lexing
open In_channel
open Pretty

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse lexbuf =
  try Parser.program Lexer.read lexbuf with
	  | Parser.Error ->
	    fprintf stderr "%a: syntax error\n" print_position lexbuf;
	    exit (-1)

let _ = 
	let ic = In_channel.create Sys.argv.(1) in
	let lexbuf = Lexing.from_channel ic in
	let program = parse lexbuf in
	print_string (pretty_program program);
	let res = Typechecker.typecheck program in
	res
