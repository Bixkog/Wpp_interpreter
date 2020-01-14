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
	(* print_string (pretty_program program); *)
	Typechecker.typecheck program;
	let (heap, var_env) = Evaluator.eval_program program in
	print_endline "VARIABLES:";
	List.iter (pretty_env var_env) print_endline;
	print_endline "HEAP:";
	List.iter (pretty_heap heap) print_endline 