open Syntax

let rec print_typ t = match t with
	| TInt -> "int"
	| TUnit -> "unit"
	| TProd (t1, t2) -> 
		"(" ^ (print_typ t1) ^ " * " ^ (print_typ t2) ^ ")"
	| TVoid -> "void"
	| TSum (t1, t2) -> "(" ^ (print_typ t1) ^ " + " ^ (print_typ t2) ^ ")"
	| TPtr t -> "Ptr " ^ (print_typ t)
	| TStruct s -> s

let rec print_expr e = match e with
	| EVar x -> x
	| EInt i -> string_of_int i 
	| EUnit -> "<>"
	| EPar (e1, e2) -> "(" ^ (print_expr e1) ^ ", " ^ (print_expr e2) ^ ")"
	| EProjL e -> "outl " ^ (print_expr e)
	| EProjR e -> "outR " ^ (print_expr e)
	| EInjL e -> "inl " ^ (print_expr e)
	| EInjR e -> "inr " ^ (print_expr e)
	| EMatch (e, (l, el), (r, er)) -> 
		"match " ^ (print_expr e) ^ " inl " ^ l ^ " -> " ^ (print_expr el)
								  ^ " inr " ^ r ^ " -> " ^ (print_expr er)
	| EDeref e -> "*(" ^ (print_expr e) ^ ")"
	| ESum (e1, e2) -> "(" ^ (print_expr e1) ^ " + " ^ (print_expr e2) ^ ")"
	| EMult (e1, e2) -> "(" ^ (print_expr e1) ^ " * " ^ (print_expr e2) ^ ")"
	| ENeg e -> "-(" ^ (print_expr e) ^ ")"
	| EAbort -> "e_abort"

let rec print_command c = match c with
	| CSkip -> "Skip"
	| CSeq (c1, c2) -> "(" ^ (print_command c1) ^ "; " ^ (print_command c2) ^ ")"
	| CIf (e, ct, cf) -> "(if " ^ (print_expr e) ^ " then " ^ (print_command ct) ^ " else " ^ (print_command cf) ^ ")"
	| CWhile (e, c) -> "(while " ^ (print_expr e) ^ " do " ^ (print_command c) ^ ")"
	| CAssign (x, e) -> "(" ^ x ^ " := " ^ (print_expr e) ^ ")"
	| CPtrAssign (x, e) -> "(*" ^ x ^ " := " ^ (print_expr e) ^ ")"
	| CVar (x, e, c) -> "( var " ^ x ^ " := " ^ (print_expr e) ^ " in " ^ (print_command c) ^ ")"
	| CPtrVar (x, e, c) -> "( var *" ^ x ^ " := new " ^ (print_expr e) ^ " in " ^ (print_command c) ^ ")"
	| CCall (x, f, args) -> "(" ^ x ^ " := " ^ f ^ "(" ^ (String.concat ", " (List.map print_expr args)) ^ "))"
	| CSwitch (e, (l, cl), (r, cr)) -> "(switch " ^ (print_expr e) ^ " as " ^
										"inl " ^ l ^ ": (" ^ (print_command cl) ^ ") " ^  
										"inr " ^ r ^ ": (" ^ (print_command cr) ^ ")"
	| CAbort -> "c_abort"
		
let print_vars_declarations vars_decl = 
	let print_var_declaration (v, e) = 
		v ^ " := " ^ (print_expr e)
	in
	"vars " ^ (String.concat ", " (List.map print_var_declaration vars_decl))

let print_functions_declarations fun_decl = 
	let print_function_declaration (f, args, typ, vars, c, e) =
		f ^ "(" ^ 
		(String.concat ", " (List.map (fun (v, t) -> v ^ " : " ^ (print_typ t)) args)) ^ ") : " ^
		(print_typ typ) ^ " = " ^
		(print_vars_declarations vars) ^ " in " ^ 
		(print_command c) ^ "; return " ^ (print_expr e)
	in
	String.concat "\n" (List.map print_function_declaration fun_decl)

let print_type_declarations type_decl = 
	let print_type_declaration (t_id, t) = t_id ^ " = " ^ (print_typ t) in
	String.concat "\n" (List.map print_type_declaration type_decl) 

let print_program (type_decl, fun_decl, vars_decl, c) = 
	print_type_declarations type_decl ^ " in " ^
	print_functions_declarations fun_decl ^ " in " ^
	print_vars_declarations vars_decl ^ " in " ^
	print_command c

