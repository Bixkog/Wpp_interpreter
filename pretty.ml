open Syntax
open Value 

let rec pretty_typ t = match t with
	| TInt -> "int"
	| TUnit -> "unit"
	| TProd (t1, t2) -> 
		"(" ^ (pretty_typ t1) ^ " * " ^ (pretty_typ t2) ^ ")"
	| TVoid -> "void"
	| TSum (t1, t2) -> "(" ^ (pretty_typ t1) ^ " + " ^ (pretty_typ t2) ^ ")"
	| TPtr t -> "Ptr " ^ (pretty_typ t)
	| TStruct s -> s
	| TUnknown -> "_"

let rec pretty_expr e = match e with
	| EVar x -> x
	| EInt i -> string_of_int i 
	| EUnit -> "<>"
	| EPar (e1, e2) -> "(" ^ (pretty_expr e1) ^ ", " ^ (pretty_expr e2) ^ ")"
	| EProjL e -> "outl " ^ (pretty_expr e)
	| EProjR e -> "outR " ^ (pretty_expr e)
	| EInjL e -> "inl " ^ (pretty_expr e)
	| EInjR e -> "inr " ^ (pretty_expr e)
	| EMatch (e, (l, el), (r, er)) -> 
		"match " ^ (pretty_expr e) ^ " inl " ^ l ^ " -> " ^ (pretty_expr el)
								  ^ " inr " ^ r ^ " -> " ^ (pretty_expr er)
	| EDeref e -> "*(" ^ (pretty_expr e) ^ ")"
	| ESum (e1, e2) -> "(" ^ (pretty_expr e1) ^ " + " ^ (pretty_expr e2) ^ ")"
	| EMult (e1, e2) -> "(" ^ (pretty_expr e1) ^ " * " ^ (pretty_expr e2) ^ ")"
	| ENeg e -> "-(" ^ (pretty_expr e) ^ ")"
	| EAbort -> "e_abort"

let rec pretty_command c = match c with
	| CSkip -> "Skip"
	| CSeq (c1, c2) -> "(" ^ (pretty_command c1) ^ "; " ^ (pretty_command c2) ^ ")"
	| CIf (e, ct, cf) -> "(if " ^ (pretty_expr e) ^ " then " ^ (pretty_command ct) ^ " else " ^ (pretty_command cf) ^ ")"
	| CWhile (e, c) -> "(while " ^ (pretty_expr e) ^ " do " ^ (pretty_command c) ^ ")"
	| CAssign (x, e) -> "(" ^ x ^ " := " ^ (pretty_expr e) ^ ")"
	| CPtrAssign (x, e) -> "(*" ^ x ^ " := " ^ (pretty_expr e) ^ ")"
	| CVar (x, e, c) -> "( var " ^ x ^ " := " ^ (pretty_expr e) ^ " in " ^ (pretty_command c) ^ ")"
	| CPtrVar (x, e, c) -> "( var " ^ x ^ " := new " ^ (pretty_expr e) ^ " in " ^ (pretty_command c) ^ ")"
	| CCall (x, f, args) -> "(" ^ x ^ " := " ^ f ^ "(" ^ (String.concat ", " (List.map pretty_expr args)) ^ "))"
	| CSwitch (e, (l, cl), (r, cr)) -> "(switch " ^ (pretty_expr e) ^ " as " ^
										"inl " ^ l ^ ": (" ^ (pretty_command cl) ^ ") " ^  
										"inr " ^ r ^ ": (" ^ (pretty_command cr) ^ ")"
	| CDebugPrint _ -> ""
	| CAbort -> "c_abort"
		
let pretty_var_declaration var_decl = match var_decl with
		Var (v, e) -> v ^ " := " ^ (pretty_expr e)
		| PtrVar (v, e) -> v ^ " := new " ^ (pretty_expr e)

let pretty_vars_declarations vars_decl = 
	"vars " ^ (String.concat ", " (List.map pretty_var_declaration vars_decl))

let pretty_function_declaration (f, args, typ, vars, c, e) =
		f ^ "(" ^ 
		(String.concat ", " (List.map (fun (v, t) -> v ^ " : " ^ (pretty_typ t)) args)) ^ ") : " ^
		(pretty_typ typ) ^ " = " ^
		(pretty_vars_declarations vars) ^ " in " ^ 
		(pretty_command c) ^ "; return " ^ (pretty_expr e)

let pretty_functions_declarations fun_decl = 
	String.concat "\n" (List.map pretty_function_declaration fun_decl)

let pretty_type_declaration (t_id, t) = "type " ^ t_id ^ " = " ^ (pretty_typ t) 

let pretty_type_declarations type_decl = 
	String.concat "\n" (List.map pretty_type_declaration type_decl) 

let pretty_program (type_decl, fun_decl, vars_decl, c) = 
	pretty_type_declarations type_decl ^ " in " ^
	pretty_functions_declarations fun_decl ^ " in " ^
	pretty_vars_declarations vars_decl ^ " in " ^
	pretty_command c

let rec pretty_value v = match v with
	| VInt n -> string_of_int n
	| VPtr p -> "&" ^ (string_of_int p)
	| VInjL v -> "inl (" ^ (pretty_value v) ^ ")"
	| VInjR v -> "inr (" ^ (pretty_value v) ^ ")"
	| VPar (v1, v2) -> "<" ^ (pretty_value v1) ^ ", " ^ (pretty_value v2) ^ ">"
	| VUnit -> "<>"

let pretty_heap heap =  Seq.fold_left 
	(fun l (k, v) -> ((string_of_int k) ^ " = " ^ (pretty_value v)) :: l)
	[] heap

let pretty_env env = List.rev (Env.fold (fun k v l -> (k ^ " = " ^ (pretty_value v)) :: l) env [])


