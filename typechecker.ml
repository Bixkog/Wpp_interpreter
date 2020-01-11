open Syntax
open Pretty
open Map

module TypeEnv = Map.Make(String);;
module VarEnv = Map.Make(String);;
module FunctionEnv = Map.Make(String);;

(* 
	checks if user defined type is correct. 
	That is, it uses only defined types, and user types are under ptr. 
*)
let check_user_type declared_types t = 
	let rec check is_ptr t = match t with
		| TStruct id -> if is_ptr then 
							(if List.mem id declared_types then () else
								failwith ("type " ^ id ^ " is never declared"))
						else failwith ("type " ^ id ^ " is not pointer type")
		| TPtr t -> check true t
		| TSum (t1, t2) -> (check false t1); (check false t2)
		| TProd (t1, t2) -> (check false t1); (check false t2)
		| _ -> ()
	in
	try check false t
	with Failure err ->  failwith ("invalid user type: " ^ (pretty_typ t) ^ " cause: " ^ err)

(* First reads all defined types then checks them. *)
let check_type_declarations type_declarations = 
	let declared_types = List.map fst type_declarations in
	let check_type_declaration type_declaration = 
		let (t_id, t) = type_declaration in
		check_user_type declared_types t
	in
	List.iter check_type_declaration type_declarations 

let rec extract_real_type type_env t = match t with
	| TStruct id -> extract_real_type type_env (TypeEnv.find id type_env)
	| t -> t

(* 
	Deduce type of expresion.
	eg. inl 5 -> TSum (TInt, TUnknown)
*)
let rec deduce_expr_type var_types e = 
	let rec deduce e = match e with 
		| EVar x -> (try VarEnv.find x var_types
					with Not_found -> failwith ("Variable " ^ x ^ " is not defined."))
		| EInt _ -> TInt
		| EUnit -> TUnit
		| EPar (e1, e2) -> TProd (deduce e, deduce e)
		| EProjL e -> (match deduce e with
			| TProd (t, _) -> t
			| _ -> failwith ("Projection argument is not a pair: " ^ (pretty_expr e)))
		| EProjR e -> (match deduce e with
			| TProd (_, t) -> t
			| _ -> failwith ("Projection argument is not a pair: " ^ (pretty_expr e)))
		| EInjL e -> TSum (deduce e, TUnknown)
		| EInjR e -> TSum (TUnknown, deduce e)
		| EMatch (e, (id_left, e_left), (id_right, e_right)) ->
			(match deduce e with
				| TSum (t1, t2) -> 
					let left_vars = VarEnv.add id_left t1 var_types in
					let right_vars = VarEnv.add id_right t2 var_types in
					let t_left = deduce_expr_type left_vars e_left in
					let t_right = deduce_expr_type right_vars e_right in
					if t_left = t_right then t_left
					else failwith ("Match branches types don't match " ^ 
						(pretty_typ t_left) ^ " =/= " ^ (pretty_typ t_right))

				| _ -> failwith ("Match argument is not a sum: " ^ (pretty_expr e)))
		| EDeref e -> (match deduce e with
			| TPtr t -> t
			| _ -> failwith ("Trying to dereference non-pointer expression: " ^ (pretty_expr e)))
		| ESum (e1, e2)-> if (deduce e1) = TInt && (deduce e2) = TInt then TInt
			else failwith ("Sum argument is not an int: " ^ (pretty_expr e))
		| EMult (e1, e2)-> if (deduce e1) = TInt && (deduce e2) = TInt then TInt
			else failwith ("Multiplication argument is not an int: " ^ (pretty_expr e))
		| ENeg e -> if deduce e = TInt then TInt
			else failwith ("Negation argument is not an int: " ^ (pretty_expr e))
		| EAbort -> TVoid
	in
	try deduce e
	with Failure err -> failwith ("Failed at type deduction of expresion: " ^ (pretty_expr e) ^ ". " ^ err)

(* Deduces types of variables defined in vars list given current variable environment. *)
let rec deduce_vars_types var_env vars = match vars with 
	| [] -> var_env
	| Var (id, e) :: tl -> let var_type = deduce_expr_type var_env e in
		deduce_vars_types (VarEnv.add id var_type var_env) tl
	| PtrVar (id, e) :: tl -> let var_type = TPtr (deduce_expr_type var_env e) in
		deduce_vars_types (VarEnv.add id var_type var_env) tl

(* Checks if command is well formed. *)
let check_command type_env function_env var_env c = 
	let rec check var_env c = match c with 
	 	| CSkip -> () 
	 	| CSeq (c1, c2) -> check var_env c1; check var_env c2;
	 	| CIf (e, c_true, c_false) -> 
	 		if extract_real_type type_env (deduce_expr_type var_env e) = TInt then
	 			(check var_env c_true; check var_env c_false)
	 		else failwith ("if condition" ^ (pretty_expr e) ^ " is not an int.")
	 	| CWhile (e, c) -> 
	 		if extract_real_type type_env (deduce_expr_type var_env e) = TInt 
	 		then check var_env c
	 		else failwith ("while condition" ^ (pretty_expr e) ^ " is not an int.")
	 	| CAssign (id, e) -> 
	 		if VarEnv.mem id var_env then
	 			let var_type = VarEnv.find id var_env in
	 			let expr_type = deduce_expr_type var_env e in
	 			if var_type <> expr_type then 
	 				failwith ("Type of expression and variable do not match at " ^ (pretty_command c) ^
	 				" variable type: " ^ (pretty_typ var_type) ^
	 				" expression type: " ^ (pretty_typ expr_type))
	 		else failwith ("Variable " ^ id ^ " is not defigned at " ^ (pretty_command c))
	 	| CPtrAssign(id, e) -> 
	 		if VarEnv.mem id var_env then
	 			let var_type = VarEnv.find id var_env in
	 			let expr_type = deduce_expr_type var_env e in
	 			if var_type <> (TPtr expr_type) then 
	 				failwith ("Type of expression and variable do not match at " ^ (pretty_command c) ^
	 				" variable type: " ^ (pretty_typ var_type) ^
	 				" expression type: " ^ (pretty_typ expr_type))
	 		else failwith ("Variable " ^ id ^ " is not defined at " ^ (pretty_command c))
	 	| CVar (id, e, c) -> let expr_type = deduce_expr_type var_env e in
	 		let extended_var_env = VarEnv.add id expr_type var_env in
	 		check extended_var_env c
	 	| CPtrVar (id, e, c) -> let expr_type = TPtr (deduce_expr_type var_env e) in
	 		let extended_var_env = VarEnv.add id expr_type var_env in
	 		check extended_var_env c
	 	| CCall (var_id, fun_id, args) -> 
	 		if not (VarEnv.mem var_id var_env) then 
	 			failwith ("Variable " ^ var_id ^ " is not defined at " ^ (pretty_command c))
	 		else
	 		if not (FunctionEnv.mem fun_id function_env) then 
	 			failwith ("Function " ^ fun_id ^ " is not defined at " ^ (pretty_command c))
	 		else
	 		let var_type = VarEnv.find var_id var_env in
	 		let fun_type = FunctionEnv.find fun_id function_env in
	 		let return_type = snd fun_type in
	 		if return_type <> var_type then 
	 			failwith ("Function return type and variable type do not match at " ^ (pretty_command c) ^
	 				" variable type: " ^ (pretty_typ var_type) ^
	 				" function return type: " ^ (pretty_typ return_type))
	 		else
	 		let args_expected_types = List.map snd (fst fun_type) in
	 		let args_actual_types = List.map (deduce_expr_type var_env) args in
	 		if args_expected_types <> args_actual_types then
	 			failwith ("Invalid argument types at " ^ (pretty_command c) ^
	 				"expected: " ^ (String.concat ", " (List.map pretty_typ args_expected_types)) ^
	 				" actual: " ^ (String.concat ", " (List.map pretty_typ args_actual_types)))
	 	| CSwitch (e, (id_left, c_left), (id_right, c_right)) ->
	 		(match extract_real_type type_env (deduce_expr_type var_env e) with
	 			| TSum (t1, t2) -> 
	 				let var_env_left = VarEnv.add id_left t1 var_env in
	 				let var_env_right = VarEnv.add id_right t2 var_env in
	 				check var_env_left c_left;
	 				check var_env_right c_right
	 			| t -> failwith ("Switch expression " ^ 
	 				(pretty_expr e) ^ " is type " ^ (pretty_typ t) ^  " and not of type sum."))
	 	| CAbort -> ()
	in
	check var_env c


(* 
	Checks if functions are defined correctly. 
	Their variables have types, commands are well formed and return expresion type is correct/
*)
let check_function_declarations type_env function_declarations =
	let function_env = FunctionEnv.of_seq 
		 (List.to_seq (List.map (fun (id, args, r_type, _, _, _) -> (id, (args, r_type))) function_declarations)) in
	let check_function_declaration (id, args, r_type, vars, c, e) = 
		let arg_types = VarEnv.of_seq (List.to_seq args) in
		let var_env = deduce_vars_types arg_types vars in
		check_command type_env function_env var_env c;
		let actual_r_type = deduce_expr_type var_env e in
		if actual_r_type <> r_type 
			then failwith ("invalid return type of function " ^ id ^ 
						   ". Is " ^ (pretty_typ actual_r_type) ^ " and should be " ^ (pretty_typ r_type) ^ ".")
	in
	List.iter check_function_declaration function_declarations;
	function_env



let typecheck (type_declarations, function_declarations, variable_declarations, command) = 
	check_type_declarations type_declarations;
	let type_env = TypeEnv.of_seq (List.to_seq type_declarations) in
	let function_env = check_function_declarations type_env function_declarations in
	let var_env = deduce_vars_types VarEnv.empty variable_declarations in
	check_command type_env function_env var_env command
