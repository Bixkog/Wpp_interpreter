open Syntax
open Pretty

type var_env = (var * typ) list
type type_env = (type_signature * typ) list

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
	check false t

let check_type_declarations type_declarations = 
	let declared_types = List.map fst type_declarations in
	let check_type_declaration user_types type_declaration = 
		let (t_id, t) = type_declaration in
		try check_user_type declared_types t; type_declaration :: user_types
		with Failure err ->  failwith ("invalid user type: " ^ (pretty_type_declaration type_declaration) ^ " cause: " ^ err)

	in
	List.fold_left check_type_declaration [] type_declarations 


let typecheck (type_declarations, function_declarations, variable_declarations, command) = 
	let user_types = check_type_declarations type_declarations in
	user_types
