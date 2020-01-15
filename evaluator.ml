open Syntax
open Pretty
open Value

let heap = Heap.create 100
let ref_count = Heap.create 100
let fun_env = ref Env.empty

let alloc = 
	let max_key = ref (-1) in
	let aux v = let id = max_key.contents + 1 in
		Heap.add heap id v;
		Heap.add ref_count id 1;
		max_key := id;
		VPtr id
	in
	aux

let read_heap_at id = match Heap.find_opt heap id with
	| Some v -> v
	| None -> failwith "Invalid pointer dereference."

let write_heap_at id v = match Heap.find_opt heap id with
	| Some _ -> Heap.replace heap id v
	| None -> failwith "Invalid pointer dereference."

let rec eval_expr var_env e = 
	let rec eval e = match e with
		| EVar id -> Env.find id var_env
		| EInt n -> VInt n
		| EUnit -> VUnit
		| EPar (e1, e2) -> VPar (eval e1, eval e2)
		| EProjL e -> (match eval e with
			| VPar (v1, v2) -> v1
			| _ -> failwith ("Stuck at projection. " ^ (pretty_expr e) ^ " doesn't evaluate to pair."))
		| EProjR e -> (match eval e with
			| VPar (v1, v2) -> v2
			| _ -> failwith ("Stuck at projection. " ^ (pretty_expr e) ^ " doesn't evaluate to pair."))
		| EInjL e -> VInjL (eval e)
		| EInjR e -> VInjR (eval e)
		| EMatch (e, (id_left, e_left), (id_right, e_right)) -> (match eval e with
			| VInjL v -> eval_expr (Env.add id_left v var_env) e_left
			| VInjR v -> eval_expr (Env.add id_right v var_env) e_right
			| _ -> failwith ("Stuck at match. " ^ (pretty_expr e) ^ " doesn't evaluate to sum."))
		| EDeref e -> (match eval e with
			| VPtr p -> read_heap_at p
			| _ -> failwith ("Stuck at dereferencion. " ^ (pretty_expr e) ^ " doesn't evaluate to pointer."))
		| ESum (e1, e2) -> (match (eval e1, eval e2) with
			| (VInt c1, VInt c2) -> VInt (c1 + c2)
			| _ -> failwith ("Stuck at sum. " ^ (pretty_expr e) ^ " doesn't evaluate to int."))
		| EMult (e1, e2) -> (match (eval e1, eval e2) with
			| (VInt c1, VInt c2) -> VInt (c1 * c2)
			| _ -> failwith ("Stuck at multiplication. " ^ (pretty_expr e) ^ " doesn't evaluate to int."))
		| ENeg e -> (match eval e with
			| VInt c -> VInt (-c)
			| _ -> failwith ("Stuck at negation. " ^ (pretty_expr e) ^ " doesn't evaluate to int."))
		| EAbort -> failwith "Abort"
	in
	eval e

let eval_vars vars_env vars = 
	let evaluate vars_env var = match var with
		| Var (id, e) -> Env.add id (eval_expr vars_env e) vars_env
		| PtrVar (id, e) -> let v = eval_expr vars_env e in
			let alloc_ptr = alloc v in
			Env.add id alloc_ptr vars_env
	in
	List.fold_left evaluate vars_env vars


let rec eval_function_call f_id args_v = 
	let (args_ids, vars, c, e) = Env.find f_id fun_env.contents in
	let args_env = Env.of_seq (List.to_seq (List.combine args_ids args_v)) in
	let var_env = eval_vars args_env vars in
	let var_env' = eval_command var_env c in
	eval_expr var_env' e
and eval_command var_env c = match c with
	| CSkip -> var_env
	| CSeq (c1, c2) -> let var_env' = eval_command var_env c1 in
			  eval_command var_env' c2
	| CIf (e, c_true, c_false) -> (match eval_expr var_env e with
		| VInt 0 -> eval_command var_env c_false
		| VInt n -> eval_command var_env c_true
		| _ -> failwith ("If condition" ^ (pretty_expr e) ^ " did not evaluate to int."))
	| CWhile (e, c') -> (match eval_expr var_env e with
		| VInt 0 -> var_env
		| VInt n -> let  var_env' = eval_command var_env c' in eval_command var_env' c
		| _ -> failwith ("While condition" ^ (pretty_expr e) ^ " did not evaluate to int."))
	| CAssign (id, e) -> Env.add id (eval_expr var_env e) var_env
	| CPtrAssign (id, e) -> let v = eval_expr var_env e in 
		(match Env.find id var_env with 
		| VPtr alloc_id ->  write_heap_at alloc_id v; var_env
		| _ -> failwith ("Variable " ^ id ^ " is not a pointer."))
	| CVar (id, e, c) -> 
		let local_var_env = Env.add id (eval_expr var_env e) var_env in 
		let var_env' = eval_command local_var_env c in
		let fixed_var_env = Env.remove id var_env' in
		fixed_var_env
	| CPtrVar (id, e, c) -> let alloc_ptr = alloc (eval_expr var_env e) in
		let local_var_env = Env.add id alloc_ptr var_env in
		let var_env' = eval_command local_var_env c in
		let fixed_var_env = Env.remove id var_env' in
		fixed_var_env
	| CCall (v_id, f_id, args) -> let args_v = List.map (eval_expr var_env) args in
		let call_result = eval_function_call f_id args_v in
		Env.add v_id call_result var_env
	| CSwitch (e, (id_left, c_left), (id_right, c_right)) -> 
		(match eval_expr var_env e with
		| VInjL v -> eval_command(Env.add id_left v var_env) c_left
		| VInjR v -> eval_command(Env.add id_right v var_env) c_right
		| _ -> failwith ("Switch condition " ^ (pretty_expr e) ^ " doesn't evaluate to sum."))
	| CDebugPrint s -> 
		print_endline ("-----" ^ s ^ "-----");
		print_endline "VARIABLES:";
		List.iter print_endline (pretty_env var_env);
		print_endline "HEAP:";
		List.iter print_endline (pretty_heap (Heap.to_seq heap));
		var_env
	| CAbort -> failwith "Abort"

let clear_global_env = 
	Heap.reset heap;
	Heap.reset ref_count;
	fun_env := Env.empty

let eval_program (_, function_declarations, variable_declarations, command) = 
	fun_env := Env.of_seq (List.to_seq (
			List.map (fun (f_id, args, _, vars, c, e) -> (f_id, ((List.map fst args), vars, c, e)))
			function_declarations));
	let var_env = eval_vars Env.empty variable_declarations in
	let output_var_env = eval_command var_env command in
	let output_heap = Heap.to_seq heap in
	clear_global_env;
	(output_heap, output_var_env)


