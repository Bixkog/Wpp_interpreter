open Syntax
open Pretty
open Value

let alloc v heap = match Heap.max_binding_opt heap with
	| Some (id, _) -> let new_id = id + 1 in
				(VPtr new_id, Heap.add new_id v heap)
	| None -> (VPtr 0, Heap.add 0 v heap)

let read id heap = match Heap.find_opt id heap with
	| Some v -> v
	| None -> failwith "Invalid pointer dereference."

let write id v heap = match Heap.find_opt id heap with
	| Some v -> Heap.add id v heap
	| None -> failwith "Invalid pointer dereference."

let rec eval_expr heap var_env e = 
	let rec eval e = match e with
		| EVar id -> Env.find id var_env
		| EInt n -> VInt n
		| EUnit -> VUnit
		| EPar (e1, e2) -> VPar (eval e1, eval e2)
		| EProjL e -> (match eval e with
			| VPar (v1, v2) -> v1
			| _ -> failwith ("Stuck at projection. " ^ (pretty_expr e) ^ "doesn't evaluate to pair."))
		| EProjR e -> (match eval e with
			| VPar (v1, v2) -> v2
			| _ -> failwith ("Stuck at projection. " ^ (pretty_expr e) ^ "doesn't evaluate to pair."))
		| EInjL e -> VInjL (eval e)
		| EInjR e -> VInjR (eval e)
		| EMatch (e, (id_left, e_left), (id_right, e_right)) -> (match eval e with
			| VInjL v -> eval_expr heap (Env.add id_left v var_env) e_left
			| VInjR v -> eval_expr heap (Env.add id_right v var_env) e_right
			| _ -> failwith ("Stuck at match. " ^ (pretty_expr e) ^ "doesn't evaluate to sum."))
		| EDeref e -> (match eval e with
			| VPtr v -> Heap.find v heap
			| _ -> failwith ("Stuck at dereferencion. " ^ (pretty_expr e) ^ "doesn't evaluate to pointer."))
		| ESum (e1, e2) -> (match (eval e1, eval e2) with
			| (VInt c1, VInt c2) -> VInt (c1 + c2)
			| _ -> failwith ("Stuck at sum. " ^ (pretty_expr e) ^ "doesn't evaluate to int."))
		| EMult (e1, e2) -> (match (eval e1, eval e2) with
			| (VInt c1, VInt c2) -> VInt (c1 * c2)
			| _ -> failwith ("Stuck at multiplication. " ^ (pretty_expr e) ^ "doesn't evaluate to int."))
		| ENeg e -> (match eval e with
			| VInt c -> VInt (-c)
			| _ -> failwith ("Stuck at negation. " ^ (pretty_expr e) ^ "doesn't evaluate to int."))
		| EAbort -> failwith "Abort"
	in
	eval e

let eval_vars heap vars_env vars = 
	let evaluate (heap, vars_env) var = match var with
		| Var (id, e) -> (heap, Env.add id (eval_expr heap vars_env e) vars_env)
		| PtrVar (id, e) -> let v = eval_expr heap vars_env e in
			let (alloc_ptr, heap') = alloc v heap in
			(heap', Env.add id alloc_ptr vars_env)
	in
	List.fold_left evaluate (heap, vars_env) vars


let rec eval_function_call fun_env heap f_id args_v = 
	let (args_ids, vars, c, e) = Env.find f_id fun_env in
	let args_env = Env.of_seq (List.to_seq (List.combine args_ids args_v)) in
	let (heap, var_env) = eval_vars heap args_env vars in
	let (heap', var_env') = eval_command fun_env heap var_env c in
	eval_expr heap var_env' e
and eval_command fun_env heap var_env c = 
	let rec eval heap var_env c = match c with
	| CSkip -> (heap, var_env)
	| CSeq (c1, c2) -> let (heap', var_env') = eval heap var_env c1 in
			  eval heap' var_env' c2
	| CIf (e, c_true, c_false) -> (match eval_expr heap var_env e with
		| VInt 0 -> eval heap var_env c_false
		| VInt n -> eval heap var_env c_true
		| _ -> failwith ("If condition" ^ (pretty_expr e) ^ " did not evaluate to int."))
	| CWhile (e, c') -> (match eval_expr heap var_env e with
		| VInt 0 -> (heap, var_env)
		| VInt n -> let (heap', var_env') = eval heap var_env c' in eval heap' var_env' c
		| _ -> failwith ("While condition" ^ (pretty_expr e) ^ " did not evaluate to int."))
	| CAssign (id, e) -> let var_env' = Env.add id (eval_expr heap var_env e) var_env in
		(heap, var_env')
	| CPtrAssign (id, e) -> let v = eval_expr heap var_env e in 
		(match Env.find id var_env with 
		| VPtr alloc_id -> let heap' = write alloc_id v heap in
			(heap', var_env)
		| _ -> failwith ("Variable " ^ id ^ "is not a pointer."))
	| CVar (id, e, c) -> 
		let local_var_env = Env.add id (eval_expr heap var_env e) var_env in 
		let (heap', var_env') = eval heap local_var_env c in
		let fixed_var_env = Env.remove id var_env' in
		(heap, fixed_var_env)
	| CPtrVar (id, e, c) -> let alloc_ptr, heap' = alloc (eval_expr heap var_env e) heap in
		let local_var_env = Env.add id alloc_ptr var_env in
		let (heap', var_env') = eval heap' local_var_env c in
		let fixed_var_env = Env.remove id var_env' in
		(heap', fixed_var_env)
	| CCall (v_id, f_id, args) -> let args_v = List.map (eval_expr heap var_env) args in
		let call_result = eval_function_call fun_env heap f_id args_v in
		(heap, Env.add v_id call_result var_env)
	| CSwitch (e, (id_left, c_left), (id_right, c_right)) -> 
		(match eval_expr heap var_env e with
		| VInjL v -> eval heap (Env.add id_left v var_env) c_left
		| VInjR v -> eval heap (Env.add id_right v var_env) c_right
		| _ -> failwith ("Switch condition " ^ (pretty_expr e) ^ " doesn't evaluate to sum."))
	| CAbort -> failwith "Abort"
	in 
	eval heap var_env c

let eval_program (_, function_declarations, variable_declarations, command) = 
	let fun_env = Env.of_seq (List.to_seq (
			List.map (fun (f_id, args, _, vars, c, e) -> (f_id, ((List.map fst args), vars, c, e)))
			function_declarations)) in
	let (heap, var_env) = eval_vars Heap.empty Env.empty variable_declarations in
	eval_command fun_env heap var_env command

