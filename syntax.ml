type type_signature = string

type typ = 
	TInt 
	| TUnit
	| TProd of typ * typ 
	| TVoid
	| TSum of typ * typ
	| TPtr of typ
	| TStruct of type_signature

type var = string
type function_name = string
type argument = var * typ

type expr = 
	EVar of var 
	| EInt of int
	| EUnit | EProd of expr * expr
	| EProjL of expr | EProjR of expr
	| EInjL of expr | EInjR of expr
	| EMatch of expr * (var * expr) * (var * expr)
	| EDeref of expr
	| ESum of expr * expr
	| EMult of expr * expr
	| ENeg of expr

type command = 
	CSkip 
	| CSeq of command * command
	| CIf of expr * command * command
	| CWhile of expr * command
	| CAssign of var * expr
	| CPtrAssign of var * expr
	| CVar of var * expr * command
	| CPtrVar of var * expr * command 
	| CCall of var * function_name * (expr list)

type vars_declarations = (var * expr) list
type function_declaration = function_name * (argument list) * typ * vars_declarations * command * expr
type type_declaration = type_signature * typ

type program = type_declaration list * function_declaration list * vars_declarations * command
