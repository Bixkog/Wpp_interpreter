type value = 
	VInt of int 
	| VPtr of int
	| VInjL of value
	| VInjR of value
	| VPar of value * value
	| VUnit