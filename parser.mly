%token <int> NUMBER
%token <string> ID

%token DOT BAR COMMA COL SEMICOL
%token IN RETURN
%token IF THEN ELSE
%token MATCH WITH SWITCH
%token SKIP WHILE DO
%token STAR
%token SUM NEG
%token ASSIGN EQUALS
%token UNIT VOID INT
%token LPAR RPAR
%token LTPAR RTPAR
%token OUTL OUTR INL INR ABORT
%token PTR
%token VARS VAR TYPE
%token NEW
%token DEBUG_PRINT
%token EOF

%nonassoc DOT ELSE COL DO IN 
%right OUTL OUTR INL INR PTR
%left NEG
%left STAR
%left SUM
%right SEMICOL

%start <Syntax.program> program

%%
    
program:
    | tds = type_declaration*; IN;
        fds = function_declaration*; IN; 
        vds = variable_declarations; IN;
        c = command; EOF 
        {(tds, fds, vds, c)}

type_declaration:
    | TYPE; type_signature = ID; EQUALS; type_definition = typ {(type_signature, type_definition)}

function_declaration:
    | function_id = ID; LPAR; args = separated_list(COMMA, typed_argument); RPAR; COL; return_type = typ; EQUALS; 
        function_variables = variable_declarations; IN;
        c = command; IN;
        RETURN; e = expr
        {(function_id, args, return_type, function_variables, c, e)}

variable_declarations:
    | VARS; vds = separated_list(COMMA, variable_declaration) {vds}

variable_declaration:
    | var_id = ID; ASSIGN; e = expr {Syntax.Var(var_id, e)}
    | var_id = ID; ASSIGN; NEW; e = expr {Syntax.PtrVar(var_id, e)}

command:
    | SKIP {Syntax.CSkip}
    | c1 = command; SEMICOL; c2 = command {Syntax.CSeq (c1, c2)}
    | IF; cond = expr; THEN; c_true = command; ELSE; c_false = command {Syntax.CIf (cond, c_true, c_false)}
    | WHILE; cond = expr; DO; c = command {Syntax.CWhile (cond, c)}
    | var_id = ID; ASSIGN; e = expr {Syntax.CAssign (var_id, e)}
    | STAR; var_id = ID; ASSIGN; e = expr {Syntax.CPtrAssign (var_id, e)}
    | VAR; var_id = ID; ASSIGN; e = expr; IN; c = command {Syntax.CVar (var_id, e, c)}
    | VAR; var_id = ID; ASSIGN; NEW; e = expr; IN; c = command {Syntax.CPtrVar (var_id, e, c)}
    | var_id = ID; ASSIGN; function_id = ID; LPAR; args = separated_list(COMMA, expr); RPAR 
        {Syntax.CCall (var_id, function_id, args)}
    | SWITCH; e = expr; INL; id_left = ID; COL; c_left = command; 
                        INR; id_right = ID; COL; c_right = command {Syntax.CSwitch (e, (id_left, c_left), (id_right, c_right))}
    | ABORT; {Syntax.CAbort}
    | DEBUG_PRINT; id = ID {Syntax.CDebugPrint(id)}
    | LPAR; c = command; RPAR {c}

abstr:
    | var_id = ID; DOT; e = expr {(var_id, e)}

expr:
    | number = NUMBER {Syntax.EInt(number)}
    | var_id = ID {Syntax.EVar(var_id)}
    | LTPAR; RTPAR; {Syntax.EUnit}
    | LTPAR; e1 = expr; COMMA; e2 = expr; RTPAR {Syntax.EPar (e1, e2)}
    | OUTL; e = expr {Syntax.EProjL (e)} | OUTR; e = expr {Syntax.EProjR (e)}
    | INL; e = expr {Syntax.EInjL (e)} | INR; e = expr {Syntax.EInjR (e)}
    | MATCH; e = expr; WITH; BAR; INL; a_left = abstr; BAR; INR; a_right = abstr {Syntax.EMatch (e, a_left, a_right)}
    | STAR; e = expr {Syntax.EDeref (e)}
    | e1 = expr; SUM; e2 = expr {Syntax.ESum(e1, e2)}
    | e1 = expr; STAR; e2 = expr {Syntax.EMult(e1, e2)}
    | NEG; e = expr {Syntax.ENeg (e)}
    | LPAR; e = expr; RPAR {e}
    | ABORT {EAbort}

typ:
    | INT {Syntax.TInt}
    | UNIT {Syntax.TUnit}
    | t1 = typ; STAR; t2 = typ {Syntax.TProd (t1, t2)}
    | VOID {Syntax.TVoid}
    | t1 = typ; SUM; t2 = typ {Syntax.TSum (t1, t2)}
    | PTR; t = typ {Syntax.TPtr (t)}
    | type_id = ID {Syntax.TStruct (type_id)}
    | LPAR; t = typ; RPAR {t}

typed_argument:
    | arg_id = ID; COL; t = typ {(arg_id, t)}