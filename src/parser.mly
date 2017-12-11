%{
  open Ast.Lang
  exception Unimplemented
%}

%token <string> VAR
%token EOF
%token ARROW
%token DOT
%token FN
%token PLUS
%token SUB
%token STAR
%token DIV
%token AND
%token OR
%token NOT
%token GT
%token GE
%token EQ
%token NE
%token LE
%token LT
%token LPAREN
%token RPAREN
%token COLON
%token IF
%token THEN
%token ELSE
%token TY_INT
%token TY_BOOL
%token <int> INT
%token <bool> BOOL

%left PLUS SUB
%left STAR DIV
%left AND OR
%right NOT
%right ARROW

%start <Ast.Lang.Term.t> main

%%

main:
| e = term EOF { e }

term:
| n = INT { Term.Int(n) }
| b = BOOL { Term.Bool(b) }
| v = VAR { Term.Var(v) }
| e1 = term b = binop e2 = term { Term.Binop(b, e1, e2) }
| e1 = term b = logop e2 = term { Term.Logop(b, e1, e2) }
| e1 = term b = comp e2 = term { Term.Comp(b, e1, e2) }
| b = NOT e = term { Term.Lognot(e) }
| FN LPAREN v = VAR COLON t = ty RPAREN DOT e = term { Term.Lam(v, t, e) }
| e1 = term e2 = term { Term.App(e1, e2) }
| IF cond = term THEN tt = term ELSE tf = term { Term.IfThenElse(cond, tt, tf) }
| LPAREN e = term RPAREN { e }

ty:
| v = VAR { Type.Var(v) }
| TY_INT { Type.Int }
| TY_BOOL { Type.Bool }
| t1 = ty ARROW t2 = ty { Type.Fn(t1, t2) }
| LPAREN t = ty RPAREN { t }

%inline binop:
| PLUS { Ast.Add }
| SUB { Ast.Sub }
| STAR { Ast.Mul }
| DIV { Ast.Div }

%inline logop:
| AND { Ast.And }
| OR { Ast.Or }

%inline comp:
| GT { Ast.Gt }
| GE { Ast.Ge }
| EQ { Ast.Eq }
| NE { Ast.Ne }
| LE { Ast.Le }
| LT { Ast.Lt }
