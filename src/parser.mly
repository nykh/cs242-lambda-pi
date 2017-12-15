%{
  open Ast.Lang
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
%token TY_STAR
%token LET
%token EQUAL
%token IN
%token <int> INT
%token <bool> BOOL

%left PLUS SUB
%left STAR DIV
%left AND OR
%right NOT
%right ARROW

%start <Ast.Lang.Expr.t> main

%%

main:
| e = expr EOF { e }

expr:
| n = INT { Expr.AInt(n) }
| b = BOOL { Expr.ABool(b) }
| v = VAR { Expr.Var(v) }
| TY_INT { Expr.Int }
| TY_BOOL { Expr.Bool }
| TY_STAR { Expr.Kind Ast.Star }
| t1 = expr ARROW t2 = expr { Expr.Fn(t1, t2) }
| e1 = expr b = binop e2 = expr { Expr.Binop(b, e1, e2) }
| e1 = expr b = logop e2 = expr { Expr.Logop(b, e1, e2) }
| e1 = expr b = comp e2 = expr { Expr.Comp(b, e1, e2) }
| NOT e = expr { Expr.Lognot(e) }
| FN LPAREN v = VAR COLON t = expr RPAREN DOT e = expr { Expr.Lam(v, t, e) }
| e1 = expr e2 = expr { Expr.App(e1, e2) }
| LET v = VAR COLON t = expr EQUAL e1 = expr IN e2 = expr { Expr.Let(v, t, e1, e2) }
| IF cond = expr THEN tt = expr ELSE tf = expr { Expr.IfThenElse(cond, tt, tf) }
| LPAREN e = expr RPAREN { e }

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
