{
  open Parser
  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| '.' { DOT }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| ".+" { VECTADD }
| "++" { VECTCAT }
| '+' { PLUS }
| '-' { SUB }
| '*' { STAR }
| '/' { DIV }
| '&' { AND }
| '|' { OR }
| '~' { NOT }
| '>' { GT }
| ">=" { GE }
| "==" { EQ }
| "<>" { NE }
| "<=" { LE }
| '<' { LT }
| "fn" { FN }
| "let" { LET }
| '=' { EQUAL }
| "in" { IN }
| "int" { TY_INT }
| "bool" { TY_BOOL }
| "star" { TY_STAR }
| "vec" { TY_VECT }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "->" { ARROW }
| "$" { DOLLAR }
| ":" { COLON }
| "true" { BOOL (true) }
| "false" { BOOL (false) }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as v { VAR v }
| ['0'-'9']+ as i { INT (int_of_string i) }
| "(*" _* "*)" { token lexbuf }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
