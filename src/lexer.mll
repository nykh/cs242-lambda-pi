{
  open Parser
  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| '.' { DOT }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { PLUS }
| '-' { SUB }
| '*' { STAR }
| '/' { DIV }
| '&' { AND }
| '|' { OR }
| '~' { NOT }
| "fn" { FN }
| "int" { TY_INT }
| "bool" { TY_BOOL }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "->" { ARROW }
| ":" { COLON }
| "true" { BOOL (true) }
| "false" { BOOL (false) }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as v { VAR v }
| ['0'-'9']+ as i { INT (int_of_string i) }
| "(*" _* "*)" { token lexbuf }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
