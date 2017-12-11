open Core
open Result.Monad_infix
open Ast

let parse input =
  let filebuf = Lexing.from_string input in
  try (Ok (Parser.main Lexer.token filebuf)) with
  | Lexer.Error msg -> Error msg
  | Parser.Error -> Error (
    Printf.sprintf "Parse error: %d" (Lexing.lexeme_start filebuf))

let makelogger verbose msg =
  if verbose then print_endline msg
  else ()

let run filename verbose =
  let log = makelogger verbose in
  let input = In_channel.read_all filename in
  let result = parse input
    >>= fun term ->
    log (Printf.sprintf "Term: %s\n" (Lang.Term.to_string term));
    let term = Translator.translate term in
    log (Printf.sprintf "Translated: %s\n" (IR.Term.to_string term));
    Typechecker.typecheck term
    >>= fun ty ->
    log (Printf.sprintf "Type: %s\n" (IR.Type.to_string ty));
    Interpreter.eval term
  in
  match result with
  | Ok e -> Printf.printf "Success: %s\n" (IR.Term.to_string e)
  | Error s -> Printf.printf "Error: %s\n" s

let main () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"Simply-typed Lambda Calculus interpreter"
    [%map_open
      let filename =
        anon ("filename" %: string)
      and verbose =
        flag "verbose" no_arg ~doc:"Print intermediate result"
      in
      fun () -> run filename verbose
    ]
  |> Command.run

let () = main ()
