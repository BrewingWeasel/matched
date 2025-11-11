open Matched.Ast

let patterns = [ PVar "first"; PLiteral "then"; PVar "second" ]
let text = read_line ()

let () =
  match Matched.Interpreter.run_match patterns text with
  | Some { matched_variables } ->
      print_endline "->";
      Matched.Interpreter.VariableMap.iter
        (fun key v -> print_endline @@ key ^ ": " ^ v)
        matched_variables
  | _ -> print_endline "No match found"
