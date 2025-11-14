open Lexer

type error =
  | UnexpectedToken of token
  | UnexpectedEndOfInput
  | ExpectedToken of token list

let error_to_string = function
  | UnexpectedToken token ->
      Printf.sprintf "Unexpected token: %s" (Lexer.token_to_string token)
  | UnexpectedEndOfInput -> "Unexpected end of input"
  | ExpectedToken tokens ->
      Printf.sprintf "Expected one of the following tokens: [%s]"
        (List.map token_to_string tokens |> String.concat ", ")
