open Lexer

type error =
  | UnexpectedToken of token
  | UnexpectedEndOfInput
  | ExpectedToken of token list * token option

let error_to_string = function
  | UnexpectedToken token ->
      Printf.sprintf "Unexpected token: %s" (Lexer.token_to_string token)
  | UnexpectedEndOfInput -> "Unexpected end of input"
  | ExpectedToken (expected, got) ->
          Printf.sprintf "Expected one of the following tokens: [%s]; got: %s"
        (List.map token_to_string expected |> String.concat ", ") (match got with | Option.None -> "end of input" | Option.Some t -> token_to_string t)
