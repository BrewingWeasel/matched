open Lexer
open Location
open Display_error

type expected_token = { token : token; because : string located_span option }

type error =
  | UnexpectedToken of token located_span
  | UnexpectedEndOfInput
  | ExpectedToken of expected_token list * token located_span option

let error_to_string = function
  | UnexpectedToken token ->
      Printf.sprintf "Unexpected token: %s" (Lexer.token_to_string token.value)
  | UnexpectedEndOfInput -> "Unexpected end of input"
  | ExpectedToken (expected, _got) ->
      let expected =
        match expected with
        | [] -> "no expected tokens"
        | [ single ] -> token_to_string single.token
        | multiple ->
            let token_strings =
              List.map (fun e -> token_to_string e.token) multiple
            in
            String.concat "\", \"" token_strings
      in
      Printf.sprintf "Expected token: \"%s\"" expected

let convert_to_highlight expected_token =
  match expected_token.because with
  | Some because ->
      Some
        {
          error_level = Context;
          start_pos = because.start_pos;
          end_pos = because.end_pos;
          message = because.value;
        }
  | None -> None

let display error =
  let primary_error_spot, highlights =
    match error with
    | UnexpectedToken token ->
        ( token.start_pos,
          [
            {
              error_level = Error;
              start_pos = token.start_pos;
              end_pos = token.end_pos;
              message =
                Printf.sprintf "Token %s found"
                  (Lexer.token_to_string token.value);
            };
          ] )
    | UnexpectedEndOfInput -> (0, [])
    | ExpectedToken (expected, got) -> (
        let initial_highlights =
          List.filter_map convert_to_highlight expected
        in
        match got with
        | None -> (0, initial_highlights)
        | Some token ->
            ( token.start_pos,
              {
                error_level = Error;
                start_pos = token.start_pos;
                end_pos = token.end_pos;
                message =
                  Printf.sprintf "Instead found token \"%s\""
                    (Lexer.token_to_string token.value);
              }
              :: initial_highlights ))
  in
  Display_error.render_error
    {
      level = Error;
      message = error_to_string error;
      start_pos = primary_error_spot;
      highlights;
    }
