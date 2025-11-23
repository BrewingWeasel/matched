open Ast
open Lexer
open Parser_error
open Location

let ( let* ) = Result.bind

let get_binding_power token =
  match token with
  | TPlus -> Some (1, fun first second -> PMultiple [ first; second ])
  | TPipe -> Some (4, fun first second -> PEither (first, second))
  | _ -> None

let rec parse_binding_power tokens min_bp =
  match tokens with
  | [] -> Error UnexpectedEndOfInput
  | token :: rest ->
      let* left_expr, left_rest = parse_left token rest in
      parse_loop min_bp left_expr left_rest

and parse_left token rest =
  match token with
  | { value = TString s; _ } as loc ->
      Ok ({ loc with value = Ast.PLiteral s }, rest)
  | { value = TIdent name; _ } as loc ->
      Ok ({ loc with value = Ast.PVar name }, rest)
  | { value = TDollars; start_pos; _ } -> (
      match rest with
      | { value = TIdent name; end_pos; _ } :: after_ident ->
          Ok ({ value = Ast.PReference name; start_pos; end_pos }, after_ident)
      | _ ->
          Error
            (ExpectedToken
               ( [
                   {
                     token = TIdent "";
                     because =
                       Some
                         {
                           value = "To match this opening parenthesis";
                           start_pos;
                           end_pos = start_pos;
                         };
                   };
                 ],
                 List.nth_opt rest 0 )))
  | { value = TLParen; start_pos; _ } as lparen_loc -> (
      match parse_binding_power rest 0 with
      | Ok (pattern, next_tokens) -> (
          match next_tokens with
          | { value = TRParen; end_pos; _ } :: after_paren ->
              Ok ({ pattern with start_pos; end_pos }, after_paren)
          | _ ->
              Error
                (ExpectedToken
                   ( [
                       {
                         token = TRParen;
                         because =
                           Some
                             {
                               lparen_loc with
                               value = "To match this opening parenthesis";
                             };
                       };
                     ],
                     List.nth_opt next_tokens 0 )))
      | Error e -> Error e)
  | _ -> Error (UnexpectedToken token)

and parse_loop min_bp left remaining_tokens =
  match remaining_tokens with
  | [] -> Ok (left, [])
  | { value = TQuestionMark; end_pos; _ } :: rest ->
      parse_loop min_bp { left with value = Ast.POptional left; end_pos } rest
  | { value = TColon; end_pos = attr_start_pos; _ } :: rest ->
      let* attribute, remaining_tokens =
        match rest with
        | { value = TIdent attr_name; end_pos; _ } :: rest ->
            Ok
              ( {
                  value =
                    Ast.PWithAttribute
                      ( left,
                        {
                          value = attr_name;
                          start_pos = attr_start_pos;
                          end_pos;
                        } );
                  start_pos = left.start_pos;
                  end_pos;
                },
                rest )
        | _ ->
            Error
              (ExpectedToken
                 ( [
                     {
                       token = TIdent "";
                       because =
                         Some
                           {
                             value = "An attribute name is expected after ':'";
                             start_pos = attr_start_pos;
                             end_pos = attr_start_pos;
                           };
                     };
                   ],
                   List.nth_opt rest 0 ))
      in
      parse_loop min_bp attribute remaining_tokens
  | token :: rest -> (
      match get_binding_power token.value with
      | Some (bp, make_expr) when bp >= min_bp ->
          let* right_expr, right_rest = parse_binding_power rest bp in
          Ok
            ( {
                value = make_expr left right_expr;
                start_pos = left.start_pos;
                end_pos = right_expr.end_pos;
              },
              right_rest )
      | _ -> Ok (left, remaining_tokens))

let parse_pattern tokens = parse_binding_power tokens 0
