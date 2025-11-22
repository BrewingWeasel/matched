open Lexer
open Parser_error
open Ast
open Location

let ( let* ) = Result.bind

let get_binding_power token =
  match token with
  | TAmpersand -> Some (1, fun first second -> EConcat (first, second))
  | _ -> None

let rec parse_binding_power tokens min_bp =
  match tokens with
  | [] -> Error UnexpectedEndOfInput
  | token :: rest ->
      let* left_expr, left_rest = parse_left token rest in
      parse_loop min_bp left_expr left_rest

and parse_left token rest =
  match token with
  | { value = TString s; _ } as loc -> Ok ({ loc with value = EString s }, rest)
  | { value = TIdent name; _ } as loc ->
      Ok ({ loc with value = EVar name }, rest)
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
  | { value = token; _ } :: rest -> (
      match get_binding_power token with
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

let parse_expr tokens = parse_binding_power tokens 0

let rec parse_arguments tokens lparen_loc acc =
  let* arg_pattern, rest = Pattern_parser.parse_pattern tokens in
  match rest with
  | { value = TComma; _ } :: after_comma ->
      parse_arguments after_comma lparen_loc (arg_pattern :: acc)
  | { value = TRParen; end_pos; _ } :: after_rparen ->
      Ok (List.rev (arg_pattern :: acc), end_pos, after_rparen)
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
             List.nth_opt rest 0 ))

let parse_definition tokens =
  match tokens with
  | ({ value = TDef; start_pos; _ } as def_loc)
    :: ({ value = TIdent name; _ } as name_loc)
    :: ({ value = TLParen; start_pos = args_start; _ } as lparen_loc)
    :: rest -> (
      let* arguments, args_end, after_args =
        parse_arguments rest lparen_loc []
      in
      match after_args with
      | { value = TEquals; end_pos; _ } :: expr_tokens ->
          let* body_expr, remaining_tokens = parse_expr expr_tokens in
          let arguments =
            { value = arguments; start_pos = args_start; end_pos = args_end }
          in
          let definition =
            DFunction
              ( { name_loc with value = name },
                { arguments; expression = body_expr } )
          in
          Ok ({ value = definition; start_pos; end_pos }, remaining_tokens)
      | _ ->
          Error
            (ExpectedToken
               ( [
                   {
                     token = TEquals;
                     because =
                       Some
                         {
                           def_loc with
                           value = "To match the definition declared here";
                         };
                   };
                 ],
                 List.nth_opt after_args 0 )))
  | { value = TPattern; start_pos; _ }
    :: ({ value = TIdent name; _ } as name_loc)
    :: { value = TEquals; end_pos; _ }
    :: rest ->
      let* pattern, remaining_tokens = Pattern_parser.parse_pattern rest in
      let definition = DPattern ({ name_loc with value = name }, pattern) in
      Ok ({ value = definition; start_pos; end_pos }, remaining_tokens)
  | _ ->
      Error
        (ExpectedToken
           ( [
               { token = TDef; because = None };
               { token = TPattern; because = None };
             ],
             List.nth_opt tokens 0 ))

let parse_file tokens =
  let rec rec_parse_file tokens acc =
    if tokens = [] then Ok acc
    else
      let* definition, rest = parse_definition tokens in
      rec_parse_file rest (definition :: acc)
  in
  rec_parse_file tokens []
