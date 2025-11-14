open Lexer
open Parser_error
open Ast

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
  | TString s -> Ok (Ast.EString s, rest)
  | TIdent name -> Ok (Ast.EVar name, rest)
  | TLParen -> (
      match parse_binding_power rest 0 with
      | Ok (pattern, next_tokens) -> (
          match next_tokens with
          | TRParen :: after_paren -> Ok (pattern, after_paren)
          | _ -> Error (ExpectedToken [ TRParen ]))
      | Error e -> Error e)
  | _ -> Error (UnexpectedToken token)

and parse_loop min_bp left remaining_tokens =
  match remaining_tokens with
  | [] -> Ok (left, [])
  | token :: rest -> (
      match get_binding_power token with
      | Some (bp, make_expr) when bp >= min_bp ->
          let* right_expr, right_rest = parse_binding_power rest bp in
          Ok (make_expr left right_expr, right_rest)
      | _ -> Ok (left, remaining_tokens))

let parse_expr tokens = parse_binding_power tokens 0

let rec parse_arguments tokens acc =
  let* arg_pattern, rest = Pattern_parser.parse_pattern tokens in
  match rest with
  | TComma :: after_comma -> parse_arguments after_comma (arg_pattern :: acc)
  | TRParen :: after_rparen -> Ok (List.rev (arg_pattern :: acc), after_rparen)
  | _ -> Error (ExpectedToken [ TComma; TRParen ])

let parse_definition tokens =
  match tokens with
  | TDef :: TIdent name :: TLParen :: rest -> (
      let* arguments, after_args = parse_arguments rest [] in
      match after_args with
      | TEquals :: expr_tokens ->
          let* body_expr, remaining_tokens = parse_expr expr_tokens in
          Ok
            ( DFunction (name, { arguments; expression = body_expr }),
              remaining_tokens )
      | _ -> Error (ExpectedToken [ TEquals ]))
  | TPattern :: TIdent name :: TEquals :: rest ->
      let* pattern, remaining_tokens = Pattern_parser.parse_pattern rest in
      Ok (DPattern (name, pattern), remaining_tokens)
  | _ -> Error (ExpectedToken [ TDef; TPattern ])

let parse_file tokens =
  let rec rec_parse_file tokens acc =
    if tokens = [] then Ok acc
    else
      let* definition, rest = parse_definition tokens in
      rec_parse_file rest (definition :: acc)
  in
  rec_parse_file tokens []
